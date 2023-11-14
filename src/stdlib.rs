mod libconsole;
mod libio;
mod libps;
mod libstr;
mod libtime;

use crate::{prelude::*, value::prelude::module_value::ModuleType};
use ahash::RandomState;
use std::{
  collections::{BTreeMap, HashMap},
  env,
};

pub(crate) mod names {
  pub const STD: &str = "std";

  pub const DEBUG: &str = "debug";

  pub const OBJ: &str = "obj";

  pub const REFLECT: &str = "reflect";

  pub const ENV: &str = "env";
  pub(crate) mod env {
    pub const ARGV: &str = "ARGV";
    pub const PATHS_ENV_VAR: &str = "SS_LIBRARY_PATHS";
    pub const PATHS: &str = "paths";
    pub const PATH_SEPARATOR: char = ';';
  }

  pub const TIME: &str = "time";

  pub const STR: &str = "str";

  pub const CONSOLE: &str = "console";

  pub const PS: &str = "ps";

  pub const MATH: &str = "math";

  pub const IO: &str = "io";
}

pub fn enable_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> HashMap<String, Value, RandomState> {
  let mut loaded_libs = BTreeMap::default();

  loaded_libs.insert(names::STD, load_std(gc, gmod, args));

  loaded_libs.into_iter().map(|(k, v)| (k.into(), v.into())).collect()
}

fn defmod<F>(gc: &mut SmartPtr<Gc>, lib: &mut UsertypeHandle<ModuleValue>, name: &str, init: F)
where
  F: FnOnce(&mut SmartPtr<Gc>, UsertypeHandle<ModuleValue>),
{
  let libval = lib.value();
  lib.define(name, ModuleBuilder::initialize(gc, ModuleType::new_child(name, libval), init));
}

fn load_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> UsertypeHandle<ModuleValue> {
  ModuleBuilder::initialize(gc, ModuleType::new_child(names::STD, gmod), |gc, mut lib| {
    let lib = &mut lib;

    lib.define(names::DEBUG, Value::native(debug));

    defmod(gc, lib, names::OBJ, |_, _lib| {
      // lib.define("fields", Value::native(fields));
    });

    defmod(gc, lib, names::REFLECT, |_, mut lib| {
      lib.define("defined", Value::native(defined));
      lib.define("disasm", Value::native(disasm));
    });

    defmod(gc, lib, names::ENV, |gc, mut lib| {
      let args = args.iter().map(|arg| gc.allocate(arg.clone())).collect::<Vec<Value>>();
      let args = gc.allocate(args);
      lib.define(names::env::ARGV, args);

      let mut lib_paths = Vec::default();

      if let Ok(paths) = env::var(names::env::PATHS_ENV_VAR) {
        lib_paths.extend(paths.split_terminator(names::env::PATH_SEPARATOR).map(|v| gc.allocate(v)));
      }

      lib.define(names::env::PATHS, gc.allocate(lib_paths));
    });

    defmod(gc, lib, names::TIME, |gc, mut lib| {
      defmod(gc, &mut lib, "mono", libtime::mono);
    });

    defmod(gc, lib, names::STR, libstr::string);

    defmod(gc, lib, names::CONSOLE, libconsole::console);

    defmod(gc, lib, names::PS, libps::ps);

    defmod(gc, lib, names::MATH, |_, mut lib| {
      lib.define("rand_i32", Value::native(math_rand_i32));
      lib.define("abs", Value::native(math_abs));
    });

    let libval = lib.value();
    lib.define(names::IO, libio::simple_script_autogen_create_module(gc, libval));
  })
}

#[native]
fn debug(value: Value) -> UsageResult<String> {
  Ok(format!("{:?}", value))
}

// #[native]
// fn fields(value: Value) -> UsageResult<Vec<StringValue>> {
//   fn get_fields(s: &StructValue) -> Vec<StringValue> {
//     s.members.keys().cloned().map(StringValue::from).collect()
//   }

//   let mut fields = Vec::default();

//   if let Some(i) = value.as_instance() {
//     fields.extend(get_fields(&i.data))
//   } else if let Some(s) = value.as_struct() {
//     fields.extend(get_fields(s))
//   }

//   Ok(fields)
// }

#[native(with_vm)]
fn defined(vm: &mut Vm, name: &StringValue) -> UsageResult<bool> {
  Ok(vm.current_env().lookup(name.as_str()).is_some())
}

#[native(with_vm)]
fn disasm(vm: &mut Vm, value: Value) -> UsageResult<String> {
  if let Some(f) = value.cast_to::<FunctionValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.program))
  } else if let Some(f) = value.cast_to::<ClosureValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.program))
  } else if let Some(f) = value.cast_to::<MethodValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.program))
  } else {
    Ok(String::new())
  }
}

#[native]
fn math_abs(arg: (Option<i32>, Option<f64>)) -> UsageResult {
  match arg {
    (Some(i), None) => Ok(Value::from(i.abs())),
    (None, Some(f)) => Ok(Value::from(f.abs())),
    _ => Err(UsageError::Infallible)?,
  }
}

#[native]
fn math_rand_i32() -> UsageResult<i32> {
  let val = rand::random();
  Ok(val)
}
