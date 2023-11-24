mod libconsole;
mod libio;
mod libps;
mod libstr;
mod libtime;

use crate::{
  prelude::*,
  value::{prelude::module_value::ModuleType, NATIVE_FN_TAG},
  FastHashMap,
};
use std::{collections::BTreeMap, env};

pub(crate) mod names {
  pub const STD: &str = "std";

  pub const TYPES: &str = "types";

  pub const DEBUG: &str = "debug";

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

  pub const VM: &str = "vm";
  pub(crate) mod vm {
    pub const GC: &str = "gc";
  }
}

pub fn enable_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> FastHashMap<String, Value> {
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

    defmod(gc, lib, names::TYPES, |gc, mut lib| {
      lib.define("i32", Value::new::<i32>(Default::default()));
      lib.define("f64", Value::new::<f64>(Default::default()));
      lib.define("char", Value::new::<char>(Default::default()));
      lib.define("bool", Value::new::<bool>(Default::default()));
      lib.define("native", Value { bits: NATIVE_FN_TAG });
      lib.define("str", gc.allocate(IdValue::new(StringValue::ID)));
    });

    lib.define(names::DEBUG, Value::new::<NativeFn>(debug));

    defmod(gc, lib, names::REFLECT, |_, mut lib| {
      lib.define("disasm", Value::new::<NativeFn>(disasm));
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
      lib.define("rand_i32", Value::new::<NativeFn>(math_rand_i32));
      lib.define("abs", Value::new::<NativeFn>(math_abs));
    });

    let libval = lib.value();
    lib.define(names::IO, libio::duck_type_autogen_create_module(gc, libval));

    defmod(gc, lib, names::VM, |gc, mut lib| {
      defmod(gc, &mut lib, names::vm::GC, |_, mut lib| {
        lib.define("total_cycles", Value::new::<NativeFn>(total_cycles));
        lib.define("print_stats", Value::new::<NativeFn>(print_stats));
      });
    });
  })
}

#[native]
fn debug(value: Value) -> UsageResult<String> {
  Ok(format!("{:?}", value))
}

#[native(with_vm)]
fn disasm(vm: &mut Vm, value: Value) -> UsageResult<String> {
  if let Some(f) = value.cast_to::<FunctionValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
  } else if let Some(f) = value.cast_to::<ClosureValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
  } else if let Some(f) = value.cast_to::<MethodValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
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

#[native(with_vm)]
fn total_cycles(vm: &mut Vm) -> UsageResult<i32> {
  Ok(vm.gc.num_cycles as i32)
}

#[native(with_vm)]
fn print_stats(vm: &mut Vm) -> UsageResult<()> {
  println!(
    "{}",
    itertools::join(
      [
        "------ Gc Stats ------",
        &format!("No. cycles ------------- {}", vm.gc.num_cycles),
        &format!("No. allocations -------- {}", vm.gc.allocations.len()),
        &format!("No. handles ------------ {}", vm.gc.native_handles.len()),
        &format!("Memory in use ---------- {}", vm.gc.allocated_memory),
        &format!("Limit till next cycle -- {}", vm.gc.limit),
      ],
      "\n"
    )
  );
  Ok(())
}
