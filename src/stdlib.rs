mod libconsole;
mod libio;
mod libps;
mod libstr;
mod libtime;

use crate::prelude::*;
use std::collections::BTreeMap;

pub fn enable_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> BTreeMap<String, Value> {
  let mut loaded_libs = BTreeMap::default();

  loaded_libs.insert("std", load_std(gc, gmod, args));

  loaded_libs.into_iter().map(|(k, v)| (k.into(), v.into())).collect()
}

fn defmod<F>(gc: &mut SmartPtr<Gc>, lib: &mut UsertypeHandle<ModuleValue>, name: &str, init: F)
where
  F: FnOnce(&mut SmartPtr<Gc>, UsertypeHandle<ModuleValue>),
{
  let libval = lib.value();
  lib.define(name, ModuleBuilder::initialize(gc, name, Some(libval), init));
}

fn load_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> UsertypeHandle<ModuleValue> {
  ModuleBuilder::initialize(gc, "std", Some(gmod), |gc, mut lib| {
    let lib = &mut lib;

    lib.define("debug", Value::native(debug));

    defmod(gc, lib, "obj", |_, _lib| {
      // lib.define("fields", Value::native(fields));
    });

    defmod(gc, lib, "reflect", |_, mut lib| {
      lib.define("defined", Value::native(defined));
    });

    defmod(gc, lib, "env", |gc, mut lib| {
      let args = args.iter().map(|arg| gc.allocate(arg.clone())).collect::<Vec<Value>>();
      let args = gc.allocate(args);
      lib.define("ARGV", args);
    });

    defmod(gc, lib, "time", |gc, mut lib| {
      defmod(gc, &mut lib, "mono", libtime::mono);
    });

    defmod(gc, lib, "str", libstr::string);

    defmod(gc, lib, "console", libconsole::console);

    defmod(gc, lib, "ps", libps::ps);

    defmod(gc, lib, "math", |_, mut lib| {
      lib.define("rand_i32", Value::native(math_rand_i32));
      lib.define("abs", Value::native(math_abs));
    });

    let libval = lib.value();
    lib.define("io", libio::simple_script_autogen_create_module(gc, libval));
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
