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

fn load_std(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> UsertypeHandle<ModuleValue> {
  ModuleBuilder::initialize(gc, Some(gmod), |gc, mut lib| {
    let libval = lib.value();

    lib.define("debug", Value::native(debug));

    lib.define(
      "obj",
      ModuleBuilder::initialize(gc, Some(libval.clone()), |_, mut lib| {
        lib.define("fields", Value::native(fields));
      }),
    );

    lib.define(
      "reflect",
      ModuleBuilder::initialize(gc, Some(libval.clone()), |_, mut lib| {
        lib.define("defined", Value::native(defined));
      }),
    );

    lib.define(
      "env",
      ModuleBuilder::initialize(gc, Some(libval.clone()), |gc, mut lib| {
        let args = args.iter().map(|arg| gc.allocate(arg.clone())).collect::<Vec<Value>>();
        let args = gc.allocate(args);
        lib.define("ARGV", args);
      }),
    );

    lib.define(
      "time",
      ModuleBuilder::initialize(gc, Some(libval.clone()), |gc, mut lib| {
        let mono = ModuleBuilder::initialize(gc, Some(libval.clone()), libtime::mono);
        lib.define("mono", mono);
      }),
    );

    lib.define("str", ModuleBuilder::initialize(gc, Some(libval.clone()), libstr::string));

    lib.define(
      "console",
      ModuleBuilder::initialize(gc, Some(libval.clone()), libconsole::console),
    );

    lib.define("ps", ModuleBuilder::initialize(gc, Some(libval.clone()), libps::ps));

    lib.define("io", libio::simple_script_autogen_create_module(gc, libval.clone()));

    lib.define(
      "math",
      ModuleBuilder::initialize(gc, Some(libval.clone()), |_, mut lib| {
        lib.define("rand_i32", Value::native(rand_i32));
      }),
    );
  })
}

#[native]
fn debug(value: Value) -> ValueResult<String> {
  Ok(format!("{:?}", value))
}

#[native]
fn fields(value: Value) -> ValueResult<Vec<StringValue>> {
  fn get_fields(s: &StructValue) -> Vec<StringValue> {
    s.members.keys().cloned().map(StringValue::from).collect()
  }

  let mut fields = Vec::default();

  if let Some(i) = value.as_instance() {
    fields.extend(get_fields(&i.data))
  } else if let Some(s) = value.as_struct() {
    fields.extend(get_fields(s))
  }

  Ok(fields)
}

#[native(with_vm)]
fn defined(vm: &mut Vm, name: &StringValue) -> ValueResult<bool> {
  Ok(vm.current_env().lookup(name.as_str()).is_some())
}

#[native]
fn rand_i32() -> ValueResult<i32> {
  let val = rand::random();
  Ok(val)
}
