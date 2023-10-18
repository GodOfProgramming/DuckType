mod libconsole;
mod libenv;
mod libio;
mod libps;
mod libstring;
mod libtime;

use crate::{
  memory::{Allocation, Gc},
  prelude::*,
};
use enum_iterator::{all, Sequence};
use libconsole::LibConsole;
use libenv::LibEnv;
use libio::LibIo;
use libps::LibPs;
use libstring::LibString;
use libtime::LibTime;
use std::collections::BTreeMap;

pub mod prelude {
  pub use super::{Lib, Library};
}

#[derive(Clone, Sequence)]
pub enum Lib {
  Std,
  Env,
  Time,
  String,
  Console,
  Ps,
  Io,
}

#[derive(Clone)]
pub enum Library {
  All,
  None,
  List(Vec<Lib>),
}

impl Default for Library {
  fn default() -> Self {
    Self::List(Default::default())
  }
}

pub fn load_libs(gc: &mut Gc, args: &[String], library: &Library) -> BTreeMap<String, Value> {
  let mut loaded_libs = BTreeMap::default();

  match library {
    Library::All => {
      for lib in all::<Lib>() {
        let (key, value) = load_lib(gc, args, &lib);
        loaded_libs.insert(key.to_string(), value);
      }
    }
    Library::List(list) => {
      for lib in list {
        let (key, value) = load_lib(gc, args, lib);
        loaded_libs.insert(key.to_string(), value);
      }
    }
    Library::None => (),
  }

  loaded_libs
}

fn load_lib(gc: &mut Gc, args: &[String], lib: &Lib) -> (&'static str, Value) {
  match lib {
    Lib::Std => ("std", load_std(gc)),
    Lib::Env => ("env", LibEnv::load(gc, args)),
    Lib::Time => ("time", LibTime::load(gc)),
    Lib::String => ("str", LibString::load(gc)),
    Lib::Console => ("console", LibConsole::load(gc)),
    Lib::Ps => ("ps", LibPs::load(gc)),
    Lib::Io => ("io", LibIo::load(gc)),
  }
}

fn load_std(gc: &mut Gc) -> Value {
  LockedModule::initialize(gc, |gc, lib| {
    lib.set(gc, "debug", Value::native(debug)).ok();
    let object = LockedModule::initialize(gc, |gc, object_module| {
      object_module.set(gc, "fields", Value::native(fields)).ok();
    });
    lib.set(gc, "Object", object).ok();
    let reflect = LockedModule::initialize(gc, |gc, lib| {
      lib.set(gc, "defined", Value::native(defined)).ok();
    });
    lib.set(gc, "reflect", reflect).ok();
  })
}

#[native]
fn debug(vm: &mut Vm, value: Value) -> ValueResult {
  Ok(vm.gc.allocate(format!("{:?}", value)))
}

#[native]
fn fields(vm: &mut Vm, value: Value) -> ValueResult<Vec<Value>> {
  fn get_fields(vm: &mut Vm, s: &StructValue) -> Vec<Value> {
    s.members.keys().cloned().map(|v| vm.gc.allocate(v)).collect::<Vec<Value>>()
  }

  let mut fields = Vec::default();

  if let Some(i) = value.as_instance() {
    fields.extend(get_fields(vm, &i.data))
  } else if let Some(s) = value.as_struct() {
    fields.extend(get_fields(vm, s))
  }

  Ok(fields)
}

fn defined(vm: &mut Vm, args: Args) -> ValueResult {
  let name: &StringValue = args.into_iter().next_arg().try_unwrap_arg("defined", 0)?;
  Ok(vm.env().lookup(name.as_str()).is_some().into())
}
