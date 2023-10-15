mod libconsole;
mod libenv;
mod libio;
mod libps;
mod libstring;
mod libtime;

use crate::prelude::*;
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

pub fn load_libs(args: &[String], library: &Library) -> BTreeMap<String, Value> {
  let mut loaded_libs = BTreeMap::default();

  match library {
    Library::All => {
      for lib in all::<Lib>() {
        let (key, value) = load_lib(args, &lib);
        loaded_libs.insert(key.to_string(), value);
      }
    }
    Library::List(list) => {
      for lib in list {
        let (key, value) = load_lib(args, lib);
        loaded_libs.insert(key.to_string(), value);
      }
    }
    Library::None => (),
  }

  loaded_libs
}

fn load_lib(args: &[String], lib: &Lib) -> (&'static str, Value) {
  match lib {
    Lib::Std => ("std", load_std()),
    Lib::Env => ("env", LibEnv::load(args)),
    Lib::Time => ("time", LibTime::load()),
    Lib::String => ("str", LibString::load()),
    Lib::Console => ("console", LibConsole::load()),
    Lib::Ps => ("ps", LibPs::load()),
    Lib::Io => ("io", LibIo::load()),
  }
}

fn load_std() -> Value {
  LockedModule::initialize(|lib| {
    lib.set("debug", Value::native(debug)).ok();

    lib
      .set(
        "Object",
        LockedModule::initialize(|object_module| {
          object_module.set("fields", Value::native(fields)).ok();
        }),
      )
      .ok();
  })
  .into()
}

#[native]
fn debug(value: Value) -> ValueResult {
  Ok(Value::from(format!("{:?}", value)))
}

#[native]
fn fields(value: Value) -> ValueResult<Vec<Value>> {
  fn get_fields(s: &StructValue) -> Vec<Value> {
    s.members.keys().cloned().map(Value::from).collect::<Vec<Value>>()
  }

  let mut fields = Vec::default();

  if let Some(i) = value.as_instance() {
    fields.extend(get_fields(&i.data))
  } else if let Some(s) = value.as_struct() {
    fields.extend(get_fields(s))
  }

  Ok(fields)
}
