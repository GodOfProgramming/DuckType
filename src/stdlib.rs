use crate::prelude::*;
use enum_iterator::{all, Sequence};
use libconsole::LibConsole;
use libenv::LibEnv;
use libps::LibPs;
use libstring::LibString;
use libtime::LibTime;
use std::collections::BTreeMap;

mod libconsole;
mod libenv;
mod libps;
mod libstring;
mod libtime;

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
  }
}

fn load_std() -> Value {
  let mut lib = StructValue::default();

  lib.set(
    "debug",
    Value::native(|_vm, _env, args| Ok(Value::from(format!("{:?}", args.list.first().unwrap_or(&Value::nil))))),
  );

  // Structs
  {
    let mut object = ClassValue::new("Object");

    object.set_static(
      "fields",
      Value::native(|_vm, _env, args| {
        let mut args = args.list.into_iter();
        let mut fields = Vec::default();

        if let Some(obj) = args.next() {
          let get_fields = |s: &StructValue| s.members.keys().cloned().map(Value::from).collect::<Vec<Value>>();

          if let Some(i) = obj.as_instance() {
            fields.extend(get_fields(&i.data))
          } else if let Some(s) = obj.as_struct() {
            fields.extend(get_fields(s))
          }
        }

        Ok(Value::from(fields))
      }),
    );

    lib.set("Object", Value::from(object));
  }

  Value::from(lib)
}
