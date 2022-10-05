use crate::Array;

use super::{ClassValue, ComplexValue, StructValue, Value};
use enum_iterator::{all, Sequence};
use libconsole::LibConsole;
use libenv::LibEnv;
use libps::LibPs;
use libstring::LibString;
use libtime::LibTime;
use std::{collections::BTreeMap, iter::FromIterator, ops::Index};

mod libconsole;
mod libenv;
mod libps;
mod libstring;
mod libtime;

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
  }

  loaded_libs
}

fn load_lib(args: &[String], lib: &Lib) -> (&'static str, Value) {
  match lib {
    Lib::Std => ("basic", load_std()),
    Lib::Env => ("env", LibEnv::load(args)),
    Lib::Time => ("time", LibTime::load()),
    Lib::String => ("str", LibString::load()),
    Lib::Console => ("console", LibConsole::load()),
    Lib::Ps => ("ps", LibPs::load()),
  }
}

fn load_std() -> Value {
  let mut lib = StructValue::default();

  // Arrays
  {
    let mut array = ClassValue::new("Array");

    array.set_static(
      "len",
      Value::new_native_closure("Array.len", |_thread, _env, args| {
        let mut args = args.list.into_iter();
        if let Some(mut arr) = args.next() {
          if let Ok(arr) = arr.as_array() {
            Value::from(arr.len() as i32)
          } else {
            Value::new_err("cannot compute length of non array")
          }
        } else {
          Value::new_err("cannot compute length of nothing")
        }
      }),
    );

    lib.set("Array", Value::from(array));
  }

  // Vectors
  {
    let mut vec = ClassValue::new("Vec");

    vec.set_native_constructor(|_thread, _env, args| {
      if let Some(mut this) = args.this {
        if let Ok(instance) = this.as_instance_mut() {
          let values = Value::from(args.list);
          instance.set("_buffer", values);
          this
        } else {
          Value::new_err(format!("self not instance type {} (logic error)", this))
        }
      } else {
        Value::new_err("no class instance to pass into constructor")
      }
    });

    vec.set_native_method("push", |_thread, _env, args| {
      let mut args = args.list.into_iter();
      if let Some(mut this) = args.next() {
        if let Ok(this) = this.as_instance_mut() {
          let mut buff = this.get("_buffer");
          if let Ok(buff) = buff.as_array_mut() {
            buff.extend(args);
          } else if buff.is_nil() {
            this.set("_buffer", Array::from(Vec::from_iter(args)).into());
          }
        }
      }

      Value::nil
    });

    vec.set_native_method("__index__", |_thread, _env, args| {
      let mut args = args.list.into_iter();
      if let Some(this) = args.next() {
        if let Some(value) = args.next() {
          if let Ok(mut value) = value.as_i32() {
            let mut buff = this.get("_buffer");
            if let Ok(arr) = buff.as_array() {
              if value < 0 {
                value = arr.len() as i32 - value;
              }
              arr[value as usize].clone()
            } else {
              Value::new_err("Vec.__index__ called on object that is not a Vec")
            }
          } else {
            Value::nil
          }
        } else {
          Value::new_err("index called without index")
        }
      } else {
        Value::new_err("index called without self")
      }
    });

    vec.set_native_method("len", |_thread, _env, args| {
      if let Some(this) = args.this {
        let mut buff = this.get("_buffer");
        if let Ok(arr) = buff.as_array() {
          Value::from(arr.len() as i32)
        } else {
          Value::new_err("buffer is not array")
        }
      } else {
        Value::new_err("len called without self")
      }
    });

    lib.set("Vec", Value::from(vec));
  }

  // Structs
  {
    let mut object = ClassValue::new("Object");

    object.set_native_static("fields", |_thread, _env, args| {
      let mut args = args.list.into_iter();
      let mut fields = Vec::default();

      if let Some(mut obj) = args.next() {
        let get_fields = |s: &StructValue| {
          s.members
            .keys()
            .cloned()
            .map(|k| Value::from(k))
            .collect::<Vec<Value>>()
        };

        if let Ok(i) = obj.as_instance() {
          fields.extend(get_fields(&i.data))
        } else if let Ok(s) = obj.as_struct() {
          fields.extend(get_fields(&s))
        }
      }

      Value::from(fields)
    });

    lib.set("Object", Value::from(object));
  }

  Value::from(lib)
}
