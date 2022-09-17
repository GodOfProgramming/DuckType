use std::alloc::System;

use super::{Class, New, Struct, Value};
use ptr::SmartPtr;

pub enum Library {
  Std,
  Env,
  Time,
  String,
  Console,
  Ps,
}

pub fn load_lib(args: &[String], lib: &Library) -> (&'static str, Value) {
  match lib {
    Library::Std => ("std", load_std()),
    Library::Env => ("env", load_env(args)),
    Library::Time => ("time", load_time()),
    Library::String => ("str", load_string()),
    Library::Console => ("console", load_console()),
    Library::Ps => ("ps", load_ps()),
  }
}

fn load_std() -> Value {
  let mut obj = Struct::default();

  // Arrays
  {
    let mut array = Class::new("Array");

    array.set_static_fn("len", |_thread, _env, args| {
      if !args.is_empty() {
        let obj = &args[0];
        match obj {
          Value::List(list) => Ok(Value::new(list.len() as f64)),
          v => return Err(format!("unable to get length of object {}", v)),
        }
      } else {
        Err("no object to get length of".to_string())
      }
    });

    obj.set("Array", Value::new(array));
  }

  // Vectors
  {
    let mut vec = Class::new("Vec");

    vec.set_initializer(Value::native("Vec.new", |_thread, _env, mut args| {
      let values = Value::new(args.drain(1..).collect::<Vec<Value>>());
      let this = args.get(0).cloned().unwrap();
      if let Value::Instance(mut instance) = this.clone() {
        instance.set("_buffer", values);
        Ok(this)
      } else {
        Err(format!("self not instance type {} (logic error)", this))
      }
    }));

    vec.set_method_fn("push", |_thread, _env, mut args| {
      if args.len() > 1 {
        let this = args.get(0).cloned().unwrap();
        let rest = args.drain(1..);

        match this {
          Value::Instance(mut instance) => match &mut instance.get("_buffer") {
            Value::List(list) => list.extend(rest.into_iter()),
            v => return Err(format!("somehow called push on non array type {}", v)),
          },
          v => return Err(format!("somehow called push on a primitive type {}", v)),
        }
      }

      Ok(Value::Nil)
    });

    vec.set_method_fn("__index__", |_thread, _env, mut args| {
      if args.len() == 2 {
        let value = args.swap_remove(1);
        let this = args.get_mut(0).unwrap();

        if let Value::Instance(this) = this {
          if let Value::List(this) = &this.get("_buffer") {
            if let Value::Num(n) = value {
              Ok(this[n as usize].clone())
            } else {
              Ok(Value::Nil)
            }
          } else {
            Err(String::from(
              "somehow have non-list as internal data for vector type",
            ))
          }
        } else {
          Err(String::from(
            "index method called with self not pointing to class instance",
          ))
        }
      } else {
        Err(String::from("invalid number of arguments for index"))
      }
    });

    vec.set_method_fn("len", |_thread, _env, args| {
      let this = args.get(0).unwrap();
      match this {
        Value::Instance(instance) => match instance.get("_buffer") {
          Value::List(list) => Ok(Value::new(list.len() as f64)),
          c => Err(format!("somehow called len on non instance of vec {}", c)),
        },
        c => Err(format!(
          "somehow called index method for non array instance {}",
          c
        )),
      }
    });

    vec.set_static_fn("push", |_thread, _env, mut args| {
      if args.len() > 1 {
        let this = args.get(0).cloned().unwrap();
        let rest = args.drain(1..);

        match this {
          Value::Instance(instance) => match instance.get("_buffer") {
            Value::List(mut list) => list.extend(rest.into_iter()),
            v => return Err(format!("called push on non array type {}", v)),
          },
          v => return Err(format!("called push on a primitive type {}", v)),
        }
      }
      Ok(Value::Nil)
    });

    obj.set("Vec", Value::new(vec));
  }

  // Structs
  {
    let mut object = Class::new("Object");

    object.set_static_fn("fields", |_thread, _env, args| {
      let obj = args.get(0).unwrap();
      let mut fields = Vec::default();

      let get_fields = |s: &Struct| {
        s.members
          .keys()
          .cloned()
          .map(|k| Value::new(k))
          .collect::<Vec<Value>>()
      };

      match obj {
        Value::Instance(i) => {
          fields.extend(get_fields(&i.data));
        }
        Value::Struct(s) => {
          fields.extend(get_fields(&s));
        }
        _ => (),
      }

      Ok(Value::new(fields))
    });

    obj.set("Object", Value::new(object));
  }

  Value::new(obj)
}

fn load_env(args: &[String]) -> Value {
  let mut obj = Struct::default();
  obj.set(
    "ARGV",
    Value::new(
      args
        .iter()
        .map(|arg| Value::new(arg.clone()))
        .collect::<Vec<Value>>(),
    ),
  );
  Value::new(obj)
}

fn load_time() -> Value {
  use std::time::Instant;

  let mut obj = Struct::default();

  // monotonic
  {
    let mut mono = Class::new("Monotonic");

    mono.set_static_fn("now", |_thread, _env, _args: Vec<Value>| {
      Ok(Value::new(Instant::now()))
    });

    mono.set_static_fn("elapsed", |_thread, _env, args: Vec<Value>| {
      let now = Instant::now();
      if let Some(Value::Instant(before)) = args.get(0) {
        let since = now.duration_since(before.clone());
        return Ok(Value::new(since.as_secs_f64()));
      }
      Err(String::from(
        "clock_diff called with wrong number of arguments or invalid types",
      ))
    });

    obj.set("Monotonic", Value::new(mono));
  }

  Value::new(obj)
}

fn load_string() -> Value {
  let mut obj = Struct::default();

  let parse_number = Value::native(
    String::from("parse_number"),
    |_thread, _env, args: Vec<Value>| {
      if let Some(arg) = args.get(0) {
        match arg {
          Value::String(string) => Ok(Value::new(
            string.parse::<f64>().map_err(|e| format!("{}", e))?,
          )),
          v => Err(format!("can not convert {} to a number", v)),
        }
      } else {
        Err(String::from("expected 1 argument"))
      }
    },
  );

  obj.set("parse_number", parse_number);

  Value::new(obj)
}

fn load_console() -> Value {
  let mut obj = Struct::default();

  let write = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
    for arg in args {
      print!("{}", arg);
    }

    Ok(Value::Nil)
  });

  let writeln = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
    for arg in args {
      print!("{}", arg);
    }
    println!();

    Ok(Value::Nil)
  });

  obj.set("write", write);
  obj.set("writeln", writeln);

  Value::new(obj)
}

fn load_ps() -> Value {
  let mut obj = Struct::default();

  let exit = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
    let exit_code = args
      .get(0)
      .map(|v| match v {
        Value::Num(n) => *n as i32,
        _ => 0,
      })
      .unwrap_or(0);

    std::process::exit(exit_code);
  });

  obj.set("exit", exit);

  Value::new(obj)
}
