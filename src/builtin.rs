use super::{Class, New, Struct, Value};
use enum_iterator::{all, Sequence};
use std::collections::BTreeMap;

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
    Lib::Std => ("std", load_std()),
    Lib::Env => ("env", load_env(args)),
    Lib::Time => ("time", load_time()),
    Lib::String => ("str", load_string()),
    Lib::Console => ("console", load_console()),
    Lib::Ps => ("ps", load_ps()),
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

  let parse_number = Value::native("parse_number", |_thread, _env, args: Vec<Value>| {
    let mut args = args.into_iter();
    if let Some(arg) = args.next() {
      match arg {
        Value::String(string) => Ok(Value::new(
          string.parse::<f64>().map_err(|e| format!("{}", e))?,
        )),
        v => Err(format!("can not convert {} to a number", v)),
      }
    } else {
      Err(String::from("expected 1 argument"))
    }
  });

  obj.set("parse_number", parse_number);

  let contains = Value::native("contains", |_thread, _env, args| {
    let mut args = args.into_iter();
    if let Some(Value::String(string)) = args.next() {
      if let Some(Value::String(value)) = args.next() {
        return Ok(Value::new(string.contains(&value)));
      }
    }
    Ok(Value::new(false))
  });

  obj.set("contains", contains);

  let is_prefix = Value::native("is_prefix", |_thread, _env, args| {
    let mut args = args.into_iter();
    if let Some(Value::String(string)) = args.next() {
      if let Some(Value::String(value)) = args.next() {
        return Ok(Value::new(string.strip_prefix(&value).is_some()));
      }
    }
    Ok(Value::new(false))
  });

  obj.set("is_prefix", is_prefix);

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
