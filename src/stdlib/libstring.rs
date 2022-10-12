use crate::{Args, StructValue, Usertype, Value};

pub struct LibString;

impl LibString {
  pub fn load() -> Value {
    let mut lib = StructValue::default();

    let parse_number = Value::new_native_fn(|_thread, _env, args: Args| {
      let mut args = args.list.into_iter();
      if let Some(string_value) = args.next() {
        if let Ok(string) = string_value.as_str() {
          match string
            .parse::<f64>()
            .map(Value::from)
            .map_err(|e| Value::new_err(format!("{}", e)))
          {
            Ok(r) => r,
            Err(r) => r,
          }
        } else {
          Value::new_err(format!("can not convert {} to a number", string_value))
        }
      } else {
        Value::new_err("expected 1 argument")
      }
    });

    lib.set("parse_number", parse_number);

    let contains = Value::new_native_fn(|_thread, _env, args| {
      let mut args = args.list.into_iter();
      if let Some(string_value) = args.next() {
        if let Ok(string) = string_value.as_str() {
          if let Some(substr_value) = args.next() {
            if let Ok(substr) = substr_value.as_str() {
              return Value::from(string.contains::<&str>(substr.as_ref()));
            }
          }
        }
      }
      Value::from(false)
    });

    lib.set("contains", contains);

    let is_prefix = Value::new_native_fn(|_thread, _env, args| {
      let mut args = args.list.into_iter();
      if let Some(string_value) = args.next() {
        if let Ok(string) = string_value.as_str() {
          if let Some(substr_value) = args.next() {
            if let Ok(substr) = substr_value.as_str() {
              return Value::from(string.strip_prefix::<&str>(substr.as_ref()).is_some());
            }
          }
        }
      }
      Value::from(false)
    });

    lib.set("is_prefix", is_prefix);

    Value::from(lib)
  }
}
