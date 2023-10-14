use crate::prelude::*;

pub struct LibString;

impl LibString {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      lib
        .set(
          "parse_number",
          Value::native(|_vm, _env, args: Args| {
            let mut args = args.list.into_iter();
            if let Some(string_value) = args.next() {
              if let Some(string) = string_value.as_str() {
                string
                  .parse::<f64>()
                  .map(Value::from)
                  .map_err(|e| ValueError::Todo(e.to_string()))
              } else {
                Err(ValueError::Todo(format!("can not convert {} to a number", string_value)))
              }
            } else {
              Err(ValueError::Todo("expected 1 argument".to_string()))
            }
          }),
        )
        .ok();

      lib
        .set(
          "contains",
          Value::native(|_vm, _env, args| {
            let mut args = args.list.into_iter();
            if let Some(string_value) = args.next() {
              if let Some(string) = string_value.as_str() {
                if let Some(substr_value) = args.next() {
                  if let Some(substr) = substr_value.as_str() {
                    return Ok(Value::from(string.contains::<&str>(substr.as_ref())));
                  }
                }
              }
            }
            Ok(Value::from(false))
          }),
        )
        .ok();

      lib
        .set(
          "is_prefix",
          Value::native(|_vm, _env, args| {
            let mut args = args.list.into_iter();
            if let Some(string_value) = args.next() {
              if let Some(string) = string_value.as_str() {
                if let Some(substr_value) = args.next() {
                  if let Some(substr) = substr_value.as_str() {
                    return Ok(Value::from(string.strip_prefix::<&str>(substr.as_ref()).is_some()));
                  }
                }
              }
            }
            Ok(Value::from(false))
          }),
        )
        .ok();
    })
    .into()
  }
}
