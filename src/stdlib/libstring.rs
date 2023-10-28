use crate::prelude::*;

pub fn string(gc: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("parse_number", Value::native(parse_number));
  lib.define("contains", Value::native(contains));
  lib.define("is_prefix", Value::native(is_prefix));
}

#[native]
fn parse_number(value: &StringValue) -> ValueResult {
  value
    .parse::<f64>()
    .map(Value::from)
    .map_err(|e| ValueError::Todo(e.to_string()))
}

#[native]
fn contains(string: Value, substr: Value) -> ValueResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.contains::<&str>(substr.as_ref())));
    }
  }

  Ok(Value::from(false))
}

#[native]
fn is_prefix(string: Value, substr: Value) -> ValueResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.strip_prefix::<&str>(substr.as_ref()).is_some()));
    }
  }

  Ok(Value::from(false))
}
