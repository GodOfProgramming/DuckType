use crate::prelude::*;

pub struct LibString;

impl LibString {
  pub fn load(gc: &mut Gc) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      lib.set(gc, "parse_number", Value::native(parse_number)).ok();
      lib.set(gc, "contains", Value::native(contains)).ok();
      lib.set(gc, "is_prefix", Value::native(is_prefix)).ok();
    })
  }
}

#[native]
fn parse_number(_vm: &mut Vm, value: &StringValue) -> ValueResult {
  value
    .parse::<f64>()
    .map(Value::from)
    .map_err(|e| ValueError::Todo(e.to_string()))
}

#[native]
fn contains(_vm: &mut Vm, string: Value, substr: Value) -> ValueResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.contains::<&str>(substr.as_ref())));
    }
  }

  Ok(Value::from(false))
}

#[native]
fn is_prefix(_vm: &mut Vm, string: Value, substr: Value) -> ValueResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.strip_prefix::<&str>(substr.as_ref()).is_some()));
    }
  }

  Ok(Value::from(false))
}
