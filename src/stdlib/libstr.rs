use crate::prelude::*;

pub fn string(_: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("parse_number", Value::native(parse_number));
  lib.define("contains", Value::native(contains));
  lib.define("is_prefix", Value::native(is_prefix));
  lib.define("concat", Value::native(concat));
  lib.define("join", Value::native(join));
}

#[native]
fn parse_number(value: &StringValue) -> UsageResult {
  value.parse::<f64>().map(Value::from).map_err(UsageError::native)
}

#[native]
fn contains(string: Value, substr: Value) -> UsageResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.contains::<&str>(substr.as_ref())));
    }
  }

  Ok(Value::from(false))
}

#[native]
fn is_prefix(string: Value, substr: Value) -> UsageResult {
  if let Some(string) = string.as_str() {
    if let Some(substr) = substr.as_str() {
      return Ok(Value::from(string.strip_prefix::<&str>(substr.as_ref()).is_some()));
    }
  }

  Ok(Value::from(false))
}

fn concat(vm: &mut Vm, args: Args) -> UsageResult {
  let str = itertools::join(args.list, "");
  Ok(vm.gc.allocate(str))
}

#[native]
fn join(sep: &StringValue, list: &VecValue) -> UsageResult<String> {
  let str = itertools::join(list.buffer.iter(), sep);
  Ok(str)
}
