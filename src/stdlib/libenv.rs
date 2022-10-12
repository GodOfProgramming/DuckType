use crate::{StructValue, Usertype, Value};

pub struct LibEnv;

impl LibEnv {
  pub fn load(args: &[String]) -> Value {
    let mut lib = StructValue::default();

    lib.set(
      "ARGV",
      Value::from(args.iter().map(|arg| Value::from(arg.clone())).collect::<Vec<Value>>()),
    );

    lib.into()
  }
}
