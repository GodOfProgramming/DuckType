use crate::prelude::*;

use macros::{methods, Class};
use tfix::prelude::*;

#[derive(Default)]
struct ValueTest {}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self::default()
  }
}

#[derive(Default, Class)]
struct ImplementedObject {
  field: i32,
}

impl ImplementedObject {
  fn new(x: i32) -> Self {
    Self { field: x }
  }
}

#[methods]
impl ImplementedObject {}

impl Usertype for ImplementedObject {
  const ID: &'static str = "ImplementedObject";
}

#[derive(Class)]
struct UnimplementedObject {}

#[methods]
impl UnimplementedObject {}

impl Usertype for UnimplementedObject {
  const ID: &'static str = "UnimplementedObject";
}

#[fixture(ValueTest)]
mod unit_tests {

  use super::*;

  #[test]
  fn nil_value_is_default(_: &mut ValueTest) {
    let v = Value::default();
    assert!(v.is_nil());
  }

  #[test]
  fn floats_supported(_: &mut ValueTest) {
    let v = Value::from(1.23);
    assert!(v.is_f64());
    assert_eq!(v.as_f64().unwrap(), 1.23);
  }

  #[test]
  fn integers_supported(_: &mut ValueTest) {
    let v = Value::from(123);
    assert!(v.is_i32());
    assert_eq!(v.as_i32().unwrap(), 123);
  }

  #[test]
  fn structs_supported(_: &mut ValueTest) {
    let mut v = StructValue::new();
    v.set("foo", 123.into());
    assert_eq!(v.get_member("foo").unwrap(), 123.into());
    v.set("field", ImplementedObject::default().into());
    assert!(v.get_member("field").unwrap().is::<ImplementedObject>());
  }

  #[test]
  fn userdata_supported(_: &mut ValueTest) {
    const V: i32 = 1;
    let value = Value::from(ImplementedObject::new(V));
    assert_eq!(value.cast_to::<ImplementedObject>().unwrap().field, V);
  }

  #[test]
  fn can_assign_different_types(_: &mut ValueTest) {
    let mut v = Value::from(ImplementedObject::default());
    assert!(v.is::<ImplementedObject>());

    v = Value::from(123);
    assert!(v.is_i32());

    v = Value::from(1.23);
    assert!(v.is_f64());

    v = Value::from(ImplementedObject::default());
    assert!(v.is::<ImplementedObject>());

    v = Value::from(Nil);
    assert!(v.is_nil());
  }
}
