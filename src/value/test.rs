use super::{Nil, StructValue, Usertype, Value};
use tfix::prelude::*;

#[derive(Default)]
struct ValueTest {}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self::default()
  }
}

struct ImplementedObject {
  field: i32,
  ptr: *mut bool,
}

impl ImplementedObject {
  fn new(ptr: &mut bool) -> Self {
    Self {
      field: 0,
      ptr: ptr as *mut _ as *mut bool,
    }
  }
}

impl Default for ImplementedObject {
  fn default() -> Self {
    Self {
      field: 0,
      ptr: std::ptr::null_mut(),
    }
  }
}

impl Usertype for ImplementedObject {
  const ID: &'static str = "ImplementedObject";
}

struct UnimplementedObject;

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
    assert_eq!(v.get("foo"), 123.into());
    v.set("field", ImplementedObject::default().into());
    assert!(v.get("field").is::<ImplementedObject>());
  }

  #[test]
  fn userdata_supported(_: &mut ValueTest) {
    let mut x = false;

    {
      Value::from(ImplementedObject::new(&mut x));
    }

    assert!(x);
  }

  #[test]
  fn can_assign_different_types(_: &mut ValueTest) {
    let mut x = false;

    {
      let mut v = Value::from(ImplementedObject::new(&mut x));
      assert!(v.is::<ImplementedObject>());
      assert!(!x);

      v = Value::from(123);
      assert!(v.is_i32());
      assert!(x);

      v = Value::from(1.23);
      assert!(v.is_f64());
      assert!(x);

      v = Value::from(ImplementedObject::new(&mut x));
      assert!(v.is::<ImplementedObject>());
      assert!(x);

      v = Value::from(Nil);
      assert!(v.is_nil());
      assert!(!x);
    }

    assert!(!x);
  }
}
