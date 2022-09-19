use super::{New, Object, Struct, Value};
use tfix::prelude::*;

#[derive(Default)]
struct ValueTest {}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self::default()
  }
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
  fn integers_supported(_: &mut ValueTest) {
    let v = Value::new(123);
    assert!(v.is_int());
    assert_eq!(v.as_int(), 123);
  }

  #[test]
  fn floats_supported(_: &mut ValueTest) {
    let v = Value::new(1.23);
    assert!(v.is_float());
    assert_eq!(v.as_float(), 1.23);
  }

  #[test]
  fn structs_supported(_: &mut ValueTest) {
    let mut v = Value::new(Struct::default());
    assert!(v.is_kind::<Struct>());
    v.set("foo", Value::new(123));
    assert_eq!(v.get("foo").unwrap(), Value::new(123));
  }

  #[test]
  fn userdata_supported(_: &mut ValueTest) {
    struct TestObject {
      field: i32,
      ptr: *mut bool,
    }

    impl TestObject {
      fn new(ptr: &mut bool) -> Self {
        Self {
          field: 0,
          ptr: ptr as *mut _ as *mut bool,
        }
      }
    }

    impl Object for TestObject {
      fn set(&mut self, name: &str, value: Value) {
        match name {
          "field" => {
            if value.is_int() {
              self.field = value.as_int();
            }
          }
          _ => (),
        }
      }

      fn get(&self, name: &str) -> Option<Value> {
        match name {
          "field" => Some(Value::new(self.field)),
          _ => None,
        }
      }

      fn drop(&mut self) {
        unsafe { *self.ptr = true };
      }
    }

    let mut x = false;
    {
      let mut v = Value::new(TestObject::new(&mut x));
      v.set("field", Value::new(123));
      assert_eq!(v.get("field").unwrap(), Value::new(123));
    }
    assert!(x);
  }
}
