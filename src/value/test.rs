use super::{Assign, Error, Nil, Object, Struct, Tag, Value};
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

impl Object for ImplementedObject {
  fn set(&mut self, name: &str, value: Value) -> Result<(), Error> {
    match name {
      "field" => {
        if value.is_i32() {
          self.field = value.as_i32();
        }
      }
      _ => panic!("should not reach"),
    }
    Ok(())
  }

  fn get(&self, name: &str) -> Value {
    match name {
      "field" => self.field.into(),
      _ => Value::nil,
    }
  }

  fn add(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field + other.as_i32()).into(),
      Tag::Pointer => {
        if other.is_obj::<ImplementedObject>() {
          (self.field + other.as_obj::<ImplementedObject>().field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn sub(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field - other.as_i32()).into(),
      Tag::Pointer => {
        if other.is_obj::<ImplementedObject>() {
          (self.field - other.as_obj::<ImplementedObject>().field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn mul(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field * other.as_i32()).into(),
      Tag::Pointer => {
        if other.is_obj::<ImplementedObject>() {
          (self.field * other.as_obj::<ImplementedObject>().field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn div(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field / other.as_i32()).into(),
      Tag::Pointer => {
        if other.is_obj::<ImplementedObject>() {
          (self.field / other.as_obj::<ImplementedObject>().field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn rem(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field % other.as_i32()).into(),
      Tag::Pointer => {
        if other.is_obj::<ImplementedObject>() {
          (self.field % other.as_obj::<ImplementedObject>().field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn drop(&mut self) {
    if !self.ptr.is_null() {
      unsafe { *self.ptr = !*self.ptr };
    }
  }
}

struct UnimplementedObject;

impl Object for UnimplementedObject {}

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
    let v = Value::from(123);
    assert!(v.is_i32());
    assert_eq!(v.as_i32(), 123);
  }

  #[test]
  fn floats_supported(_: &mut ValueTest) {
    let v = Value::from(1.23);
    assert!(v.is_f64());
    assert_eq!(v.as_f64(), 1.23);
  }

  #[test]
  fn structs_supported(_: &mut ValueTest) {
    let mut v = Value::new_struct();
    assert!(v.is_obj::<Struct>());
    v.set("foo", 123.into()).unwrap();
    assert_eq!(v.get("foo"), Value::from(123));
    assert!(v.set("field", ImplementedObject::default().into()).is_ok());
    assert!(v.get("field").is_obj::<ImplementedObject>());
  }

  #[test]
  fn userdata_supported(_: &mut ValueTest) {
    let mut x = false;

    {
      let mut v = Value::from(ImplementedObject::new(&mut x));
      v.set("field", 123.into()).unwrap();
      assert_eq!(v.get("field"), Value::from(123));
    }

    assert!(x);
  }

  #[test]
  fn can_assign_different_types(_: &mut ValueTest) {
    let mut x = false;

    {
      let mut v = Value::from(ImplementedObject::new(&mut x));
      assert!(v.is_obj::<ImplementedObject>());
      assert!(!x);

      v.assign(123);
      assert!(v.is_i32());
      assert!(x);

      v.assign(1.23);
      assert!(v.is_f64());
      assert!(x);

      v.assign(ImplementedObject::new(&mut x));
      assert!(v.is_obj::<ImplementedObject>());
      assert!(x);

      v.assign(Nil);
      assert!(v.is_nil());
      assert!(!x);
    }

    assert!(!x);
  }

  #[test]
  fn object_trait_returns_expected_value_when_implemented(_: &mut ValueTest) {
    let mut x = false;
    let mut obj = ImplementedObject::new(&mut x);
    obj.field = 0;
    let mut obj = Value::from(obj);

    let mut other = ImplementedObject::new(&mut x);
    other.field = 5;
    let other = Value::from(other);

    assert!(obj.set("field", 15.into()).is_ok());
    assert_eq!(obj.get("field"), Value::from(15));

    assert_eq!(obj.add(1.into()), Value::from(16));
    assert_eq!(obj.sub(1.into()), Value::from(14));
    assert_eq!(obj.mul(2.into()), Value::from(30));
    assert_eq!(obj.div(3.into()), Value::from(5));
    assert_eq!(obj.rem(7.into()), Value::from(1));

    assert_eq!(obj.add(other.clone()), Value::from(20));
    assert_eq!(obj.sub(other.clone()), Value::from(10));
    assert_eq!(obj.mul(other.clone()), Value::from(75));
    assert_eq!(obj.div(other.clone()), Value::from(3));
    assert_eq!(obj.rem(other.clone()), Value::from(0));

    assert_eq!(other.as_obj::<ImplementedObject>().field, 5);
  }

  #[test]
  fn object_trait_returns_error_when_unimplemented(_: &mut ValueTest) {
    let mut obj = UnimplementedObject;

    assert!(obj.set("", Value::nil).is_err());
    assert!(obj.get("").is_err(), "{}", obj.get(""));
    assert!(obj.add(Value::nil).is_err());
    assert!(obj.sub(Value::nil).is_err());
    assert!(obj.mul(Value::nil).is_err());
    assert!(obj.div(Value::nil).is_err());
    assert!(obj.rem(Value::nil).is_err());
  }
}
