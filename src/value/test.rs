use super::{Assign, Nil, StructValue, Tag, Usertype, UsertypeId, Value};
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
  const ID: UsertypeId = "ImplementedObject";

  fn set(&mut self, name: &str, value: Value) -> Value {
    match name {
      "field" => {
        if let Ok(i) = value.as_i32() {
          self.field = i;
        }
      }
      _ => panic!("should not reach"),
    }
    value
  }

  fn get(&self, name: &str) -> Value {
    match name {
      "field" => self.field.into(),
      _ => Value::nil,
    }
  }

  fn add(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field + other.as_i32().unwrap()).into(),
      Tag::Pointer => {
        if let Ok(obj) = other.cast_to::<ImplementedObject>() {
          (self.field + obj.field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn sub(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field - other.as_i32().unwrap()).into(),
      Tag::Pointer => {
        if let Ok(obj) = other.cast_to::<ImplementedObject>() {
          (self.field - obj.field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn mul(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field * other.as_i32().unwrap()).into(),
      Tag::Pointer => {
        if let Ok(obj) = other.cast_to::<ImplementedObject>() {
          (self.field * obj.field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn div(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field / other.as_i32().unwrap()).into(),
      Tag::Pointer => {
        if let Ok(obj) = other.cast_to::<ImplementedObject>() {
          (self.field / obj.field).into()
        } else {
          Value::new_err(format!("cannot add ImplementedObject and {}", other))
        }
      }
      _ => Value::new_err(format!("cannot add ImplementedObject and {}", other)),
    }
  }

  fn rem(&self, other: Value) -> Value {
    match other.tag() {
      Tag::I32 => (self.field % other.as_i32().unwrap()).into(),
      Tag::Pointer => {
        if let Ok(obj) = other.cast_to::<ImplementedObject>() {
          (self.field % obj.field).into()
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

impl Usertype for UnimplementedObject {
  const ID: UsertypeId = "UnimplementedObject";
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
    let mut v = Value::new_struct();
    assert!(v.is::<StructValue>());
    v.set("foo", 123.into());
    assert_eq!(v.get("foo"), 123.into());
    assert!(!v.set("field", ImplementedObject::default().into()).is_err());
    assert!(v.get("field").is::<ImplementedObject>());
  }

  #[test]
  fn userdata_supported(_: &mut ValueTest) {
    let mut x = false;

    {
      let mut v = Value::from(ImplementedObject::new(&mut x));
      v.set("field", 123.into());
      assert_eq!(v.get("field"), 123.into());
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

      v.assign(123);
      assert!(v.is_i32());
      assert!(x);

      v.assign(1.23);
      assert!(v.is_f64());
      assert!(x);

      v.assign(ImplementedObject::new(&mut x));
      assert!(v.is::<ImplementedObject>());
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

    assert!(!obj.set("field", 15.into()).is_err());
    assert_eq!(obj.get("field"), 15.into());

    assert_eq!(obj.clone() + 1.into(), 16.into());
    assert_eq!(obj.clone() - 1.into(), 14.into());
    assert_eq!(obj.clone() * 2.into(), 30.into());
    assert_eq!(obj.clone() / 3.into(), 5.into());
    assert_eq!(obj.clone() % 7.into(), 1.into());

    assert_eq!(obj.clone() + other.clone(), 20.into());
    assert_eq!(obj.clone() - other.clone(), 10.into());
    assert_eq!(obj.clone() * other.clone(), 75.into());
    assert_eq!(obj.clone() / other.clone(), 3.into());
    assert_eq!(obj.clone() % other.clone(), 0.into());

    assert_eq!(other.cast_to::<ImplementedObject>().unwrap().field, 5);
  }

  #[test]
  fn object_trait_returns_error_when_unimplemented(_: &mut ValueTest) {
    let mut obj = Value::from(UnimplementedObject);

    assert!(obj.set("", Value::nil).is_err());
    assert!(obj.get("").is_err());
    assert!((obj.clone() + Value::nil).is_err());
    assert!((obj.clone() - Value::nil).is_err());
    assert!((obj.clone() * Value::nil).is_err());
    assert!((obj.clone() / Value::nil).is_err());
    assert!((obj % Value::nil).is_err());
  }
}
