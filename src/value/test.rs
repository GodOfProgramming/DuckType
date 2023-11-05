use crate::prelude::*;
use tfix::prelude::*;

struct ValueTest {
  vm: Vm,
}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self {
      vm: Vm::new(SmartPtr::new(Gc::default()), []),
    }
  }
}

#[derive(Default, Usertype, Fields)]
#[uuid("9ae5ec94-64f1-4294-8a16-77f753865a82")]
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

#[derive(Default, Usertype, Fields)]
#[uuid("dc03970c-c5c2-451e-b4b1-3f62e99a042c")]
struct UnimplementedObject {}

#[methods]
impl UnimplementedObject {}

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
  fn structs_supported(t: &mut ValueTest) {
    let mut v = StructValue::new([("foo", Value::nil), ("field", Value::nil)]);
    v.set("foo", 123.into());
    assert_eq!(v.get_field(&mut t.vm.gc, "foo").unwrap().unwrap(), 123.into());
    v.set("field", t.vm.gc.allocate(ImplementedObject::default()));
    assert!(v.get_field(&mut t.vm.gc, "field").unwrap().unwrap().is::<ImplementedObject>());
  }

  #[test]
  fn userdata_supported(t: &mut ValueTest) {
    const V: i32 = 1;
    let value = t.vm.gc.allocate(ImplementedObject::new(V));
    assert_eq!(value.cast_to::<ImplementedObject>().unwrap().field, V);
  }

  #[test]
  fn can_assign_different_types(t: &mut ValueTest) {
    let mut v = t.vm.gc.allocate(ImplementedObject::default());
    assert!(v.is::<ImplementedObject>());

    v = Value::from(123);
    assert!(v.is_i32());

    v = Value::from(1.23);
    assert!(v.is_f64());

    v = t.vm.gc.allocate(ImplementedObject::default());
    assert!(v.is::<ImplementedObject>());

    v = Value::from(Nil);
    assert!(v.is_nil());
  }
}
