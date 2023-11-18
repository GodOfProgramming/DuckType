use crate::prelude::*;
use tfix::prelude::*;

struct ValueTest {
  vm: Vm,
}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self {
      vm: Vm::new(SmartPtr::new(Gc::always_run()), false, []),
    }
  }
}

#[derive(Default, Usertype, Fields, NoMethods, NoOperators)]
#[uuid("9ae5ec94-64f1-4294-8a16-77f753865a82")]
struct ImplementedObject {
  field: i32,
}

impl ImplementedObject {
  fn new(x: i32) -> Self {
    Self { field: x }
  }
}

#[derive(Default, Usertype, Fields, NoMethods, NoOperators)]
#[uuid("dc03970c-c5c2-451e-b4b1-3f62e99a042c")]
struct UnimplementedObject {}

#[fixture(ValueTest)]
mod unit_tests {
  use super::*;
  use crate::value::prelude::native_value::Addr;

  #[test]
  fn nil_value_is_default(_: &mut ValueTest) {
    let v = Value::default();
    assert!(v.is::<()>());
  }

  #[test]
  fn floats_supported(_: &mut ValueTest) {
    let v = Value::from(1.23);
    assert!(v.is::<f64>());
    assert_eq!(v.cast_to::<f64>().unwrap(), 1.23);
  }

  #[test]
  fn integers_supported(_: &mut ValueTest) {
    let v = Value::from(123);
    assert!(v.is::<i32>());
    assert_eq!(v.cast_to::<i32>().unwrap(), 123);
  }

  #[test]
  fn structs_supported(t: &mut ValueTest) {
    let mut v = StructValue::new([(("foo", 0), Value::nil), (("field", 1), Value::nil)]);
    v.set_field(&mut t.vm.gc, Field::named("foo"), 123.into()).unwrap();
    assert_eq!(v.get_field(&mut t.vm.gc, Field::named("foo")).unwrap().unwrap(), 123.into());
    let obj = t.vm.gc.allocate(ImplementedObject::default());
    v.set(&mut t.vm.gc, Field::named("field"), obj).unwrap();
    assert!(v
      .get_field(&mut t.vm.gc, Field::named("field"))
      .unwrap()
      .unwrap()
      .is::<ImplementedObject>());
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
    assert!(v.is::<i32>());

    v = Value::from(1.23);
    assert!(v.is::<f64>());

    v = t.vm.gc.allocate(ImplementedObject::default());
    assert!(v.is::<ImplementedObject>());

    v = Value::from(());
    assert!(v.is::<()>());
  }

  #[test]
  fn can_cast_to_native_fn(t: &mut ValueTest) {
    #[native]
    fn some_fn() -> UsageResult<i32> {
      Ok(1)
    }

    let v = Value::new::<NativeFn>(some_fn);
    let f = v.cast_to::<NativeFn>().expect("should be a native fn");
    let s: NativeFn = some_fn;
    assert_eq!(f.addr(), s.addr());
    let o = f(&mut t.vm, Default::default()).expect("f should not fail");
    let i = o.cast_to::<i32>().expect("output should be an integer");
    assert_eq!(i, 1);
  }
}
