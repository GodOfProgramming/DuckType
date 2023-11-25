use super::*;
use ptr::SmartPtr;

#[derive(Usertype, Fields, NoMethods, NoOperators)]
#[uuid("random")]
struct SomeType {}

#[test]
fn gc_can_allocate_and_clean() {
  let mut gc = Gc::test_default();

  gc.allocate(SomeType {});

  let cleaned = gc
    .clean(
      &Default::default(),
      &Default::default(),
      &mut Default::default(),
      &Default::default(),
      &[],
    )
    .unwrap();
  assert_eq!(cleaned, 1);
}

#[test]
fn gc_does_not_clean_more_than_it_needs_to() {
  let mut gc = Gc::test_default();

  gc.allocate(SomeType {});

  let cleaned = gc
    .clean(
      &Default::default(),
      &Default::default(),
      &mut Default::default(),
      &Default::default(),
      &[],
    )
    .unwrap();

  assert_eq!(cleaned, 1);
}

#[test]
fn gc_does_not_clean_open_handles() {
  let gc = SmartPtr::new(Gc::test_default());
  let mut vm = Vm::new(gc, false, []);

  let mut struct_value = StructValue::new([(("child", 0), Value::nil)]);
  let child = vm.make_value_from(SomeType {});
  struct_value.set_field(&mut vm, Field::named("child"), child).unwrap();

  {
    let _handle = vm.make_usertype_handle_from(struct_value);
    let cleaned = vm.force_gc().unwrap();
    assert_eq!(cleaned, 0);
  }

  let cleaned = vm.force_gc().unwrap();
  assert_eq!(cleaned, 2);
}
