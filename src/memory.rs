use crate::{
  prelude::*,
  value::{tags::*, MutVoid, ValueMeta},
};
use std::{
  collections::HashSet,
  mem,
  sync::atomic::{AtomicUsize, Ordering},
};

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub struct ValueHandle {
  value: Value,
}

impl ValueHandle {
  fn new(mut value: Value) -> Self {
    if value.is_ptr() {
      value.meta_mut().ref_count.fetch_add(1, Ordering::Relaxed);
    }
    Self { value }
  }
}

impl Clone for ValueHandle {
  fn clone(&self) -> Self {
    if self.value.is_ptr() {
      self.value.meta().ref_count.fetch_add(1, Ordering::Relaxed);
    }

    Self {
      value: self.value.clone(),
    }
  }
}

impl Drop for ValueHandle {
  fn drop(&mut self) {
    if self.value.is_ptr() {
      let meta = self.value.meta();

      if meta.ref_count.load(Ordering::Relaxed) > 0 {
        meta.ref_count.fetch_sub(1, Ordering::Relaxed);
      }
    }
  }
}

#[derive(Default)]
pub struct Gc {
  allocations: HashSet<u64>,
}

#[derive(Default)]
pub struct Marker {
  marked_values: HashSet<u64>,
}

impl Marker {
  pub fn trace(&mut self, value: &Value) {
    if !self.marked_values.contains(&value.bits) {
      self.marked_values.insert(value.bits);
      value.trace(self);
    }
  }
}

impl Into<HashSet<u64>> for Marker {
  fn into(self) -> HashSet<u64> {
    self.marked_values
  }
}

impl Gc {
  pub fn allocate_handle<T: Usertype>(&mut self, item: T) -> ValueHandle {
    let value = self.allocate(item);
    ValueHandle::new(value)
  }

  fn allocate_usertype<T: Usertype>(&mut self, item: T) -> Value {
    let allocated = unsafe { &mut *Self::allocate_type(AllocatedObject::new(item)) };

    let ptr = &mut allocated.obj as *mut T as MutVoid;
    debug_assert_eq!(allocated as *const _ as *const (), &allocated.meta as *const _ as *const ());

    // ensure the pointer to the allocated object is offset by the right distance
    debug_assert_eq!(
      unsafe { (ptr as *const u8).offset(META_OFFSET) as *const () },
      allocated as *const _ as *const ()
    );

    // ensure the pointer fits in 48 bits
    debug_assert_eq!(ptr as u64 & POINTER_TAG, 0);

    // return the pointer to the object, hiding the vtable & ref count behind the returned address
    let bits = ptr as u64 | POINTER_TAG;
    let new = self.allocations.insert(bits);
    debug_assert!(new);
    Value { bits }
  }

  pub(crate) fn consume<T: Usertype>(this: *mut T) {
    let _ = unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) };
  }

  fn allocate_type<T>(item: T) -> *mut T {
    Box::into_raw(Box::new(item))
  }

  pub fn clean(&mut self, vm: &mut Vm) {
    let mut marked_allocations = Marker::default();
    vm.trace(&mut marked_allocations);
    let marked_allocations = marked_allocations.into();

    let unmarked_allocations = self.allocations.difference(&marked_allocations).cloned();

    for alloc in unmarked_allocations {
      let value = Value { bits: alloc };
      if value.meta().ref_count.load(Ordering::Relaxed) == 0 {
        self.drop_value(value);
      }
    }
  }

  fn drop_value(&self, mut value: Value) {
    debug_assert!(value.is_ptr());
    let pointer = value.pointer_mut();
    let meta = value.meta();
    (meta.vtable.dealloc)(pointer);
  }
}

#[repr(C)]
struct AllocatedObject<T: Usertype> {
  meta: ValueMeta,
  obj: T,
}

impl<T: Usertype> AllocatedObject<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: AtomicUsize::new(0),
      vtable: &T::VTABLE,
    };
    Self { obj, meta }
  }
}

pub trait Allocation<T> {
  fn allocate(&mut self, value: T) -> Value;
}

impl Allocation<Value> for Gc {
  fn allocate(&mut self, value: Value) -> Value {
    value
  }
}

impl Allocation<&Value> for Gc {
  fn allocate(&mut self, value: &Value) -> Value {
    value.clone()
  }
}

impl Allocation<()> for Gc {
  fn allocate(&mut self, value: ()) -> Value {
    Value::from(value)
  }
}

impl Allocation<f64> for Gc {
  fn allocate(&mut self, value: f64) -> Value {
    Value { bits: value.to_bits() }
  }
}

impl Allocation<i32> for Gc {
  fn allocate(&mut self, value: i32) -> Value {
    Value::from(&value)
  }
}

impl Allocation<&i32> for Gc {
  fn allocate(&mut self, value: &i32) -> Value {
    Value::from(value)
  }
}

impl Allocation<bool> for Gc {
  fn allocate(&mut self, value: bool) -> Value {
    Value::from(value)
  }
}

impl Allocation<char> for Gc {
  fn allocate(&mut self, value: char) -> Value {
    Value::from(value)
  }
}

impl Allocation<NativeFn> for Gc {
  fn allocate(&mut self, value: NativeFn) -> Value {
    Value::from(value)
  }
}

impl Allocation<Nil> for Gc {
  fn allocate(&mut self, value: Nil) -> Value {
    Value::from(value)
  }
}

impl Allocation<&str> for Gc {
  fn allocate(&mut self, value: &str) -> Value {
    self.allocate_usertype::<StringValue>(value.into())
  }
}

impl Allocation<String> for Gc {
  fn allocate(&mut self, value: String) -> Value {
    self.allocate_usertype::<StringValue>(value.into())
  }
}

impl Allocation<&String> for Gc {
  fn allocate(&mut self, value: &String) -> Value {
    self.allocate_usertype::<StringValue>(value.clone().into())
  }
}

impl Allocation<&[Value]> for Gc {
  fn allocate(&mut self, value: &[Value]) -> Value {
    self.allocate_usertype(ArrayValue::new_from_slice(value))
  }
}

impl Allocation<Vec<Value>> for Gc {
  fn allocate(&mut self, value: Vec<Value>) -> Value {
    self.allocate_usertype(ArrayValue::new_from_vec(&value))
  }
}

impl<T> Allocation<T> for Gc
where
  T: Usertype,
{
  fn allocate(&mut self, value: T) -> Value {
    self.allocate_usertype::<T>(value)
  }
}
