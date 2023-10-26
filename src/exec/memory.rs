use ptr::{MutPtr, SmartPtr};

use crate::{
  prelude::*,
  value::{tags::*, MutVoid, ValueMeta},
};
use std::{
  collections::HashSet,
  mem,
  ops::{Deref, DerefMut},
  sync::atomic::{AtomicUsize, Ordering},
};

use super::StackFrame;

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub struct UsertypeHandle<T>
where
  T: Usertype,
{
  pub(crate) usertype: MutPtr<T>,
  pub handle: ValueHandle,
}

impl<T> UsertypeHandle<T>
where
  T: Usertype,
{
  pub fn new(mut handle: ValueHandle) -> Self {
    Self {
      usertype: handle.value.reinterpret_cast::<T>(),
      handle,
    }
  }
}

impl<T> Clone for UsertypeHandle<T>
where
  T: Usertype,
{
  fn clone(&self) -> Self {
    Self {
      usertype: self.usertype.clone(),
      handle: self.handle.clone(),
    }
  }
}

impl<T> From<UsertypeHandle<T>> for ValueHandle
where
  T: Usertype,
{
  fn from(utype: UsertypeHandle<T>) -> Self {
    utype.handle
  }
}

impl<T> MaybeFrom<ValueHandle> for UsertypeHandle<T>
where
  T: Usertype,
{
  fn maybe_from(handle: ValueHandle) -> Option<Self> {
    if handle.value.is::<T>() {
      Some(UsertypeHandle::new(handle))
    } else {
      None
    }
  }
}

impl<T> Deref for UsertypeHandle<T>
where
  T: Usertype,
{
  type Target = T;
  fn deref(&self) -> &Self::Target {
    &self.usertype
  }
}

impl<T> DerefMut for UsertypeHandle<T>
where
  T: Usertype,
{
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.usertype
  }
}

pub struct ValueHandle {
  pub value: Value,
  gc: SmartPtr<Gc>,
}

impl ValueHandle {
  pub fn new(gc: SmartPtr<Gc>, mut value: Value) -> ValueHandle {
    value.meta_mut().ref_count.fetch_add(1, Ordering::Relaxed);

    Self { value, gc }
  }
}

impl Deref for ValueHandle {
  type Target = Value;
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl DerefMut for ValueHandle {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.value
  }
}

impl From<ValueHandle> for Value {
  fn from(handle: ValueHandle) -> Self {
    handle.value.clone()
  }
}

impl Clone for ValueHandle {
  fn clone(&self) -> Self {
    if self.value.is_ptr() {
      self.value.meta().ref_count.fetch_add(1, Ordering::Relaxed);
    }

    Self {
      value: self.value.clone(),
      gc: self.gc.clone(),
    }
  }
}

impl Drop for ValueHandle {
  fn drop(&mut self) {
    if self.value.is_ptr() {
      let meta = self.value.meta();

      let ref_count = meta.ref_count.load(Ordering::Relaxed).saturating_sub(1);
      meta.ref_count.store(ref_count, Ordering::Relaxed);
      if ref_count == 0 {
        self.gc.drop_handle(self.value.clone());
      }
    }
  }
}

#[derive(Default)]
pub struct Marker {
  marked_values: HashSet<u64>,
}

impl Marker {
  pub fn trace(&mut self, value: &Value) {
    if !self.marked_values.contains(&value.bits) {
      self.marked_values.insert(value.bits);
      value.trace_vtable(self);
    }
  }
}

impl From<Marker> for HashSet<u64> {
  fn from(value: Marker) -> Self {
    value.marked_values
  }
}

#[derive(Default)]
pub struct Gc {
  allocations: HashSet<u64>,
  handles: HashSet<u64>,
}

impl Gc {
  pub fn clean<'v>(
    &mut self,
    current_frame: &StackFrame,
    stack_frames: &Vec<StackFrame>,
    cached_values: impl IntoIterator<Item = &'v Value>,
  ) -> usize {
    let mut cleaned = 0;

    let mut marked_allocations = Marker::default();

    for value in cached_values {
      marked_allocations.trace(value);
    }

    for value in &self.handles {
      marked_allocations.trace(&Value { bits: *value });
    }

    for value in &current_frame.stack {
      marked_allocations.trace(value);
    }

    for frame in stack_frames {
      for value in &frame.stack {
        marked_allocations.trace(value);
      }
    }

    for handle in &self.handles {
      marked_allocations.trace(&Value { bits: *handle })
    }

    let marked_allocations = marked_allocations.into();

    let unmarked_allocations: Vec<u64> = self.allocations.difference(&marked_allocations).cloned().collect();

    for alloc in unmarked_allocations {
      let value = Value { bits: alloc };
      debug_assert!(value.is_ptr());
      if value.meta().ref_count.load(Ordering::Relaxed) == 0 {
        self.allocations.remove(&alloc);
        self.drop_value(value);
        cleaned += 1;
      }
    }

    cleaned
  }

  pub fn handle_from(this: &mut SmartPtr<Self>, value: Value) -> ValueHandle {
    this.handles.insert(value.bits);
    ValueHandle::new(this.clone(), value)
  }

  pub fn allocate_handle<T: Usertype>(this: &mut SmartPtr<Self>, item: T) -> UsertypeHandle<T> {
    let value = this.allocate(item);
    let new = this.handles.insert(value.bits);
    debug_assert!(new);
    UsertypeHandle::new(ValueHandle::new(this.clone(), value))
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
    let new_allocation = self.allocations.insert(bits);
    debug_assert!(new_allocation);
    Value { bits }
  }

  pub(crate) fn consume<T: Usertype>(this: *mut T) {
    let _ = unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) };
  }

  fn allocate_type<T>(item: T) -> *mut T {
    Box::into_raw(Box::new(item))
  }

  fn drop_handle(&mut self, value: Value) {
    let present = self.handles.remove(&value.bits);
    debug_assert!(present);
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
    self.allocate_usertype(ArrayValue::new_from_vec(value))
  }
}

impl<T> Allocation<Vec<T>> for Gc
where
  T: Usertype,
{
  fn allocate(&mut self, value: Vec<T>) -> Value {
    let list = value.into_iter().map(|v| self.allocate(v)).collect();
    self.allocate_usertype(ArrayValue::new_from_vec(list))
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::code::Reflection;
  use ptr::SmartPtr;
  use std::rc::Rc;

  #[derive(Usertype, Fields)]
  #[uuid("random")]
  struct SomeType {}

  #[methods]
  impl SomeType {}

  fn new_ctx() -> SmartPtr<Context> {
    SmartPtr::new(Context::new(
      Some("main"),
      Reflection::new(Rc::new(Default::default()), Rc::new(Default::default())),
    ))
  }

  #[test]
  fn gc_can_allocate_and_clean() {
    let mut gc = Gc::default();
    let ctx = new_ctx();

    gc.allocate(SomeType {});

    let cleaned = gc.clean(&StackFrame::new(ctx), &Default::default(), []);
    assert_eq!(cleaned, 1);
  }

  #[test]
  fn gc_does_not_clean_more_than_it_needs_to() {
    let mut gc = Gc::default();
    let ctx = new_ctx();

    let _x = gc.allocate(1);
    let _y = gc.allocate(1.0);
    let _b = gc.allocate(true);
    let _c = gc.allocate('c');

    gc.allocate(SomeType {});

    let cleaned = gc.clean(&StackFrame::new(ctx), &Default::default(), []);
    assert_eq!(cleaned, 1);
  }

  #[test]
  fn gc_does_not_clean_open_handles() {
    let mut gc = SmartPtr::new(Gc::default());
    let ctx = new_ctx();

    let mut value = StructValue::default();
    let child = gc.allocate(SomeType {});
    value.set("child", child);

    {
      let _handle = Gc::allocate_handle(&mut gc, value);
      let cleaned = gc.clean(&StackFrame::new(ctx.clone()), &Default::default(), []);
      assert_eq!(cleaned, 0);
    }

    let cleaned = gc.clean(&StackFrame::new(ctx), &Default::default(), []);
    assert_eq!(cleaned, 2);
  }
}
