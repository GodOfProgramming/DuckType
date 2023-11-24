use super::{ModuleStack, Stack, StackFrame};
use crate::{
  prelude::*,
  value::{tags::*, Mark, MutVoid, ValueMeta},
  FastHashSet,
};
use ahash::HashSetExt;
use ptr::MutPtr;
use std::{
  fmt::{self, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
  sync::{
    atomic::{AtomicUsize, Ordering},
    mpsc,
  },
  thread::{self, JoinHandle},
};

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

type AllocationSet = FastHashSet<u64>;

#[derive(Clone, Copy, Debug)]
pub enum Memory {
  Kb(usize),
  Mb(usize),
  Gb(usize),
}

impl Display for Memory {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{self:?}")
  }
}

impl From<Memory> for usize {
  fn from(memory: Memory) -> Self {
    match memory {
      Memory::Kb(kb) => kb * 1024,
      Memory::Mb(mb) => Memory::Kb(mb * 1024).into(),
      Memory::Gb(gb) => Memory::Mb(gb * 1024).into(),
    }
  }
}

pub struct Gc {
  pub(crate) allocations: AllocationSet,
  native_handles: AllocationSet,
  disposer: AsyncDisposal,

  limit: usize,
  allocated_memory: usize,

  pub(crate) num_cycles: usize,
}

impl Gc {
  pub fn new(initial_limit: Memory) -> Self {
    Self {
      allocations: AllocationSet::with_capacity(1024),
      native_handles: AllocationSet::with_capacity(512),
      disposer: AsyncDisposal::new(),
      limit: initial_limit.into(),
      allocated_memory: 0,
      num_cycles: 0,
    }
  }

  pub(crate) fn clean_if_time(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> Result<(), SystemError> {
    if self.allocated_memory > self.limit {
      // println!("cleaning {} bytes of memory with limit {}", self.allocated_memory, self.limit);
      self.clean(stack, envs, cache, stack_frame, stack_frames)?;
    }
    Ok(())
  }

  pub(crate) fn clean(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> Result<usize, SystemError> {
    self.ref_check_native_handles();

    let marked_allocations = self.trace(stack, envs, cache, stack_frame, stack_frames);

    let unmarked_allocations = self.find_unmarked(marked_allocations);

    let cleaned = unmarked_allocations.len();

    let mut released = 0;
    for alloc in &unmarked_allocations {
      released += Value { bits: *alloc }.meta().size;
      let removed = self.allocations.remove(alloc);
      debug_assert!(removed);
      cache.forget(alloc);
    }

    if cleaned > 0 {
      self.disposer.dispose(unmarked_allocations)?;
    }

    self.allocated_memory = self.allocated_memory.saturating_sub(released);
    self.limit = usize::max(self.allocated_memory.saturating_mul(2), self.limit);

    self.num_cycles += 1;

    Ok(cleaned)
  }

  fn trace(
    &self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> AllocationSet {
    let mut marked_allocations = Marker::with_estimated_size(stack.len() + envs.len() + cache.len() + 1 + stack_frames.len());

    for handle in &self.native_handles {
      marked_allocations.trace(&Value { bits: *handle });
    }

    for value in stack.iter() {
      marked_allocations.trace(value)
    }

    for value in cache.values() {
      marked_allocations.trace(value);
    }

    for env in envs.iter() {
      let handle = env.module();
      let value = handle.value();
      marked_allocations.trace(&value);
    }

    if let Some(value) = &stack_frame.export {
      marked_allocations.trace(value);
    }

    for frame in stack_frames {
      if let Some(value) = &frame.export {
        marked_allocations.trace(value);
      }
    }

    marked_allocations.into()
  }

  fn find_unmarked(&self, marked_allocations: AllocationSet) -> Vec<u64> {
    self
      .allocations
      .difference(&marked_allocations)
      .filter(|a| {
        let value = Value { bits: **a };
        debug_assert!(value.is_ptr());
        value.meta().ref_count.load(Ordering::Relaxed) == 0
      })
      .cloned()
      .collect()
  }

  pub fn handle_from(&mut self, value: Value) -> ValueHandle {
    self.allocations.remove(&value.bits);
    self.native_handles.insert(value.bits);

    ValueHandle::new(value)
  }

  pub fn allocate_typed_handle<T: Usertype>(&mut self, item: T) -> UsertypeHandle<T> {
    let value = self.allocate(item);
    let handle = self.handle_from(value);
    UsertypeHandle::new(handle)
  }

  pub fn terminate(&mut self) {
    self.disposer.terminate()
  }

  fn allocate_usertype<T: Usertype>(&mut self, item: T) -> Value {
    fn allocate_type<T>(item: T) -> *mut T {
      Box::into_raw(Box::new(item))
    }

    let allocated = unsafe { &mut *allocate_type(AllocatedObject::new(item)) };
    // println!("allocating {} bytes for id {}", allocated.meta.size, T::ID);
    self.allocated_memory += allocated.meta.size;

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

  fn drop_value(mut value: Value) {
    debug_assert!(value.is_ptr());
    let pointer = value.pointer_mut();
    let meta = value.meta();
    (meta.vtable.dealloc)(pointer);
  }

  fn ref_check_native_handles(&mut self) {
    let transferred = self
      .native_handles
      .iter()
      .map(|alloc| Value { bits: *alloc })
      .filter(|v| v.meta().ref_count.load(Ordering::Relaxed) == 0)
      .collect::<Vec<Value>>();

    for transfer in transferred {
      self.native_handles.remove(&transfer.bits);
      self.allocations.insert(transfer.bits);
    }
  }
}

#[cfg(test)]
impl Gc {
  pub fn test_default() -> Self {
    Self::new(Memory::Mb(10))
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
      vtable: &T::VTABLE,
      ref_count: AtomicUsize::new(0),
      size: mem::size_of::<Self>(),
      mark: Mark::default(),
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
    self.allocate_usertype(VecValue::new_from_slice(value))
  }
}

impl Allocation<Vec<Value>> for Gc {
  fn allocate(&mut self, value: Vec<Value>) -> Value {
    self.allocate_usertype(VecValue::new_from_vec(value))
  }
}

impl<T> Allocation<Vec<T>> for Gc
where
  T: Usertype,
{
  fn allocate(&mut self, value: Vec<T>) -> Value {
    let list = value.into_iter().map(|v| self.allocate(v)).collect();
    self.allocate_usertype(VecValue::new_from_vec(list))
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
      usertype: MutPtr::new(handle.value.reinterpret_cast_to_mut::<T>()),
      handle,
    }
  }

  pub fn value(&self) -> Value {
    self.handle.value.clone()
  }
}

impl<T> Clone for UsertypeHandle<T>
where
  T: Usertype,
{
  fn clone(&self) -> Self {
    Self {
      usertype: self.usertype,
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

impl<T> Display for UsertypeHandle<T>
where
  T: Usertype,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.handle)
  }
}

pub struct ValueHandle {
  pub value: Value,
}

impl ValueHandle {
  pub fn new(mut value: Value) -> ValueHandle {
    if value.is_ptr() {
      value.meta_mut().ref_count.fetch_add(1, Ordering::Relaxed);
    }
    Self { value }
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
    }
  }
}

impl Display for ValueHandle {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

impl Drop for ValueHandle {
  fn drop(&mut self) {
    if self.value.is_ptr() {
      let meta = self.value.meta();

      #[cfg(debug_assertions)]
      let before = meta.ref_count.load(Ordering::Relaxed);

      meta.ref_count.fetch_sub(1, Ordering::Relaxed);

      #[cfg(debug_assertions)]
      {
        let after = meta.ref_count.load(Ordering::Relaxed);

        debug_assert!(before > after);
      }
    }
  }
}

pub struct Marker {
  marked_values: AllocationSet,
}

impl Marker {
  fn with_estimated_size(num_items: usize) -> Self {
    Self {
      marked_values: AllocationSet::with_capacity(num_items),
    }
  }
  pub fn trace(&mut self, value: &Value) {
    if !self.marked_values.contains(&value.bits) {
      self.marked_values.insert(value.bits);
      value.trace_vtable(self);
    }
  }
}

impl From<Marker> for AllocationSet {
  fn from(value: Marker) -> Self {
    value.marked_values
  }
}

pub struct AsyncDisposal {
  chute: Option<mpsc::Sender<Vec<u64>>>,
  th: Option<JoinHandle<()>>,
}

impl AsyncDisposal {
  pub fn new() -> Self {
    let (sender, receiver) = mpsc::channel();

    let th = thread::spawn(move || {
      while let Ok(allocations) = receiver.recv() {
        for alloc in allocations {
          let value = Value { bits: alloc };
          Gc::drop_value(value);
        }
      }
    });

    Self {
      chute: Some(sender),
      th: Some(th),
    }
  }

  fn dispose(&mut self, allocations: Vec<u64>) -> Result<(), mpsc::SendError<Vec<u64>>> {
    match &self.chute {
      Some(chute) => chute.send(allocations)?,
      None => Err(mpsc::SendError::<Vec<u64>>(allocations))?,
    };
    Ok(())
  }

  fn terminate(&mut self) {
    self.chute = None;

    if let Some(th) = self.th.take() {
      let _ = th.join();
    }
  }
}

impl Default for AsyncDisposal {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
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

    let _x = gc.allocate(1);
    let _y = gc.allocate(1.0);
    let _b = gc.allocate(true);
    let _c = gc.allocate('c');

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
    let mut gc = SmartPtr::new(Gc::test_default());

    let mut struct_value = StructValue::new([(("child", 0), Value::nil)]);
    let child = gc.allocate(SomeType {});
    struct_value.set_field(&mut gc, Field::named("child"), child).unwrap();

    {
      let _handle = gc.allocate_typed_handle(struct_value);
      let cleaned = gc
        .clean(
          &Default::default(),
          &Default::default(),
          &mut Default::default(),
          &Default::default(),
          &[],
        )
        .unwrap();
      assert_eq!(cleaned, 0);
    }

    let cleaned = gc
      .clean(
        &Default::default(),
        &Default::default(),
        &mut Default::default(),
        &Default::default(),
        &[],
      )
      .unwrap();
    assert_eq!(cleaned, 2);
  }
}
