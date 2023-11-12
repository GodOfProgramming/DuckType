use super::{EnvStack, Stack, StackFrame};
use crate::{
  exec::Register,
  prelude::*,
  value::{tags::*, Mark, MutVoid, ValueMeta},
};
use ahash::RandomState;
use ptr::{MutPtr, SmartPtr};
use std::{
  collections::HashSet,
  fmt::{self, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
  sync::{
    atomic::{AtomicUsize, Ordering},
    mpsc,
  },
  thread::{self, JoinHandle},
  time::{Duration, Instant},
};
use strum::IntoEnumIterator;

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

impl Display for ValueHandle {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
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
  marked_values: AllocationSet,
}

impl Marker {
  pub fn trace(&mut self, value: &Value) {
    if !self.marked_values.contains(&value.bits) {
      // println!("tracing {value} {:p}", value.pointer());
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

type AllocationSet = HashSet<u64, RandomState>;

pub struct Gc {
  next_run: Instant,
  frequency: Duration,
  pub(crate) allocations: AllocationSet,
  pub(crate) handles: AllocationSet,
  disposer: AsyncDisposal,

  pub(crate) cleans: usize,
  // generational_allocations: Vec<AllocationSet>,
}

impl Gc {
  pub fn new(frequency: Duration) -> Self {
    Self {
      next_run: Instant::now() + frequency,
      frequency,
      allocations: Default::default(),
      handles: Default::default(),
      disposer: AsyncDisposal::new(),
      cleans: 0,
      // generational_allocations: Default::default(),
    }
  }

  pub(crate) fn clean_if_time<'v>(
    &mut self,
    stack: &Stack,
    current_frame: &StackFrame,
    stack_frames: &Vec<StackFrame>,
    envs: &EnvStack,
    cached_values: impl IntoIterator<Item = &'v Value>,
    export: Option<&Value>,
  ) -> Result<usize, SystemError> {
    let now = Instant::now();

    if now < self.next_run {
      return Ok(0);
    }

    self.next_run = now + self.frequency;

    self.clean(stack, current_frame, stack_frames, envs, cached_values, export)
  }

  pub(crate) fn clean<'v>(
    &mut self,
    stack: &Stack,
    current_frame: &StackFrame,
    stack_frames: &Vec<StackFrame>,
    envs: &EnvStack,
    cached_values: impl IntoIterator<Item = &'v Value>,
    export: Option<&Value>,
  ) -> Result<usize, SystemError> {
    let marked_allocations = self.trace(stack, current_frame, stack_frames, envs, cached_values, export);

    let loose_allocations = self.find_unmarked(marked_allocations);

    let cleaned = loose_allocations.len();

    for alloc in &loose_allocations {
      let removed = self.allocations.remove(alloc);
      debug_assert!(removed);
    }

    if cleaned > 0 {
      self.disposer.dispose(loose_allocations)?;
    }

    self.cleans += 1;

    Ok(cleaned)
  }

  fn trace<'v>(
    &self,
    stack: &Stack,
    current_frame: &StackFrame,
    stack_frames: &Vec<StackFrame>,
    envs: &EnvStack,
    cached_values: impl IntoIterator<Item = &'v Value>,
    export: Option<&Value>,
  ) -> AllocationSet {
    let mut marked_allocations = Marker::default();

    for value in stack.iter() {
      marked_allocations.trace(value)
    }

    for value in cached_values {
      marked_allocations.trace(value);
    }

    for value in &self.handles {
      marked_allocations.trace(&Value { bits: *value });
    }

    for ctx in &current_frame.registers {
      for reg in Register::iter() {
        marked_allocations.trace(&ctx[reg]);
      }
    }

    for frame in stack_frames {
      for ctx in &frame.registers {
        for reg in Register::iter() {
          marked_allocations.trace(&ctx[reg]);
        }
      }
    }

    for env in envs.iter() {
      let handle = env.module();
      let value = handle.value();
      marked_allocations.trace(&value);
    }

    if let Some(value) = export {
      marked_allocations.trace(value);
    }

    for handle in &self.handles {
      marked_allocations.trace(&Value { bits: *handle })
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

  pub fn handle_from(this: &mut SmartPtr<Self>, value: Value) -> ValueHandle {
    this.handles.insert(value.bits);
    ValueHandle::new(this.clone(), value)
  }

  pub fn terminate(&mut self) {
    self.disposer.terminate()
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

  fn drop_value(mut value: Value) {
    debug_assert!(value.is_ptr());
    let pointer = value.pointer_mut();
    let meta = value.meta();
    (meta.vtable.dealloc)(pointer);
  }
}

#[cfg(test)]
impl Gc {
  pub fn always_run() -> Self {
    Self::new(Duration::from_nanos(0))
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{code::Reflection, util::FileIdType};
  use ptr::SmartPtr;
  use std::rc::Rc;

  const FILE_ID: FileIdType = 0;

  #[derive(Usertype, Fields)]
  #[uuid("random")]
  struct SomeType {}

  #[methods]
  impl SomeType {}

  fn new_ctx() -> SmartPtr<Context> {
    SmartPtr::new(Context::new(
      0,
      Reflection::new(Some("main"), Some(FILE_ID), Rc::new(Default::default())),
    ))
  }

  #[test]
  fn gc_can_allocate_and_clean() {
    let mut gc = Gc::always_run();
    let ctx = new_ctx();

    gc.allocate(SomeType {});

    let cleaned = gc
      .clean(
        &Default::default(),
        &StackFrame::new(ctx, 0),
        &Default::default(),
        &Default::default(),
        [],
        None,
      )
      .unwrap();
    assert_eq!(cleaned, 1);
  }

  #[test]
  fn gc_does_not_clean_more_than_it_needs_to() {
    let mut gc = Gc::always_run();
    let ctx = new_ctx();

    let _x = gc.allocate(1);
    let _y = gc.allocate(1.0);
    let _b = gc.allocate(true);
    let _c = gc.allocate('c');

    gc.allocate(SomeType {});

    let cleaned = gc
      .clean(
        &Default::default(),
        &StackFrame::new(ctx, 0),
        &Default::default(),
        &Default::default(),
        [],
        None,
      )
      .unwrap();

    assert_eq!(cleaned, 1);
  }

  #[test]
  fn gc_does_not_clean_open_handles() {
    let mut gc = SmartPtr::new(Gc::always_run());
    let ctx = new_ctx();

    let mut value = StructValue::new([(("child", 0), Value::nil)]);
    let child = gc.allocate(SomeType {});
    value.set_field(&mut gc, Field::named("child"), child).unwrap();

    {
      let _handle = Gc::allocate_handle(&mut gc, value);
      let cleaned = gc
        .clean(
          &Default::default(),
          &StackFrame::new(ctx.clone(), 0),
          &Default::default(),
          &Default::default(),
          [],
          None,
        )
        .unwrap();
      assert_eq!(cleaned, 0);
    }

    let cleaned = gc
      .clean(
        &Default::default(),
        &StackFrame::new(ctx, 0),
        &Default::default(),
        &Default::default(),
        [],
        None,
      )
      .unwrap();
    assert_eq!(cleaned, 2);
  }
}
