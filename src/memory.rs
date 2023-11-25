use super::{ModuleStack, Stack, StackFrame};
use crate::{
  prelude::*,
  value::{tags::*, Mark, MutVoid, ValueMeta},
  FastHashSet,
};
use ahash::HashSetExt;
use std::{
  fmt::{self, Display, Formatter},
  mem,
  sync::{atomic::AtomicUsize, mpsc},
  thread::{self, JoinHandle},
};

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

type AllocationSet = FastHashSet<Value>;

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

pub struct Gc<D = SyncDisposal>
where
  D: Disposal,
{
  disposer: D,

  pub(crate) allocations: FastHashSet<Value>,

  pub(crate) limit: usize,
  pub(crate) allocated_memory: usize,

  pub(crate) num_cycles: usize,

  num_limit_repeats: usize,
}

impl<D> Gc<D>
where
  D: Disposal,
{
  pub fn new(initial_limit: Memory) -> Self {
    Self {
      disposer: D::default(),
      allocations: FastHashSet::with_capacity(512),
      limit: initial_limit.into(),
      allocated_memory: 0,
      num_cycles: 0,
      num_limit_repeats: 0,
    }
  }

  pub fn terminate(&mut self) {
    self.disposer.terminate()
  }

  pub(crate) fn poll(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> Result<(), SystemError> {
    if self.allocated_memory > self.limit {
      self.deep_clean(stack, envs, cache, stack_frame, stack_frames)?;
    }

    Ok(())
  }

  pub(crate) fn deep_clean(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> Result<usize, SystemError> {
    self.ref_check_native_handles(cache);

    let marked_allocations = self.deep_trace_roots(stack, envs, cache, stack_frame, stack_frames);

    let unreferenced_allocations = self.find_unreferenced(marked_allocations);

    let cleaned = unreferenced_allocations.len();

    let mut released_memory = 0;
    for value in unreferenced_allocations {
      released_memory += value.meta().size;
      cache.forget(value);
      self.purge(value).map_err(|e| e.into())?;
    }

    self.calc_new_limit(released_memory);

    self.num_cycles += 1;

    Ok(cleaned)
  }

  fn deep_trace_roots(
    &self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> AllocationSet {
    let mut marked_allocations = Marker::with_estimated_size(stack.len() + envs.len() + cache.len() + 1 + stack_frames.len());

    for value in stack.iter() {
      marked_allocations.trace(value)
    }

    cache.deep_trace(&mut marked_allocations);

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

  fn find_unreferenced(&self, marked_allocations: AllocationSet) -> Vec<Value> {
    self
      .allocations
      .difference(&marked_allocations)
      .filter(|value| {
        debug_assert!(value.is_ptr());
        value.is_unreferenced()
      })
      .cloned()
      .collect()
  }

  fn calc_new_limit(&mut self, released: usize) {
    const REPEAT_LIM: usize = 7;
    const ALLOC_DENOMINATOR: usize = 2;

    let previously_allocated = self.allocated_memory;
    self.allocated_memory = self.allocated_memory.saturating_sub(released);

    let prev_limit = self.limit;
    self.limit = {
      if self.num_limit_repeats > REPEAT_LIM && previously_allocated / ALLOC_DENOMINATOR > self.allocated_memory {
        self.num_limit_repeats = 0;
        // memory isn't being allocated rapidly, try to find a middle ground
        self.limit / ALLOC_DENOMINATOR
      } else {
        // memory is possibly being allocated rapidly, either increase the limit to account or restore the existing
        usize::max(self.allocated_memory.saturating_mul(2), self.limit)
      }
    };

    if self.limit == prev_limit {
      self.num_limit_repeats += 1;
    }
  }

  pub(crate) fn allocate_untracked<T: Usertype>(&mut self, item: T) -> Value {
    fn allocate_type<T>(item: T) -> *mut T {
      Box::into_raw(Box::new(item))
    }

    let allocated = unsafe { &mut *allocate_type(AllocatedObject::new(item)) };
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
    Value::new_pointer(ptr)
  }

  pub fn allocate<T: Usertype>(&mut self, item: T) -> Value {
    let value = self.allocate_untracked(item);

    #[cfg(debug_assertions)]
    {
      debug_assert!(self.allocations.iter().find(|v| v.bits == value.bits).is_none());
    }

    self.track(value);

    value
  }

  fn ref_check_native_handles(&mut self, cache: &mut Cache) {
    let transfers = cache
      .native_handles
      .iter()
      .filter(|v| v.is_unreferenced())
      .cloned()
      .collect::<Vec<Value>>();

    for transfer in transfers {
      cache.native_handles.remove(&transfer);
      self.track(transfer);
    }
  }

  fn track(&mut self, value: Value) {
    debug_assert!(value.is_ptr());
    let is_new = self.allocations.insert(value);
    debug_assert!(is_new)
  }

  pub(crate) fn forget(&mut self, value: Value) {
    self.allocations.remove(&value);
  }

  fn purge(&mut self, value: Value) -> Result<(), D::Error> {
    self.forget(value);
    self.disposer.dispose(value)
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
    if value.is_ptr() && !self.marked_values.contains(value) {
      self.marked_values.insert(*value);
      value.trace_vtable(self);
    }
  }
}

impl From<Marker> for AllocationSet {
  fn from(value: Marker) -> Self {
    value.marked_values
  }
}

pub trait Disposal: Default {
  type Error: Into<SystemError>;
  fn dispose(&mut self, value: Value) -> Result<(), Self::Error>;

  fn terminate(&mut self);
}

pub struct AsyncDisposal {
  chute: Option<mpsc::Sender<Value>>,
  th: Option<JoinHandle<()>>,
}

impl Default for AsyncDisposal {
  fn default() -> Self {
    let (sender, receiver) = mpsc::channel();

    let th = thread::spawn(move || {
      while let Ok(value) = receiver.recv() {
        drop_value(value);
      }
    });

    Self {
      chute: Some(sender),
      th: Some(th),
    }
  }
}

impl Disposal for AsyncDisposal {
  type Error = mpsc::SendError<Value>;

  fn dispose(&mut self, value: Value) -> Result<(), Self::Error> {
    match &self.chute {
      Some(chute) => chute.send(value)?,
      None => Err(mpsc::SendError::<Value>(value))?,
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

#[derive(Default)]
pub struct SyncDisposal;

impl Disposal for SyncDisposal {
  type Error = SystemError;

  fn dispose(&mut self, value: Value) -> Result<(), Self::Error> {
    drop_value(value);
    Ok(())
  }

  fn terminate(&mut self) {}
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

#[cfg(test)]
mod tests;
