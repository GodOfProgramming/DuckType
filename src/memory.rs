use super::{ModuleStack, Stack, StackFrame};
use crate::{
  prelude::*,
  value::{tags::*, MutVoid, ValueMeta},
  FastHashSet,
};
use ahash::HashSetExt;
use ptr::MutPtr;
use std::{
  fmt::{self, Display, Formatter},
  mem,
  sync::atomic::AtomicUsize,
};

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

type AllocationSet = FastHashSet<Value>;

pub struct Gc<D = SyncDisposal>
where
  D: Disposal,
{
  mode: GcMode,

  disposer: D,

  pub(crate) allocations: AllocationSet,

  grays: AllocationSet,
  blacks: AllocationSet,

  pub(crate) limit: usize,
  pub(crate) initial_limit: usize,
  pub(crate) allocated_memory: usize,

  pub(crate) deep_cleans: usize,
  pub(crate) incremental_cleans: usize,
  pub(crate) increments: usize,

  num_limit_repeats: usize,
}

impl<D> Gc<D>
where
  D: Disposal,
{
  pub fn new(initial_limit: Memory) -> Self {
    let initial_limit = initial_limit.into();
    Self {
      mode: GcMode::Standard,
      disposer: D::default(),
      allocations: AllocationSet::with_capacity(512),
      blacks: Default::default(),
      grays: Default::default(),
      limit: initial_limit,
      initial_limit,
      allocated_memory: 0,
      deep_cleans: 0,
      incremental_cleans: 0,
      increments: 0,
      num_limit_repeats: 0,
    }
  }

  pub fn set_mode(&mut self, mode: GcMode) {
    self.mode = mode;
  }

  pub(crate) fn poll(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    match self.mode {
      GcMode::Standard => {
        self.poll_deep(stack, envs, cache, stack_frame, stack_frames);
        self.poll_inc(stack, envs, cache, stack_frame, stack_frames);
      }
      GcMode::Incremental => self.poll_inc(stack, envs, cache, stack_frame, stack_frames),
      GcMode::Deep => self.poll_deep(stack, envs, cache, stack_frame, stack_frames),
    }
  }

  pub(crate) fn poll_deep(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    if self.allocated_memory > self.limit {
      self.deep_clean(stack, envs, cache, stack_frame, stack_frames);
    }
  }

  pub(crate) fn poll_inc(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    if self.allocated_memory > self.limit / 2 {
      self.increments += 1;
      self.incremental(stack, envs, cache, stack_frame, stack_frames);
    }
  }

  pub(crate) fn deep_clean(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) -> usize {
    self.grays.clear();
    self.blacks.clear();
    self.ref_check_native_handles(cache);
    self.deep_trace_roots(stack, envs, cache, stack_frame, stack_frames);
    self.deep_cleans += 1;
    self.clean(cache, self.find_unreferenced())
  }

  pub(crate) fn incremental(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    self.ref_check_native_handles(cache);
    self.incremental_trace_roots(stack, envs, cache, stack_frame, stack_frames);
    if self.grays.is_empty() {
      self.incremental_cleans += 1;
      self.clean(cache, self.find_unreferenced());
      self.blacks.clear();
    }
  }

  fn deep_trace_roots(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    let mut tracer = Tracer::new(&mut self.blacks);

    for value in stack.iter() {
      tracer.deep_trace(value)
    }

    cache.deep_trace(&mut tracer);

    for env in envs.iter() {
      let handle = env.module();
      let value = handle.value();
      tracer.deep_trace(&value);
    }

    if let Some(value) = &stack_frame.export {
      tracer.deep_trace(value);
    }

    for frame in stack_frames {
      if let Some(value) = &frame.export {
        tracer.deep_trace(value);
      }
    }
  }

  fn incremental_trace_roots(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &Cache,
    stack_frame: &StackFrame,
    stack_frames: &[StackFrame],
  ) {
    let mut tracer = Tracer::new(&mut self.blacks);

    for value in self.grays.drain() {
      tracer.trace_gray(&value);
    }

    for value in stack.iter() {
      tracer.try_mark_gray(value)
    }

    cache.incremental_trace(&mut tracer);

    for env in envs.iter() {
      let value = env.module().value();
      tracer.try_mark_gray(&value);
    }

    if let Some(value) = &stack_frame.export {
      tracer.try_mark_gray(value);
    }

    for frame in stack_frames {
      if let Some(value) = &frame.export {
        tracer.try_mark_gray(value);
      }
    }

    self.grays = tracer.grays;
  }

  fn clean(&mut self, cache: &mut Cache, unreferenced: Vec<Value>) -> usize {
    let cleaned = unreferenced.len();

    let mut released_memory = 0;
    for value in unreferenced {
      released_memory += value.meta().size;
      cache.forget(value);
      self.purge(value);
    }

    self.calc_new_limit(released_memory);

    cleaned
  }

  #[must_use]
  fn find_unreferenced(&self) -> Vec<Value> {
    self
      .allocations
      .difference(&self.blacks)
      .filter(|value| {
        debug_assert!(value.is_ptr());
        value.is_unreferenced()
      })
      .cloned()
      .collect()
  }

  fn calc_new_limit(&mut self, released: usize) {
    let previously_allocated = self.allocated_memory;

    self.allocated_memory = self.allocated_memory.saturating_sub(released);

    let prev_limit = self.limit;
    self.limit = {
      {
        const REPEAT_LIM: usize = 7;
        const REDUCTION_PERCENT: f64 = 0.7;
        if self.num_limit_repeats > REPEAT_LIM
          && previously_allocated as f64 * REDUCTION_PERCENT > (2.0 - REDUCTION_PERCENT) * self.allocated_memory as f64
        {
          self.num_limit_repeats = 0;
          // memory isn't being allocated rapidly, try to find a middle ground

          usize::max((self.limit as f64 * REDUCTION_PERCENT) as usize, self.initial_limit)
        } else {
          // memory is possibly being allocated rapidly, either increase the limit to account or restore the existing
          usize::max(self.allocated_memory.saturating_mul(2), self.limit)
        }
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

  fn purge(&mut self, value: Value) {
    self.forget(value);
    self.disposer.dispose(value)
  }

  pub(crate) fn invalidate(&mut self, value: &Value) {
    self.grays.remove(&value);
    self.blacks.remove(&value);
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
    };
    Self { obj, meta }
  }
}

pub struct Tracer {
  grays: AllocationSet,
  blacks: MutPtr<AllocationSet>,
}

impl Tracer {
  fn new(blacks: &mut AllocationSet) -> Self {
    Self {
      grays: AllocationSet::default(),
      blacks: MutPtr::new(blacks),
    }
  }

  pub fn deep_trace(&mut self, value: &Value) {
    if value.is_ptr() && !self.blacks.contains(value) {
      self.blacks.insert(*value);
      value.deep_trace_children(self);
    }
  }

  pub fn try_mark_gray(&mut self, value: &Value) {
    if value.is_ptr() && !self.grays.contains(value) && !self.blacks.contains(value) {
      self.grays.insert(*value);
    }
  }

  pub fn trace_gray(&mut self, value: &Value) {
    self.grays.remove(value);
    self.blacks.insert(*value);
    value.incremental_trace_children(self);
  }
}

pub trait Disposal: Default {
  fn dispose(&mut self, value: Value);
}

#[derive(Default)]
pub struct SyncDisposal;

impl Disposal for SyncDisposal {
  fn dispose(&mut self, value: Value) {
    drop_value(value);
  }
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

pub enum GcMode {
  Standard,
  Incremental,
  Deep,
}

#[cfg(test)]
mod tests;

/* Async Disposal impl
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
*/
