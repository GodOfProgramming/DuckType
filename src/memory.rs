use super::{ModuleStack, Stack, StackFrame};
use crate::{
  FastHashSet, RapidHashSet,
  prelude::*,
  value::{ConstVoid, MutVoid, ValueMeta, tags::*},
};
use ahash::HashSetExt;
use ptr::MutPtr;
use std::{
  fmt::{self, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
  sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

type AllocationSet = FastHashSet<Value>;

pub(crate) type ConstAddr<T> = *const T;
pub(crate) type MutAddr<T> = *mut T;

pub trait Gc {
  fn allocate<T: Usertype>(&mut self, item: T) -> Value;
  fn allocate_handle<T: Usertype>(&mut self, item: T) -> UsertypeHandle<T>;
}

/// A simple and reliable garbage collector
pub struct SimpleGc<D = SyncDisposal>
where
  D: Disposer,
{
  /// The current mode the GC is running in
  mode: GcMode,

  /// The method of disposal of allocations
  disposer: D,

  /// Addresses allocated by the GC and available for collection
  allocations: AllocationSet,

  /// Addresses allocated by the GC while it is in the process of incremental collection
  protected_allocations: AllocationSet,

  /// Allocations that point to unmarked allocations
  untraced_allocations: AllocationSet,

  /// Allocations that do not point to unmarked allocations
  traced_allocations: AllocationSet,

  /// Set of values that are used in native code
  native_handles: RapidHashSet<Value>,

  /// The maximum amount of bytes allowed to be allocated before a deep clean
  limit: usize,

  /// The initial limit that the GC was started with
  initial_limit: usize,

  /// The current number of bytes allocated by the GC, both regular allocations and protected
  allocated_memory: usize,

  /// Statistics of the GC
  pub stats: GcStats,

  /// The max number of repeated limits in a row before decreasing the limit closer to the initial value
  num_limit_repeats: usize,
}

impl<D> Gc for SimpleGc<D>
where
  D: Disposer,
{
  /// Allocate and track a new value
  #[profiling::function]
  fn allocate<T: Usertype>(&mut self, item: T) -> Value {
    let value = self.allocate_untracked(item);

    #[cfg(debug_assertions)]
    {
      debug_assert!(!self.allocations.iter().any(|v| v.bits == value.bits));
    }

    self.track(value);

    value
  }

  #[profiling::function]
  fn allocate_handle<T: Usertype>(&mut self, item: T) -> UsertypeHandle<T> {
    let value = self.allocate_untracked(item);
    let handle = self.make_handle(value);
    UsertypeHandle::new(handle)
  }
}

impl<D> SimpleGc<D>
where
  D: Disposer,
{
  /// Create a new garbage collector with the specified memory limit/target
  pub fn new(initial_limit: Memory) -> Self {
    let initial_limit = initial_limit.into();
    Self {
      mode: GcMode::Standard,
      disposer: D::default(),
      allocations: AllocationSet::with_capacity(512),
      protected_allocations: AllocationSet::new(),
      native_handles: RapidHashSet::default(),
      traced_allocations: Default::default(),
      untraced_allocations: Default::default(),
      limit: initial_limit,
      initial_limit,
      allocated_memory: 0,
      stats: GcStats::default(),
      num_limit_repeats: 0,
    }
  }

  /// Set which mode the GC should operate on
  pub fn set_mode(&mut self, mode: GcMode) {
    self.mode = mode;
  }

  /// Check if the GC routine of the current configuration should run
  #[profiling::function]
  pub(super) fn poll(&mut self, stack: &Stack, envs: &ModuleStack, cache: &mut Cache, call_stack: &[StackFrame]) {
    match self.mode {
      GcMode::Standard => {
        self.poll_deep(stack, envs, cache, call_stack);
        self.poll_inc(stack, envs, cache, call_stack);
      }
      GcMode::Incremental => self.poll_inc(stack, envs, cache, call_stack),
      GcMode::Deep => self.poll_deep(stack, envs, cache, call_stack),
    }
  }

  /// Check if a deep clean should be performed
  #[profiling::function]
  fn poll_deep(&mut self, stack: &Stack, envs: &ModuleStack, cache: &mut Cache, call_stack: &[StackFrame]) {
    if self.allocated_memory > self.limit {
      self.deep_clean(stack, envs, cache, call_stack);
    }
  }

  /// Check if an incremental clean should start and if so begin
  #[profiling::function]
  fn poll_inc(&mut self, stack: &Stack, envs: &ModuleStack, cache: &mut Cache, call_stack: &[StackFrame]) {
    if self.allocated_memory > self.limit / 2 {
      self.stats.total_increments += 1;
      self.incremental(stack, envs, cache, call_stack);
    }
  }

  /// Performs a full deep clean of all objects
  #[profiling::function]
  pub(super) fn deep_clean(
    &mut self,
    stack: &Stack,
    envs: &ModuleStack,
    cache: &mut Cache,
    call_stack: &[StackFrame],
  ) -> usize {
    self.ref_check_native_handles();

    self.drain_protected();

    self.untraced_allocations.clear();
    self.traced_allocations.clear();

    self.deep_trace_roots(stack, envs, cache, call_stack);

    self.stats.total_deep_cleans += 1;

    self.clean(cache, self.find_unreferenced())
  }

  /// Perform an incremental clean
  ///
  /// If this is the first increment the roots are scanned
  /// Otherwise the next tier of grays is scanned
  #[profiling::function]
  fn incremental(&mut self, stack: &Stack, envs: &ModuleStack, cache: &mut Cache, call_stack: &[StackFrame]) {
    self.ref_check_native_handles();

    if self.untraced_allocations.is_empty() {
      self.stats.total_incremental_root_traces += 1;
      self.incremental_trace_roots(stack, envs, cache, call_stack);
    } else {
      self.stats.total_incremental_traces += 1;
      self.incremental_trace();
    }

    if self.untraced_allocations.is_empty() {
      self.stats.total_incremental_cleans += 1;

      self.clean(cache, self.find_unreferenced());

      self.drain_protected();

      self.traced_allocations.clear();
    }
  }

  #[profiling::function]
  fn deep_trace_roots(&mut self, stack: &Stack, envs: &ModuleStack, cache: &Cache, call_stack: &[StackFrame]) {
    let mut tracer = Tracer::new(&mut self.traced_allocations);

    let values = self
      .native_handles
      .iter()
      .cloned()
      .chain(stack.iter().cloned())
      .chain(envs.iter().map(UsertypeHandle::value))
      .chain(call_stack.iter().filter_map(|f| f.export.as_ref()).cloned());

    for value in values {
      tracer.deep_trace(value)
    }

    cache.deep_trace(&mut tracer);
  }

  #[profiling::function]
  fn incremental_trace_roots(&mut self, stack: &Stack, envs: &ModuleStack, cache: &Cache, call_stack: &[StackFrame]) {
    let mut tracer = Tracer::new(&mut self.traced_allocations);

    let values = self
      .native_handles
      .iter()
      .cloned()
      .chain(stack.iter().cloned())
      .chain(envs.iter().map(UsertypeHandle::value))
      .chain(call_stack.iter().filter_map(|f| f.export.as_ref()).cloned());

    for value in values {
      tracer.try_mark_gray(value)
    }

    cache.incremental_trace(&mut tracer);

    self.untraced_allocations = tracer.grays;
  }

  #[profiling::function]
  fn incremental_trace(&mut self) {
    let mut tracer = Tracer::new(&mut self.traced_allocations);

    for value in self.untraced_allocations.drain() {
      tracer.trace_gray(&value);
    }

    self.untraced_allocations = tracer.grays;
  }

  #[profiling::function]
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

  /// Find all unreferenced allocations after a scan
  #[profiling::function]
  #[must_use]
  fn find_unreferenced(&self) -> Vec<Value> {
    self
      .allocations
      .difference(&self.traced_allocations)
      .cloned()
      .filter(garbage)
      .collect()
  }

  /// Calculate the new limit for the GC
  #[profiling::function]
  fn calc_new_limit(&mut self, released: usize) {
    let previously_allocated = self.allocated_memory;

    self.allocated_memory = self.allocated_memory.saturating_sub(released);

    let prev_limit = self.limit;
    self.limit = {
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
    };

    if self.limit == prev_limit {
      self.num_limit_repeats += 1;
    }
  }

  /// Create a new allocation but do not track it
  ///
  /// Without being turned into a handle or calling track, this is a memory leak
  fn allocate_untracked<T: Usertype>(&mut self, item: T) -> Value {
    fn allocate_type<T>(item: T) -> &'static mut T {
      unsafe { &mut *Box::into_raw(Box::new(item)) }
    }

    let allocated_obj = allocate_type(AllocatedObject::new(item, self.is_cleaning()));
    self.allocated_memory += allocated_obj.meta.size;

    let ptr = &raw mut allocated_obj.obj as MutVoid;
    debug_assert_eq!(
      &raw const *allocated_obj as ConstVoid,
      &raw const allocated_obj.meta as ConstVoid
    );

    // ensure the pointer to the allocated object is offset by the right distance
    debug_assert_eq!(
      unsafe { (ptr as ConstAddr<u8>).offset(META_OFFSET) as ConstVoid },
      &raw const *allocated_obj as ConstVoid
    );

    // ensure the pointer fits in 48 bits
    debug_assert_eq!(ptr as u64 & POINTER_TAG, 0);

    // return the pointer to the object, hiding the vtable & ref count behind the returned address
    Value::new_pointer(ptr)
  }

  /// Make a handle from a value
  ///
  /// This is used when values don't start off as tracked so adding a call to forget() would serve no purpose
  pub(super) fn make_handle(&mut self, value: Value) -> ValueHandle {
    self.native_handles.insert(value);
    ValueHandle::new(value)
  }

  /// Check the ref count of native handles, any that are 0 transfer back to the main allocation set
  fn ref_check_native_handles(&mut self) {
    let transfers = self
      .native_handles
      .iter()
      .cloned()
      .filter(Value::is_unreferenced)
      .collect::<Vec<Value>>();

    for value in transfers {
      self.native_handles.remove(&value);
      self.track(value);
    }
  }

  /// Track an allocation in the GC
  ///
  /// Values that aren't pointers shouldn't be added here
  fn track(&mut self, value: Value) {
    debug_assert!(value.is_ptr());

    let is_new = if self.is_cleaning() {
      self.protected_allocations.insert(value)
    } else {
      self.allocations.insert(value)
    };

    debug_assert!(is_new)
  }

  /// Have the GC forget about a value and stop tracking it
  pub(super) fn forget(&mut self, value: Value) {
    self.allocations.remove(&value);
    self.protected_allocations.remove(&value);
  }

  /// Remove a value and deallocate it
  fn purge(&mut self, value: Value) {
    debug_assert!(!self.protected_allocations.contains(&value));

    self.forget(value);
    self.disposer.dispose(value)
  }

  /// Drain all protected values into the set of allocations
  fn drain_protected(&mut self) {
    self
      .allocations
      .extend(self.protected_allocations.drain().map(Value::unprotect));
  }

  /// Whether or not the GC is in the middle of a cycle
  fn is_cleaning(&self) -> bool {
    !self.untraced_allocations.is_empty()
  }

  /// Return a string representing the current stats of the GC
  pub fn stats_string(&self) -> String {
    let inc_ratio = self.stats.total_increments as f64 / self.stats.total_incremental_cleans as f64;
    let inc_per_deep_ratio = self.stats.total_deep_cleans as f64 / self.stats.total_incremental_cleans as f64;
    let incremental_traces_per_root_scan =
      self.stats.total_incremental_traces as f64 / self.stats.total_incremental_root_traces as f64;

    format!(
      concat!(
        "------------------ Gc Stats ------------------\n",
        "Memory in use ---------- {allocated_memory}\n",
        "Limit till next cycle -- {limit}\n",
        "No. allocations -------- {no_allocations}\n",
        "No. handles ------------ {no_handles}\n",
        "No. deep cleans -------- {no_deep_cleans}\n",
        "No. inc cleans --------- {no_inc_cleans}\n",
        "No. increments --------- {no_increments}\n",
        "No. inc root traces ---- {no_inc_root_traces}\n",
        "No. inc traces --------- {no_traces}\n",
        "Inc. trace/root scan --- {incremental_traces_per_root_scan}\n",
        "Inc. ratio ------------- {inc_ratio}\n",
        "Deep/Inc ratio --------- {inc_per_deep_ratio}",
      ),
      allocated_memory = self.allocated_memory,
      limit = self.limit,
      no_allocations = self.allocations.len(),
      no_handles = self.native_handles.len(),
      no_deep_cleans = self.stats.total_deep_cleans,
      no_inc_cleans = self.stats.total_incremental_cleans,
      no_increments = self.stats.total_increments,
      no_inc_root_traces = self.stats.total_incremental_root_traces,
      no_traces = self.stats.total_incremental_traces,
      incremental_traces_per_root_scan = incremental_traces_per_root_scan,
      inc_ratio = inc_ratio,
      inc_per_deep_ratio = inc_per_deep_ratio,
    )
  }
}

#[cfg(test)]
impl SimpleGc {
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
  fn new(obj: T, protected: bool) -> Self {
    let meta = ValueMeta {
      vtable: &T::VTABLE,
      references: AtomicUsize::new(0),
      protected: AtomicBool::new(protected),
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

  pub fn deep_trace(&mut self, value: Value) {
    if value.is_ptr() && !self.blacks.contains(&value) {
      self.blacks.insert(value);
      value.deep_trace_children(self);
    }
  }

  pub fn try_mark_gray(&mut self, value: Value) {
    if value.is_ptr() && !self.grays.contains(&value) && !self.blacks.contains(&value) {
      self.grays.insert(value);
    }
  }

  pub fn trace_gray(&mut self, value: &Value) {
    self.grays.remove(value);
    self.blacks.insert(*value);
    value.incremental_trace_children(self);
  }
}

pub trait Disposer: Default {
  fn dispose(&mut self, value: Value);
}

#[derive(Default)]
pub struct SyncDisposal;

impl Disposer for SyncDisposal {
  fn dispose(&mut self, value: Value) {
    debug_assert!(value.is_unprotected());
    debug_assert!(value.is_unreferenced());
    drop_value(value);
  }
}

pub(crate) fn consume<T: Usertype>(this: MutAddr<T>) {
  let _ = unsafe { Box::from_raw((this as MutAddr<u8>).offset(META_OFFSET) as MutAddr<AllocatedObject<T>>) };
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
  /// Check for a deep clean first, then incremental
  Standard,

  /// Only perform incremental cleans
  Incremental,

  /// Only perform deep cleans
  Deep,
}

#[derive(Default)]
pub struct GcStats {
  total_deep_cleans: usize,
  total_incremental_cleans: usize,
  total_increments: usize,
  total_incremental_root_traces: usize,
  total_incremental_traces: usize,
}

impl GcStats {
  pub fn reset(&mut self) {
    *self = Self::default();
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
      usertype: MutPtr::new(handle.value.unchecked_cast_to_mut::<T>()),
      handle,
    }
  }

  pub fn value(&self) -> Value {
    self.handle.value
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
  pub fn new(value: Value) -> ValueHandle {
    if value.is_ptr() {
      value.meta().references.fetch_add(1, Ordering::Relaxed);
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
    handle.value
  }
}

impl Clone for ValueHandle {
  fn clone(&self) -> Self {
    if self.value.is_ptr() {
      self.value.meta().references.fetch_add(1, Ordering::Relaxed);
    }

    Self { value: self.value }
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
      let before = meta.references.load(Ordering::Relaxed);

      meta.references.fetch_sub(1, Ordering::Relaxed);

      #[cfg(debug_assertions)]
      {
        let after = meta.references.load(Ordering::Relaxed);

        debug_assert!(before > after);
      }
    }
  }
}

fn garbage(value: &Value) -> bool {
  value.is_unreferenced() && value.is_unprotected()
}

#[cfg(test)]
mod tests;
