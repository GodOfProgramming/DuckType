use crate::{Env, ExecutionThread};

use super::New;
use static_assertions::assert_eq_size;
use std::{
  any::TypeId,
  collections::BTreeMap,
  fmt::{Debug, Formatter, Result as FmtResult},
  mem,
};

#[cfg(test)]
mod test;

type ConstVoid = *const ();
type MutVoid = *mut ();

assert_eq_size!(usize, f64);
assert_eq_size!(usize, i64);
assert_eq_size!(usize, MutVoid);

const NIL_VALUE: MutVoid = 0 as usize as MutVoid;

const INF_VALUE: u64 = 0xfff8000000000000;
const TAG_BITMASK: u64 = 0x0007000000000000;

const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

const fn make_tag<const I: u8>() -> u64 {
  ((I as u64) << 48) | INF_VALUE
}

const INTEGER_TAG: u64 = make_tag::<1>();
const POINTER_TAG: u64 = make_tag::<7>();

#[repr(u64)]
#[derive(Debug, PartialEq)]
pub enum Tag {
  Float = 0,
  Integer = INTEGER_TAG,
  Pointer = POINTER_TAG, // nil, struct, class, instance, function, etc..
}

pub union Value {
  ptr: MutVoid,
  bits: u64,
  f64: f64,
}

impl Value {
  fn pointer(&self) -> MutVoid {
    unsafe { (self.bits & !(Tag::Pointer as u64)) as MutVoid }
  }

  pub fn tag(&self) -> Tag {
    unsafe {
      if self.bits < INF_VALUE {
        Tag::Float
      } else {
        mem::transmute::<u64, Tag>((self.bits & TAG_BITMASK | INF_VALUE) as u64)
      }
    }
  }

  pub fn is_nil(&self) -> bool {
    self.pointer() == NIL_VALUE
  }

  pub fn is_int(&self) -> bool {
    self.is_type::<INTEGER_TAG>()
  }

  pub fn as_int(&self) -> i32 {
    debug_assert!(self.is_int());
    unsafe { (self.bits & !INTEGER_TAG) as i32 }
  }

  pub fn is_float(&self) -> bool {
    unsafe { self.bits < INF_VALUE }
  }

  pub fn as_float(&self) -> f64 {
    debug_assert!(self.is_float());
    unsafe { self.f64 }
  }

  pub fn is_kind<T: Object>(&self) -> bool {
    self.is_type::<POINTER_TAG>() && self.kind() == T::kind()
  }

  pub fn as_struct(&self) -> &mut Struct {
    self.convert::<Struct>()
  }

  fn meta(&self) -> &mut ValueMeta {
    unsafe { &mut *((self.pointer() as *mut u8).offset(META_OFFSET) as *mut ValueMeta) }
  }

  fn vtable(&self) -> &VTable {
    &self.meta().vtable
  }

  fn is_type<const T: u64>(&self) -> bool {
    unsafe { self.bits & T == T }
  }

  fn kind(&self) -> TypeId {
    (self.vtable().kind)()
  }

  fn convert<T>(&self) -> &mut T {
    unsafe { &mut *(self.pointer() as *mut T) }
  }
}

impl Default for Value {
  fn default() -> Self {
    Self { ptr: NIL_VALUE }
  }
}

impl Drop for Value {
  fn drop(&mut self) {
    if self.tag() == Tag::Pointer {
      let meta = self.meta();

      meta.ref_count -= 1;

      if meta.ref_count == 0 {
        (meta.vtable.drop)(self.pointer());
        (meta.vtable.dealloc)(self.pointer())
      }
    }
  }
}

impl Clone for Value {
  fn clone(&self) -> Self {
    if self.tag() == Tag::Pointer {
      self.meta().ref_count += 1;
    }
    unsafe { Self { bits: self.bits } }
  }
}

impl New<f64> for Value {
  fn new(item: f64) -> Self {
    Self { f64: item }
  }
}

impl New<i32> for Value {
  fn new(item: i32) -> Self {
    Self {
      bits: (item as u64) | (Tag::Integer as u64),
    }
  }
}

impl<T> New<T> for Value
where
  T: Object,
{
  fn new(item: T) -> Self {
    let allocated = unsafe { &mut *(Box::into_raw(Box::new(AllocatedObject::new(item)))) };

    let ptr = &mut allocated.obj as *mut T as MutVoid;
    debug_assert_eq!(
      allocated as *const _ as *const (),
      &allocated._meta as *const _ as *const ()
    );

    debug_assert_eq!(
      unsafe { (ptr as *const u8).offset(META_OFFSET) as *const () },
      allocated as *const _ as *const ()
    );

    // ensure the pointer fits in 48 bits
    debug_assert_eq!(ptr as u64 & Tag::Pointer as u64, 0);

    Value {
      bits: ptr as u64 | Tag::Pointer as u64,
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    unsafe { write!(f, "{:p}, {:X}, {}", self.ptr, self.bits, self.f64) }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    unsafe { self.bits == other.bits }
  }
}

impl Object for Value {
  fn set(&mut self, name: &str, value: Value) {
    (self.vtable().set)(self.pointer(), name, value)
  }

  fn get(&self, name: &str) -> Option<Value> {
    (self.vtable().get)(self.pointer(), name)
  }

  fn drop(&mut self) {
    (self.vtable().drop)(self.pointer())
  }
}

struct VTable {
  kind: fn() -> TypeId,
  set: fn(MutVoid, name: &str, value: Value),
  get: fn(ConstVoid, name: &str) -> Option<Value>,
  drop: fn(MutVoid),
  dealloc: fn(MutVoid),
}

impl VTable {
  fn new<T: Object>() -> Self {
    Self {
      kind: || <T as Object>::kind(),
      set: |this, name, value| {
        <T as Object>::set(unsafe { &mut *Self::void_to_mut(this) }, name, value)
      },
      get: |this, name| <T as Object>::get(unsafe { &*Self::void_to(this) }, name),
      drop: |this| <T as Object>::drop(unsafe { &mut *Self::void_to_mut(this) }),
      dealloc: |this| <T as Object>::dealloc(this as *mut T),
    }
  }

  fn void_to<T>(ptr: ConstVoid) -> *const T {
    ptr as *const T
  }

  fn void_to_mut<T>(ptr: MutVoid) -> *mut T {
    ptr as *mut T
  }
}

struct ValueMeta {
  ref_count: usize,
  vtable: VTable,
}

pub trait Object
where
  Self: Sized + 'static,
{
  fn kind() -> TypeId {
    TypeId::of::<Self>()
  }

  fn set(&mut self, name: &str, value: Value);
  fn get(&self, name: &str) -> Option<Value>;
  fn drop(&mut self) {}
  fn dealloc(this: *mut Self) {
    consume::<Self>(this);
  }
}

struct AllocatedObject<T: Object> {
  _meta: ValueMeta,
  obj: T,
}

impl<T: Object> AllocatedObject<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: 1,
      vtable: VTable::new::<T>(),
    };
    Self { obj, _meta: meta }
  }
}

#[derive(Default)]
pub struct Struct {
  pub members: BTreeMap<String, Value>,
}

impl Object for Struct {
  fn set(&mut self, name: &str, value: Value) {
    self.members.insert(name.to_string(), value);
  }

  fn get(&self, name: &str) -> Option<Value> {
    self.members.get(name).cloned()
  }
}

pub struct Class {}

pub struct Instance {}

// type NativeFnTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static;
type NativeFnType = dyn FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String>;

pub struct NativeFn {
  pub name: String,
  pub callee: Box<NativeFnType>,
}

impl NativeFn {
  pub fn new<F>(name: String, callee: F) -> Self
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static,
  {
    Self {
      name,
      callee: Box::new(callee),
    }
  }
}

fn consume<T: Object>(this: *mut T) {
  unsafe {
    Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>);
  }
}
