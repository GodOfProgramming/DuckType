pub use builtin_types::*;
use static_assertions::assert_eq_size;
use std::{
  any::TypeId,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  mem,
};
pub use tags::*;

use crate::dbg::here;

mod builtin_types;
mod tags;
#[cfg(test)]
mod test;

mod prelude {
  pub use super::{builtin_types::*, Assign, Object, Value};
}

type ConstVoid = *const ();
type MutVoid = *mut ();

// ensuring 64 bit platforms, redundancy is just sanity checks
assert_eq_size!(usize, ConstVoid);
assert_eq_size!(usize, MutVoid);
assert_eq_size!(usize, f64);
assert_eq_size!(usize, u64);

const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub union Value {
  ptr: MutVoid,
  bits: u64,
  f64: f64,
}

impl Value {
  #[allow(non_upper_case_globals)]
  pub const nil: Value = Value { bits: NIL_TAG };

  pub fn tag(&self) -> Tag {
    unsafe {
      if self.bits < INF_VALUE {
        Tag::F64
      } else {
        mem::transmute::<u64, Tag>((self.bits & TAG_BITMASK | INF_VALUE) as u64)
      }
    }
  }

  // float

  pub fn is_f64(&self) -> bool {
    unsafe { self.bits < INF_VALUE }
  }

  pub fn as_f64(&self) -> f64 {
    debug_assert!(self.is_f64());
    unsafe { self.f64 }
  }

  // int

  pub fn is_i32(&self) -> bool {
    self.is_type::<I32_TAG>()
  }

  pub fn as_i32(&self) -> i32 {
    debug_assert!(self.is_i32());
    unsafe { mem::transmute::<u32, i32>((self.bits & !I32_TAG) as u32) }
  }

  // char

  pub fn is_char(&self) -> bool {
    self.is_type::<CHAR_TAG>()
  }

  pub fn as_char(&self) -> char {
    debug_assert!(self.is_char());
    unsafe { mem::transmute::<u32, char>((self.bits & !CHAR_TAG) as u32) }
  }

  // string

  pub fn new_str() -> Self {
    Self::from(Str::default())
  }

  pub fn is_str(&self) -> bool {
    self.is_obj::<Str>()
  }

  pub fn as_str(&self) -> &mut Str {
    debug_assert!(self.is_str());
    self.convert()
  }

  // array

  pub fn new_array() -> Self {
    Self::from(Array::default())
  }

  pub fn is_array(&self) -> bool {
    self.is_obj::<Array>()
  }

  pub fn as_array(&self) -> &mut Array {
    debug_assert!(self.is_array());
    self.convert()
  }

  // struct

  pub fn new_struct() -> Self {
    Self::from(Struct::default())
  }

  pub fn is_struct(&self) -> bool {
    self.is_obj::<Struct>()
  }

  pub fn as_struct(&self) -> &mut Struct {
    debug_assert!(self.is_struct());
    self.convert()
  }

  // error

  pub fn new_err<T: ToString>(msg: T) -> Self {
    Self::from(Error::from(msg.to_string()))
  }

  pub fn is_err(&self) -> bool {
    self.is_obj::<Error>()
  }

  pub fn as_err(&self) -> &mut Error {
    debug_assert!(self.is_err());
    self.convert()
  }

  // obj pointer

  pub fn is_obj<T: Object>(&self) -> bool {
    (self.is_type::<POINTER_TAG>() && self.type_id() == T::type_id())
  }

  pub fn as_obj<T: Object>(&self) -> &mut T {
    self.convert()
  }

  // nil

  pub fn is_nil(&self) -> bool {
    self.is_type::<NIL_TAG>()
  }

  // Object Methods

  pub fn set(&mut self, name: &str, value: Value) -> Result<(), Error> {
    (self.vtable().set)(self.pointer(), name, value)
  }

  pub fn get(&self, name: &str) -> Value {
    (self.vtable().get)(self.pointer(), name)
  }

  pub fn add(&self, other: Value) -> Value {
    (self.vtable().add)(self.pointer(), other)
  }

  pub fn sub(&self, other: Value) -> Value {
    (self.vtable().sub)(self.pointer(), other)
  }

  pub fn mul(&self, other: Value) -> Value {
    (self.vtable().mul)(self.pointer(), other)
  }

  pub fn div(&self, other: Value) -> Value {
    (self.vtable().div)(self.pointer(), other)
  }

  pub fn rem(&self, other: Value) -> Value {
    (self.vtable().rem)(self.pointer(), other)
  }

  pub fn basic_desc(&self) -> &'static str {
    (self.vtable().basic_desc)()
  }

  fn pointer(&self) -> MutVoid {
    unsafe { (self.bits & POINTER_BITMASK) as MutVoid }
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

  // TypeId of the underlying type
  fn type_id(&self) -> TypeId {
    (self.vtable().type_id)()
  }

  fn convert<T>(&self) -> &mut T {
    unsafe { &mut *(self.pointer() as *mut T) }
  }

  fn allocate<T: Object>(item: T) -> Self {
    let allocated = unsafe { &mut *(Box::into_raw(Box::new(AllocatedObject::new(item)))) };

    let ptr = &mut allocated.obj as *mut T as MutVoid;
    debug_assert_eq!(
      allocated as *const _ as *const (),
      &allocated.meta as *const _ as *const ()
    );

    // ensure the pointer to the allocated object is offset by the right distance
    debug_assert_eq!(
      unsafe { (ptr as *const u8).offset(META_OFFSET) as *const () },
      allocated as *const _ as *const ()
    );

    // ensure the pointer fits in 48 bits
    debug_assert_eq!(ptr as u64 & POINTER_TAG, 0);

    // return the pointer to the object, hiding the vtable & ref count behind the returned address
    Value {
      bits: ptr as u64 | POINTER_TAG,
    }
  }
}

impl Default for Value {
  fn default() -> Self {
    Self { bits: NIL_TAG }
  }
}

impl Drop for Value {
  fn drop(&mut self) {
    if self.tag() == Tag::Pointer {
      let meta = self.meta();

      meta.ref_count -= 1;

      if meta.ref_count == 0 {
        (meta.vtable.drop)(self.pointer());
        (meta.vtable.dealloc)(self.pointer());
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

impl From<f64> for Value {
  fn from(item: f64) -> Self {
    Self { f64: item }
  }
}

impl From<i32> for Value {
  fn from(item: i32) -> Self {
    Self {
      bits: (item as u64) | (Tag::I32 as u64),
    }
  }
}

impl From<String> for Value {
  fn from(item: String) -> Self {
    Value::allocate::<Str>(item.into())
  }
}

impl From<&[Value]> for Value {
  fn from(vec: &[Value]) -> Self {
    Value::allocate::<Array>(vec.into())
  }
}

impl From<Nil> for Value {
  fn from(_: Nil) -> Self {
    Self { bits: NIL_TAG }
  }
}

impl<T> From<T> for Value
where
  T: Object,
{
  fn from(item: T) -> Self {
    Value::allocate::<T>(item)
  }
}

impl Assign<i32> for Value {}

impl Assign<f64> for Value {}

impl<T: Object> Assign<T> for Value {}

impl Assign<Nil> for Value {}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => write!(f, "{}", self.as_f64()),
      Tag::I32 => write!(f, "{}", self.as_i32()),
      Tag::Char => write!(f, "{}", self.as_char()),
      Tag::Pointer => write!(f, "{}", self.basic_desc()),
      Tag::Nil => write!(f, "nil"),
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

struct VTable {
  set: fn(MutVoid, name: &str, value: Value) -> Result<(), Error>,
  get: fn(ConstVoid, name: &str) -> Value,
  add: fn(ConstVoid, other: Value) -> Value,
  sub: fn(ConstVoid, other: Value) -> Value,
  mul: fn(ConstVoid, other: Value) -> Value,
  div: fn(ConstVoid, other: Value) -> Value,
  rem: fn(ConstVoid, other: Value) -> Value,
  drop: fn(MutVoid),
  dealloc: fn(MutVoid),
  type_id: fn() -> TypeId,
  basic_desc: fn() -> &'static str,
}

impl VTable {
  fn new<T: Object>() -> Self {
    Self {
      set: |this, name, value| {
        <T as Object>::set(unsafe { &mut *Self::void_to_mut(this) }, name, value)
      },
      get: |this, name| <T as Object>::get(unsafe { &*Self::void_to(this) }, name),
      add: |this, other| <T as Object>::add(unsafe { &*Self::void_to(this) }, other),
      sub: |this, other| <T as Object>::sub(unsafe { &*Self::void_to(this) }, other),
      mul: |this, other| <T as Object>::mul(unsafe { &*Self::void_to(this) }, other),
      div: |this, other| <T as Object>::div(unsafe { &*Self::void_to(this) }, other),
      rem: |this, other| <T as Object>::rem(unsafe { &*Self::void_to(this) }, other),
      drop: |this| <T as Object>::drop(unsafe { &mut *Self::void_to_mut(this) }),
      dealloc: |this| <T as Object>::dealloc(this as *mut T),
      type_id: || <T as Object>::type_id(),
      basic_desc: || <T as Object>::basic_desc(),
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

#[repr(C)]
struct AllocatedObject<T: Object> {
  meta: ValueMeta,
  obj: T,
}

impl<T: Object> AllocatedObject<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: 1,
      vtable: VTable::new::<T>(),
    };
    Self { obj, meta: meta }
  }
}

pub trait Assign<T>: From<T>
where
  Self: Sized,
{
  fn assign(&mut self, t: T) {
    *self = Self::from(t);
  }
}

pub trait ValueType: Object {}
