pub use builtin_types::*;
use static_assertions::assert_eq_size;
use std::{
  any::TypeId,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  mem,
};
pub use tags::*;

use crate::{Env, ExecutionThread};

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

  pub fn type_of(&self) -> Type {
    if self.is_f64() {
      Type::F64
    } else if self.is_i32() {
      Type::I32
    } else if self.is_bool() {
      Type::Bool
    } else if self.is_char() {
      Type::Char
    } else if self.is_str() {
      Type::String
    } else if self.is_array() {
      Type::Array
    } else if self.is_struct() {
      Type::Struct
    } else if self.is_class() {
      Type::Class
    } else if self.is_instance() {
      Type::Instance
    } else if self.is_fn() {
      Type::Function
    } else if self.is_method() {
      Type::Method
    } else if self.is_native_fn() {
      Type::NativeFn
    } else if self.is_native_method() {
      Type::NativeMethod
    } else if self.is_err() {
      Type::Error
    } else {
      Type::UserData
    }
  }

  pub fn truthy(&self) -> bool {
    !self.is_nil() && (!self.is_bool() || self.as_bool())
  }

  pub fn falsy(&self) -> bool {
    self.is_nil() || (self.is_bool() && !self.as_bool())
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

  // bool

  pub fn is_bool(&self) -> bool {
    self.is_type::<BOOL_TAG>()
  }

  pub fn as_bool(&self) -> bool {
    debug_assert!(self.is_bool());
    unsafe { self.bits & POINTER_BITMASK > 0 }
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

  pub fn as_str(&mut self) -> &mut Str {
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

  pub fn as_array(&mut self) -> &mut Array {
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

  pub fn as_struct(&mut self) -> &mut Struct {
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

  pub fn as_err(&mut self) -> &mut Error {
    debug_assert!(self.is_err());
    self.convert()
  }

  // class

  pub fn new_class<T: ToString>(name: T) -> Self {
    Self::from(Class::new(name.to_string()))
  }

  pub fn is_class(&self) -> bool {
    self.is_obj::<Class>()
  }

  pub fn as_class(&mut self) -> &mut Class {
    debug_assert!(self.is_class());
    self.convert()
  }

  // instance

  pub fn is_instance(&self) -> bool {
    self.is_obj::<Instance>()
  }

  pub fn as_instance(&mut self) -> &mut Instance {
    debug_assert!(self.is_instance());
    self.convert()
  }

  // function

  pub fn is_fn(&self) -> bool {
    self.is_obj::<Function>()
  }

  pub fn as_fn(&mut self) -> &mut Function {
    debug_assert!(self.is_fn());
    self.convert()
  }

  // method

  pub fn is_method(&self) -> bool {
    self.is_obj::<Method>()
  }

  pub fn as_method(&mut self) -> &mut Method {
    debug_assert!(self.is_method());
    self.convert()
  }

  // native

  pub fn new_native_fn<F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static>(
    name: &str,
    f: F,
  ) -> Self {
    Self::from(NativeFn::new(name, f))
  }

  pub fn is_native_fn(&self) -> bool {
    self.is_obj::<NativeFn>()
  }

  pub fn as_native_fn(&mut self) -> &mut NativeFn {
    debug_assert!(self.is_native_fn());
    self.convert()
  }

  pub fn new_native_method<
    F: FnMut(&mut ExecutionThread, &mut Env, Value, Vec<Value>) -> Value + 'static,
  >(
    name: &str,
    f: F,
  ) -> Self {
    Self::from(NativeMethod::new(name, f))
  }

  pub fn is_native_method(&self) -> bool {
    self.is_obj::<NativeMethod>()
  }

  pub fn as_native_method(&mut self) -> &mut NativeMethod {
    debug_assert!(self.is_native_method());
    self.convert()
  }

  // obj pointer

  pub fn is_ptr(&self) -> bool {
    self.is_type::<POINTER_TAG>()
  }

  pub fn is_obj<T: Object>(&self) -> bool {
    self.is_ptr() && self.type_id() == T::type_id()
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

  // utility

  /// Executes f only if self is nil, otherwise returns self
  pub fn or_else<F: FnOnce() -> Self>(self, f: F) -> Self {
    if self.is_nil() {
      f()
    } else {
      self
    }
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
      bits: unsafe { mem::transmute::<i32, u32>(item) as u64 } | I32_TAG,
    }
  }
}

impl From<bool> for Value {
  fn from(item: bool) -> Self {
    Self {
      bits: if item { 1 } else { 0 } | BOOL_TAG,
    }
  }
}

impl From<char> for Value {
  fn from(item: char) -> Self {
    Self {
      bits: unsafe { mem::transmute::<char, u32>(item) as u64 } | CHAR_TAG,
    }
  }
}

impl From<&str> for Value {
  fn from(item: &str) -> Self {
    Value::allocate::<Str>(item.into())
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
      Tag::Bool => write!(f, "{}", self.as_bool()),
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
