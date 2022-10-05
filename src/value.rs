pub use builtin_types::*;
use static_assertions::assert_eq_size;
use std::{
  any::TypeId,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  mem,
  ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
};
pub use tags::*;

use crate::{Env, ExecutionThread};

mod builtin_types;
mod tags;
#[cfg(test)]
mod test;

pub mod prelude {
  pub use super::{builtin_types::*, Assign, ComplexValue, Value};
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

  pub fn type_of(&self) -> Type {
    if self.is_f64() {
      Type::F64
    } else if self.is_i32() {
      Type::I32
    } else if self.is_bool() {
      Type::Bool
    } else if self.is_char() {
      Type::Char
    } else if self.is_ptr() {
      Type::Object
    } else {
      Type::Undefined
    }
  }

  pub fn truthy(&self) -> bool {
    !self.is_nil() && (!self.is_bool() || unsafe { self.bits > 0 })
  }

  pub fn falsy(&self) -> bool {
    self.is_nil() || (self.is_bool() && unsafe { self.bits == 0 })
  }

  // float

  pub fn is_f64(&self) -> bool {
    unsafe { self.bits < INF_VALUE }
  }

  pub fn as_f64(&self) -> ConversionResult<f64> {
    if self.is_f64() {
      Ok(unsafe { self.f64 })
    } else {
      Err(ConversionError::WrongType)
    }
  }

  // int

  pub fn is_i32(&self) -> bool {
    self.is_type::<I32_TAG>()
  }

  pub fn as_i32(&self) -> ConversionResult<i32> {
    if self.is_i32() {
      Ok(unsafe { mem::transmute::<u32, i32>((self.bits & !I32_TAG) as u32) })
    } else {
      Err(ConversionError::WrongType)
    }
  }

  // bool

  pub fn is_bool(&self) -> bool {
    self.is_type::<BOOL_TAG>()
  }

  pub fn as_bool(&self) -> ConversionResult<bool> {
    if self.is_bool() {
      Ok(unsafe { self.bits & POINTER_BITMASK > 0 })
    } else {
      Err(ConversionError::WrongType)
    }
  }

  // char

  pub fn is_char(&self) -> bool {
    self.is_type::<CHAR_TAG>()
  }

  pub fn as_char(&self) -> ConversionResult<char> {
    if self.is_char() {
      Ok(unsafe { mem::transmute::<u32, char>((self.bits & !CHAR_TAG) as u32) })
    } else {
      Err(ConversionError::WrongType)
    }
  }

  // string

  pub fn new_str() -> Self {
    Self::from(StringValue::default())
  }

  pub fn is_str(&self) -> bool {
    self.is::<StringValue>()
  }

  pub fn as_str(&self) -> ConversionResult<&StringValue> {
    self.as_obj::<StringValue>()
  }

  pub fn as_str_mut(&mut self) -> ConversionResult<&mut StringValue> {
    self.as_obj_mut::<StringValue>()
  }

  // array

  pub fn new_array() -> Self {
    Self::from(Array::default())
  }

  pub fn is_array(&self) -> bool {
    self.is::<Array>()
  }

  pub fn as_array(&self) -> ConversionResult<&Array> {
    self.as_obj::<Array>()
  }

  pub fn as_array_mut(&mut self) -> ConversionResult<&mut Array> {
    self.as_obj_mut::<Array>()
  }

  // struct

  pub fn new_struct() -> Self {
    Self::from(StructValue::default())
  }

  pub fn is_struct(&self) -> bool {
    self.is::<StructValue>()
  }

  pub fn as_struct(&self) -> ConversionResult<&StructValue> {
    self.as_obj::<StructValue>()
  }

  pub fn as_struct_mut(&mut self) -> ConversionResult<&mut StructValue> {
    self.as_obj_mut::<StructValue>()
  }

  // error

  pub fn new_err<T: ToString>(msg: T) -> Self {
    Self::from(ErrorValue::from(msg.to_string()))
  }

  pub fn is_err(&self) -> bool {
    self.is::<ErrorValue>()
  }

  pub fn as_err(&self) -> ConversionResult<&ErrorValue> {
    self.as_obj::<ErrorValue>()
  }

  pub fn as_err_mut(&mut self) -> ConversionResult<&mut ErrorValue> {
    self.as_obj_mut::<ErrorValue>()
  }

  // class

  pub fn new_class<T: ToString>(name: T) -> Self {
    Self::from(ClassValue::new(name.to_string()))
  }

  pub fn is_class(&self) -> bool {
    self.is::<ClassValue>()
  }

  pub fn as_class(&self) -> ConversionResult<&ClassValue> {
    self.as_obj::<ClassValue>()
  }

  pub fn as_class_mut(&mut self) -> ConversionResult<&mut ClassValue> {
    self.as_obj_mut::<ClassValue>()
  }

  // instance

  pub fn is_instance(&self) -> bool {
    self.is::<InstanceValue>()
  }

  pub fn as_instance(&self) -> ConversionResult<&InstanceValue> {
    self.as_obj::<InstanceValue>()
  }

  pub fn as_instance_mut(&mut self) -> ConversionResult<&mut InstanceValue> {
    self.as_obj_mut::<InstanceValue>()
  }

  // function

  pub fn is_fn(&self) -> bool {
    self.is::<FunctionValue>()
  }

  pub fn as_fn(&self) -> ConversionResult<&FunctionValue> {
    self.as_obj::<FunctionValue>()
  }

  pub fn as_fn_mut(&mut self) -> ConversionResult<&mut FunctionValue> {
    self.as_obj_mut::<FunctionValue>()
  }

  // closure

  pub fn is_closure(&self) -> bool {
    self.is::<ClosureValue>()
  }

  pub fn as_closure(&self) -> ConversionResult<&ClosureValue> {
    self.as_obj::<ClosureValue>()
  }

  pub fn as_closure_mut(&self) -> ConversionResult<&mut ClosureValue> {
    self.as_obj_mut::<ClosureValue>()
  }

  // method

  pub fn is_method(&self) -> bool {
    self.is::<MethodValue>()
  }

  pub fn as_method(&self) -> ConversionResult<&MethodValue> {
    self.as_obj::<MethodValue>()
  }

  pub fn as_method_mut(&mut self) -> ConversionResult<&mut MethodValue> {
    self.as_obj_mut::<MethodValue>()
  }

  // native

  pub fn new_native_fn(f: NativeFn) -> Self {
    Self::from(f)
  }

  pub fn is_native_fn(&self) -> bool {
    self.is_type::<FN_TAG>()
  }

  pub fn as_native_fn(&self) -> Result<NativeFn, ConversionError> {
    if self.is_native_fn() {
      Ok(unsafe { mem::transmute::<u64, NativeFn>((self.bits & !FN_TAG) as u64) })
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn new_native_closure<N, F>(name: N, f: F) -> Self
  where
    N: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    Self::from(NativeClosureValue::new(name, f))
  }

  pub fn is_native_closure(&self) -> bool {
    self.is::<NativeClosureValue>()
  }

  pub fn as_native_closure(&self) -> ConversionResult<&NativeClosureValue> {
    self.as_obj::<NativeClosureValue>()
  }

  pub fn as_native_closure_mut(&mut self) -> ConversionResult<&mut NativeClosureValue> {
    self.as_obj_mut::<NativeClosureValue>()
  }

  pub fn new_native_method<T: ToString, F>(name: T, f: F) -> Self
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    Self::from(NativeMethodValue::from(NativeClosureValue::new(name, f)))
  }

  pub fn is_native_method(&self) -> bool {
    self.is::<NativeMethodValue>()
  }

  pub fn as_native_method(&self) -> ConversionResult<&NativeMethodValue> {
    self.as_obj::<NativeMethodValue>()
  }

  pub fn as_native_method_mut(&mut self) -> ConversionResult<&mut NativeMethodValue> {
    self.as_obj_mut::<NativeMethodValue>()
  }

  // obj pointer

  pub fn is_ptr(&self) -> bool {
    self.is_type::<POINTER_TAG>()
  }

  pub fn is<T: ComplexValue>(&self) -> bool {
    self.is_ptr() && self.type_id() == T::type_id()
  }

  #[cfg(not(no_type_checking))]
  pub fn as_obj<T: ComplexValue>(&self) -> ConversionResult<&T> {
    if self.is::<T>() {
      self.convert()
    } else {
      Err(ConversionError::WrongType)
    }
  }

  #[cfg(not(no_type_checking))]
  pub fn as_obj_mut<T: ComplexValue>(&self) -> ConversionResult<&mut T> {
    if self.is::<T>() {
      self.convert_mut()
    } else {
      Err(ConversionError::WrongType)
    }
  }

  #[cfg(no_type_checking)]
  pub fn as_obj<T: ComplexValue>(&self) -> ConversionResult<T> {
    self.convert()
  }

  #[cfg(no_type_checking)]
  pub fn as_obj_mut<T: ComplexValue>(&self) -> ConversionResult<T> {
    self.convert_mut()
  }

  // nil

  pub fn is_nil(&self) -> bool {
    self.is_type::<NIL_TAG>()
  }

  // ComplexValue Methods

  pub fn set(&mut self, name: &str, value: Value) -> Result<(), ErrorValue> {
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

  pub fn index(&self, index: Value) -> Value {
    (self.vtable().index)(self.pointer(), index)
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

  fn convert<T>(&self) -> ConversionResult<&T> {
    Ok(unsafe { &*(self.pointer() as *const T) })
  }

  fn convert_mut<T>(&self) -> ConversionResult<&mut T> {
    Ok(unsafe { &mut *(self.pointer() as *mut T) })
  }

  fn allocate<T: ComplexValue>(item: T) -> Self {
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
    if self.is_ptr() {
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
    if self.is_ptr() {
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
    Value::allocate::<StringValue>(item.into())
  }
}

impl From<String> for Value {
  fn from(item: String) -> Self {
    Value::allocate::<StringValue>(item.into())
  }
}

impl From<&[Value]> for Value {
  fn from(array: &[Value]) -> Self {
    Value::allocate::<Array>(array.into())
  }
}

impl From<Vec<Value>> for Value {
  fn from(vec: Vec<Value>) -> Self {
    Value::allocate::<Array>(vec.into())
  }
}

impl From<NativeFn> for Value {
  fn from(f: NativeFn) -> Self {
    Self {
      bits: unsafe { mem::transmute::<NativeFn, u64>(f) } | FN_TAG,
    }
  }
}

impl From<Nil> for Value {
  fn from(_: Nil) -> Self {
    Self { bits: NIL_TAG }
  }
}

impl<T> From<T> for Value
where
  T: ComplexValue,
{
  fn from(item: T) -> Self {
    Value::allocate::<T>(item)
  }
}

impl Assign<i32> for Value {}

impl Assign<f64> for Value {}

impl<T: ComplexValue> Assign<T> for Value {}

impl Assign<Nil> for Value {}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    if let Ok(v) = self.as_f64() {
      write!(f, "{}", v)
    } else if let Ok(v) = self.as_i32() {
      write!(f, "{}", v)
    } else if let Ok(v) = self.as_bool() {
      write!(f, "{}", v)
    } else if let Ok(v) = self.as_char() {
      write!(f, "{}", v)
    } else if let Ok(v) = self.as_bool() {
      write!(f, "{}", v)
    } else if self.is_nil() {
      write!(f, "nil")
    } else {
      write!(f, "{}", self.basic_desc())
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

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    todo!()
  }
}

impl Add for Value {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    todo!()
  }
}

impl Sub for Value {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    todo!()
  }
}

impl Mul for Value {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    todo!()
  }
}

impl Div for Value {
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output {
    todo!()
  }
}

impl Rem for Value {
  type Output = Self;

  fn rem(self, rhs: Self) -> Self::Output {
    todo!()
  }
}

impl Neg for Value {
  type Output = Self;

  fn neg(self) -> Self::Output {
    todo!()
  }
}

impl Not for Value {
  type Output = Self;

  fn not(self) -> Self::Output {
    todo!()
  }
}

pub struct VTable {
  set: fn(MutVoid, name: &str, value: Value) -> Result<(), ErrorValue>,
  get: fn(ConstVoid, name: &str) -> Value,
  add: fn(ConstVoid, other: Value) -> Value,
  sub: fn(ConstVoid, other: Value) -> Value,
  mul: fn(ConstVoid, other: Value) -> Value,
  div: fn(ConstVoid, other: Value) -> Value,
  rem: fn(ConstVoid, other: Value) -> Value,
  index: fn(ConstVoid, index: Value) -> Value,
  drop: fn(MutVoid),
  dealloc: fn(MutVoid),
  type_id: fn() -> TypeId,
  basic_desc: fn() -> &'static str,
}

impl VTable {
  const fn new<T: ComplexValue>() -> Self {
    Self {
      set: |this, name, value| {
        <T as ComplexValue>::set(unsafe { &mut *Self::void_to_mut(this) }, name, value)
      },
      get: |this, name| <T as ComplexValue>::get(unsafe { &*Self::void_to(this) }, name),
      add: |this, other| <T as ComplexValue>::add(unsafe { &*Self::void_to(this) }, other),
      sub: |this, other| <T as ComplexValue>::sub(unsafe { &*Self::void_to(this) }, other),
      mul: |this, other| <T as ComplexValue>::mul(unsafe { &*Self::void_to(this) }, other),
      div: |this, other| <T as ComplexValue>::div(unsafe { &*Self::void_to(this) }, other),
      rem: |this, other| <T as ComplexValue>::rem(unsafe { &*Self::void_to(this) }, other),
      index: |this, index| <T as ComplexValue>::index(unsafe { &*Self::void_to(this) }, index),
      drop: |this| <T as ComplexValue>::drop(unsafe { &mut *Self::void_to_mut(this) }),
      dealloc: |this| <T as ComplexValue>::dealloc(this as *mut T),
      type_id: || <T as ComplexValue>::type_id(),
      basic_desc: || <T as ComplexValue>::basic_desc(),
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
  vtable: &'static VTable,
}

#[repr(C)]
struct AllocatedObject<T: ComplexValue> {
  meta: ValueMeta,
  obj: T,
}

impl<T: ComplexValue> AllocatedObject<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: 1,
      vtable: &T::VTABLE,
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

pub type ConversionResult<T: ComplexValue> = Result<T, ConversionError>;

#[derive(Debug, PartialEq)]
pub enum ConversionError {
  WrongType,
}
