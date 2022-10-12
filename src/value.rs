pub use builtin_types::*;
use static_assertions::assert_eq_size;
use std::{
  cmp::Ordering,
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
  pub use super::{builtin_types::*, Assign, ComplexValue, ComplexValueId, Value};
}

type ConstVoid = *const ();
type MutVoid = *mut ();
pub type ComplexValueId = &'static str;

// ensuring 64 bit platforms, redundancy is just sanity checks
assert_eq_size!(usize, ConstVoid);
assert_eq_size!(usize, MutVoid);
assert_eq_size!(usize, f64);
assert_eq_size!(usize, u64);

const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub struct Value {
  bits: u64,
}

impl Value {
  #[allow(non_upper_case_globals)]
  pub const nil: Value = Value { bits: NIL_TAG };

  pub fn tag(&self) -> Tag {
    if self.is_f64() {
      Tag::F64
    } else {
      unsafe { mem::transmute(self.bits & TAG_BITMASK) }
    }
  }

  pub fn take(&mut self) -> Value {
    let mut new = Value::nil;
    mem::swap(self, &mut new);
    new
  }

  pub fn raw_value(&self) -> u64 {
    self.bits & VALUE_BITMASK
  }

  pub fn truthy(&self) -> bool {
    !self.falsy()
  }

  pub fn falsy(&self) -> bool {
    self.is_nil() || self.is_bool() && self.bits & VALUE_BITMASK == 0
  }

  // float

  pub fn is_f64(&self) -> bool {
    self.bits < F64_MAX
  }

  pub fn as_f64(&self) -> ConversionResult<f64> {
    if self.is_f64() {
      Ok(self.as_f64_unchecked())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn as_f64_unchecked(&self) -> f64 {
    debug_assert!(self.is_f64());
    f64::from_bits(self.bits)
  }

  // int

  pub fn is_i32(&self) -> bool {
    self.is_type::<I32_TAG>()
  }

  pub fn as_i32(&self) -> ConversionResult<i32> {
    if self.is_i32() {
      Ok(self.as_i32_unchecked())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn as_i32_unchecked(&self) -> i32 {
    debug_assert!(self.is_i32());
    unsafe { mem::transmute((self.bits & VALUE_BITMASK) as u32) }
  }

  // bool

  pub fn is_bool(&self) -> bool {
    self.is_type::<BOOL_TAG>()
  }

  pub fn as_bool(&self) -> ConversionResult<bool> {
    if self.is_bool() {
      Ok(self.as_bool_unchecked())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn as_bool_unchecked(&self) -> bool {
    debug_assert!(self.is_bool());
    self.bits & VALUE_BITMASK > 0
  }

  // char

  pub fn is_char(&self) -> bool {
    self.is_type::<CHAR_TAG>()
  }

  pub fn as_char(&self) -> ConversionResult<char> {
    if self.is_char() {
      Ok(self.as_char_unchecked())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn as_char_unchecked(&self) -> char {
    debug_assert!(self.is_char());
    char::from_u32((self.bits & VALUE_BITMASK) as u32).unwrap_or_default()
  }

  // string

  pub fn new_str() -> Self {
    Self::from(StringValue::default())
  }

  pub fn is_str(&self) -> bool {
    self.is::<StringValue>()
  }

  pub fn as_str(&self) -> ConversionResult<&StringValue> {
    self.cast_to::<StringValue>()
  }

  pub fn as_str_mut(&mut self) -> ConversionResult<&mut StringValue> {
    self.cast_to_mut::<StringValue>()
  }

  // array

  pub fn new_array() -> Self {
    Self::from(ArrayValue::default())
  }

  pub fn is_array(&self) -> bool {
    self.is::<ArrayValue>()
  }

  pub fn as_array(&self) -> ConversionResult<&ArrayValue> {
    self.cast_to::<ArrayValue>()
  }

  pub fn as_array_mut(&mut self) -> ConversionResult<&mut ArrayValue> {
    self.cast_to_mut::<ArrayValue>()
  }

  // struct

  pub fn new_struct() -> Self {
    Self::from(StructValue::default())
  }

  pub fn is_struct(&self) -> bool {
    self.is::<StructValue>()
  }

  pub fn as_struct(&self) -> ConversionResult<&StructValue> {
    self.cast_to::<StructValue>()
  }

  pub fn as_struct_mut(&mut self) -> ConversionResult<&mut StructValue> {
    self.cast_to_mut::<StructValue>()
  }

  // error

  pub fn new_err<T: ToString>(msg: T) -> Self {
    Self::from(ErrorValue::from(msg.to_string()))
  }

  /// Convenience function to check if the value is not an error
  pub fn is_ok(&self) -> bool {
    !self.is::<ErrorValue>()
  }

  pub fn is_err(&self) -> bool {
    self.is::<ErrorValue>()
  }

  pub fn as_err(&self) -> ConversionResult<&ErrorValue> {
    self.cast_to::<ErrorValue>()
  }

  pub fn as_err_mut(&mut self) -> ConversionResult<&mut ErrorValue> {
    self.cast_to_mut::<ErrorValue>()
  }

  // class

  pub fn new_class<T: ToString>(name: T) -> Self {
    Self::from(ClassValue::new(name.to_string()))
  }

  pub fn is_class(&self) -> bool {
    self.is::<ClassValue>()
  }

  pub fn as_class(&self) -> ConversionResult<&ClassValue> {
    self.cast_to::<ClassValue>()
  }

  pub fn as_class_mut(&mut self) -> ConversionResult<&mut ClassValue> {
    self.cast_to_mut::<ClassValue>()
  }

  // instance

  pub fn is_instance(&self) -> bool {
    self.is::<InstanceValue>()
  }

  pub fn as_instance(&self) -> ConversionResult<&InstanceValue> {
    self.cast_to::<InstanceValue>()
  }

  pub fn as_instance_mut(&mut self) -> ConversionResult<&mut InstanceValue> {
    self.cast_to_mut::<InstanceValue>()
  }

  // function

  pub fn is_fn(&self) -> bool {
    self.is::<FunctionValue>()
  }

  pub fn as_fn(&self) -> ConversionResult<&FunctionValue> {
    self.cast_to::<FunctionValue>()
  }

  pub fn as_fn_mut(&mut self) -> ConversionResult<&mut FunctionValue> {
    self.cast_to_mut::<FunctionValue>()
  }

  pub fn as_fn_unchecked(&self) -> &FunctionValue {
    self.convert()
  }

  pub fn as_fn_unchecked_mut(&mut self) -> &FunctionValue {
    self.convert_mut()
  }

  // closure

  pub fn is_closure(&self) -> bool {
    self.is::<ClosureValue>()
  }

  pub fn as_closure(&self) -> ConversionResult<&ClosureValue> {
    self.cast_to::<ClosureValue>()
  }

  pub fn as_closure_mut(&mut self) -> ConversionResult<&mut ClosureValue> {
    self.cast_to_mut::<ClosureValue>()
  }

  pub fn as_closure_unchecked(&self) -> &ClosureValue {
    self.convert()
  }

  pub fn as_closure_unchecked_mut(&mut self) -> &mut ClosureValue {
    self.convert_mut()
  }

  // method

  pub fn is_method(&self) -> bool {
    self.is::<MethodValue>()
  }

  pub fn as_method(&self) -> ConversionResult<&MethodValue> {
    self.cast_to::<MethodValue>()
  }

  pub fn as_method_mut(&mut self) -> ConversionResult<&mut MethodValue> {
    self.cast_to_mut::<MethodValue>()
  }

  pub fn as_method_unchecked(&self) -> &MethodValue {
    self.convert()
  }

  pub fn as_method_unchecked_mut(&mut self) -> &mut MethodValue {
    self.convert_mut()
  }

  // native

  // -- native fn

  pub fn new_native_fn(f: NativeFn) -> Self {
    Self::from(f)
  }

  pub fn is_native_fn(&self) -> bool {
    self.is_type::<FN_TAG>()
  }

  pub fn as_native_fn(&self) -> Result<NativeFn, ConversionError> {
    if self.is_native_fn() {
      Ok(self.as_native_fn_unchecked())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn as_native_fn_unchecked(&self) -> NativeFn {
    unsafe { mem::transmute((self.bits & VALUE_BITMASK) as u64) }
  }

  // -- native closure

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
    self.cast_to::<NativeClosureValue>()
  }

  pub fn as_native_closure_mut(&mut self) -> ConversionResult<&mut NativeClosureValue> {
    self.cast_to_mut::<NativeClosureValue>()
  }

  pub fn as_native_closure_unchecked(&self) -> &NativeClosureValue {
    self.convert()
  }

  pub fn as_native_closure_unchecked_mut(&mut self) -> &mut NativeClosureValue {
    self.convert_mut()
  }

  // -- native closure method

  pub fn new_native_fn_method(f: NativeFn) -> Self {
    Self::from(NativeMethodValue::from(f))
  }

  pub fn new_native_closure_method<T: ToString, F>(name: T, f: F) -> Self
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    Self::from(NativeMethodValue::from(NativeClosureValue::new(name, f)))
  }

  pub fn is_native_method(&self) -> bool {
    self.is::<NativeMethodValue>()
  }

  pub fn as_native_method(&self) -> ConversionResult<&NativeMethodValue> {
    self.cast_to::<NativeMethodValue>()
  }

  pub fn as_native_method_mut(&mut self) -> ConversionResult<&mut NativeMethodValue> {
    self.cast_to_mut::<NativeMethodValue>()
  }

  pub fn as_native_method_unchecked(&self) -> &NativeMethodValue {
    self.convert()
  }

  pub fn as_native_method_unchecked_mut(&mut self) -> &mut NativeMethodValue {
    self.convert_mut()
  }

  // cmplx value pointer

  pub fn is_ptr(&self) -> bool {
    self.is_type::<POINTER_TAG>()
  }

  pub fn is<T: ComplexValue>(&self) -> bool {
    self.is_ptr() && self.type_id() == T::ID
  }

  pub fn cast_to<T: ComplexValue>(&self) -> ConversionResult<&T> {
    if self.is::<T>() {
      Ok(self.convert())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  pub fn cast_to_mut<T: ComplexValue>(&mut self) -> ConversionResult<&mut T> {
    if self.is::<T>() {
      Ok(self.convert_mut())
    } else {
      Err(ConversionError::WrongType)
    }
  }

  // nil

  pub fn is_nil(&self) -> bool {
    self.is_type::<NIL_TAG>()
  }

  // Into Callable

  pub fn as_callable(&self) -> ConversionResult<Callable> {
    Callable::from_value(self.clone()).map_err(|_e| ConversionError::WrongType)
  }

  // ComplexValue Methods

  pub fn set(&mut self, name: &str, value: Value) -> Value {
    (self.vtable().set)(self.pointer_mut(), name, value)
  }

  pub fn get(&self, name: &str) -> Value {
    (self.vtable().get)(self.pointer(), name)
  }

  pub fn index(&self, index: Value) -> Value {
    (self.vtable().index)(self.pointer(), index)
  }

  fn add_impl(&self, other: Value) -> Value {
    (self.vtable().add)(self.pointer(), other)
  }

  fn sub_impl(&self, other: Value) -> Value {
    (self.vtable().sub)(self.pointer(), other)
  }

  fn mul_impl(&self, other: Value) -> Value {
    (self.vtable().mul)(self.pointer(), other)
  }

  fn div_impl(&self, other: Value) -> Value {
    (self.vtable().div)(self.pointer(), other)
  }

  fn rem_impl(&self, other: Value) -> Value {
    (self.vtable().rem)(self.pointer(), other)
  }

  fn neg_impl(&self) -> Value {
    (self.vtable().neg)(self.pointer())
  }

  fn not_impl(&self) -> Value {
    (self.vtable().not)(self.pointer())
  }

  fn eq_impl(&self, other: &Value) -> bool {
    (self.vtable().eq)(self.pointer(), other)
  }

  fn cmp_impl(&self, other: &Value) -> Option<Ordering> {
    (self.vtable().cmp)(self.pointer(), other)
  }

  pub fn stringify(&self) -> String {
    (self.vtable().stringify)(self.pointer())
  }

  pub fn debug_string(&self) -> String {
    (self.vtable().debug_string)(self.pointer())
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

  fn pointer(&self) -> ConstVoid {
    (self.bits & VALUE_BITMASK) as ConstVoid
  }

  fn pointer_mut(&mut self) -> MutVoid {
    (self.bits & VALUE_BITMASK) as MutVoid
  }

  fn meta(&self) -> &ValueMeta {
    unsafe { &*((self.pointer() as *const u8).offset(META_OFFSET) as *const ValueMeta) }
  }

  fn meta_mut(&mut self) -> &mut ValueMeta {
    unsafe { &mut *((self.pointer_mut() as *mut u8).offset(META_OFFSET) as *mut ValueMeta) }
  }

  /// Bypass mutable self and access mut ValueMeta
  #[allow(clippy::mut_from_ref)]
  fn meta_mut_bypass(&self) -> &mut ValueMeta {
    unsafe { &mut *((self.pointer() as *mut u8).offset(META_OFFSET) as *mut ValueMeta) }
  }

  fn vtable(&self) -> &VTable {
    if self.is_ptr() {
      self.meta().vtable
    } else {
      &Primitive::VTABLE
    }
  }

  fn is_type<const T: u64>(&self) -> bool {
    self.bits & TAG_BITMASK == T
  }

  // TypeId of the underlying type
  fn type_id(&self) -> &'static str {
    (self.vtable().type_id)()
  }

  fn type_name(&self) -> String {
    (self.vtable().type_name)()
  }

  fn convert<T>(&self) -> &T {
    unsafe { &*(self.pointer() as *const T) }
  }

  fn convert_mut<T>(&mut self) -> &mut T {
    unsafe { &mut *(self.pointer_mut() as *mut T) }
  }

  fn allocate<T: ComplexValue>(item: T) -> Self {
    let allocated = unsafe { &mut *(Box::into_raw(Box::new(AllocatedObject::new(item)))) };

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
      let pointer = self.pointer_mut();
      let meta = self.meta_mut();

      meta.ref_count -= 1;

      if meta.ref_count == 0 {
        (meta.vtable.drop)(pointer);
        (meta.vtable.dealloc)(pointer);
      }
    }
  }
}

impl Clone for Value {
  fn clone(&self) -> Self {
    if self.is_ptr() {
      self.meta_mut_bypass().ref_count += 1;
    }
    Self { bits: self.bits }
  }
}

impl From<f64> for Value {
  fn from(item: f64) -> Self {
    Self { bits: item.to_bits() }
  }
}

impl From<i32> for Value {
  fn from(item: i32) -> Self {
    Self {
      bits: unsafe { mem::transmute::<i64, u64>(item as i64) } | I32_TAG,
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
    Value::allocate::<ArrayValue>(array.into())
  }
}

impl From<Vec<Value>> for Value {
  fn from(vec: Vec<Value>) -> Self {
    Value::allocate::<ArrayValue>(vec.into())
  }
}

impl From<NativeFn> for Value {
  fn from(f: NativeFn) -> Self {
    Self {
      bits: f as usize as u64 | FN_TAG,
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
    match self.tag() {
      Tag::F64 => write!(f, "{}", self.as_f64_unchecked()),
      Tag::I32 => write!(f, "{}", self.as_i32_unchecked()),
      Tag::Bool => write!(f, "{}", self.as_bool_unchecked()),
      Tag::Char => write!(f, "{}", self.as_char_unchecked()),
      Tag::NativeFn => write!(f, "{:p}", &self.as_native_fn_unchecked()),
      Tag::Pointer => write!(f, "{}", self.stringify()),
      Tag::Nil => write!(f, "nil"),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_f64_unchecked(), self.raw_value()),
      Tag::I32 => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_i32_unchecked(), self.raw_value()),
      Tag::Bool => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_bool_unchecked(), self.raw_value()),
      Tag::Char => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_char_unchecked(), self.raw_value()),
      Tag::NativeFn => write!(
        f,
        "{:?} {:p} (0x{:x})",
        self.tag(),
        &self.as_native_fn_unchecked(),
        self.raw_value()
      ),
      Tag::Pointer => write!(
        f,
        "{:?} {} (0x{:x}) {}",
        self.tag(),
        self.debug_string(),
        self.raw_value(),
        self.type_name()
      ),
      Tag::Nil => write!(f, "nil"),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    if self.is_ptr() {
      self.eq_impl(other)
    } else if self.tag() == other.tag() {
      self.bits == other.bits
    } else {
      match self.tag() {
        Tag::I32 => {
          let v = self.as_i32_unchecked();
          match other.tag() {
            Tag::F64 => v as f64 == other.as_f64_unchecked(),
            _ => false,
          }
        }
        Tag::F64 => {
          let v = self.as_f64_unchecked();
          match other.tag() {
            Tag::I32 => v == other.as_i32_unchecked() as f64,
            _ => false,
          }
        }
        _ => false,
      }
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match other.tag() {
          Tag::F64 => v.partial_cmp(&other.as_f64_unchecked()),
          Tag::I32 => v.partial_cmp(&(other.as_i32_unchecked() as f64)),
          _ => None,
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match other.tag() {
          Tag::F64 => (v as f64).partial_cmp(&(other.as_f64_unchecked())),
          Tag::I32 => v.partial_cmp(&other.as_i32_unchecked()),
          _ => None,
        }
      }
      Tag::Pointer => self.cmp_impl(other),
      _ => None,
    }
  }
}

impl Add for Value {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v + rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v + rhs.as_i32_unchecked() as f64),
          _ => Self::new_err(format!("cannot coalesce {} to f64", rhs)),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v as f64 + rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v + rhs.as_i32_unchecked()),
          _ => Self::new_err(format!("cannot coalesce {} to i32", rhs)),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 + rhs.as_i32_unchecked()) as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err("could not add i32 with char"),
          },
          Tag::Char => match char::from_u32(v as u32 + rhs.as_char_unchecked() as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err(format!("could not add {} with {}", v, rhs)),
          },
          _ => Self::new_err(format!("could not coalesce {} to char", rhs)),
        }
      }
      Tag::Pointer => self.add_impl(rhs),
      _ => Self::new_err(format!("add not implemented for {}", self)),
    }
  }
}

impl Sub for Value {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v - rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v - rhs.as_i32_unchecked() as f64),
          _ => Self::new_err(format!("cannot coalesce {} to f64", rhs)),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v as f64 - rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v - rhs.as_i32_unchecked()),
          _ => Self::new_err(format!("cannot coalesce {} to i32", rhs)),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 - rhs.as_i32_unchecked()) as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err("could not sub i32 with char"),
          },
          Tag::Char => match char::from_u32(v as u32 - rhs.as_char_unchecked() as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err(format!("could not sub {} with {}", v, rhs)),
          },
          _ => Self::new_err(format!("could not coalesce {} to char", rhs)),
        }
      }
      Tag::Pointer => self.sub_impl(rhs),
      _ => Self::new_err(format!("sub not implemented for {}", self)),
    }
  }
}

impl Mul for Value {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v * rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v * rhs.as_i32_unchecked() as f64),
          _ => Self::new_err(format!("cannot coalesce {} to f64", rhs)),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v as f64 * rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v * rhs.as_i32_unchecked()),
          _ => Self::new_err(format!("cannot coalesce {} to i32", rhs)),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 * rhs.as_i32_unchecked()) as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err("could not mul i32 with char"),
          },
          Tag::Char => match char::from_u32(v as u32 * rhs.as_char_unchecked() as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err(format!("could not mul {} with {}", v, rhs)),
          },
          _ => Self::new_err(format!("could not coalesce {} to char", rhs)),
        }
      }
      Tag::Pointer => self.mul_impl(rhs),
      _ => Self::new_err(format!("mul not implemented for {}", self)),
    }
  }
}

impl Div for Value {
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v / rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v / rhs.as_i32_unchecked() as f64),
          _ => Self::new_err(format!("cannot coalesce {} to f64", rhs)),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v as f64 / rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v / rhs.as_i32_unchecked()),
          _ => Self::new_err(format!("cannot coalesce {} to i32", rhs)),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 / rhs.as_i32_unchecked()) as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err("could not div i32 with char"),
          },
          Tag::Char => match char::from_u32(v as u32 / rhs.as_char_unchecked() as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err(format!("could not div {} with {}", v, rhs)),
          },
          _ => Self::new_err(format!("could not coalesce {} to char", rhs)),
        }
      }
      Tag::Pointer => self.div_impl(rhs),
      _ => Self::new_err(format!("div not implemented for {}", self)),
    }
  }
}

impl Rem for Value {
  type Output = Self;

  fn rem(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v % rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v % rhs.as_i32_unchecked() as f64),
          _ => Self::new_err(format!("cannot coalesce {} to f64", rhs)),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Self::from(v as f64 % rhs.as_f64_unchecked()),
          Tag::I32 => Self::from(v % rhs.as_i32_unchecked()),
          _ => Self::new_err(format!("cannot coalesce {} to i32", rhs)),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 % rhs.as_i32_unchecked()) as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err("could not mod i32 with char"),
          },
          Tag::Char => match char::from_u32(v as u32 % rhs.as_char_unchecked() as u32) {
            Some(c) => Self::from(c),
            None => Self::new_err(format!("could not div {} with {}", v, rhs)),
          },
          _ => Self::new_err(format!("could not coalesce {} to char", rhs)),
        }
      }
      Tag::Pointer => self.rem_impl(rhs),
      _ => Self::new_err(format!("rem not implemented for {}", self)),
    }
  }
}

impl Neg for Value {
  type Output = Self;

  fn neg(self) -> Self::Output {
    match self.tag() {
      Tag::F64 => Value::from(-self.as_f64_unchecked()),
      Tag::I32 => Value::from(-self.as_i32_unchecked()),
      Tag::Pointer => self.neg_impl(),
      _ => Self::new_err(format!("- not implemented for {}", self)),
    }
  }
}

impl Not for Value {
  type Output = Self;

  fn not(self) -> Self::Output {
    if self.is_ptr() {
      self.not_impl()
    } else {
      Value::from(!self.truthy())
    }
  }
}

pub struct VTable {
  set: fn(MutVoid, &str, Value) -> Value,
  get: fn(ConstVoid, &str) -> Value,
  index: fn(ConstVoid, Value) -> Value,
  add: fn(ConstVoid, Value) -> Value,
  sub: fn(ConstVoid, Value) -> Value,
  mul: fn(ConstVoid, Value) -> Value,
  div: fn(ConstVoid, Value) -> Value,
  rem: fn(ConstVoid, Value) -> Value,
  neg: fn(ConstVoid) -> Value,
  not: fn(ConstVoid) -> Value,
  eq: fn(ConstVoid, &Value) -> bool,
  cmp: fn(ConstVoid, &Value) -> Option<Ordering>,
  stringify: fn(ConstVoid) -> String,
  debug_string: fn(ConstVoid) -> String,
  drop: fn(MutVoid),
  dealloc: fn(MutVoid),
  type_id: fn() -> &'static str,
  type_name: fn() -> String,
}

impl VTable {
  const fn new<T: ComplexValue>() -> Self {
    Self {
      set: |this, name, value| <T as ComplexValue>::set(unsafe { &mut *Self::void_to_mut(this) }, name, value),
      get: |this, name| <T as ComplexValue>::get(unsafe { &*Self::void_to(this) }, name),
      index: |this, index| <T as ComplexValue>::index(unsafe { &*Self::void_to(this) }, index),
      add: |this, other| <T as ComplexValue>::add(unsafe { &*Self::void_to(this) }, other),
      sub: |this, other| <T as ComplexValue>::sub(unsafe { &*Self::void_to(this) }, other),
      mul: |this, other| <T as ComplexValue>::mul(unsafe { &*Self::void_to(this) }, other),
      div: |this, other| <T as ComplexValue>::div(unsafe { &*Self::void_to(this) }, other),
      rem: |this, other| <T as ComplexValue>::rem(unsafe { &*Self::void_to(this) }, other),
      neg: |this| <T as ComplexValue>::neg(unsafe { &*Self::void_to(this) }),
      not: |this| <T as ComplexValue>::not(unsafe { &*Self::void_to(this) }),
      eq: |this, other| <T as ComplexValue>::eq(unsafe { &*Self::void_to(this) }, other),
      cmp: |this, other| <T as ComplexValue>::cmp(unsafe { &*Self::void_to(this) }, other),
      stringify: |this| <T as ComplexValue>::stringify(unsafe { &*Self::void_to(this) }),
      debug_string: |this| <T as ComplexValue>::debug_string(unsafe { &*Self::void_to(this) }),
      drop: |this| <T as ComplexValue>::drop(unsafe { &mut *Self::void_to_mut(this) }),
      dealloc: |this| <T as ComplexValue>::dealloc(this as *mut T),
      type_id: || <T as ComplexValue>::type_id(),
      type_name: || <T as ComplexValue>::type_name(),
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
    Self { obj, meta }
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

pub type ConversionResult<T> = Result<T, ConversionError>;

#[derive(Debug, PartialEq, Eq)]
pub enum ConversionError {
  WrongType,
  NotCallable,
}

pub enum Callable {
  Fn(Value),
  Closure(Value),
  Method(Value),
  NativeFn(NativeFn),
  NativeClosure(Value),
  NativeMethod(Value),
}

impl Debug for Callable {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      Self::Fn(arg0) => f.debug_tuple("Fn").field(arg0).finish(),
      Self::Closure(arg0) => f.debug_tuple("Closure").field(arg0).finish(),
      Self::Method(arg0) => f.debug_tuple("Method").field(arg0).finish(),
      Self::NativeFn(arg0) => write!(f, "{:p}", &arg0),
      Self::NativeClosure(arg0) => f.debug_tuple("NativeClosure").field(arg0).finish(),
      Self::NativeMethod(arg0) => f.debug_tuple("NativeMethod").field(arg0).finish(),
    }
  }
}

impl Callable {
  fn from_value(value: Value) -> ConversionResult<Self> {
    match value.tag() {
      Tag::NativeFn => Ok(Callable::NativeFn(value.as_native_fn_unchecked())),
      Tag::Pointer => {
        if value.is_fn() {
          Ok(Callable::Fn(value))
        } else if value.is_closure() {
          Ok(Callable::Closure(value))
        } else if value.is_method() {
          Ok(Callable::Method(value))
        } else if value.is_native_closure() {
          Ok(Callable::NativeClosure(value))
        } else if value.is_native_method() {
          Ok(Callable::NativeMethod(value))
        } else {
          Err(ConversionError::NotCallable)?
        }
      }
      _ => Err(ConversionError::NotCallable)?,
    }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) {
    match self {
      Callable::Fn(f) => f.as_fn_unchecked().call(thread, args.list),
      Callable::Closure(c) => c.as_closure_unchecked().call(thread, args.list),
      Callable::Method(m) => m.as_method_unchecked().call(thread, args),
      Callable::NativeFn(f) => {
        let value = f(thread, env, args);
        thread.stack_push(value);
      }
      Callable::NativeClosure(c) => {
        let value = c.as_native_closure_unchecked_mut().call(thread, env, args);
        thread.stack_push(value);
      }
      Callable::NativeMethod(m) => {
        let value = m.as_native_method_unchecked_mut().call(thread, env, args);
        thread.stack_push(value);
      }
    }
  }
}
