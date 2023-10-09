use crate::{code::ConstantValue, prelude::*};
use static_assertions::assert_eq_size;
use std::{
  cmp::Ordering,
  error::Error,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  mem,
  ops::{Add, Deref, Div, Mul, Neg, Not, Rem, Sub},
};
pub use tags::*;

pub(crate) mod builtin_types;
mod tags;
#[cfg(test)]
mod test;

pub mod prelude {
  pub use super::{builtin_types::*, MaybeFrom, Tag, Value};
}

type ConstVoid = *const ();
type MutVoid = *mut ();

// ensuring 64 bit platforms, redundancy is just sanity checks
assert_eq_size!(usize, ConstVoid);
assert_eq_size!(usize, MutVoid);
assert_eq_size!(usize, f64);
assert_eq_size!(usize, u64);

const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub trait MaybeFrom<T>
where
  Self: Sized,
{
  fn maybe_from(value: T) -> Option<Self>;
}

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

  pub fn from_constant(v: &ConstantValue) -> Self {
    match v {
      ConstantValue::Nil => Self::nil,
      ConstantValue::Bool(v) => Self::from(*v),
      ConstantValue::Integer(v) => Self::from(*v),
      ConstantValue::Float(v) => Self::from(*v),
      ConstantValue::String(v) => Self::from(v),
      ConstantValue::Fn(v) => Value::from(FunctionValue::from(v)),
      ConstantValue::Class(v) => Value::from(ClassValue::from(v)),
    }
  }

  // float

  pub fn is_f64(&self) -> bool {
    self.bits < F64_MAX
  }

  pub fn as_f64(&self) -> Option<f64> {
    if self.is_f64() {
      Some(self.as_f64_unchecked())
    } else {
      None
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

  pub fn as_i32(&self) -> Option<i32> {
    if self.is_i32() {
      Some(self.as_i32_unchecked())
    } else {
      None
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

  pub fn as_bool(&self) -> Option<bool> {
    if self.is_bool() {
      Some(self.as_bool_unchecked())
    } else {
      None
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

  pub fn as_char(&self) -> Option<char> {
    if self.is_char() {
      Some(self.as_char_unchecked())
    } else {
      None
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

  pub fn as_str(&self) -> Option<&StringValue> {
    self.cast_to::<StringValue>()
  }

  pub fn as_str_mut(&mut self) -> Option<&mut StringValue> {
    self.cast_to_mut::<StringValue>()
  }

  // array

  pub fn new_array() -> Self {
    Self::from(ArrayValue::default())
  }

  pub fn is_array(&self) -> bool {
    self.is::<ArrayValue>()
  }

  pub fn as_array(&self) -> Option<&'static ArrayValue> {
    self.cast_to::<ArrayValue>()
  }

  pub fn as_array_mut(&mut self) -> Option<&mut ArrayValue> {
    self.cast_to_mut::<ArrayValue>()
  }

  // struct

  pub fn new_struct() -> Self {
    Self::from(StructValue::default())
  }

  pub fn is_struct(&self) -> bool {
    self.is::<StructValue>()
  }

  pub fn as_struct(&self) -> Option<&StructValue> {
    self.cast_to::<StructValue>()
  }

  pub fn as_struct_mut(&mut self) -> Option<&mut StructValue> {
    self.cast_to_mut::<StructValue>()
  }

  // class

  pub fn new_class<T: ToString>(name: T) -> Self {
    Self::from(ClassValue::new(name.to_string()))
  }

  pub fn is_class(&self) -> bool {
    self.is::<ClassValue>()
  }

  pub fn as_class(&self) -> Option<&ClassValue> {
    self.cast_to::<ClassValue>()
  }

  pub fn as_class_mut(&mut self) -> Option<&mut ClassValue> {
    self.cast_to_mut::<ClassValue>()
  }

  // instance

  pub fn is_instance(&self) -> bool {
    self.is::<InstanceValue>()
  }

  pub fn as_instance(&self) -> Option<&InstanceValue> {
    self.cast_to::<InstanceValue>()
  }

  pub fn as_instance_mut(&mut self) -> Option<&mut InstanceValue> {
    self.cast_to_mut::<InstanceValue>()
  }

  // function

  pub fn is_fn(&self) -> bool {
    self.is::<FunctionValue>()
  }

  pub fn as_fn(&self) -> Option<&FunctionValue> {
    self.cast_to::<FunctionValue>()
  }

  pub fn as_fn_mut(&mut self) -> Option<&mut FunctionValue> {
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

  pub fn as_closure(&self) -> Option<&ClosureValue> {
    self.cast_to::<ClosureValue>()
  }

  pub fn as_closure_mut(&mut self) -> Option<&mut ClosureValue> {
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

  pub fn as_method(&self) -> Option<&MethodValue> {
    self.cast_to::<MethodValue>()
  }

  pub fn as_method_mut(&mut self) -> Option<&mut MethodValue> {
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

  pub fn native(f: NativeFn) -> Self {
    Self::from(f)
  }

  pub fn is_native_fn(&self) -> bool {
    self.is_type::<FN_TAG>()
  }

  pub fn as_native_fn(&self) -> Option<NativeFn> {
    if self.is_native_fn() {
      Some(self.as_native_fn_unchecked())
    } else {
      None
    }
  }

  pub fn as_native_fn_unchecked(&self) -> NativeFn {
    unsafe { mem::transmute((self.bits & VALUE_BITMASK) as u64) }
  }

  // -- native closure

  pub fn new_native_closure<N, F>(name: N, f: F) -> Self
  where
    N: ToString,
    F: FnMut(&mut Vm, &mut Env, Args) -> ValueResult + 'static,
  {
    Self::from(NativeClosureValue::new(name, f))
  }

  pub fn is_native_closure(&self) -> bool {
    self.is::<NativeClosureValue>()
  }

  pub fn as_native_closure(&self) -> Option<&NativeClosureValue> {
    self.cast_to::<NativeClosureValue>()
  }

  pub fn as_native_closure_mut(&mut self) -> Option<&mut NativeClosureValue> {
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
    F: FnMut(&mut Vm, &mut Env, Args) -> ValueResult + 'static,
  {
    Self::from(NativeMethodValue::from(NativeClosureValue::new(name, f)))
  }

  pub fn is_native_method(&self) -> bool {
    self.is::<NativeMethodValue>()
  }

  pub fn as_native_method(&self) -> Option<&NativeMethodValue> {
    self.cast_to::<NativeMethodValue>()
  }

  pub fn as_native_method_mut(&mut self) -> Option<&mut NativeMethodValue> {
    self.cast_to_mut::<NativeMethodValue>()
  }

  pub fn as_native_method_unchecked(&self) -> &NativeMethodValue {
    self.convert()
  }

  pub fn as_native_method_unchecked_mut(&mut self) -> &mut NativeMethodValue {
    self.convert_mut()
  }

  pub fn as_native_class(&self) -> Option<&NativeClass> {
    if self.tag() == Tag::NativeClass {
      Some(self.as_native_class_unchecked())
    } else {
      None
    }
  }

  pub fn as_native_class_mut(&mut self) -> Option<&mut NativeClass> {
    if self.tag() == Tag::NativeClass {
      Some(self.as_native_class_mut_unchecked())
    } else {
      None
    }
  }

  pub fn as_native_class_unchecked(&self) -> &NativeClass {
    self.convert()
  }

  pub fn as_native_class_mut_unchecked(&mut self) -> &mut NativeClass {
    self.convert_mut()
  }

  // value pointer

  pub fn is_ptr(&self) -> bool {
    self.is_type::<POINTER_TAG>()
  }

  pub fn is<T: Usertype>(&self) -> bool {
    self.is_ptr() && self.type_id() == T::ID
  }

  pub fn cast_to<T: Usertype>(&self) -> Option<&'static T> {
    if self.is::<T>() {
      Some(self.convert())
    } else {
      None
    }
  }

  pub fn cast_to_mut<T: Usertype>(&mut self) -> Option<&'static mut T> {
    if self.is::<T>() {
      Some(self.convert_mut())
    } else {
      None
    }
  }

  // nil

  pub fn is_nil(&self) -> bool {
    self.is_type::<NIL_TAG>()
  }

  // Into Callable

  pub fn as_callable(&self) -> Option<Callable> {
    Callable::from_value(self.clone())
  }

  // value methods

  pub fn lookup(&self, name: &str) -> Value {
    if self.is_ptr() {
      (self.vtable().lookup)(self.pointer(), name)
    } else {
      Value::nil
    }
  }

  pub fn assign(&mut self, name: &str, value: Value) {
    if self.is_ptr() {
      (self.vtable().assign)(self.pointer_mut(), name, value);
    }
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

  fn convert<T>(&self) -> &'static T {
    unsafe { &*(self.pointer() as *const T) }
  }

  fn convert_mut<T>(&mut self) -> &'static mut T {
    unsafe { &mut *(self.pointer_mut() as *mut T) }
  }

  fn allocate<T>(item: T) -> *mut T {
    Box::into_raw(Box::new(item))
  }

  fn allocate_usertype<T: Usertype>(item: T) -> Self {
    let allocated = unsafe { &mut *Self::allocate(AllocatedObject::new(item)) };

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

impl From<&Value> for Value {
  fn from(value: &Value) -> Self {
    value.clone()
  }
}

impl From<()> for Value {
  fn from(value: ()) -> Self {
    Self::nil
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
    Value::allocate_usertype::<StringValue>(item.into())
  }
}

impl From<String> for Value {
  fn from(item: String) -> Self {
    Value::allocate_usertype::<StringValue>(item.into())
  }
}

impl From<&String> for Value {
  fn from(item: &String) -> Self {
    Value::allocate_usertype::<StringValue>(item.clone().into())
  }
}

impl From<&[Value]> for Value {
  fn from(array: &[Value]) -> Self {
    Value::allocate_usertype(ArrayValue::new_from_slice(array))
  }
}

impl From<Vec<Value>> for Value {
  fn from(vec: Vec<Value>) -> Self {
    Value::allocate_usertype(ArrayValue::new_from_vec(&vec))
  }
}

impl From<NativeFn> for Value {
  fn from(f: NativeFn) -> Self {
    Self {
      bits: f as usize as u64 | FN_TAG,
    }
  }
}

impl From<NativeClass> for Value {
  fn from(class: NativeClass) -> Self {
    let allocated = Self::allocate(class);
    Self {
      bits: allocated as *const _ as u64 | NATIVE_CLASS_TAG,
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
  T: Usertype,
{
  fn from(item: T) -> Self {
    Value::allocate_usertype::<T>(item)
  }
}

impl MaybeFrom<Value> for Value {
  fn maybe_from(value: Value) -> Option<Self> {
    Some(value)
  }
}

impl MaybeFrom<Value> for i32 {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_i32()
  }
}

impl MaybeFrom<Value> for &'static Vec<Value> {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_array().map(|a| &**a)
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => write!(f, "{}", self.as_f64_unchecked()),
      Tag::I32 => write!(f, "{}", self.as_i32_unchecked()),
      Tag::Bool => write!(f, "{}", self.as_bool_unchecked()),
      Tag::Char => write!(f, "{}", self.as_char_unchecked()),
      Tag::NativeFn => write!(f, "{:p}", &self.as_native_fn_unchecked()),
      Tag::NativeClass => write!(f, "{:p}", self.as_native_class_unchecked().name()),
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
      Tag::NativeClass => write!(f, "{:?} (0x{:x})", self.tag(), self.raw_value()),
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
    if self.tag() == other.tag() {
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
      _ => None,
    }
  }
}

impl Add for Value {
  type Output = ValueResult;

  fn add(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v + rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v + rhs.as_i32_unchecked() as f64)),
          _ => Err(ValueError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 + rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v + rhs.as_i32_unchecked())),
          _ => Err(ValueError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 + rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::Todo("could not add i32 with char".to_string())),
          },
          Tag::Char => match char::from_u32(v as u32 + rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::CoercionError(rhs, "i32")),
          },
          _ => Err(ValueError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(ValueError::UnimplementedError("add", self)),
    }
  }
}

impl Sub for Value {
  type Output = ValueResult;

  fn sub(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v - rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v - rhs.as_i32_unchecked() as f64)),
          _ => Err(ValueError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 - rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v - rhs.as_i32_unchecked())),
          _ => Err(ValueError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 - rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::Todo("could not sub i32 with char".to_string())),
          },
          Tag::Char => match char::from_u32(v as u32 - rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('-', self, rhs)),
          },
          _ => Err(ValueError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(ValueError::UnimplementedError("sub", self)),
    }
  }
}

impl Mul for Value {
  type Output = ValueResult;

  fn mul(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v * rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v * rhs.as_i32_unchecked() as f64)),
          _ => Err(ValueError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 * rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v * rhs.as_i32_unchecked())),
          _ => Err(ValueError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 * rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('*', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 * rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('*', self, rhs)),
          },
          _ => Err(ValueError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(ValueError::UnimplementedError("mul", self)),
    }
  }
}

impl Div for Value {
  type Output = ValueResult;

  // TODO if is infinity, set to reserved INF value
  fn div(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v / rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v / rhs.as_i32_unchecked() as f64)),
          _ => Err(ValueError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 / rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v / rhs.as_i32_unchecked())),
          _ => Err(ValueError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 / rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('/', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 / rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('/', self, rhs)),
          },
          _ => Err(ValueError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(ValueError::UnimplementedError("div", self)),
    }
  }
}

impl Rem for Value {
  type Output = ValueResult;

  fn rem(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v % rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v % rhs.as_i32_unchecked() as f64)),
          _ => Err(ValueError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 % rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v % rhs.as_i32_unchecked())),
          _ => Err(ValueError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 % rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('%', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 % rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(ValueError::InvalidOperation('/', self, rhs)),
          },
          _ => Err(ValueError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(ValueError::UnimplementedError("rem", self)),
    }
  }
}

impl Neg for Value {
  type Output = ValueResult;

  fn neg(self) -> Self::Output {
    match self.tag() {
      Tag::F64 => Ok(Value::from(-self.as_f64_unchecked())),
      Tag::I32 => Ok(Value::from(-self.as_i32_unchecked())),
      _ => Err(ValueError::UnimplementedError("negate", self)),
    }
  }
}

impl Not for Value {
  type Output = Self;

  fn not(self) -> Self::Output {
    Value::from(!self.truthy())
  }
}

pub struct VTable {
  lookup: fn(ConstVoid, &str) -> Value,
  assign: fn(MutVoid, &str, Value) -> ValueResult<()>,
  stringify: fn(ConstVoid) -> String,
  debug_string: fn(ConstVoid) -> String,
  dealloc: fn(MutVoid),
  type_id: fn() -> &'static str,
  type_name: fn() -> String,
}

impl VTable {
  const fn new<T: Usertype>() -> Self {
    Self {
      lookup: |this, name| <T as Class>::get(Self::cast(this), name).unwrap_or_default(),
      assign: |this, name, value| <T as Class>::set(Self::cast_mut(this), name, value),
      stringify: |this| <T as Usertype>::stringify(Self::cast(this)),
      debug_string: |this| <T as Usertype>::debug_string(Self::cast(this)),
      dealloc: |this| consume(this as *mut T),
      type_id: || <T as Usertype>::ID,
      type_name: || std::any::type_name::<T>().to_string(),
    }
  }

  fn cast<'t, T>(ptr: ConstVoid) -> &'t T {
    unsafe { &*(ptr as *const T) }
  }

  fn cast_mut<'t, T>(ptr: MutVoid) -> &'t mut T {
    unsafe { &mut *(ptr as *mut T) }
  }
}

struct ValueMeta {
  ref_count: usize,
  vtable: &'static VTable,
}

#[repr(C)]
struct AllocatedObject<T: Usertype> {
  meta: ValueMeta,
  obj: T,
}

impl<T: Usertype> AllocatedObject<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: 1,
      vtable: &T::VTABLE,
    };
    Self { obj, meta }
  }
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
  fn from_value(value: Value) -> Option<Self> {
    match value.tag() {
      Tag::NativeFn => Some(Callable::NativeFn(value.as_native_fn_unchecked())),
      Tag::Pointer => {
        if value.is_fn() {
          Some(Callable::Fn(value))
        } else if value.is_closure() {
          Some(Callable::Closure(value))
        } else if value.is_method() {
          Some(Callable::Method(value))
        } else if value.is_native_closure() {
          Some(Callable::NativeClosure(value))
        } else if value.is_native_method() {
          Some(Callable::NativeMethod(value))
        } else {
          None
        }
      }
      _ => None,
    }
  }

  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) {
    match self {
      Callable::Fn(f) => f.as_fn_unchecked().call(vm, args.list),
      Callable::Closure(c) => c.as_closure_unchecked().call(vm, args.list),
      Callable::Method(m) => m.as_method_unchecked().call(vm, args),
      Callable::NativeFn(f) => {
        let value = f(vm, env, args).expect("TODO must handle error here");
        vm.stack_push(value);
      }
      Callable::NativeClosure(c) => {
        let value = c
          .as_native_closure_unchecked_mut()
          .call(vm, env, args)
          .expect("TODO must handle error here");
        vm.stack_push(value);
      }
      Callable::NativeMethod(m) => {
        let value = m
          .as_native_method_unchecked_mut()
          .call(vm, env, args)
          .expect("TODO must handle error here");
        vm.stack_push(value);
      }
    }
  }
}

fn consume<T: Usertype>(this: *mut T) {
  let _ = unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) };
}
