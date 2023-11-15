use crate::{code::ConstantValue, prelude::*};
use ptr::MutPtr;
use static_assertions::assert_eq_size;
use std::{
  cmp::Ordering,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  hash::Hash,
  mem,
  ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
  sync::atomic::AtomicUsize,
};
pub use tags::*;
use uuid::Uuid;

pub(crate) mod builtin_types;
pub(crate) mod tags;

pub mod conv;
#[cfg(test)]
mod test;

pub mod prelude {
  pub use super::{builtin_types::*, conv::*, Tag, Value};
}

pub(crate) type ConstVoid = *const ();
pub(crate) type MutVoid = *mut ();

// ensuring 64 bit platforms, redundancy is just sanity checks
assert_eq_size!(usize, ConstVoid);
assert_eq_size!(usize, MutVoid);
assert_eq_size!(usize, f64);
assert_eq_size!(usize, u64);

#[derive(Clone)]
pub struct Value {
  pub bits: u64,
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

  pub fn bits(&self) -> u64 {
    self.bits & VALUE_BITMASK
  }

  pub fn truthy(&self) -> bool {
    !self.falsy()
  }

  pub fn falsy(&self) -> bool {
    self.is_nil() || self.is_bool() && self.bits & VALUE_BITMASK == 0
  }

  pub fn from_constant(gc: &mut Gc, env: Value, v: &ConstantValue) -> Self {
    match v {
      ConstantValue::Integer(v) => Self::from(*v),
      ConstantValue::Float(v) => Self::from(*v),
      ConstantValue::String(v) => gc.allocate(v),
      ConstantValue::StaticString(v) => gc.allocate(*v),
      ConstantValue::Fn(v) => {
        let env = gc.allocate(ModuleValue::new_scope(env));
        gc.allocate(FunctionValue::from_constant(v, env))
      }
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

  pub fn is_str(&self) -> bool {
    self.is::<StringValue>()
  }

  pub fn as_str(&self) -> Option<&'static StringValue> {
    self.cast_to::<StringValue>()
  }

  pub fn as_str_mut(&mut self) -> Option<&mut StringValue> {
    self.cast_to_mut::<StringValue>()
  }

  // module

  pub fn is_module(&self) -> bool {
    self.is::<ModuleValue>()
  }

  pub fn as_module(&self) -> Option<&ModuleValue> {
    self.cast_to::<ModuleValue>()
  }

  pub fn as_module_mut(&mut self) -> Option<&mut ModuleValue> {
    self.cast_to_mut::<ModuleValue>()
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
    self.is_type::<NATIVE_FN_TAG>()
  }

  pub fn as_native_fn(&self) -> Option<NativeFn> {
    if self.is_native_fn() {
      Some(self.as_native_fn_unchecked())
    } else {
      None
    }
  }

  pub fn as_native_fn_unchecked(&self) -> NativeFn {
    unsafe { mem::transmute(self.bits & VALUE_BITMASK) }
  }

  // -- native closure

  pub fn new_native_closure<N, F>(gc: &mut Gc, name: N, f: F) -> Self
  where
    N: ToString,
    F: FnMut(&mut Vm, Args) -> UsageResult + 'static,
  {
    gc.allocate(NativeClosureValue::new(name, f))
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

  pub fn new_native_fn_method(gc: &mut Gc, this: Value, f: NativeFn) -> Self {
    gc.allocate(NativeMethodValue::new_native_fn(this, f))
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

  // value pointer

  pub fn is_ptr(&self) -> bool {
    self.is_type::<POINTER_TAG>()
  }

  pub fn is<T: Usertype>(&self) -> bool {
    self.is_ptr() && *self.type_id() == T::ID
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

  pub fn reinterpret_cast<T: Usertype>(&mut self) -> MutPtr<T> {
    MutPtr::new(self.convert_mut())
  }

  // nil

  pub fn is_nil(&self) -> bool {
    self.is_type::<NIL_TAG>()
  }

  pub fn call(&mut self, vm: &mut Vm, airity: usize) -> UsageResult {
    if self.tag() == Tag::NativeFn {
      let args = vm.stack_drain_from(airity);
      let f = self.as_native_fn_unchecked();
      let output = f(vm, Args::new(args))?;
      Ok(output)
    } else {
      (self.vtable().invoke)(self.pointer_mut(), MutPtr::new(vm), self.clone(), airity)
    }
  }

  // value methods

  pub fn get_member(&self, gc: &mut Gc, name: Field) -> UsageResult<Option<Value>> {
    (self.vtable().get_member)(self, MutPtr::new(gc), name)
  }

  pub fn set_member(&mut self, gc: &mut Gc, name: Field, value: Value) -> UsageResult<()> {
    (self.vtable().set_member)(self.pointer_mut(), MutPtr::new(gc), name, value)
  }

  pub fn define(&mut self, name: impl AsRef<str>, value: impl Into<Value>) -> UsageResult<bool> {
    (self.vtable().define)(self.pointer_mut(), name.as_ref(), value.into())
  }

  pub fn assign(&mut self, name: impl AsRef<str>, value: impl Into<Value>) -> UsageResult<bool> {
    (self.vtable().assign)(self.pointer_mut(), name.as_ref(), value.into())
  }

  pub fn resolve(&self, name: impl AsRef<str>) -> UsageResult {
    (self.vtable().resolve)(self.pointer(), name.as_ref())
  }

  pub fn display_string(&self) -> String {
    (self.vtable().display_string)(self.pointer())
  }

  pub fn debug_string(&self) -> String {
    (self.vtable().debug_string)(self.pointer())
  }

  pub fn trace(&self, marks: &mut Marker) {
    marks.trace(self);
  }

  pub fn trace_vtable(&self, marks: &mut Marker) {
    (self.vtable().trace)(self.pointer(), marks as *mut Marker as MutVoid);
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

  pub(crate) fn pointer(&self) -> ConstVoid {
    (self.bits & VALUE_BITMASK) as ConstVoid
  }

  pub(crate) fn pointer_mut(&mut self) -> MutVoid {
    (self.bits & VALUE_BITMASK) as MutVoid
  }

  pub(crate) fn meta(&self) -> &ValueMeta {
    unsafe { &*((self.pointer() as *const u8).offset(META_OFFSET) as *const ValueMeta) }
  }

  pub(crate) fn meta_mut(&mut self) -> &mut ValueMeta {
    unsafe { &mut *((self.pointer_mut() as *mut u8).offset(META_OFFSET) as *mut ValueMeta) }
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
  fn type_id(&self) -> &'static Uuid {
    (self.vtable().type_id)()
  }

  #[allow(unused)]
  fn type_name(&self) -> String {
    (self.vtable().type_name)()
  }

  fn convert<T>(&self) -> &'static T {
    unsafe { &*(self.pointer() as *const T) }
  }

  fn convert_mut<T>(&mut self) -> &'static mut T {
    unsafe { &mut *(self.pointer_mut() as *mut T) }
  }
}

impl Default for Value {
  fn default() -> Self {
    Self { bits: NIL_TAG }
  }
}

impl Hash for Value {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.bits.hash(state);
  }
}

impl TryFrom<Value> for i32 {
  type Error = UsageError;

  fn try_from(value: Value) -> Result<Self, Self::Error> {
    match value.tag() {
      Tag::I32 => Ok(value.as_i32_unchecked()),
      Tag::F64 => Ok(value.as_f64_unchecked() as i32),
      _ => Err(UsageError::CoercionError(value, "i32")),
    }
  }
}

impl From<&Value> for Value {
  fn from(item: &Value) -> Value {
    item.clone()
  }
}

impl From<()> for Value {
  fn from(_item: ()) -> Value {
    Value::nil
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
      bits: unsafe { mem::transmute::<i64, u64>(item as i64 & u32::MAX as i64 | i64::default()) } | I32_TAG,
    }
  }
}

impl From<&i32> for Value {
  fn from(item: &i32) -> Self {
    Self::from(*item)
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
      bits: unsafe { mem::transmute::<char, u32>(item) as u64 & u32::MAX as u64 } | CHAR_TAG,
    }
  }
}

impl From<NativeFn> for Value {
  fn from(f: NativeFn) -> Self {
    Self {
      bits: f as usize as u64 | NATIVE_FN_TAG,
    }
  }
}

impl From<Nil> for Value {
  fn from(_: Nil) -> Self {
    Self { bits: NIL_TAG }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => write!(f, "{}", self.as_f64_unchecked()),
      Tag::I32 => write!(f, "{}", self.as_i32_unchecked()),
      Tag::Bool => write!(f, "{}", self.as_bool_unchecked()),
      Tag::Char => write!(f, "{}", self.as_char_unchecked()),
      Tag::NativeFn => write!(f, "<native fn {:p}>", &self.as_native_fn_unchecked()),
      Tag::Pointer => write!(f, "{}", self.display_string()),
      Tag::Nil => write!(f, "nil"),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    const PTR_WIDTH: usize = mem::size_of::<usize>() * 2;
    const PTR_DISPLAY_WIDTH: usize = PTR_WIDTH + 2;
    match self.tag() {
      Tag::F64 => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_f64_unchecked(), self.bits()),
      Tag::I32 => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_i32_unchecked(), self.bits()),
      Tag::Bool => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_bool_unchecked(), self.bits()),
      Tag::Char => write!(f, "{:?} {} (0x{:x})", self.tag(), self.as_char_unchecked(), self.bits()),
      Tag::NativeFn => write!(
        f,
        "<@{addr:<width$} {:?}>",
        self.tag(),
        addr = format!("0x{:0>width$x}", self.bits(), width = PTR_WIDTH),
        width = PTR_DISPLAY_WIDTH,
      ),
      Tag::Pointer => write!(
        f,
        "<@{addr:<width$} {} : {}>",
        self.type_id(),
        self.debug_string(),
        addr = format!("0x{:0>width$x}", self.bits(), width = PTR_WIDTH),
        width = PTR_DISPLAY_WIDTH,
      ),
      Tag::Nil => write!(f, "nil (0x{:x})", self.bits()),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    // TODO need operators as part of vtable
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
  type Output = UsageResult;

  fn add(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v + rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v + rhs.as_i32_unchecked() as f64)),
          _ => Err(UsageError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 + rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v + rhs.as_i32_unchecked())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 + rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('+', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 + rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::CoercionError(rhs, "i32")),
          },
          _ => Err(UsageError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(UsageError::UnimplementedError("add", self)),
    }
  }
}

impl Sub for Value {
  type Output = UsageResult;

  fn sub(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v - rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v - rhs.as_i32_unchecked() as f64)),
          _ => Err(UsageError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 - rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v - rhs.as_i32_unchecked())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 - rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('-', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 - rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('-', self, rhs)),
          },
          _ => Err(UsageError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(UsageError::UnimplementedError("sub", self)),
    }
  }
}

impl Mul for Value {
  type Output = UsageResult;

  fn mul(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v * rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v * rhs.as_i32_unchecked() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 * rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v * rhs.as_i32_unchecked())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 * rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('*', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 * rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('*', self, rhs)),
          },
          _ => Err(UsageError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(UsageError::UnimplementedError("mul", self)),
    }
  }
}

impl Div for Value {
  type Output = UsageResult;

  // TODO if is infinity, set to reserved INF value
  fn div(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v / rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v / rhs.as_i32_unchecked() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 / rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v / rhs.as_i32_unchecked())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 / rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('/', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 / rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('/', self, rhs)),
          },
          _ => Err(UsageError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(UsageError::UnimplementedError("div", self)),
    }
  }
}

impl Rem for Value {
  type Output = UsageResult;

  fn rem(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.as_f64_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v % rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v % rhs.as_i32_unchecked() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.as_i32_unchecked();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 % rhs.as_f64_unchecked())),
          Tag::I32 => Ok(Self::from(v % rhs.as_i32_unchecked())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => todo!(),
      Tag::Char => {
        let v = self.as_char_unchecked();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 % rhs.as_i32_unchecked()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('%', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 % rhs.as_char_unchecked() as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('/', self, rhs)),
          },
          _ => Err(UsageError::CoercionError(rhs, "char")),
        }
      }
      _ => Err(UsageError::UnimplementedError("rem", self)),
    }
  }
}

impl Neg for Value {
  type Output = UsageResult;

  fn neg(self) -> Self::Output {
    match self.tag() {
      Tag::F64 => Ok(Value::from(-self.as_f64_unchecked())),
      Tag::I32 => Ok(Value::from(-self.as_i32_unchecked())),
      _ => Err(UsageError::UnimplementedError("negate", self)),
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
  get_member: fn(&Value, MutPtr<Gc>, Field) -> UsageResult<Option<Value>>,
  set_member: fn(MutVoid, MutPtr<Gc>, Field, Value) -> UsageResult<()>,

  define: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  assign: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  resolve: fn(ConstVoid, &str) -> UsageResult,

  invoke: fn(MutVoid, MutPtr<Vm>, Value, usize) -> UsageResult,

  display_string: fn(ConstVoid) -> String,
  debug_string: fn(ConstVoid) -> String,

  trace: fn(ConstVoid, MutVoid),
  pub(crate) dealloc: fn(MutVoid),

  type_id: fn() -> &'static Uuid,
  type_name: fn() -> String,
}

impl VTable {
  pub const fn new<T: Usertype>() -> Self {
    Self {
      get_member: |this, gc, name| <T as Usertype>::get(Self::cast(this.pointer()), Self::typed_cast_mut(gc.raw()), this, name),
      set_member: |this, gc, name, value| {
        <T as Usertype>::set(Self::cast_mut(this), Self::typed_cast_mut(gc.raw()), name, value)
      },

      define: |this, name, value| <T as ResolvableValue>::__def__(Self::cast_mut(this), name, value),
      assign: |this, name, value| <T as ResolvableValue>::__def__(Self::cast_mut(this), name, value).map(|new| !new),
      resolve: |this, field| <T as ResolvableValue>::__res__(Self::cast(this), field),

      invoke: |this, vm, this_value, airity| {
        <T as InvocableValue>::__ivk__(Self::cast_mut(this), Self::typed_cast_mut(vm.raw()), this_value, airity)
      },

      display_string: |this| <T as DisplayValue>::__str__(Self::cast(this)),
      debug_string: |this| <T as DebugValue>::__dbg__(Self::cast(this)),

      trace: |this, marks| <T as TraceableValue>::trace(Self::cast(this), Self::cast_mut(marks)),
      dealloc: |this| Gc::consume(this as *mut T),

      type_id: || &<T as Usertype>::ID,
      type_name: || std::any::type_name::<T>().to_string(),
    }
  }

  fn cast<'t, T>(ptr: ConstVoid) -> &'t T {
    unsafe { &*(ptr as *const T) }
  }

  fn cast_mut<'t, T>(ptr: MutVoid) -> &'t mut T {
    unsafe { &mut *(ptr as *mut T) }
  }

  fn typed_cast_mut<T>(ptr: *mut T) -> &'static mut T {
    unsafe { &mut *ptr }
  }
}

#[derive(Default)]
pub(crate) enum Mark {
  #[default]
  White,
  Gray,
  Black,
}

pub(crate) struct ValueMeta {
  pub(crate) vtable: &'static VTable,

  /// Reference count to values that exist in native code and can't be traced
  pub(crate) ref_count: AtomicUsize,

  pub(crate) mark: Mark,
}
