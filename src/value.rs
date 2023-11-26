use crate::{memory, prelude::*};
use nohash_hasher::IsEnabled;
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

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value {
  pub bits: u64,
}

impl Value {
  #[allow(non_upper_case_globals)]
  pub const nil: Value = Value { bits: NIL_TAG };

  pub fn new<T>(v: T) -> Self
  where
    Self: From<T>,
  {
    Self::from(v)
  }

  pub fn new_pointer(ptr: ConstVoid) -> Self {
    Self {
      bits: ptr as u64 | POINTER_TAG,
    }
  }

  pub fn tag(&self) -> Tag {
    if self.is::<f64>() {
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
    self.is::<()>() || self.is::<bool>() && self.bits & VALUE_BITMASK == 0
  }

  pub fn is<T>(&self) -> bool
  where
    Self: IsType<T>,
  {
    self.is_type()
  }

  pub fn cast_to<T>(&self) -> Option<<Self as Cast<T>>::CastType>
  where
    Self: Cast<T>,
  {
    self.cast()
  }

  pub fn cast_to_mut<T>(&mut self) -> Option<<Self as CastMut<T>>::CastTypeMut>
  where
    Self: CastMut<T>,
  {
    self.cast_mut()
  }

  pub fn reinterpret_cast_to<T>(&self) -> <Self as Cast<T>>::CastType
  where
    Self: ReinterpretCast<T>,
  {
    self.reinterpret_cast()
  }

  pub fn reinterpret_cast_to_mut<T>(&mut self) -> <Self as CastMut<T>>::CastTypeMut
  where
    Self: ReinterpretCastMut<T>,
  {
    self.reinterpret_cast_mut()
  }

  pub fn maybe_into<T>(&mut self) -> Option<T>
  where
    T: MaybeFrom<Self>,
  {
    T::maybe_from(*self)
  }

  // -- native closure

  pub fn native_closure<N, F>(vm: &mut Vm, name: N, f: F) -> Self
  where
    N: ToString,
    F: FnMut(&mut Vm, Args) -> UsageResult + 'static,
  {
    vm.make_value_from(NativeClosureValue::new(name, f))
  }

  // -- native closure method

  pub fn native_method(vm: &mut Vm, this: Value, f: NativeFn) -> Self {
    vm.make_value_from(NativeMethodValue::new(this, f))
  }

  // value pointer

  pub fn is_ptr(&self) -> bool {
    self.bits & TAG_BITMASK == POINTER_TAG
  }

  // value methods

  pub fn call(&mut self, vm: &mut Vm, airity: usize) -> UsageResult {
    if let Some(f) = self.cast_to::<NativeFn>() {
      let args = vm.stack_drain_from(airity);
      let output = f(vm, Args::new(args))?;
      Ok(output)
    } else {
      (self.vtable().invoke)(self.pointer_mut(), MutPtr::new(vm), *self, airity)
    }
  }

  pub fn get_member(&self, vm: &mut Vm, name: Field) -> UsageResult<Option<Value>> {
    (self.vtable().get_member)(*self, MutPtr::new(vm), name)
  }

  pub fn set_member(&mut self, vm: &mut Vm, name: Field, value: Value) -> UsageResult<()> {
    vm.gc.invalidate(self);
    (self.vtable().set_member)(self.pointer_mut(), MutPtr::new(vm), name, value)
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

  pub fn deep_trace(&self, marks: &mut Tracer) {
    marks.deep_trace(self);
  }

  pub fn incremental_trace(&self, marks: &mut Tracer) {
    marks.try_mark_gray(self);
  }

  pub fn deep_trace_children(&self, marks: &mut Tracer) {
    (self.vtable().deep_trace)(self.pointer(), MutPtr::new(marks));
  }

  pub fn incremental_trace_children(&self, marks: &mut Tracer) {
    (self.vtable().incremental_trace)(self.pointer(), MutPtr::new(marks));
  }

  pub fn equals(&self, other: Self) -> bool {
    self.compare(other) == Some(Ordering::Equal)
  }

  pub fn not_equals(&self, other: Self) -> bool {
    !self.equals(other)
  }

  pub fn less_than(&self, other: Self) -> bool {
    self.compare(other) == Some(Ordering::Less)
  }

  pub fn less_equal(&self, other: Self) -> bool {
    matches!(self.compare(other), Some(Ordering::Less) | Some(Ordering::Equal))
  }

  pub fn greater_than(&self, other: Self) -> bool {
    self.compare(other) == Some(Ordering::Greater)
  }

  pub fn greater_equal(&self, other: Self) -> bool {
    matches!(self.compare(other), Some(Ordering::Greater) | Some(Ordering::Equal))
  }

  fn compare(&self, other: Self) -> Option<Ordering> {
    match (self.tag(), other.tag()) {
      (Tag::F64, Tag::F64) => {
        let a = self.reinterpret_cast_to::<f64>();
        let b = other.reinterpret_cast_to::<f64>();
        a.partial_cmp(&b)
      }
      (Tag::F64, Tag::I32) => {
        let a = self.reinterpret_cast_to::<f64>();
        let b = other.reinterpret_cast_to::<i32>() as f64;
        a.partial_cmp(&b)
      }
      (Tag::I32, Tag::F64) => {
        let a = self.reinterpret_cast_to::<i32>() as f64;
        let b = other.reinterpret_cast_to::<f64>();
        a.partial_cmp(&b)
      }
      (t1, t2) if t1 == t2 => self.bits.partial_cmp(&other.bits),
      _ => None,
    }
  }

  // utility

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

  pub(crate) fn is_unreferenced(&self) -> bool {
    self.meta().ref_count.load(std::sync::atomic::Ordering::Relaxed) == 0
  }

  pub(crate) fn vtable(&self) -> &VTable {
    if self.is_ptr() {
      self.meta().vtable
    } else {
      &Primitive::VTABLE
    }
  }

  // TypeId of the underlying type
  pub fn type_id(&self) -> &'static Uuid {
    (self.vtable().type_id)()
  }

  #[allow(unused)]
  pub fn type_name(&self) -> String {
    (self.vtable().type_name)()
  }

  /// Executes f only if self is nil, otherwise returns self
  pub fn or_else<F: FnOnce() -> Self>(self, f: F) -> Self {
    if self.is::<()>() {
      f()
    } else {
      self
    }
  }
}

impl Default for Value {
  fn default() -> Self {
    Self::nil
  }
}

impl TryFrom<Value> for i32 {
  type Error = UsageError;

  fn try_from(value: Value) -> Result<Self, Self::Error> {
    match value.tag() {
      Tag::I32 => Ok(value.reinterpret_cast_to::<i32>()),
      Tag::F64 => Ok(value.reinterpret_cast_to::<f64>() as i32),
      _ => Err(UsageError::CoercionError(value, "i32")),
    }
  }
}

impl From<&Value> for Value {
  fn from(item: &Value) -> Value {
    *item
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

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => {
        let fv = self.reinterpret_cast_to::<f64>();
        if fv < 0.0001 {
          write!(f, "{:e}", fv)
        } else {
          write!(f, "{}", fv)
        }
      }
      Tag::I32 => write!(f, "{}", self.reinterpret_cast_to::<i32>()),
      Tag::Bool => write!(f, "{}", self.reinterpret_cast_to::<bool>()),
      Tag::Char => write!(f, "{}", self.reinterpret_cast_to::<char>()),
      Tag::NativeFn => write!(f, "<native fn {:p}>", &self.reinterpret_cast_to::<NativeFn>()),
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
      Tag::F64 => write!(
        f,
        "{:?} {} (0x{:x})",
        self.tag(),
        self.reinterpret_cast_to::<f64>(),
        self.bits()
      ),
      Tag::I32 => write!(
        f,
        "{:?} {} (0x{:x})",
        self.tag(),
        self.reinterpret_cast_to::<i32>(),
        self.bits()
      ),
      Tag::Bool => write!(
        f,
        "{:?} {} (0x{:x})",
        self.tag(),
        self.reinterpret_cast_to::<bool>(),
        self.bits()
      ),
      Tag::Char => write!(
        f,
        "{:?} {} (0x{:x})",
        self.tag(),
        self.reinterpret_cast_to::<char>(),
        self.bits()
      ),
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

impl Add for Value {
  type Output = UsageResult;

  fn add(self, rhs: Self) -> Self::Output {
    match self.tag() {
      Tag::F64 => {
        let v = self.reinterpret_cast_to::<f64>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v + rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v + rhs.reinterpret_cast_to::<i32>() as f64)),
          _ => Err(UsageError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 + rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v + rhs.reinterpret_cast_to::<i32>())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => Err(UsageError::InvalidBinary),
      Tag::Char => {
        let v = self.reinterpret_cast_to::<char>();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 + rhs.reinterpret_cast_to::<i32>()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('+', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 + rhs.reinterpret_cast_to::<char>() as u32) {
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
        let v = self.reinterpret_cast_to::<f64>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v - rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v - rhs.reinterpret_cast_to::<i32>() as f64)),
          _ => Err(UsageError::CoercionError(self, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 - rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v - rhs.reinterpret_cast_to::<i32>())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => Err(UsageError::InvalidBinary),
      Tag::Char => {
        let v = self.reinterpret_cast_to::<char>();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 - rhs.reinterpret_cast_to::<i32>()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('-', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 - rhs.reinterpret_cast_to::<char>() as u32) {
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
        let v = self.reinterpret_cast_to::<f64>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v * rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v * rhs.reinterpret_cast_to::<i32>() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 * rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v * rhs.reinterpret_cast_to::<i32>())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => Err(UsageError::InvalidBinary),
      Tag::Char => {
        let v = self.reinterpret_cast_to::<char>();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 * rhs.reinterpret_cast_to::<i32>()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('*', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 * rhs.reinterpret_cast_to::<char>() as u32) {
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
        let v = self.reinterpret_cast_to::<f64>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v / rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v / rhs.reinterpret_cast_to::<i32>() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 / rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v / rhs.reinterpret_cast_to::<i32>())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => Err(UsageError::InvalidBinary),
      Tag::Char => {
        let v = self.reinterpret_cast_to::<char>();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 / rhs.reinterpret_cast_to::<i32>()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('/', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 / rhs.reinterpret_cast_to::<char>() as u32) {
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
        let v = self.reinterpret_cast_to::<f64>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v % rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v % rhs.reinterpret_cast_to::<i32>() as f64)),
          _ => Err(UsageError::CoercionError(rhs, "f64")),
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match rhs.tag() {
          Tag::F64 => Ok(Self::from(v as f64 % rhs.reinterpret_cast_to::<f64>())),
          Tag::I32 => Ok(Self::from(v % rhs.reinterpret_cast_to::<i32>())),
          _ => Err(UsageError::CoercionError(rhs, "i32")),
        }
      }
      Tag::Bool => Err(UsageError::InvalidBinary),
      Tag::Char => {
        let v = self.reinterpret_cast_to::<char>();
        match rhs.tag() {
          Tag::I32 => match char::from_u32((v as u32 as i32 % rhs.reinterpret_cast_to::<i32>()) as u32) {
            Some(c) => Ok(Self::from(c)),
            None => Err(UsageError::InvalidOperation('%', self, rhs)),
          },
          Tag::Char => match char::from_u32(v as u32 % rhs.reinterpret_cast_to::<char>() as u32) {
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
      Tag::F64 => Ok(Value::from(-self.reinterpret_cast_to::<f64>())),
      Tag::I32 => Ok(Value::from(-self.reinterpret_cast_to::<i32>())),
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

pub(crate) type NativeUnaryOp = for<'a> fn(MutPtr<Vm>, Value) -> UsageResult;
pub(crate) type NativeBinaryOp = for<'a> fn(MutPtr<Vm>, Value, Value) -> UsageResult;
pub(crate) type NativeTernaryOp = for<'a> fn(MutPtr<Vm>, Value, Value, Value) -> UsageResult;

pub struct VTable {
  get_member: fn(Value, MutPtr<Vm>, Field) -> UsageResult<Option<Value>>,
  set_member: fn(MutVoid, MutPtr<Vm>, Field, Value) -> UsageResult<()>,

  define: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  assign: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  resolve: fn(ConstVoid, &str) -> UsageResult,

  // ops
  pub(crate) neg: NativeUnaryOp,
  pub(crate) not: NativeUnaryOp,
  pub(crate) add: NativeBinaryOp,
  pub(crate) sub: NativeBinaryOp,
  pub(crate) mul: NativeBinaryOp,
  pub(crate) div: NativeBinaryOp,
  pub(crate) rem: NativeBinaryOp,
  pub(crate) eq: NativeBinaryOp,
  pub(crate) neq: NativeBinaryOp,
  pub(crate) less: NativeBinaryOp,
  pub(crate) leq: NativeBinaryOp,
  pub(crate) greater: NativeBinaryOp,
  pub(crate) geq: NativeBinaryOp,
  pub(crate) index: NativeBinaryOp,
  pub(crate) assign_index: NativeTernaryOp,

  invoke: fn(MutVoid, MutPtr<Vm>, Value, usize) -> UsageResult,

  display_string: fn(ConstVoid) -> String,
  debug_string: fn(ConstVoid) -> String,

  deep_trace: fn(ConstVoid, MutPtr<Tracer>),
  incremental_trace: fn(ConstVoid, MutPtr<Tracer>),
  pub(crate) dealloc: fn(MutVoid),

  type_id: fn() -> &'static Uuid,
  type_name: fn() -> String,
}

impl VTable {
  pub const fn new<T: Usertype>() -> Self {
    Self {
      get_member: |this, vm, name| <T as Usertype>::get(Self::cast(this.pointer()), Self::typed_cast_mut(vm), this, name),
      set_member: |this, vm, name, value| <T as Usertype>::set(Self::cast_mut(this), Self::typed_cast_mut(vm), name, value),
      neg: |vm, value| <T as Operators>::__neg__(Self::typed_cast_mut(vm), value),
      not: |vm, value| <T as Operators>::__not__(Self::typed_cast_mut(vm), value),
      add: |vm, left, right| <T as Operators>::__add__(Self::typed_cast_mut(vm), left, right),
      sub: |vm, left, right| <T as Operators>::__sub__(Self::typed_cast_mut(vm), left, right),
      mul: |vm, left, right| <T as Operators>::__mul__(Self::typed_cast_mut(vm), left, right),
      div: |vm, left, right| <T as Operators>::__div__(Self::typed_cast_mut(vm), left, right),
      rem: |vm, left, right| <T as Operators>::__rem__(Self::typed_cast_mut(vm), left, right),
      eq: |vm, left, right| <T as Operators>::__eq__(Self::typed_cast_mut(vm), left, right),
      neq: |vm, left, right| <T as Operators>::__neq__(Self::typed_cast_mut(vm), left, right),
      less: |vm, left, right| <T as Operators>::__less__(Self::typed_cast_mut(vm), left, right),
      leq: |vm, left, right| <T as Operators>::__leq__(Self::typed_cast_mut(vm), left, right),
      greater: |vm, left, right| <T as Operators>::__greater__(Self::typed_cast_mut(vm), left, right),
      geq: |vm, left, right| <T as Operators>::__geq__(Self::typed_cast_mut(vm), left, right),

      index: |vm, left, right| <T as Operators>::__index__(Self::typed_cast_mut(vm), left, right),
      assign_index: |vm, left, mid, right| <T as Operators>::__idxeq__(Self::typed_cast_mut(vm), left, mid, right),

      invoke: |this, vm, this_value, airity| {
        <T as Operators>::__ivk__(Self::cast_mut(this), Self::typed_cast_mut(vm), this_value, airity)
      },

      define: |this, name, value| <T as Operators>::__def__(Self::cast_mut(this), name, value),

      assign: |this, name, value| <T as Operators>::__def__(Self::cast_mut(this), name, value).map(|new| !new),

      resolve: |this, field| <T as Operators>::__res__(Self::cast(this), field),

      display_string: |this| <T as Operators>::__str__(Self::cast(this)),

      debug_string: |this| <T as Operators>::__dbg__(Self::cast(this)),

      deep_trace: |this, marks| <T as TraceableValue>::deep_trace(Self::cast(this), Self::typed_cast_mut(marks)),
      incremental_trace: |this, marks| <T as TraceableValue>::incremental_trace(Self::cast(this), Self::typed_cast_mut(marks)),

      dealloc: |this| memory::consume(this as *mut T),

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

  fn typed_cast_mut<T>(ptr: MutPtr<T>) -> &'static mut T {
    unsafe { &mut *ptr.raw() }
  }
}

impl Hash for Value {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.bits.hash(state);
  }
}

impl IsEnabled for Value {}

pub(crate) struct ValueMeta {
  pub(crate) vtable: &'static VTable,

  /// Reference count to values that exist in native code and can't be traced
  pub(crate) ref_count: AtomicUsize,

  /// The size of the allocated item and the meta
  pub(crate) size: usize,
}

pub trait IsType<T>: private::Sealed {
  fn is_type(&self) -> bool;
}

pub trait Cast<T>: private::Sealed {
  type CastType;
  fn cast(&self) -> Option<Self::CastType>;
}

pub trait CastMut<T>: private::Sealed {
  type CastTypeMut;
  fn cast_mut(&mut self) -> Option<Self::CastTypeMut>;
}

pub trait ReinterpretCast<T>: Cast<T> + private::Sealed {
  fn reinterpret_cast(&self) -> Self::CastType;
}

pub trait ReinterpretCastMut<T>: CastMut<T> + private::Sealed {
  fn reinterpret_cast_mut(&mut self) -> Self::CastTypeMut;
}

mod private {
  pub trait Sealed {}
}

impl private::Sealed for Value {}
