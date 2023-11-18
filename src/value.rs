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

  pub fn new<T>(v: T) -> Self
  where
    Self: From<T>,
  {
    Self::from(v)
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
    T::maybe_from(self.clone())
  }

  // -- native closure

  pub fn native_closure<N, F>(gc: &mut Gc, name: N, f: F) -> Self
  where
    N: ToString,
    F: FnMut(&mut Vm, Args) -> UsageResult + 'static,
  {
    gc.allocate(NativeClosureValue::new(name, f))
  }

  // -- native closure method

  pub fn native_method(gc: &mut Gc, this: Value, f: NativeFn) -> Self {
    gc.allocate(NativeMethodValue::new(this, f))
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
      (self.vtable().invoke)(self.pointer_mut(), MutPtr::new(vm), self.clone(), airity)
    }
  }

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

impl Hash for Value {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.bits.hash(state);
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

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::F64 => write!(f, "{}", self.reinterpret_cast_to::<f64>()),
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

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    // TODO need operators as part of vtable
    if self.tag() == other.tag() {
      self.bits == other.bits
    } else {
      match self.tag() {
        Tag::I32 => {
          let v = self.reinterpret_cast_to::<i32>();
          match other.tag() {
            Tag::F64 => v as f64 == other.reinterpret_cast_to::<f64>(),
            _ => false,
          }
        }
        Tag::F64 => {
          let v = self.reinterpret_cast_to::<f64>();
          match other.tag() {
            Tag::I32 => v == other.reinterpret_cast_to::<i32>() as f64,
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
        let v = self.reinterpret_cast_to::<f64>();
        match other.tag() {
          Tag::F64 => v.partial_cmp(&other.reinterpret_cast_to::<f64>()),
          Tag::I32 => v.partial_cmp(&(other.reinterpret_cast_to::<i32>() as f64)),
          _ => None,
        }
      }
      Tag::I32 => {
        let v = self.reinterpret_cast_to::<i32>();
        match other.tag() {
          Tag::F64 => (v as f64).partial_cmp(&(other.reinterpret_cast_to::<f64>())),
          Tag::I32 => v.partial_cmp(&other.reinterpret_cast_to::<i32>()),
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
      Tag::Bool => todo!(),
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
      Tag::Bool => todo!(),
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
      Tag::Bool => todo!(),
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
      Tag::Bool => todo!(),
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
      Tag::Bool => todo!(),
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

type NativeBinaryOp = for<'a> fn(MutPtr<Vm>, Value, Value) -> UsageResult;

type NativeTernaryOp = for<'a> fn(MutPtr<Vm>, Value, Value, Value) -> UsageResult;

pub struct VTable {
  get_member: fn(&Value, MutPtr<Gc>, Field) -> UsageResult<Option<Value>>,
  set_member: fn(MutVoid, MutPtr<Gc>, Field, Value) -> UsageResult<()>,

  define: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  assign: fn(MutVoid, &str, Value) -> UsageResult<bool>,
  resolve: fn(ConstVoid, &str) -> UsageResult,

  // ops
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

      add: |vm, left, right| <T as Operators>::__add__(Self::typed_cast_mut(vm.raw()), left, right),
      sub: |vm, left, right| <T as Operators>::__sub__(Self::typed_cast_mut(vm.raw()), left, right),
      mul: |vm, left, right| <T as Operators>::__mul__(Self::typed_cast_mut(vm.raw()), left, right),
      div: |vm, left, right| <T as Operators>::__div__(Self::typed_cast_mut(vm.raw()), left, right),
      rem: |vm, left, right| <T as Operators>::__rem__(Self::typed_cast_mut(vm.raw()), left, right),
      eq: |vm, left, right| <T as Operators>::__eq__(Self::typed_cast_mut(vm.raw()), left, right),
      neq: |vm, left, right| <T as Operators>::__neq__(Self::typed_cast_mut(vm.raw()), left, right),
      less: |vm, left, right| <T as Operators>::__less__(Self::typed_cast_mut(vm.raw()), left, right),
      leq: |vm, left, right| <T as Operators>::__leq__(Self::typed_cast_mut(vm.raw()), left, right),
      greater: |vm, left, right| <T as Operators>::__greater__(Self::typed_cast_mut(vm.raw()), left, right),
      geq: |vm, left, right| <T as Operators>::__geq__(Self::typed_cast_mut(vm.raw()), left, right),

      index: |vm, left, right| <T as Operators>::__index__(Self::typed_cast_mut(vm.raw()), left, right),
      assign_index: |vm, left, mid, right| <T as Operators>::__idxeq__(Self::typed_cast_mut(vm.raw()), left, mid, right),

      invoke: |this, vm, this_value, airity| {
        <T as Operators>::__ivk__(Self::cast_mut(this), Self::typed_cast_mut(vm.raw()), this_value, airity)
      },

      define: |this, name, value| <T as Operators>::__def__(Self::cast_mut(this), name, value),

      assign: |this, name, value| <T as Operators>::__def__(Self::cast_mut(this), name, value).map(|new| !new),

      resolve: |this, field| <T as Operators>::__res__(Self::cast(this), field),

      display_string: |this| <T as Operators>::__str__(Self::cast(this)),

      debug_string: |this| <T as Operators>::__dbg__(Self::cast(this)),

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
  #[allow(unused)]
  Gray,
  #[allow(unused)]
  Black,
}

pub(crate) struct ValueMeta {
  pub(crate) vtable: &'static VTable,

  /// Reference count to values that exist in native code and can't be traced
  pub(crate) ref_count: AtomicUsize,

  #[allow(unused)]
  pub(crate) mark: Mark,
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
