use crate::{Env, ExecutionThread};
use static_assertions::assert_eq_size;
use std::{
  any::TypeId,
  collections::BTreeMap,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  mem,
};
pub use tags::*;

mod tags;
#[cfg(test)]
mod test;

type ConstVoid = *const ();
type MutVoid = *mut ();
type ObjectResult<T> = Result<T, String>;

assert_eq_size!(usize, f64);
assert_eq_size!(usize, i64);
assert_eq_size!(usize, MutVoid);

const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub struct Nil;

#[repr(u64)]
#[derive(Debug, PartialEq)]
pub enum Tag {
  Float,
  Nil = tags::NIL_TAG,
  Integer = tags::INTEGER_TAG,
  Pointer = tags::POINTER_TAG, // struct, class, instance, function, etc..
}

pub union Value {
  ptr: MutVoid,
  bits: u64,
  f64: f64,
}

impl Value {
  #[allow(non_upper_case_globals)]
  pub const nil: Value = Value { bits: NIL_TAG };

  fn new_struct() -> Self {
    Self::from(Struct::default())
  }

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
    self.is_type::<NIL_TAG>()
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

  pub fn is_obj<T: Object>(&self) -> bool {
    self.is_type::<POINTER_TAG>() && self.kind() == T::kind()
  }

  pub fn as_obj<T: Object>(&self) -> &mut T {
    self.convert::<T>()
  }

  pub fn as_struct(&self) -> &mut Struct {
    self.convert::<Struct>()
  }

  // Object Methods

  pub fn set(&mut self, name: &str, value: Value) -> ObjectResult<()> {
    (self.vtable().set)(self.pointer(), name, value)
  }

  pub fn get(&self, name: &str) -> ObjectResult<Value> {
    (self.vtable().get)(self.pointer(), name)
  }

  pub fn add(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().add)(self.pointer(), other)
  }

  pub fn sub(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().sub)(self.pointer(), other)
  }

  pub fn sub_inv(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().sub_inv)(self.pointer(), other)
  }

  pub fn mul(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().mul)(self.pointer(), other)
  }

  pub fn div(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().div)(self.pointer(), other)
  }

  pub fn div_inv(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().div_inv)(self.pointer(), other)
  }

  pub fn rem(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().rem)(self.pointer(), other)
  }

  pub fn rem_inv(&self, other: Value) -> ObjectResult<Value> {
    (self.vtable().rem_inv)(self.pointer(), other)
  }

  pub fn basic_desc(&self) -> &'static str {
    (self.vtable().basic_desc)()
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

impl From<Nil> for Value {
  fn from(_: Nil) -> Self {
    Self { bits: NIL_TAG }
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
      bits: (item as u64) | (Tag::Integer as u64),
    }
  }
}

impl<T> From<T> for Value
where
  T: Object,
{
  fn from(item: T) -> Self {
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

impl Assign<Nil> for Value {}

impl Assign<i32> for Value {}

impl Assign<f64> for Value {}

impl<T: Object> Assign<T> for Value {}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self.tag() {
      Tag::Float => write!(f, "{}", self.as_float()),
      Tag::Integer => write!(f, "{}", self.as_int()),
      Tag::Nil => write!(f, "nil"),
      Tag::Pointer => write!(f, "{}", self.basic_desc()),
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
  set: fn(MutVoid, name: &str, value: Value) -> ObjectResult<()>,
  get: fn(ConstVoid, name: &str) -> ObjectResult<Value>,
  add: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  sub: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  sub_inv: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  mul: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  div: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  div_inv: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  rem: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  rem_inv: fn(ConstVoid, other: Value) -> ObjectResult<Value>,
  drop: fn(MutVoid),
  dealloc: fn(MutVoid),
  kind: fn() -> TypeId,
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
      sub_inv: |this, other| <T as Object>::sub_inv(unsafe { &*Self::void_to(this) }, other),
      mul: |this, other| <T as Object>::mul(unsafe { &*Self::void_to(this) }, other),
      div: |this, other| <T as Object>::div(unsafe { &*Self::void_to(this) }, other),
      div_inv: |this, other| <T as Object>::div_inv(unsafe { &*Self::void_to(this) }, other),
      rem: |this, other| <T as Object>::rem(unsafe { &*Self::void_to(this) }, other),
      rem_inv: |this, other| <T as Object>::rem_inv(unsafe { &*Self::void_to(this) }, other),
      drop: |this| <T as Object>::drop(unsafe { &mut *Self::void_to_mut(this) }),
      dealloc: |this| <T as Object>::dealloc(this as *mut T),
      kind: || <T as Object>::kind(),
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

pub trait Object
where
  Self: Sized + 'static,
{
  #[allow(unused_variables)]
  fn set(&mut self, name: &str, value: Value) -> ObjectResult<()> {
    Err(UnimplementedFunction::Set.to_string())
  }

  #[allow(unused_variables)]
  fn get(&self, name: &str) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Get.to_string())
  }

  #[allow(unused_variables)]
  fn add(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Add.to_string())
  }

  #[allow(unused_variables)]
  fn sub(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Sub.to_string())
  }

  #[allow(unused_variables)]
  fn sub_inv(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::SubInv.to_string())
  }

  #[allow(unused_variables)]
  fn mul(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Mul.to_string())
  }

  #[allow(unused_variables)]
  fn div(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Div.to_string())
  }

  #[allow(unused_variables)]
  fn div_inv(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::DivInv.to_string())
  }

  #[allow(unused_variables)]
  fn rem(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::Rem.to_string())
  }

  #[allow(unused_variables)]
  fn rem_inv(&self, other: Value) -> ObjectResult<Value> {
    Err(UnimplementedFunction::RemInv.to_string())
  }

  fn drop(&mut self) {}

  // override this only if necessary
  fn dealloc(this: *mut Self) {
    consume::<Self>(this);
  }

  // below this line should not to be reimplemented by the user

  fn kind() -> TypeId {
    TypeId::of::<Self>()
  }

  fn basic_desc() -> &'static str {
    std::any::type_name::<Self>()
  }
}

pub enum UnimplementedFunction {
  Set,
  Get,
  Add,
  Sub,
  SubInv,
  Mul,
  Div,
  DivInv,
  Rem,
  RemInv,
  Custom(String),
}

impl Display for UnimplementedFunction {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{} is unimplemented",
      match self {
        UnimplementedFunction::Set => "set",
        UnimplementedFunction::Get => "get",
        UnimplementedFunction::Add => "add",
        UnimplementedFunction::Sub => "sub",
        UnimplementedFunction::SubInv => "sub_inv",
        UnimplementedFunction::Mul => "mul",
        UnimplementedFunction::Div => "div",
        UnimplementedFunction::DivInv => "div_inv",
        UnimplementedFunction::Rem => "rem",
        UnimplementedFunction::RemInv => "rem_inv",
        UnimplementedFunction::Custom(s) => s,
      }
    )
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

pub trait Assign<T>: From<T>
where
  Self: Sized,
{
  fn assign(&mut self, t: T) {
    *self = Self::from(t);
  }
}

#[derive(Default)]
pub struct Struct {
  pub members: BTreeMap<String, Value>,
}

impl Object for Struct {
  fn set(&mut self, name: &str, value: Value) -> ObjectResult<()> {
    self.members.insert(name.to_string(), value);
    Ok(())
  }

  fn get(&self, name: &str) -> ObjectResult<Value> {
    Ok(self.members.get(name).cloned().unwrap_or_default())
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
