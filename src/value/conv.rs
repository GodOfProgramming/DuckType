use super::{
  Cast, CastMut, IsType, ReinterpretCast, ReinterpretCastMut, BOOL_TAG, CHAR_TAG, F64_MAX, I32_TAG, NATIVE_FN_TAG, NIL_TAG,
  TAG_BITMASK, VALUE_BITMASK,
};
use crate::{dbg::macros::here, prelude::*};
use std::mem;

impl<T> From<UsertypeHandle<T>> for Value
where
  T: Usertype,
{
  fn from(utype: UsertypeHandle<T>) -> Self {
    Value::from(&utype)
  }
}

impl<T> From<&UsertypeHandle<T>> for Value
where
  T: Usertype,
{
  fn from(utype: &UsertypeHandle<T>) -> Self {
    Value::from(utype.handle.clone())
  }
}

pub trait MaybeFrom<T>
where
  Self: Sized,
{
  fn maybe_from(value: T) -> Option<Self>;
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

impl MaybeFrom<Value> for f64 {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_f64()
  }
}

impl MaybeFrom<Value> for &'static String {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<StringValue>().map(|s| &**s)
  }
}

impl MaybeFrom<Value> for &'static Vec<Value> {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<VecValue>().map(|a| &**a)
  }
}

impl MaybeFrom<Value> for &[Value] {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<VecValue>().map(|a| &***a)
  }
}

impl<T> MaybeFrom<Value> for &'static T
where
  T: Usertype,
{
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<T>()
  }
}

impl<T> MaybeFrom<Value> for &'static mut T
where
  T: Usertype,
{
  fn maybe_from(mut value: Value) -> Option<Self> {
    value.cast_to_mut::<T>()
  }
}

impl<T0, T1> MaybeFrom<Value> for (Option<T0>, Option<T1>)
where
  T0: MaybeFrom<Value>,
  T1: MaybeFrom<Value>,
{
  fn maybe_from(value: Value) -> Option<Self> {
    let t0 = T0::maybe_from(value.clone());
    let t1 = T1::maybe_from(value);
    if t0.is_none() && t1.is_none() || t0.is_some() && t1.is_some() {
      None
    } else {
      Some((t0, t1))
    }
  }
}

// f64

impl IsType<f64> for Value {
  fn is_type(&self) -> bool {
    self.bits < F64_MAX
  }
}

// i32

impl IsType<i32> for Value {
  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == I32_TAG
  }
}

// bool

impl IsType<bool> for Value {
  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == BOOL_TAG
  }
}

// char

impl IsType<char> for Value {
  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == CHAR_TAG
  }
}

// fn

impl IsType<NativeFn> for Value {
  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == NATIVE_FN_TAG
  }
}

impl Cast<NativeFn> for Value {
  fn cast(&self) -> Option<&'static NativeFn> {
    if self.is::<NativeFn>() {
      Some(<Self as ReinterpretCast<NativeFn>>::reinterpret_cast(self))
    } else {
      None
    }
  }
}

impl ReinterpretCast<NativeFn> for Value {
  fn reinterpret_cast(&self) -> &'static NativeFn {
    here!("0x{:x}", self.bits);
    unsafe { mem::transmute(self.bits & VALUE_BITMASK) }
  }
}

// pointer

impl<T> IsType<T> for Value
where
  T: Usertype,
{
  fn is_type(&self) -> bool {
    self.is_ptr() && *self.type_id() == T::ID
  }
}

impl<T> Cast<T> for Value
where
  T: Usertype,
{
  fn cast(&self) -> Option<&'static T> {
    if self.is::<T>() {
      Some(self.reinterpret_cast())
    } else {
      None
    }
  }
}

impl<T> CastMut<T> for Value
where
  T: Usertype,
{
  fn cast_mut(&mut self) -> Option<&'static mut T> {
    if self.is::<T>() {
      Some(self.reinterpret_cast_mut())
    } else {
      None
    }
  }
}

impl<T> ReinterpretCast<T> for Value
where
  T: Usertype,
{
  fn reinterpret_cast(&self) -> &'static T {
    unsafe { &*(self.pointer() as *const T) }
  }
}

impl<T> ReinterpretCastMut<T> for Value
where
  T: Usertype,
{
  fn reinterpret_cast_mut(&mut self) -> &'static mut T {
    unsafe { &mut *(self.pointer_mut() as *mut T) }
  }
}

// nil

impl IsType<()> for Value {
  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == NIL_TAG
  }
}
