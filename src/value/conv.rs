use super::{BOOL_TAG, CHAR_TAG, Cast, F64_MAX, I32_TAG, NATIVE_FN_TAG, NIL_TAG, TAG_BITMASK, VALUE_BITMASK};
use crate::prelude::*;
use std::mem;

impl Cast<f64> for Value {
  type Out = f64;
  type OutMut = f64;

  fn is_type(&self) -> bool {
    self.bits < F64_MAX
  }

  fn unchecked_cast(&self) -> Self::Out {
    debug_assert!(self.is::<f64>());
    f64::from_bits(self.bits)
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    self.unchecked_cast_to::<Self::Out>()
  }
}

impl Cast<i32> for Value {
  type Out = i32;
  type OutMut = i32;

  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == I32_TAG
  }

  fn unchecked_cast(&self) -> Self::Out {
    unsafe { mem::transmute((self.bits & VALUE_BITMASK) as u32) }
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    self.unchecked_cast_to::<Self::Out>()
  }
}

impl Cast<bool> for Value {
  type Out = bool;
  type OutMut = bool;

  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == BOOL_TAG
  }

  fn unchecked_cast(&self) -> Self::Out {
    debug_assert!(self.is::<bool>());
    self.bits & VALUE_BITMASK > 0
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    self.unchecked_cast_to::<Self::Out>()
  }
}

impl Cast<char> for Value {
  type Out = char;
  type OutMut = char;

  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == CHAR_TAG
  }

  fn unchecked_cast(&self) -> Self::Out {
    debug_assert!(self.is::<char>());
    char::from_u32((self.bits & VALUE_BITMASK) as u32).unwrap_or_default()
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    self.unchecked_cast_to::<Self::Out>()
  }
}

impl Cast<NativeFn> for Value {
  type Out = NativeFn;
  type OutMut = NativeFn;

  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == NATIVE_FN_TAG
  }

  fn unchecked_cast(&self) -> Self::Out {
    debug_assert!(self.is::<NativeFn>());
    unsafe { mem::transmute(self.bits & VALUE_BITMASK) }
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    self.unchecked_cast_to::<Self::Out>()
  }
}

impl<T> Cast<T> for Value
where
  T: Usertype,
{
  type Out = &'static T;
  type OutMut = &'static mut T;

  fn is_type(&self) -> bool {
    self.is_ptr() && *self.type_id() == T::ID
  }

  fn unchecked_cast(&self) -> Self::Out {
    debug_assert!(self.is::<T>());
    unsafe { &*(self.pointer() as ConstAddr<T>) }
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    unsafe { &mut *(self.pointer_mut() as MutAddr<T>) }
  }
}

impl Cast<()> for Value {
  type Out = ();
  type OutMut = ();

  fn is_type(&self) -> bool {
    self.bits & TAG_BITMASK == NIL_TAG
  }

  fn unchecked_cast(&self) -> Self::Out {}

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {}
}

// utility conversions

impl<T0, T1> Cast<(Option<T0>, Option<T1>)> for Value
where
  Self: Cast<T0> + Cast<T1>,
{
  type Out = (Option<<Self as Cast<T0>>::Out>, Option<<Self as Cast<T1>>::Out>);
  type OutMut = (Option<<Self as Cast<T0>>::OutMut>, Option<<Self as Cast<T1>>::OutMut>);

  fn is_type(&self) -> bool {
    let is_t0 = self.is::<T0>();
    let is_t1 = self.is::<T1>();

    is_t0 || is_t1
  }

  /// Do not use unless absolutely needed, unchecked is both faster and has safety
  fn cast(&self) -> Option<Self::Out> {
    let t0 = self.cast_to::<T0>();
    let t1 = self.cast_to::<T1>();

    if t0.is_none() && t1.is_none() { None } else { Some((t0, t1)) }
  }

  /// Do not use unless absolutely needed, unchecked is both faster and has safety
  fn cast_mut(&mut self) -> Option<Self::OutMut> {
    let t0 = self.cast_to_mut::<T0>();
    let t1 = self.cast_to_mut::<T1>();

    if t0.is_none() && t1.is_none() { None } else { Some((t0, t1)) }
  }

  fn unchecked_cast(&self) -> Self::Out {
    (self.cast_to::<T0>(), self.cast_to::<T1>())
  }

  fn unchecked_cast_mut(&mut self) -> Self::OutMut {
    (self.cast_to_mut::<T0>(), self.cast_to_mut::<T1>())
  }
}

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
    value.cast_to::<i32>()
  }
}

impl MaybeFrom<Value> for f64 {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<f64>()
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
    let t0 = T0::maybe_from(value);
    let t1 = T1::maybe_from(value);

    if t0.is_none() && t1.is_none() { None } else { Some((t0, t1)) }
  }
}
