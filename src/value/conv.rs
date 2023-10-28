use crate::prelude::*;

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

impl MaybeFrom<Value> for &'static String {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_str().map(|s| &**s)
  }
}

impl MaybeFrom<Value> for &'static Vec<Value> {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_vec().map(|a| &**a)
  }
}

impl MaybeFrom<Value> for &[Value] {
  fn maybe_from(value: Value) -> Option<Self> {
    value.as_vec().map(|a| &***a)
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
