use crate::{
  prelude::*,
  value::{tags::*, MutVoid, ValueMeta},
};
use std::mem;

pub(crate) const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

#[derive(Default)]
pub struct Gc {
  allocations: Vec<u64>,
}

impl Gc {
  fn allocate_usertype<T: Usertype>(&mut self, item: T) -> Value {
    let allocated = unsafe { &mut *Self::allocate_type(AllocatedObject::new(item)) };

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
    let bits = ptr as u64 | POINTER_TAG;
    self.allocations.push(bits);
    Value { bits }
  }

  pub(crate) fn consume<T: Usertype>(this: *mut T) {
    let _ = unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) };
  }

  fn allocate_type<T>(item: T) -> *mut T {
    Box::into_raw(Box::new(item))
  }
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

pub trait Allocation<T> {
  fn allocate(&mut self, value: T) -> Value;
}

impl Allocation<Value> for Gc {
  fn allocate(&mut self, value: Value) -> Value {
    value
  }
}

impl Allocation<&Value> for Gc {
  fn allocate(&mut self, value: &Value) -> Value {
    value.clone()
  }
}

impl Allocation<()> for Gc {
  fn allocate(&mut self, value: ()) -> Value {
    Value::from(value)
  }
}

impl Allocation<f64> for Gc {
  fn allocate(&mut self, value: f64) -> Value {
    Value { bits: value.to_bits() }
  }
}

impl Allocation<i32> for Gc {
  fn allocate(&mut self, value: i32) -> Value {
    Value::from(&value)
  }
}

impl Allocation<&i32> for Gc {
  fn allocate(&mut self, value: &i32) -> Value {
    Value::from(value)
  }
}

impl Allocation<bool> for Gc {
  fn allocate(&mut self, value: bool) -> Value {
    Value::from(value)
  }
}

impl Allocation<char> for Gc {
  fn allocate(&mut self, value: char) -> Value {
    Value::from(value)
  }
}

impl Allocation<NativeFn> for Gc {
  fn allocate(&mut self, value: NativeFn) -> Value {
    Value::from(value)
  }
}

impl Allocation<Nil> for Gc {
  fn allocate(&mut self, value: Nil) -> Value {
    Value::from(value)
  }
}

impl Allocation<&str> for Gc {
  fn allocate(&mut self, value: &str) -> Value {
    self.allocate_usertype::<StringValue>(value.into())
  }
}

impl Allocation<String> for Gc {
  fn allocate(&mut self, value: String) -> Value {
    self.allocate_usertype::<StringValue>(value.into())
  }
}

impl Allocation<&String> for Gc {
  fn allocate(&mut self, value: &String) -> Value {
    self.allocate_usertype::<StringValue>(value.clone().into())
  }
}

impl Allocation<&[Value]> for Gc {
  fn allocate(&mut self, value: &[Value]) -> Value {
    self.allocate_usertype(ArrayValue::new_from_slice(value))
  }
}

impl Allocation<Vec<Value>> for Gc {
  fn allocate(&mut self, value: Vec<Value>) -> Value {
    self.allocate_usertype(ArrayValue::new_from_vec(&value))
  }
}

impl<T> Allocation<T> for Gc
where
  T: Usertype,
{
  fn allocate(&mut self, value: T) -> Value {
    self.allocate_usertype::<T>(value)
  }
}
