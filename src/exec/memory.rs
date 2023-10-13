use crate::prelude::*;
use crate::value::{tags::*, ValueMeta};
use std::mem;

pub const META_OFFSET: isize = -(mem::size_of::<ValueMeta>() as isize);

pub type ConstVoid = *const ();
pub type MutVoid = *mut ();

pub trait Allocator: Sized {
  fn allocate<'gc, T>(&'gc mut self, item: T) -> Handle<'gc, Self, T>
  where
    T: Usertype;

  fn free_memory(&mut self, vm: &Vm<Self>);
}

pub struct Handle<'gc, A, T>
where
  A: Allocator,
  T: Usertype,
{
  gc: &'gc mut A,
  data: &'gc mut AllocatedValue<T>,
}

impl<'gc, A, T> Handle<'gc, A, T>
where
  A: Allocator,
  T: Usertype,
{
  fn new(gc: &'gc mut A, data: &'gc mut AllocatedValue<T>) -> Self {
    data.meta.add_ref();
    Self { gc, data }
  }
}

impl<'gc, A, T> Clone for Handle<'gc, A, T>
where
  A: Allocator,
  T: Usertype,
{
  fn clone(&self) -> Self {
    Self::new(self.gc, self.data)
  }
}

impl<'gc, A, T> Drop for Handle<'gc, A, T>
where
  A: Allocator,
  T: Usertype,
{
  fn drop(&mut self) {
    self.data.meta.drop_ref();
  }
}

pub struct GC {
  allocations: Vec<usize>,
}

impl GC {
  fn allocate_any<T>(item: T) -> *mut T {
    Box::into_raw(Box::new(item))
  }

  fn deallocate<T: Usertype>(this: *mut T) -> Box<AllocatedValue<T>> {
    unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedValue<T>) }
  }
}

impl Allocator for GC {
  fn allocate<'gc, T>(&'gc mut self, item: T) -> Handle<'gc, Self, T>
  where
    T: Usertype,
  {
    let allocated_ptr = Self::allocate_any(AllocatedValue::new(item));
    let allocated = unsafe { &mut *allocated_ptr };
    let obj_ptr = &mut allocated.obj as *mut T as MutVoid;

    // ensure the pointer to the allocated object is offset by the right distance
    debug_assert_eq!(allocated as *const _ as *const (), &allocated.meta as *const _ as *const ());
    debug_assert_eq!(
      unsafe { (obj_ptr as *const u8).offset(META_OFFSET) as *const () },
      allocated as *const _ as *const ()
    );

    // ensure the pointer fits in 48 bits
    debug_assert_eq!(obj_ptr as u64 & POINTER_TAG, 0);

    self.allocations.push(allocated_ptr);

    // return a handle to the object
    Handle::new(self, allocated)
  }

  fn free_memory(&mut self, vm: &Vm<Self>) {
    let i = 0usize;
    while i < self.allocations.len() {
      if vm.addr_in_use(self.allocations[i]) {
        i += 1;
      } else {
        let addr = self.allocations.swap_remove(i);
      }
    }
  }
}

#[repr(C)]
struct AllocatedValue<T: Usertype> {
  meta: ValueMeta,
  obj: T,
}

impl<T: Usertype> AllocatedValue<T> {
  fn new(obj: T) -> Self {
    let meta = ValueMeta {
      ref_count: 1,
      vtable: &T::VTABLE,
    };
    Self { obj, meta }
  }

  fn ref_count(&self) -> usize {
    self.meta.ref_count()
  }
}
