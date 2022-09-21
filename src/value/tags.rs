pub const TAG_BITMASK: u64 = 0x0007000000000000;
pub const INF_VALUE: u64 = 0xfff8000000000000;
pub const POINTER_BITMASK: u64 = !(INF_VALUE | TAG_BITMASK);

const fn make_tag<const I: u8>() -> u64 {
  ((I as u64) << 48) | INF_VALUE
}

pub const F64_TAG: u64 = 0;
pub const I32_TAG: u64 = make_tag::<1>();
pub const CHAR_TAG: u64 = make_tag::<2>();
pub const POINTER_TAG: u64 = make_tag::<6>();
pub const NIL_TAG: u64 = make_tag::<7>();

#[repr(u64)]
#[derive(Debug, PartialEq)]
pub enum Tag {
  F64 = F64_TAG,
  I32 = I32_TAG,
  Char = CHAR_TAG,
  Pointer = POINTER_TAG, // struct, class, instance, function, error, etc..
  Nil = NIL_TAG,
}
