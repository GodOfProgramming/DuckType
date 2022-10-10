pub const F64_MAX: u64 = 0xfff8000000000000;
pub const TAG_BITMASK: u64 = F64_MAX | 0x0007000000000000;
pub const VALUE_BITMASK: u64 = !TAG_BITMASK;

const fn make_tag<const I: u8>() -> u64 {
  ((I as u64) << 48) | F64_MAX
}

pub const I32_TAG: u64 = make_tag::<1>();
pub const BOOL_TAG: u64 = make_tag::<2>();
pub const CHAR_TAG: u64 = make_tag::<3>();
pub const FN_TAG: u64 = make_tag::<5>();
pub const POINTER_TAG: u64 = make_tag::<6>();
pub const NIL_TAG: u64 = make_tag::<7>();

#[repr(u64)]
#[derive(PartialEq, Eq, Debug)]
pub enum Tag {
  F64 = 0,
  I32 = I32_TAG,
  Bool = BOOL_TAG,
  Char = CHAR_TAG,
  NativeFn = FN_TAG,
  Pointer = POINTER_TAG,
  Nil = NIL_TAG,
}
