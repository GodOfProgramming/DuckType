pub const INF_VALUE: u64 = 0xfff8000000000000;
pub const TAG_BITMASK: u64 = INF_VALUE | 0x0007000000000000;
pub const POINTER_BITMASK: u64 = !TAG_BITMASK;

const fn make_tag<const I: u8>() -> u64 {
  ((I as u64) << 48) | INF_VALUE
}

pub const I32_TAG: u64 = make_tag::<1>();
pub const BOOL_TAG: u64 = make_tag::<2>();
pub const CHAR_TAG: u64 = make_tag::<3>();
pub const FN_TAG: u64 = make_tag::<5>();
pub const POINTER_TAG: u64 = make_tag::<6>();
pub const NIL_TAG: u64 = make_tag::<7>();
