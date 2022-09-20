pub const TAG_BITMASK: u64 = 0x0007000000000000;
pub const INF_VALUE: u64 = 0xfff8000000000000;

pub const NIL_TAG: u64 = make_tag::<1>();
pub const INTEGER_TAG: u64 = make_tag::<2>();
pub const POINTER_TAG: u64 = make_tag::<7>();

const fn make_tag<const I: u8>() -> u64 {
  ((I as u64) << 48) | INF_VALUE
}
