pub const F64_MAX: u64 = 0xfff8000000000000;
pub const TAG_BITMASK: u64 = F64_MAX | 0x0007000000000000;
pub const VALUE_BITMASK: u64 = !TAG_BITMASK;

const fn make_tag<const I: u64>() -> u64 {
  I << 48 | F64_MAX
}

pub const I32_TAG: u64 = make_tag::<1>();
pub const BOOL_TAG: u64 = make_tag::<2>();
pub const CHAR_TAG: u64 = make_tag::<3>();
pub const NATIVE_FN_TAG: u64 = make_tag::<4>();
pub const POINTER_TAG: u64 = make_tag::<6>();
pub const NIL_TAG: u64 = make_tag::<7>();

#[repr(u64)]
#[derive(PartialEq, Eq, Debug)]
pub enum Tag {
  F64 = 0,
  I32 = I32_TAG,
  Bool = BOOL_TAG,
  Char = CHAR_TAG,
  NativeFn = NATIVE_FN_TAG,
  Pointer = POINTER_TAG,
  Nil = NIL_TAG,
}

#[allow(unused)]
mod unused_idea {
  pub const PRIMITIVE_VALUE_BITMASK: u64 = 2u64.pow(32) - 1 << 32;

  const fn make_variant<const I: u64, const V: u64>() -> u64 {
    I | reverse_bits::<V>() >> 16
  }

  const fn reverse_bits<const N: u64>() -> u64 {
    let mut reverse = 0;

    const NO_OF_BITS: u64 = 64;
    let mut i = 0;
    loop {
      if i >= NO_OF_BITS {
        break;
      }

      if N & (1 << i) > 0 {
        reverse |= 1 << (NO_OF_BITS - 1 - i)
      }

      i += 1;
    }

    reverse
  }
}
