#[allow(clippy::all)]
#[allow(warnings)]
mod ffi {
  include!("generated/ffi.rs");
}

pub use ffi::*;
