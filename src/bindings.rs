#[cfg(target_os = "linux")]
#[allow(clippy::all)]
#[allow(warnings)]
mod ffi {
  include!("generated/ffi.rs");
}

#[cfg(target_os = "linux")]
pub use ffi::*;
