pub mod strproc;

#[cfg(windows)]
pub mod windows;
#[cfg(windows)]
pub use windows::*;
#[cfg(windows)]
pub type PlatformMetadata = WindowsMetadata;

#[cfg(unix)]
pub mod unix;
#[cfg(unix)]
pub use unix::*;
#[cfg(unix)]
pub type PlatformMetadata = UnixMetadata;

pub mod prelude {
  pub use super::{FileMetadata, PlatformMetadata};
}

use anyhow::Result;
use std::path::Path;

pub trait FileMetadata {
  fn id_of(path: &Path) -> Result<FileIdType>;
}

pub(crate) trait UnwrapAnd<T> {
  fn unwrap_and(self, f: impl FnOnce(T)) -> bool;
}

impl<T> UnwrapAnd<T> for Option<T> {
  fn unwrap_and(self, f: impl FnOnce(T)) -> bool {
    if let Some(inner) = self {
      f(inner);
      true
    } else {
      false
    }
  }
}
