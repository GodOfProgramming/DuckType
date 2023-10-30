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

use anyhow::Result;
use std::path::Path;

pub trait FileMetadata {
  fn id_of(path: &Path) -> Result<FileIdType>;
}
