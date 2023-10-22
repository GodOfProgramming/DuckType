#[cfg(windows)]
pub mod windows;

use anyhow::Result;
use std::path::Path;

#[cfg(windows)]
pub use windows::*;

#[cfg(windows)]
pub type PlatformMetadata = WindowsMetadata;

pub trait FileMetadata {
  fn id_of(path: &Path) -> Result<FileIdType>;
}
