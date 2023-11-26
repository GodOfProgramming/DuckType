use super::FileMetadata;
use anyhow::Result;
use nix::{fcntl::OFlag, sys::stat::Mode};
use std::path::Path;

pub struct UnixMetadata;

pub type FileIdType = u64;

impl FileMetadata for UnixMetadata {
  fn id_of(path: &Path) -> Result<FileIdType> {
    let fd = nix::fcntl::open(path, OFlag::O_RDONLY, Mode::S_IRWXU)?;
    let stat = nix::sys::stat::fstat(fd)?;
    Ok(stat.st_ino)
  }
}
