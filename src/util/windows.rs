use super::*;
use anyhow::{bail, Result};
use std::{
  ffi::{c_void, CString},
  mem,
};
use windows_sys::Win32::{
  Foundation::{GENERIC_ACCESS_RIGHTS, GENERIC_READ, HANDLE},
  Storage::FileSystem::{
    self, FileIdInfo, FILE_ATTRIBUTE_NORMAL, FILE_CREATION_DISPOSITION, FILE_FLAGS_AND_ATTRIBUTES, FILE_ID_128, FILE_ID_INFO,
    FILE_INFO_BY_HANDLE_CLASS, FILE_SHARE_MODE, FILE_SHARE_READ, OPEN_EXISTING,
  },
};

pub struct WindowsMetadata;

pub type FileIdType = u128;

impl FileMetadata for WindowsMetadata {
  fn id_of(path: &Path) -> Result<FileIdType> {
    const FILE_ID_INFO_SIZE: usize = mem::size_of::<FILE_ID_INFO>();
    const CLASS: FILE_INFO_BY_HANDLE_CLASS = FileIdInfo;
    const ACCESS: GENERIC_ACCESS_RIGHTS = GENERIC_READ;
    const SHARED_MODE: FILE_SHARE_MODE = FILE_SHARE_READ;
    const CREATION_DISPOSITION: FILE_CREATION_DISPOSITION = OPEN_EXISTING;
    const ATTRIBUTE_FLAGS: FILE_FLAGS_AND_ATTRIBUTES = FILE_ATTRIBUTE_NORMAL;

    static_assertions::const_assert!(FILE_ID_INFO_SIZE < u32::MAX as usize);

    let path_str = path.display().to_string();
    let path_str = CString::new(path_str)?;
    let filename = path_str.as_c_str().as_ptr() as *const u8;

    let hfile = unsafe {
      FileSystem::CreateFileA(
        filename,
        ACCESS,
        SHARED_MODE,
        std::ptr::null(),
        CREATION_DISPOSITION,
        ATTRIBUTE_FLAGS,
        HANDLE::default(),
      )
    };

    let mut lpfileinfo = FILE_ID_INFO {
      VolumeSerialNumber: 0,
      FileId: FILE_ID_128 { Identifier: [0; 16] },
    };

    let success = unsafe {
      FileSystem::GetFileInformationByHandleEx(
        hfile,
        CLASS,
        &mut lpfileinfo as *mut FILE_ID_INFO as *mut c_void,
        FILE_ID_INFO_SIZE as u32,
      ) != 0
    };

    if success {
      Ok(u128::from_le_bytes(lpfileinfo.FileId.Identifier))
    } else {
      bail!("failed to query file info");
    }
  }
}
