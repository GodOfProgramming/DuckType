use std::{fs, io::Read, ops::Deref, path::PathBuf};

use crate::prelude::*;

pub struct LibIo;

impl LibIo {
  pub fn load(gc: &mut Gc) -> Value {
    io::simple_script_autogen_create_module(gc)
  }
}

fn open_file(path: (Option<&String>, Option<&io::PathValue>)) -> ValueResult<fs::File> {
  let path = match path {
    (Some(str_path), None) => PathBuf::from(str_path.as_str()),
    (None, Some(path)) => (*path).clone(),
    _ => Err(ValueError::Infallible)?,
  };

  let file = fs::File::open(path).map_err(|e| ValueError::NativeApi(e.to_string()))?;

  Ok(file)
}

#[native]
mod io {
  #[native]
  fn open(path: (Option<&String>, Option<&io::PathValue>)) -> ValueResult<FileValue> {
    let file = super::open_file(path)?;
    Ok(FileValue::new(file))
  }

  #[derive(Usertype, Fields, Renameable, Default)]
  #[uuid("d5548736-3896-4b0c-bcd4-84c1280d5008")]
  #[rename("File")]
  pub struct FileValue {
    internal: Option<fs::File>,
  }

  impl FileValue {
    fn new(file: fs::File) -> Self {
      Self { internal: Some(file) }
    }
  }

  #[methods]
  impl FileValue {
    fn __new__() -> ValueResult<FileValue> {
      Ok(Default::default())
    }

    fn open(&mut self, path: (Option<&String>, Option<&io::PathValue>)) -> ValueResult<()> {
      let file = super::open_file(path)?;
      self.internal = Some(file);
      Ok(())
    }

    fn read(&mut self) -> ValueResult<String> {
      if let Some(file) = &mut self.internal {
        let mut out = String::new();

        file.read_to_string(&mut out).map_err(|e| ValueError::Todo(e.to_string()))?;

        Ok(out)
      } else {
        Err(ValueError::runtime_error("no file open for reading"))
      }
    }
  }

  #[derive(Usertype, Renameable, Fields)]
  #[uuid("19d99235-9ed4-4181-bc02-a0e0f09ecab9")]
  #[rename("Path")]
  pub struct PathValue {
    internal: PathBuf,
  }

  impl PathValue {
    fn new(path: PathBuf) -> Self {
      Self { internal: path }
    }

    fn join(&self, part: impl Into<PathBuf>) -> Self {
      Self::new(self.internal.join(part.into()))
    }
  }

  #[methods]
  impl PathValue {
    fn __new__(path: (Option<&String>, Option<&PathValue>)) -> ValueResult<PathValue> {
      let path = match path {
        (Some(str_path), None) => PathBuf::from(str_path.as_str()),
        (None, Some(path)) => (*path).clone(),
        _ => Err(ValueError::Infallible)?,
      };
      Ok(PathValue::new(path))
    }

    fn __div__(&self, other: (Option<&String>, Option<&Self>)) -> ValueResult<Self> {
      match other {
        (Some(str), None) => Ok(self.join(str.clone())),
        (None, Some(path)) => Ok(self.join(path.internal.clone())),
        (None, None) => Err(ValueError::InvalidArgument("__div__", 1)),
        (Some(_), Some(_)) => Err(ValueError::Infallible),
      }
    }

    fn __str__(&self) -> String {
      format!("{}", self.internal.display())
    }
  }

  impl Deref for io::PathValue {
    type Target = PathBuf;
    fn deref(&self) -> &Self::Target {
      &self.internal
    }
  }
}
