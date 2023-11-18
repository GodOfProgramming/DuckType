use crate::prelude::*;
use std::{fs, io::Read, ops::Deref, path::PathBuf};

pub use io::duck_type_autogen_create_module;

fn open_file(path: (Option<&String>, Option<&io::PathValue>)) -> UsageResult<fs::File> {
  let path = match path {
    (Some(str_path), None) => PathBuf::from(str_path.as_str()),
    (None, Some(path)) => (*path).clone(),
    _ => Err(UsageError::Infallible)?,
  };

  let file = fs::File::open(path).map_err(|e| UsageError::NativeApi(e.to_string()))?;

  Ok(file)
}

#[native(no_entry)]
mod io {
  #[native]
  fn open(path: (Option<&String>, Option<&io::PathValue>)) -> UsageResult<FileValue> {
    let file = super::open_file(path)?;
    Ok(FileValue::new(file))
  }

  #[derive(Usertype, Fields, Renameable, Default, NoOperators)]
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
    fn __new__() -> UsageResult<FileValue> {
      Ok(Default::default())
    }

    fn open(&mut self, path: (Option<&String>, Option<&io::PathValue>)) -> UsageResult<()> {
      let file = super::open_file(path)?;
      self.internal = Some(file);
      Ok(())
    }

    fn read(&mut self) -> UsageResult<String> {
      if let Some(file) = &mut self.internal {
        let mut out = String::new();

        file.read_to_string(&mut out).map_err(UsageError::native)?;

        Ok(out)
      } else {
        Err(UsageError::NativeApi("no file open for reading".to_string()))
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
    fn __new__(path: (Option<&String>, Option<&PathValue>)) -> UsageResult<PathValue> {
      let path = match path {
        (Some(str_path), None) => PathBuf::from(str_path.as_str()),
        (None, Some(path)) => (*path).clone(),
        _ => Err(UsageError::Infallible)?,
      };
      Ok(PathValue::new(path))
    }
  }

  impl Operators for PathValue {
    #[binary]
    fn __div__(left: &PathValue, right: (Option<&String>, Option<&PathValue>)) -> UsageResult<PathValue> {
      match right {
        (Some(str), None) => Ok(left.join(str.clone())),
        (None, Some(path)) => Ok(left.join(path.internal.clone())),
        (None, None) => Err(UsageError::InvalidArgument("__div__", 1)),
        (Some(_), Some(_)) => Err(UsageError::Infallible),
      }
    }

    fn __str__(&self) -> String {
      format!("{}", self.internal.display())
    }

    fn __dbg__(&self) -> String {
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

#[cfg(test)]
mod tests {
  use crate::prelude::*;

  #[macros::native]
  mod for_testing_full_generation {}
}
