use std::{fs, io::Read, path::PathBuf};

use crate::prelude::*;

pub struct LibIo;

impl LibIo {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      lib.set("open", Value::native(open)).ok();
    })
    .into()
  }
}

#[derive(Usertype, Fields)]
#[uuid("d5548736-3896-4b0c-bcd4-84c1280d5008")]
struct File {
  internal: Option<fs::File>,
}

impl File {
  fn new(file: fs::File) -> Self {
    Self { internal: Some(file) }
  }
}

#[native]
fn open(filename: &StringValue) -> ValueResult {
  let path = PathBuf::from(filename.as_str());
  let file = fs::File::open(path).map_err(|e| ValueError::Todo(e.to_string()))?;
  Ok(Value::from(File::new(file)))
}

#[methods]
impl File {
  fn __new__() -> ValueResult {
    Err(ValueError::Infallible)
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
