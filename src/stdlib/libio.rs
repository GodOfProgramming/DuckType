use std::{fs, io::Read, path::PathBuf};

use crate::prelude::*;

pub struct LibIo;

impl LibIo {
  pub fn load(gc: &mut Gc) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      lib.set(gc, "open", Value::native(open)).ok();
    })
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
fn open(vm: &mut Vm, filename: &StringValue) -> ValueResult {
  let path = PathBuf::from(filename.as_str());
  let file = fs::File::open(path).map_err(|e| ValueError::Todo(e.to_string()))?;
  Ok(vm.gc.allocate(File::new(file)))
}

#[methods]
impl File {
  fn __new__() -> ValueResult {
    Err(ValueError::Infallible)
  }

  fn read(&mut self, _gc: &mut Gc) -> ValueResult<String> {
    if let Some(file) = &mut self.internal {
      let mut out = String::new();

      file.read_to_string(&mut out).map_err(|e| ValueError::Todo(e.to_string()))?;

      Ok(out)
    } else {
      Err(ValueError::runtime_error("no file open for reading"))
    }
  }
}
