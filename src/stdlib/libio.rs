use std::{fs, io::Read, path::PathBuf};

use crate::prelude::*;

pub struct LibIo;

impl LibIo {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      lib
        .set(
          "open",
          Value::native(|_vm, _env, args| {
            if let Some(filename) = args.list.first().map(|a| a.as_str()).flatten() {
              let path = PathBuf::from(filename.to_string());
              let file = fs::File::open(path).map_err(|e| ValueError::Todo(e.to_string()))?;
              Ok(Value::from(File::new(file)))
            } else {
              Err(ValueError::ArgumentError(0, 1))
            }
          }),
        )
        .ok();
    })
    .into()
  }
}

#[derive(Usertype, Class)]
struct File {
  internal: fs::File,
}

impl File {
  fn new(file: fs::File) -> Self {
    Self { internal: file }
  }
}

#[methods]
impl File {
  fn read(&mut self) -> ValueResult<String> {
    let mut out = String::new();

    self
      .internal
      .read_to_string(&mut out)
      .map_err(|e| ValueError::Todo(e.to_string()))?;

    Ok(out)
  }
}
