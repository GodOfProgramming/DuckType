use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[allow(unused)]
#[cfg(debug_assertions)]
macro_rules! here {
  () => {
    crate::dbg::_here(file!(), line!());
  };
}

#[allow(unused)]
#[cfg(debug_assertions)]
pub(crate) use here;

use crate::code::{OpCodeReflection, Opcode};

pub mod prelude {
  #[allow(unused)]
  #[cfg(debug_assertions)]
  pub(crate) use super::here;
  pub use super::RuntimeError;
}

pub fn _here(file: &str, line: u32) {
  use std::io::{stdout, Write};
  println!("{}:{}", file, line);
  stdout().flush().unwrap();
}

#[derive(Default, PartialEq, Eq)]
pub struct RuntimeError {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl RuntimeError {
  pub fn from_ref<M: ToString>(msg: M, opcode: &Opcode, opcode_ref: OpCodeReflection) -> Self {
    let mut err = Self {
      msg: msg.to_string(),
      file: opcode_ref.file.clone(),
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    err.format_with_src_line(opcode_ref.source_line);
    err.msg = format!("{}\nOffending OpCode: {:?}", err.msg, opcode);
    err
  }

  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!("{}\n{}\n{}^", self.msg, src, " ".repeat(self.column - 1));
  }
}

impl Debug for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(f, "{} ({}, {}): {}", self.file, self.line, self.column, self.msg)
  }
}

impl Display for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(f, "{} ({}, {}): {}", self.file, self.line, self.column, self.msg)
  }
}
