use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

macro_rules! here {
  () => {
    crate::dbg::_here(file!(), line!());
  };
}

pub(crate) use here;

use crate::code::{OpCode, OpCodeReflection};

pub fn _here(file: &str, line: u32) {
  use std::io::{stdout, Write};
  println!("{}:{}", file, line);
  stdout().flush().unwrap();
}

#[derive(Default, PartialEq)]
pub struct RuntimeError {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl RuntimeError {
  pub fn from_ref<M: ToString>(msg: M, opcode: &OpCode, opcode_ref: OpCodeReflection) -> Self {
    let mut e = Self {
      msg: msg.to_string(),
      file: opcode_ref.file.clone(),
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    e.format_with_src_line(opcode_ref.source_line);
    e.msg = format!("{}\nOffending OpCode: {:?}", e.msg, opcode);
    e
  }

  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!("{}\n{}\n{}^", self.msg, src, " ".repeat(self.column - 1));
  }
}

impl Debug for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

impl Display for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}
