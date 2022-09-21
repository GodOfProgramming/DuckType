use super::Object;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub struct Error {
  msg: String,
}

impl Object for Error {}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", self.msg)
  }
}

impl From<String> for Error {
  fn from(msg: String) -> Self {
    Self { msg }
  }
}
