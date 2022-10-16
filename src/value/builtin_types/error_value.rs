use super::Usertype;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub struct ErrorValue {
  msg: String,
}

impl Usertype for ErrorValue {
  const ID: crate::value::UsertypeId = "Error";

  fn stringify(&self) -> String {
    self.msg.clone()
  }
}

impl Display for ErrorValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", self.msg)
  }
}

impl From<String> for ErrorValue {
  fn from(msg: String) -> Self {
    Self { msg }
  }
}