use crate::prelude::*;

#[derive(Usertype, Class)]
pub struct ModuleValue {}

impl ModuleValue {
  pub fn new() -> Self {
    Self {}
  }
}

#[methods]
impl ModuleValue {}
