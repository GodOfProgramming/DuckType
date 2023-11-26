mod env;

pub mod prelude {
  pub use super::env::prelude::*;
  #[allow(unused_imports)]
  pub(crate) use super::env::{ContextDisassembler, InstructionDisassembler};
}

use crate::prelude::*;
use ptr::SmartPtr;
use std::{
  fmt::{self, Debug, Display, Formatter},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct StackFrame {
  #[cfg(feature = "jtbl")]
  ip: Box<usize>,

  #[cfg(not(feature = "jtbl"))]
  ip: usize,

  pub sp: usize,
  pub ctx: SmartPtr<Context>,
  pub export: Option<Value>,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>, sp: usize) -> Self {
    Self {
      ip: Default::default(),
      sp,
      ctx,
      export: None,
    }
  }

  pub fn ip(&self) -> usize {
    #[cfg(feature = "jtbl")]
    {
      *self.ip
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip
    }
  }

  pub fn ip_inc(&mut self, offset: usize) {
    #[cfg(feature = "jtbl")]
    {
      *self.ip += offset;
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip += offset;
    }
  }

  pub fn ip_dec(&mut self, offset: usize) {
    #[cfg(feature = "jtbl")]
    {
      *self.ip -= offset;
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip -= offset;
    }
  }

  #[cfg(feature = "jtbl")]
  pub fn ip_ptr(&mut self) -> *mut usize {
    &mut *self.ip as *mut usize
  }
}

#[derive(Default)]
pub(crate) struct ModuleStack {
  envs: Vec<ModuleEntry>,
}

impl ModuleStack {
  pub(crate) fn iter(&self) -> std::slice::Iter<ModuleEntry> {
    self.envs.iter()
  }

  pub(crate) fn len(&self) -> usize {
    self.envs.len()
  }

  pub(crate) fn push(&mut self, entry: ModuleEntry) {
    self.envs.push(entry);
  }

  pub(crate) fn pop(&mut self) -> ModuleEntry {
    self.envs.pop().expect("pop: the env stack should never be empty")
  }

  pub(crate) fn last(&self) -> &UsertypeHandle<ModuleValue> {
    match self.envs.last().expect("last: the env stack should never be empty") {
      ModuleEntry::Fn(e) => e,
      ModuleEntry::Mod(e) => e,
      ModuleEntry::File(e) => e,
      ModuleEntry::String(e) => e,
    }
  }

  pub(crate) fn last_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    match self.envs.last_mut().expect("last_mut: the env stack should never be empty") {
      ModuleEntry::Fn(e) => e,
      ModuleEntry::Mod(e) => e,
      ModuleEntry::File(e) => e,
      ModuleEntry::String(e) => e,
    }
  }
}

pub(crate) enum ModuleEntry {
  Fn(UsertypeHandle<ModuleValue>),
  Mod(UsertypeHandle<ModuleValue>),
  File(UsertypeHandle<ModuleValue>),
  String(UsertypeHandle<ModuleValue>),
}

impl ModuleEntry {
  pub(crate) fn module(&self) -> UsertypeHandle<ModuleValue> {
    match self {
      Self::Fn(m) => m.clone(),
      Self::Mod(m) => m.clone(),
      Self::File(m) => m.clone(),
      Self::String(m) => m.clone(),
    }
  }
}

impl Debug for ModuleEntry {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fn(_) => f.debug_tuple("Fn").finish(),
      Self::Mod(_) => f.debug_tuple("Mod").finish(),
      Self::File(_) => f.debug_tuple("File").finish(),
      Self::String(_) => f.debug_tuple("String").finish(),
    }
  }
}
