use crate::{
  Vm,
  exec::Context,
  prelude::{NativeFn, Usertype},
  value::Tag,
};
use std::{collections::HashMap, path::PathBuf};
use uuid::Uuid;

#[cfg(feature = "serde")]
impl serde::Serialize for Vm {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    todo!()
  }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Vm {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    todo!()
  }
}

pub struct State {
  stack: Vec<Value>,

  call_stack: Vec<StackFrame>,

  globals: HashMap<String, Value>,
  consts: Vec<ConstantValue>,

  dependencies: Vec<PathBuf>,
}

impl From<&Vm> for State {
  fn from(vm: &Vm) -> Self {
    todo!()
  }
}

pub struct StackFrame {
  ip: usize,

  pub bp: usize,

  pub ctx: Context,

  pub export: Option<Value>,

  pub is_req: bool,

  pub module_index: usize,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Value {
  Float(f64),
  Integer(i32),
  Bool(bool),
  Char(char),
  Native(String),
  Usertype(),
  Nil,
}

impl Value {
  pub fn from_vm(vm: &Vm, value: crate::value::Value) -> Self {
    match value.tag() {
      Tag::F64 => Self::Float(value.unchecked_cast_to::<f64>()),
      Tag::I32 => Self::Integer(value.unchecked_cast_to::<i32>()),
      Tag::Bool => Self::Bool(value.unchecked_cast_to::<bool>()),
      Tag::Char => Self::Char(value.unchecked_cast_to::<char>()),
      Tag::NativeFn => {
        todo!();
      }
      Tag::Pointer => {
        todo!();
      }
      Tag::Nil => Value::Nil,
    }
  }
}

pub enum ConstantValue {
  Integer(i32),
  Float(f64),
  String(String),
  StaticString(String),
  Fn { airity: usize, ctx: Context },
}
