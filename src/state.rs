use crate::{Vm, value::Tag};
use ahash::HashMap;
use bimap::BiHashMap;
use derive_more::Deref;
use std::path::PathBuf;

const VERSION: u64 = 0;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct State {
  header: Header,
  value_table: ValueTable,
  stack_data: StackData,
}

impl From<&Vm> for State {
  fn from(vm: &Vm) -> Self {
    let header = Header::new(vm);
    let (value_table, value_map) = ValueTable::new(vm);
    let stack_data = StackData::new(vm, &value_map);

    Self {
      header,
      value_table,
      stack_data,
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Header {
  version: u64,
  /// List of libraries used in this state and in the order they were loaded in
  dependencies: Vec<PathBuf>,
}

impl Header {
  fn new(vm: &Vm) -> Self {
    Self {
      version: VERSION,
      dependencies: vm.dependency_list(),
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ValueTable {
  /// This contains all the variables in use, their raw representation is the key to ensure uniqueness
  values: Vec<Value>,

  ///
  consts: Vec<ConstantValue>,

  /// Mapping of global variables to their index into the value list
  globals: HashMap<String, usize>,
}

impl ValueTable {
  fn new(vm: &Vm) -> (Self, HashMap<u64, usize>) {
    // TODO this needs to still do two things
    // 1. recurse to child values, maybe leverage the trace functionality since that would have to do the same for gc purposes anyway
    // 2. condense duplicates, as of now the vec builds regardless of how many times a particular value appears
    let (values, value_map): (Vec<_>, HashMap<_, _>) = vm
      .active_values()
      .enumerate()
      .map(|(i, v)| (Value::from_vm(vm, v), (v.bits(), i)))
      .unzip();

    let consts = vm.consts().into_iter().map(|c| ConstantValue::from_vm(vm, c)).collect();

    let globals = vm
      .globals()
      .filter_map(|(cv, v)| {
        if let crate::code::ConstantValue::String(s) = cv {
          Some((s.clone(), value_map[&v.bits()]))
        } else {
          None
        }
      })
      .collect();

    (Self { values, consts, globals }, value_map)
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StackData {
  /// This is the stack, but the values are stored in the value table, this is just a list of their indexes to pull from when reconstructing
  stack: Vec<usize>,

  /// This is the list of active stack frames
  frames: Vec<StackFrame>,
}

impl StackData {
  fn new(vm: &Vm, value_map: &HashMap<u64, usize>) -> Self {
    let stack = vm.stack.iter().map(|v| value_map[&v.bits()]).collect();
    let frames = vm.call_stack.iter().map(|sf| StackFrame::new(vm, sf, value_map)).collect();

    Self { stack, frames }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Value {
  Float(f64),
  Integer(i32),
  Bool(bool),
  Char(char),
  Native(),
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

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ConstantValue {
  Integer(i32),
  Float(f64),
  String(String),
  StaticString(String),
  Fn { airity: usize, chunk: Chunk },
}

impl ConstantValue {
  fn from_vm(vm: &Vm, c: &crate::code::ConstantValue) -> Self {
    match c {
      crate::code::ConstantValue::Integer(i) => Self::Integer(*i),
      crate::code::ConstantValue::Float(f) => Self::Float(*f),
      crate::code::ConstantValue::String(s) => Self::String(s.clone()),
      crate::code::ConstantValue::StaticString(s) => Self::StaticString(String::from(*s)),
      crate::code::ConstantValue::Fn(fc) => Self::Fn {
        airity: fc.airity,
        chunk: Chunk::new(vm, &fc.ctx),
      },
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Chunk {
  instructions: Vec<Instruction>,
  metadata: InstructionMetadata,
}

impl Chunk {
  fn new(vm: &Vm, ctx: &crate::exec::Context) -> Self {
    Self {
      instructions: ctx.instructions.iter().map(|i| Instruction(**i)).collect(),
      metadata: InstructionMetadata::new(vm, &ctx.meta),
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Instruction(u64);

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InstructionMetadata {
  pub name: Option<String>,
  pub file: Option<PathBuf>,
  pub source: String,
  pub instruction_locs: Vec<InstructionLocation>,
}

impl InstructionMetadata {
  fn new(vm: &Vm, meta: &crate::code::InstructionMetadata) -> Self {
    Self {
      name: meta.name.clone(),
      file: vm.filemap.get(meta.file_id).cloned(),
      source: (**meta.source).clone(),
      instruction_locs: meta.opcode_info.iter().map(|i| InstructionLocation::from(i)).collect(),
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InstructionLocation {
  pub line: usize,
  pub column: usize,
}

impl From<&crate::code::SourceLocation> for InstructionLocation {
  fn from(value: &crate::code::SourceLocation) -> Self {
    Self {
      line: value.line,
      column: value.column,
    }
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StackFrame {
  ip: usize,

  bp: usize,

  chunk: Chunk,

  export: Option<usize>,

  is_req: bool,

  module_index: usize,
}

impl StackFrame {
  fn new(vm: &Vm, sf: &crate::exec::StackFrame, value_map: &HashMap<u64, usize>) -> Self {
    Self {
      ip: sf.ip(),
      bp: sf.bp,
      chunk: Chunk::new(vm, &sf.ctx),
      export: sf.export.map(|v| value_map[&v.bits()]),
      is_req: sf.is_req,
      module_index: sf.module_index,
    }
  }
}
