use crate::prelude::*;
use common::errors::Error;
use common::util::FileIdType;
use gen::BytecodeGenerator;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  path::PathBuf,
  rc::Rc,
  str,
};

pub mod gen;

pub(crate) struct CompileOpts {
  pub(crate) optimize: bool,
}

pub(crate) fn compile_file(
  cache: &mut Cache,
  file_id: FileIdType,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  compile(cache, Some(file_id), source, opts)
}

pub(crate) fn compile_string(
  cache: &mut Cache,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  compile(cache, None, source, opts)
}

pub(crate) fn compile(
  cache: &mut Cache,
  file_id: Option<FileIdType>,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  let (tokens, token_locations) = lex::tokenize(file_id, source.as_ref())?;

  let mut ast = ast::generate(file_id, tokens, token_locations)?;

  if opts.optimize {
    ast = ast::optimize(ast);
  }

  let source = Rc::new(source.as_ref().to_string());
  let reflection = Reflection::new(Some("<main>"), file_id, Rc::clone(&source));
  let ctx = SmartPtr::new(Context::new(0, reflection));

  BytecodeGenerator::new(cache, file_id, ctx).generate(ast)
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
  Integer(i32),
  Float(f64),
  String(String),
  StaticString(&'static str),
  Fn(FunctionConstant),
}

impl From<i32> for ConstantValue {
  fn from(value: i32) -> Self {
    Self::Integer(value)
  }
}

impl From<f64> for ConstantValue {
  fn from(value: f64) -> Self {
    Self::Float(value)
  }
}

impl From<String> for ConstantValue {
  fn from(value: String) -> Self {
    Self::String(value)
  }
}

impl From<&String> for ConstantValue {
  fn from(value: &String) -> Self {
    Self::String(value.clone())
  }
}

impl From<&'static str> for ConstantValue {
  fn from(value: &'static str) -> Self {
    Self::StaticString(value)
  }
}

impl From<FunctionConstant> for ConstantValue {
  fn from(value: FunctionConstant) -> Self {
    Self::Fn(value)
  }
}

#[derive(Clone)]
pub struct FunctionConstant {
  pub airity: usize,
  pub ctx: SmartPtr<Context>,
}

impl FunctionConstant {
  pub fn new(airity: usize, ctx: SmartPtr<Context>) -> Self {
    Self { airity, ctx }
  }

  fn name(&self) -> &str {
    self.ctx.meta.name.as_deref().unwrap_or("<lambda>")
  }
}

impl Debug for FunctionConstant {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}

impl Display for ConstantValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      Self::Integer(v) => write!(f, "{}", v),
      Self::Float(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "{}", v),
      Self::StaticString(v) => write!(f, "{}", v),
      Self::Fn(v) => write!(f, "{}", v.name()),
    }
  }
}

#[derive(Debug)]
pub struct Reflection {
  pub name: Option<String>,
  pub file_id: Option<FileIdType>,
  pub source: Rc<String>,
  pub opcode_info: Vec<OpcodeInfo>,
}

impl Reflection {
  pub(crate) fn new(name: Option<impl ToString>, file_id: Option<FileIdType>, source: Rc<String>) -> Self {
    Reflection {
      name: name.map(|n| n.to_string()),
      file_id,
      source,
      opcode_info: Default::default(),
    }
  }

  pub(crate) fn add(&mut self, line: usize, column: usize) {
    self.opcode_info.push(OpcodeInfo { line, column });
  }

  pub fn info(&self, offset: usize) -> Option<OpcodeInfo> {
    self.opcode_info.get(offset).cloned()
  }

  pub fn reflect(&self, inst: Instruction, offset: usize) -> Option<InstructionReflection<'_>> {
    self.info(offset).map(|info| InstructionReflection {
      inst,
      file_id: self.file_id,
      source: &self.source,
      line: info.line,
      column: info.column,
    })
  }
}

#[derive(Default)]
pub struct FileMap {
  map: BTreeMap<FileIdType, PathBuf>,
}

impl FileMap {
  pub(crate) fn add(&mut self, id: FileIdType, path: impl Into<PathBuf>) {
    self.map.insert(id, path.into());
  }

  pub(crate) fn get(&self, id: Option<FileIdType>) -> PathBuf {
    id.and_then(|id| self.map.get(&id).cloned())
      .unwrap_or_else(|| PathBuf::from("<anonymous>"))
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpcodeInfo {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionReflection<'src> {
  pub inst: Instruction,
  pub file_id: Option<FileIdType>,
  pub source: &'src str,
  pub line: usize,
  pub column: usize,
}
