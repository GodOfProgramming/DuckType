use crate::{
  error::{CompiletimeErrors, Error},
  prelude::*,
  util::{FileIdType, FileMetadata, PlatformMetadata},
};
use ast::Ast;
use gen::BytecodeGenerator;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  convert::TryFrom,
  fmt::{self, Debug, Display, Formatter, Result as FmtResult},
  path::PathBuf,
  rc::Rc,
  str,
};

pub mod ast;
pub mod gen;
pub mod lex;
pub mod opt;

pub fn compile_file(file: impl Into<PathBuf>, source: impl AsRef<str>) -> Result<SmartPtr<Context>, Error> {
  let file = file.into();
  let file_id = PlatformMetadata::id_of(&file).map_err(Error::other_system_err)?;
  let ctx = compile(Some(file_id), source)?;
  Ok(ctx)
}

pub fn compile_string(source: impl AsRef<str>) -> Result<SmartPtr<Context>, CompiletimeErrors> {
  compile(None, source)
}

pub(crate) fn compile(file_id: Option<FileIdType>, source: impl AsRef<str>) -> Result<SmartPtr<Context>, CompiletimeErrors> {
  let scanner = Scanner::new(file_id, source.as_ref());

  let (tokens, token_locations) = scanner.into_tokens()?;

  let ast = Ast::try_from(file_id, tokens, token_locations)?;

  let optimizer = Optimizer::<1>::new(ast);

  let ast = optimizer.optimize();

  let source = Rc::new(source.as_ref().to_string());
  let reflection = Reflection::new(Some("<main>"), file_id, Rc::clone(&source));
  let ctx = SmartPtr::new(Context::new(reflection));

  let generator = BytecodeGenerator::new(file_id, ctx);

  generator.generate(ast)
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug)]
pub enum ConstantValue {
  Nil,
  Bool(bool),
  Integer(i32),
  Float(f64),
  String(String),
  StaticString(&'static str),
  Fn(FunctionConstant),
}

#[derive(Clone)]
pub struct FunctionConstant {
  pub airity: usize,
  pub locals: usize,
  pub ctx: SmartPtr<Context>,
}

impl FunctionConstant {
  pub fn new(airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self { airity, locals, ctx }
  }

  fn name(&self) -> &str {
    self.ctx.meta.name.as_ref().map(|n| n.as_str()).unwrap_or("<lambda>")
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
      Self::Nil => write!(f, "nil"),
      Self::Bool(v) => write!(f, "{}", v),
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

  pub fn reflect(&self, opcode: Opcode, offset: usize) -> Option<OpcodeReflection<'_>> {
    self.info(offset).map(|info| OpcodeReflection {
      opcode,
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
    id.map(|id| self.map.get(&id).cloned())
      .flatten()
      .unwrap_or_else(|| PathBuf::from("<string input>"))
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpcodeInfo {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpcodeReflection<'src> {
  pub opcode: Opcode,
  pub file_id: Option<FileIdType>,
  pub source: &'src str,
  pub line: usize,
  pub column: usize,
}
