use crate::{dbg, prelude::*};
use ast::Ast;
use gen::BytecodeGenerator;
use inter_struct::prelude::*;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  convert::TryFrom,
  error::Error,
  fmt::{self, Debug, Display, Formatter, Result as FmtResult},
  path::PathBuf,
  rc::Rc,
  str,
};

pub mod ast;
pub mod gen;
pub mod lex;
pub mod opt;

#[derive(Default)]
pub struct Compiler;

impl Compiler {
  pub fn compile(file: PathBuf, source: &str) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    dbg::profile_function!();

    let file = Rc::new(file);
    let mut scanner = Scanner::new(Rc::clone(&file), source);

    let (tokens, meta) = scanner.scan().map_err(|errs| Self::reformat_errors(source, errs))?;

    let (ast, errors) = Ast::from(tokens, meta);

    if !errors.is_empty() {
      #[cfg(feature = "visit-ast")]
      {
        ast.dump(&file);
      }

      return Err(Self::reformat_errors(source, errors));
    }

    let optimizer = Optimizer::<1>::new(ast);

    let ast = optimizer.optimize();

    let source = Rc::new(source.to_string());
    let reflection = Reflection::new(file, Rc::clone(&source));
    let ctx = SmartPtr::new(Context::new(Some("*main*"), reflection));

    let generator = BytecodeGenerator::new(ctx);

    generator.generate(ast).map_err(|errs| Self::reformat_errors(&source, errs))
  }

  fn reformat_errors(source: &str, errs: Vec<RuntimeError>) -> Vec<RuntimeError> {
    errs
      .into_iter()
      .map(|mut e| {
        if let Some(src) = source.lines().nth(e.line - 1) {
          e.format_with_src_line(src);
        }
        e
      })
      .collect()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
  pub file: Rc<PathBuf>,
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpCodeInfo {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpCodeReflection<'src> {
  pub file: Rc<PathBuf>,
  pub source_line: &'src str,
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

#[derive(Clone, StructMerge)]
#[struct_merge("crate::value::builtin_types::class_value::ClassValue")]
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
    self.ctx.name.as_ref().map(|n| n.as_ref()).unwrap_or("<lambda>")
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
  pub file: Rc<PathBuf>,
  pub source: Rc<String>,
  pub opcode_info: Vec<OpCodeInfo>,
}

impl Reflection {
  pub(crate) fn new(file: Rc<PathBuf>, source: Rc<String>) -> Self {
    Reflection {
      file,
      source,
      opcode_info: Default::default(),
    }
  }

  pub(crate) fn add(&mut self, line: usize, column: usize) {
    self.opcode_info.push(OpCodeInfo { line, column });
  }

  pub fn get(&self, offset: usize) -> Option<OpCodeReflection<'_>> {
    if let Some(info) = self.opcode_info.get(offset).cloned() {
      self.source.lines().nth(info.line - 1).map(|src| OpCodeReflection {
        file: Rc::clone(&self.file),
        source_line: src,
        line: info.line,
        column: info.column,
      })
    } else {
      None
    }
  }
}
