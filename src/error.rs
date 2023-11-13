use crate::{
  code::{FileMap, InstructionReflection},
  exec::prelude::Instruction,
  util::FileIdType,
  value::Value,
};
use std::{
  convert::Infallible,
  fmt::{self, Debug, Display},
  io,
  path::PathBuf,
  sync::mpsc,
};

pub type UsageResult<T = Value> = Result<T, UsageError>;

pub enum Error {
  CompiletimeErrors(CompiletimeErrors),
  RuntimeError(RuntimeError),
  SystemError(SystemError),
}

impl Error {
  pub(crate) fn runtime_error(e: RuntimeError) -> Self {
    Self::RuntimeError(e)
  }

  pub fn other_system_err(e: impl ToString) -> Self {
    Self::SystemError(SystemError::other(e))
  }

  fn into_runtime(self, filemap: &FileMap) -> RuntimeError {
    match self {
      Error::CompiletimeErrors(errs) => RuntimeError {
        msg: itertools::join(
          errs.into_iter().map(|e| {
            let e = e.with_filename(filemap);
            format!(
              "{file} ({line}, {column}): {msg}",
              file = e.file_display.unwrap_or_default(),
              line = e.line,
              column = e.column,
              msg = e.msg
            )
          }),
          "\n",
        ),
        file: filemap.get(None),
        line: 0,
        column: 0,
        nested: true,
      },
      Error::RuntimeError(err) => err,
      Error::SystemError(err) => RuntimeError {
        msg: err.to_string(),
        file: filemap.get(None),
        line: 0,
        column: 0,
        nested: true,
      },
    }
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::CompiletimeErrors(errs) => {
        let output = itertools::join(errs.0.iter(), "\n");
        write!(f, "{output}")
      }
      Error::RuntimeError(e) => write!(f, "{e}"),
      Error::SystemError(e) => write!(f, "{e}"),
    }
  }
}

impl Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

/// A list of errors produced at compiletime
#[derive(Default, Debug)]
pub struct CompiletimeErrors(Vec<CompiletimeError>);

impl CompiletimeErrors {
  pub fn add(&mut self, err: CompiletimeError) {
    self.0.push(err)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  pub fn with_filename(self, filemap: &FileMap) -> Self {
    Self(self.0.into_iter().map(|e| e.with_filename(filemap)).collect())
  }
}

impl IntoIterator for CompiletimeErrors {
  type Item = CompiletimeError;
  type IntoIter = std::vec::IntoIter<Self::Item>;
  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

#[derive(Default, PartialEq, Eq)]
pub enum FileDisplay {
  #[default]
  None,
  Id(FileIdType),
  Path(PathBuf),
}

impl Display for FileDisplay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      FileDisplay::None => write!(f, "<anonymous>"),
      FileDisplay::Id(id) => write!(f, "{id}"),
      FileDisplay::Path(path) => write!(f, "{}", path.display()),
    }
  }
}

impl Debug for FileDisplay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

#[derive(PartialEq, Eq)]
pub struct CompiletimeError {
  pub msg: String,
  pub file_display: Option<FileDisplay>,
  pub line: usize,
  pub column: usize,
}

impl CompiletimeError {
  fn with_filename(self, filemap: &FileMap) -> Self {
    Self {
      msg: self.msg,
      file_display: if let Some(FileDisplay::Id(id)) = self.file_display {
        Some(FileDisplay::Path(filemap.get(Some(id))))
      } else {
        self.file_display
      },
      line: self.line,
      column: self.column,
    }
  }
}

impl Display for CompiletimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{file} ({line}, {column}): {msg}",
      file = self.file_display.as_ref().unwrap_or(&FileDisplay::None),
      line = self.line,
      column = self.column,
      msg = self.msg
    )
  }
}

impl Debug for CompiletimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

/// Error at runtime, only one of these should exist when erroring out
#[derive(Debug)]
pub struct RuntimeError {
  pub msg: String,
  pub file: PathBuf,
  pub line: usize,
  pub column: usize,
  pub nested: bool,
}

impl RuntimeError {
  pub fn new(err: UsageError, filemap: &FileMap, opcode_ref: InstructionReflection) -> Self {
    match err {
      UsageError::Preformated(err) => err.into_runtime(filemap),
      err => Self {
        msg: Self::format_src(&opcode_ref, err),
        file: filemap.get(opcode_ref.file_id),
        line: opcode_ref.line,
        column: opcode_ref.column,
        nested: false,
      },
    }
  }

  pub fn format_src(opcode_ref: &InstructionReflection, msg: impl ToString) -> String {
    opcode_ref
      .source
      .lines()
      .nth(opcode_ref.line - 1)
      .map(|line| {
        format!(
          "{msg}\n{src_line}\n{space}^\nOpcode: {opcode:?}",
          msg = msg.to_string(),
          src_line = line,
          space = " ".repeat(opcode_ref.column - 1),
          opcode = opcode_ref.inst.opcode().unwrap_or_default()
        )
      })
      .unwrap_or_else(|| format!("invalid line at {}", opcode_ref.line))
  }
}

impl Display for RuntimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.nested {
      write!(f, "{msg}", msg = self.msg)
    } else {
      write!(
        f,
        "{file} ({line}, {column}): {msg}",
        file = self.file.display(),
        line = self.line,
        column = self.column,
        msg = self.msg
      )
    }
  }
}

#[derive(Debug, thiserror::Error)]
pub enum UsageError {
  /// fn name
  #[error("MissingSelf: missing self in call to {0}")]
  MissingSelf(&'static str),

  /// fn name, argument index
  #[error("InvalidArgument: wrong type passed to {0} in argument position {1}")]
  InvalidArgument(&'static str, usize),

  /// fn name, type, actual/this
  #[error("BadCast: wrong cast in {0} (casting from {2} to {1})")]
  BadCast(&'static str, &'static str, Value),

  /// given, expected
  #[error("ArgumentError: wrong number of arguments (given {0} expected {1})")]
  ArgumentError(usize, usize),

  /// tried, needed
  #[error("CoercionError: cannot coerce {0} to {1}")]
  CoercionError(Value, &'static str),

  /// name
  #[error("Undefined variable {0}")]
  UndefinedVar(String),

  /// op, lhs, rhs
  #[error("Tried to perform '{0}' with {1} and {2}")]
  InvalidOperation(char, Value, Value),

  /// method, value
  #[error("{0} not implemented for {1}")]
  UnimplementedError(&'static str, Value),

  #[error("{0} is undefined for {0}")]
  UndefinedMethod(&'static str, String),

  /// index, value
  #[error("Index {0} out of bounds in {1}")]
  InvalidIndex(i32, Value),

  /// member name
  #[error("Tried assigning a value to unimplemented member {0}")]
  InvalidAssignment(String),

  /// value
  #[error("Tried to lookup a member on a primitive '{0}'")]
  InvalidLookup(Value),

  /// ident
  #[error("Cannot modify immutable object '{0}'")]
  Immutable(String),

  /// ident
  #[error("Tried to access undefined member '{0}'")]
  UndefinedMember(String),

  // idx
  #[error("Tried to access undefined member id '{0}'")]
  UndefinedMemberId(usize),

  /// actual, expected
  #[error("{0} is not a {1}")]
  TypeError(String, String),

  /// ident
  #[error("use of undefined variable {0}")]
  NameError(String),

  /// Native function produced an error
  /// fn name
  #[error("error in native function: {0}")]
  NativeApi(String),

  /// Default return value for usertype initializers
  #[error("Undefined initializer reached")]
  UndefinedInitializer,

  #[error("unable to find file, tried: {0:#?}")]
  BadReq(Vec<PathBuf>),

  #[error("unexpected nil value")]
  UnexpectedNil,

  #[error("Expected field name but found none")]
  EmptyField,

  #[error("quack {0}")]
  Quack(Value),

  /* Below can only be reached from bad bytecode generation */
  #[error("Empty stack")]
  EmptyStack,

  #[error("Invalid unary operation")]
  InvalidUnary,

  #[error("Invalid binary operation")]
  InvalidBinary,

  #[error("Invalid identifier {0}")]
  InvalidIdentifier(String),

  #[error("Constant not found at index {0}")]
  InvalidConst(usize),

  #[error("Stack entry not found at index {0}")]
  InvalidStackIndex(usize),

  #[error("could not fetch info for instruction {0:04X}")]
  IpOutOfBounds(usize),

  #[error("tried redefining a global variable at {level:?}: {name}")]
  Redefine { level: Option<usize>, name: String },

  #[error("methods can only be defined on classes")]
  MethodAssignment,

  #[error("methods can only functions")]
  MethodType,

  #[error("closures can only functions")]
  ClosureType,

  #[error("capture list must be a vec")]
  CaptureType,

  #[error("Invalid instruction: {0}")]
  InvalidInstruction(Instruction),

  #[error("Used a register in an invalid context")]
  InvalidRegisterUsage,

  #[error("{0}")]
  Preformated(Error),

  #[error("Infallible")]
  Infallible,
}

impl UsageError {
  pub fn native(e: impl ToString) -> Self {
    Self::NativeApi(e.to_string())
  }
}

#[derive(Debug, thiserror::Error)]
pub enum SystemError {
  #[error("gc failure: {0}")]
  GcError(String),

  #[error("io failure: {0}")]
  IoError(std::io::Error),

  #[error("other: {0}")]
  Other(String),
}

impl SystemError {
  pub(crate) fn other(e: impl ToString) -> Self {
    Self::Other(e.to_string())
  }
}

// TODO fill out this error

impl From<CompiletimeErrors> for Error {
  fn from(value: CompiletimeErrors) -> Self {
    Self::CompiletimeErrors(value)
  }
}

impl From<RuntimeError> for Error {
  fn from(value: RuntimeError) -> Self {
    Self::RuntimeError(value)
  }
}

impl From<SystemError> for Error {
  fn from(value: SystemError) -> Self {
    Self::SystemError(value)
  }
}

impl From<Infallible> for UsageError {
  fn from(_: Infallible) -> Self {
    Self::Infallible
  }
}

impl<T> From<mpsc::SendError<T>> for SystemError {
  fn from(value: mpsc::SendError<T>) -> Self {
    Self::GcError(value.to_string())
  }
}

impl From<io::Error> for SystemError {
  fn from(value: io::Error) -> Self {
    Self::IoError(value)
  }
}
