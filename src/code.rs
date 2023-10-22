use crate::{exec::FileInfo, prelude::*};
use ast::Ast;
use gen::BytecodeGenerator;
use inter_struct::prelude::*;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  collections::{btree_map::Iter, BTreeMap},
  convert::TryFrom,
  env,
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

pub mod prelude {
  pub use super::{Context, Env};
}

#[derive(Default)]
pub struct Compiler;

impl Compiler {
  pub fn compile(file: PathBuf, source: &str, env: SmartPtr<Env>) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
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
    let ctx = SmartPtr::new(Context::new(Some("*main*"), env, reflection));

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
  Class(ClassConstant),
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

#[derive(Debug)]
pub struct ClassConstant {
  pub name: Option<String>,
  pub initializer: Option<FunctionConstant>,
  pub methods: BTreeMap<String, FunctionConstant>,
  pub statics: BTreeMap<String, FunctionConstant>,
}

impl ClassConstant {
  fn new(name: Option<String>) -> Self {
    Self {
      name,
      initializer: None,
      methods: Default::default(),
      statics: Default::default(),
    }
  }

  fn set_constructor(&mut self, c: FunctionConstant) {
    self.initializer = Some(c);
  }

  fn set_static(&mut self, key: String, value: FunctionConstant) {
    self.statics.insert(key, value);
  }

  fn set_method(&mut self, key: String, value: FunctionConstant) {
    self.methods.insert(key, value);
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
      Self::Class(v) => write!(
        f,
        "{}",
        if let Some(name) = &v.name {
          name.as_str()
        } else {
          "<unnamed class>"
        }
      ),
    }
  }
}

pub struct Context {
  pub name: Option<String>,
  pub id: usize, // the function id within the local file

  global: SmartPtr<Context>,

  instructions: Vec<Opcode>,

  consts: Vec<ConstantValue>,
  // map of string to const vec location to save memory
  strings: BTreeMap<String, usize>,

  pub env: SmartPtr<Env>,
  pub meta: Reflection,
}

impl Context {
  pub(crate) fn new(name: Option<impl Into<String>>, env: SmartPtr<Env>, reflection: Reflection) -> Self {
    Self {
      name: name.map(|n| n.into()),
      id: Default::default(),
      global: Default::default(),
      instructions: Default::default(),
      consts: Default::default(),
      strings: Default::default(),
      env,
      meta: reflection,
    }
  }

  pub fn trace_all(&self, marks: &mut Marker) {
    if self.global.valid() {
      self.global.trace(marks);
    }
    self.env.trace(marks);
  }

  fn new_child(name: Option<String>, id: usize, ctx: SmartPtr<Context>, reflection: Reflection) -> Self {
    let env = ctx.env.clone();
    let global = if ctx.global.valid() {
      ctx.global.clone()
    } else {
      // is global ctx
      ctx
    };

    Self {
      name,
      id,
      global,
      consts: Default::default(),
      strings: Default::default(),
      instructions: Default::default(),
      env,
      meta: reflection,
    }
  }

  pub fn global_ctx(&self) -> &Context {
    if self.global.valid() {
      &self.global
    } else {
      self
    }
  }

  pub fn global_ctx_mut(&mut self) -> &mut Context {
    if self.global.valid() {
      &mut self.global
    } else {
      self
    }
  }

  pub fn next(&self, index: usize) -> Option<Opcode> {
    self.instructions.get(index).cloned()
  }

  pub fn const_at(&self, index: usize) -> Option<&ConstantValue> {
    self.consts.get(index)
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub fn global_const_at(&self, index: usize) -> Option<&ConstantValue> {
    self.global_ctx().consts.get(index)
  }

  fn write(&mut self, op: Opcode, line: usize, column: usize) {
    #[cfg(test)]
    {
      println!("emitting {:?}", op);
    }
    self.instructions.push(op);
    self.meta.add(line, column);
  }

  fn write_const(&mut self, c: ConstantValue, line: usize, column: usize) {
    let c = self.add_const(c);
    self.write(Opcode::Const(c), line, column);
  }

  fn add_const(&mut self, c: ConstantValue) -> usize {
    let string = if let ConstantValue::String(string) = &c {
      if let Some(index) = self.strings.get(string.as_str()) {
        return *index;
      }
      Some(string.clone())
    } else {
      None
    };

    let index = self.consts.len();
    self.consts.push(c);

    if let Some(string) = string {
      self.strings.insert(string, index);
    }

    index
  }

  pub fn num_instructions(&self) -> usize {
    self.instructions.len()
  }

  fn replace_instruction(&mut self, index: usize, op: Opcode) -> bool {
    if let Some(inst) = self.instructions.get_mut(index) {
      *inst = op;
      true
    } else {
      false
    }
  }

  #[cfg(debug_assertions)]
  pub fn disassemble(&self) {
    self.display_opcodes();

    for value in self.consts() {
      match value {
        ConstantValue::Fn(f) => {
          f.ctx.disassemble();
        }
        ConstantValue::Class(c) => {
          if let Some(i) = &c.initializer {
            i.ctx.disassemble();
          }

          for value in c.methods.values() {
            value.ctx.disassemble();
          }

          for value in c.statics.values() {
            value.ctx.disassemble();
          }
        }
        _ => (),
      }
    }
  }

  pub fn display_opcodes(&self) {
    let default = self.id.to_string();
    let name = self.name.as_ref().unwrap_or(&default);
    println!(">>>>>> {} <<<<<<", name);

    for (i, op) in self.instructions.iter().enumerate() {
      self.display_instruction(op, i);
    }

    println!("======={}=======", "=".repeat(name.len()));
  }

  pub fn display_instruction(&self, op: &Opcode, offset: usize) {
    print!("{} ", Self::address_of(offset));
    if let Some(curr) = self.meta.get(offset) {
      if offset > 0 {
        if let Some(prev) = self.meta.get(offset - 1) {
          if curr.line == prev.line {
            print!("   | ");
          } else {
            print!("{:#04} ", curr.line);
          }
        } else {
          print!("?????");
        }
      } else {
        print!("{:#04} ", curr.line);
      }
    } else {
      print!("?????");
    }

    match op {
      Opcode::Const(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("Const"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      Opcode::PopN(count) => println!("{} {}", Self::opcode_column("PopN"), Self::value_column(*count)),
      Opcode::LookupGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("LookupGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      Opcode::ForceAssignGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("ForceAssignGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name)
      ),
      Opcode::DefineGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("DefineGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      Opcode::AssignGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("AssignGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      Opcode::LookupLocal(index) => {
        println!("{} {}", Self::opcode_column("LookupLocal"), Self::value_column(*index))
      }
      Opcode::AssignLocal(index) => {
        println!("{} {}", Self::opcode_column("AssignLocal"), Self::value_column(*index))
      }
      Opcode::AssignMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      Opcode::LookupMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      Opcode::Jump(count) => println!("{} {: >14}", Self::opcode_column("Jump"), Self::address_of(offset + count)),
      Opcode::JumpIfFalse(count) => {
        println!(
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + count)
        )
      }
      Opcode::Loop(count) => println!("{} {: >14}", Self::opcode_column("Loop"), Self::address_of(offset - count)),
      Opcode::Or(count) => println!("{} {: >14}", Self::opcode_column("Or"), Self::address_of(offset + count)),
      Opcode::And(count) => println!("{} {: >14}", Self::opcode_column("And"), Self::address_of(offset + count)),
      Opcode::Call(count) => println!("{} {}", Self::opcode_column("Call"), Self::value_column(*count)),
      Opcode::CreateList(count) => println!("{} {}", Self::opcode_column("CreateList"), Self::value_column(*count)),
      x => println!("{}", Self::opcode_column(format!("{:?}", x))),
    }
  }

  fn opcode_column<O: ToString>(opcode: O) -> String {
    format!("{:<20}", opcode.to_string())
  }

  fn value_column(value: usize) -> String {
    format!("{: >4}", value)
  }

  fn global_const_at_column(&self, index: usize) -> String {
    format!(
      "{: >4?}",
      self
        .global_const_at(index)
        .unwrap_or(&ConstantValue::String("????".to_string()))
    )
  }

  fn const_at_column(&self, index: usize) -> String {
    let cval = &ConstantValue::StaticString("????");
    let value = self.const_at(index).unwrap_or(cval);
    format!("{value: >4?}")
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}

#[derive(Default)]
pub struct StackFrame {
  pub ip: usize,
  pub ctx: SmartPtr<Context>,
  pub stack: Vec<Value>,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ip: Default::default(),
      ctx,
      stack: Default::default(),
    }
  }

  /**
   * Clear the current stack frame, returning the previous
   */
  pub fn clear_out(&mut self) -> Self {
    let mut old = Self::default();
    std::mem::swap(&mut old, self);
    old
  }
}

pub struct Yield {
  pub current_frame: StackFrame,
  pub stack_frames: Vec<StackFrame>,
  pub(crate) opened_files: Vec<FileInfo>,
}

impl Yield {
  pub(crate) fn new(current_frame: StackFrame, stack_frames: Vec<StackFrame>, opened_files: Vec<FileInfo>) -> Self {
    Self {
      current_frame,
      stack_frames,
      opened_files,
    }
  }
}

impl Display for Yield {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "yield")
  }
}

pub struct Env {
  vars: BTreeMap<String, Value>,
  libs: Library,
  args: Vec<String>,
}

impl Env {
  pub fn initialize(gc: &mut Gc, args: &[String], library: Library) -> Self {
    let mut env = Env {
      vars: Default::default(),
      libs: library,
      args: args.into(),
    };

    env.vars = stdlib::load_libs(gc, &env.args, &env.libs);

    let mut lib_paths = Vec::default();

    if let Ok(paths) = env::var("SIMPLE_LIBRARY_PATHS") {
      lib_paths.extend(paths.split_terminator(';').map(|v| gc.allocate(v)));
    }

    let module = LockedModule::initialize(gc, |gc, module| {
      let lib_paths = gc.allocate(lib_paths);

      module.set(gc, "path", lib_paths).ok();
    });

    env.assign("$LIBRARY", module);

    env
  }

  /// Defines a new variable. Returns true if the variable is new, false otherwise
  pub fn define<T: ToString>(&mut self, name: T, value: impl Into<Value>) -> bool {
    self.vars.insert(name.to_string(), value.into()).is_none()
  }

  pub fn is_available(&self, name: &str) -> bool {
    !self.is_defined(name)
  }

  pub fn is_defined(&self, name: &str) -> bool {
    self.vars.contains_key(name)
  }

  pub fn assign<T: ToString>(&mut self, name: T, value: Value) -> bool {
    self.vars.insert(name.to_string(), value).is_some()
  }

  pub fn lookup<T: AsRef<str>>(&self, name: T) -> Option<Value> {
    self.vars.get(name.as_ref()).cloned()
  }

  pub fn iter(&self) -> Iter<'_, String, Value> {
    self.vars.iter()
  }

  pub fn trace(&self, marks: &mut Marker) {
    for value in self.vars.values() {
      marks.trace(value);
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

  fn add(&mut self, line: usize, column: usize) {
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
