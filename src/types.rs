use crate::{
  code::{Context, Env, OpCode, OpCodeReflection},
  ExecutionThread, New,
};
use ptr::SmartPtr;
use std::{
  cmp::{Ordering, PartialEq, PartialOrd},
  collections::BTreeMap,
  fmt::{self, Debug, Display, Formatter},
  ops::{Add, Deref, Div, Index, IndexMut, Mul, Neg, Not, RangeBounds, Rem, Sub},
  slice::Iter,
};

#[derive(Default, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl Error {
  pub fn from_ref(msg: String, opcode: &OpCode, opcode_ref: OpCodeReflection) -> Self {
    let mut e = Self {
      msg,
      file: opcode_ref.file.clone(),
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    e.format_with_src_line(opcode_ref.source_line);
    e.msg = format!("{}\nOffending OpCode: {:?}", e.msg, opcode);
    e
  }

  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!("{}\n{}\n{}^", self.msg, src, " ".repeat(self.column - 1));
  }
}

impl Debug for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Num(f64),
  String(String),
  List(SmartPtr<List>),
  Function(SmartPtr<Function>),
  Closure(SmartPtr<Closure>),
  NativeFunction(SmartPtr<NativeFn>),
  Struct(SmartPtr<Struct>),
  Method(SmartPtr<Method>),
  Class(SmartPtr<Class>),
  Instance(SmartPtr<Instance>),

  U128(u128), // internal use only
}

impl Value {
  pub fn truthy(&self) -> bool {
    if *self == Value::Nil {
      return false;
    }

    if let Value::Bool(b) = self {
      return *b;
    }

    true
  }

  pub fn len(&self) -> Option<usize> {
    if let Self::List(list) = self {
      Some(list.len())
    } else {
      None
    }
  }

  pub fn is_empty(&self) -> Option<bool> {
    if let Self::List(list) = self {
      Some(list.is_empty())
    } else {
      None
    }
  }

  pub fn orig_index(&'_ self, idx: usize) -> Option<&'_ Self> {
    if let Self::List(list) = self {
      Some(&list[idx])
    } else {
      None
    }
  }

  pub fn orig_index_mut(&'_ mut self, idx: usize) -> Option<&'_ mut Self> {
    if let Self::List(list) = self {
      Some(&mut list[idx])
    } else {
      None
    }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Vec<Value>,
  ) -> Result<Value, Vec<String>> {
    match self {
      Value::Function(f) => f.call(thread, env, args),
      Value::Closure(c) => c.call(thread, env, args),
      Value::NativeFunction(f) => f.call(env, args).map_err(|e| vec![e]),
      Value::Method(m) => m.call(thread, env, args),
      Value::Class(c) => Class::construct(c.clone(), thread, env, args),
      _ => return Err(vec![format!("unable to call non callable '{}'", self)]),
    }
  }

  pub fn index(&self, index: Value) -> ValueOpResult {
    match self {
      Value::List(values) => match index {
        Value::Num(n) => {
          if n == n as usize as f64 {
            if let Some(v) = values.get(n as usize) {
              Ok(v.clone())
            } else {
              Ok(Value::Nil)
            }
          } else {
            Ok(Value::Nil)
          }
        }
        _ => Err(format!("cannot index list with {}", index)),
      },
      Value::String(string) => match index {
        Value::Num(n) => {
          if n == n as usize as f64 {
            if let Some(c) = string.chars().nth(n as usize) {
              Ok(Value::new(String::from(c)))
            } else {
              Ok(Value::Nil)
            }
          } else {
            Ok(Value::Nil)
          }
        }
        _ => Err(format!("cannot index string with {}", index)),
      },
      _ => Err(format!("cannot index {}", self)),
    }
  }

  pub fn set<N: ToString>(&mut self, name: N, value: Value) -> Result<(), String> {
    match self {
      Self::Instance(obj) => obj.set(name, value),
      Self::Struct(obj) => {
        obj.set(name, value);
        Ok(())
      }
      v => Err(format!("cannot set member on type {}", v)),
    }
  }

  pub fn get<N: ToString>(&self, name: &N) -> Value {
    match self {
      Self::Instance(obj) => obj.get(name),
      Self::Struct(obj) => obj.get(name),
      _ => Value::Nil,
    }
  }

  pub fn native<F: FnMut(&mut Env, Vec<Value>) -> ValueOpResult + 'static>(
    name: String,
    call: F,
  ) -> Self {
    Self::new((name, call))
  }
}

impl New<bool> for Value {
  fn new(item: bool) -> Self {
    Self::Bool(item)
  }
}

impl New<f64> for Value {
  fn new(item: f64) -> Self {
    Self::Num(item)
  }
}

impl New<u128> for Value {
  fn new(item: u128) -> Self {
    Self::U128(item)
  }
}

impl New<usize> for Value {
  fn new(item: usize) -> Self {
    Self::new(item as f64)
  }
}

impl New<i64> for Value {
  fn new(item: i64) -> Self {
    Self::new(item as f64)
  }
}

impl New<i32> for Value {
  fn new(item: i32) -> Self {
    Self::new(item as f64)
  }
}

impl New<String> for Value {
  fn new(item: String) -> Self {
    Self::String(item)
  }
}

impl New<&str> for Value {
  fn new(item: &str) -> Self {
    Self::new(String::from(item))
  }
}

impl New<Vec<Value>> for Value {
  fn new(item: Vec<Value>) -> Self {
    Self::List(SmartPtr::new(List::new(item)))
  }
}

impl New<List> for Value {
  fn new(item: List) -> Self {
    Self::List(SmartPtr::new(item))
  }
}

impl New<Function> for Value {
  fn new(func: Function) -> Self {
    Self::Function(SmartPtr::new(func))
  }
}

impl New<Closure> for Value {
  fn new(func: Closure) -> Self {
    Self::Closure(SmartPtr::new(func))
  }
}

impl New<Struct> for Value {
  fn new(item: Struct) -> Self {
    Self::Struct(SmartPtr::new(item))
  }
}

impl New<Method> for Value {
  fn new(item: Method) -> Self {
    Self::Method(SmartPtr::new(item))
  }
}

impl New<Class> for Value {
  fn new(item: Class) -> Self {
    Self::Class(SmartPtr::new(item))
  }
}

impl New<Instance> for Value {
  fn new(item: Instance) -> Self {
    Self::Instance(SmartPtr::new(item))
  }
}

impl<F: FnMut(&mut Env, Vec<Value>) -> ValueOpResult + 'static> New<(String, F)> for Value {
  fn new((name, call): (String, F)) -> Self {
    Self::NativeFunction(SmartPtr::new(NativeFn::new(name, call)))
  }
}

pub type ValueOpResult = Result<Value, String>;

impl Add for Value {
  type Output = ValueOpResult;
  fn add(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a + b)),
        Self::String(b) => Ok(Self::String(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::String(a) => match other {
        Self::Num(b) => Ok(Self::String(format!("{}{}", a, b))),
        Self::String(b) => Ok(Self::String(format!("{}{}", a, b))),
        Self::U128(b) => Ok(Self::String(format!("{}{}", a, b))),
        Self::Struct(b) => Ok(Self::String(format!("{}{}", a, b))),
        Self::Bool(b) => Ok(Self::String(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::U128(a) => match other {
        Self::String(b) => Ok(Self::String(format!("{}{}", a, b))),
        Self::U128(b) => Ok(Self::U128(a + b)),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::Struct(a) => match other {
        Self::String(b) => Ok(Self::String(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::Bool(a) => match other {
        Self::String(b) => Ok(Self::String(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      _ => Err(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl Sub for Value {
  type Output = ValueOpResult;
  fn sub(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a - b)),
        _ => Err(format!("cannot sub {} and {}", a, other)),
      },
      _ => Err(format!("cannot sub {} and {}", self, other)),
    }
  }
}

impl Mul for Value {
  type Output = ValueOpResult;
  fn mul(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a * b)),
        Self::String(b) => {
          if a > 0.0 {
            Ok(Self::String(b.repeat(a as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", b))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      Self::String(a) => match other {
        Self::Num(b) => {
          if b > 0.0 {
            Ok(Self::new(a.repeat(b as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", a))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      _ => Err(format!("cannot multiply {} and {}", self, other)),
    }
  }
}

impl Div for Value {
  type Output = ValueOpResult;
  fn div(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a / b)),
        _ => Err(format!("cannot divide {} by {}", self, other)),
      },
      _ => Err(format!("cannot divide {} by {}", self, other)),
    }
  }
}

impl Rem for Value {
  type Output = ValueOpResult;
  fn rem(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a % b)),
        _ => Err(format!("cannot modulus {} by {}", self, other)),
      },
      _ => Err(format!("cannot modulus {} by {}", self, other)),
    }
  }
}

impl Not for Value {
  type Output = Self;
  fn not(self) -> Self::Output {
    Value::Bool(!self.truthy())
  }
}

impl Neg for Value {
  type Output = ValueOpResult;
  fn neg(self) -> Self::Output {
    match self {
      Self::Num(n) => Ok(Self::Num(-n)),
      _ => Err(format!("cannot negate '{}'", self)),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match self {
      Self::Bool(a) => {
        if let Self::Bool(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::String(a) => {
        if let Self::String(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Num(a) => {
        if let Self::Num(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::U128(a) => {
        if let Self::U128(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::List(a) => {
        if let Self::List(b) = other {
          let a = &a.0;
          let b = &b.0;

          if a.len() == b.len() {
            for it in a.iter().zip(b.iter()) {
              let (ai, bi) = it;
              if ai != bi {
                return false;
              }
            }

            true
          } else {
            false
          }
        } else {
          false
        }
      }
      Self::Nil => {
        matches!(other, Value::Nil)
      }
      Self::Function(a) => {
        if let Self::Function(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Closure(a) => {
        if let Self::Closure(b) = other {
          a.context_ptr().raw() == b.context_ptr().raw()
        } else {
          false
        }
      }
      Self::NativeFunction(a) => {
        if let Self::NativeFunction(b) = other {
          a.raw() == b.raw()
        } else {
          false
        }
      }
      Self::Struct(a) => {
        if let Self::Struct(b) = other {
          a.raw() == b.raw()
        } else {
          false
        }
      }
      Self::Method(a) => {
        if let Self::Method(b) = other {
          a.this.raw() == b.this.raw() && a.function == b.function
        } else {
          false
        }
      }
      Self::Class(a) => {
        if let Self::Class(b) = other {
          a.raw() == b.raw()
        } else {
          false
        }
      }
      Self::Instance(a) => {
        if let Self::Instance(b) = other {
          a.data == b.data && a.class.raw() == b.class.raw()
        } else {
          false
        }
      }
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => {
          if a < b {
            Some(Ordering::Less)
          } else if a > b {
            Some(Ordering::Greater)
          } else if (a - b).abs() < f64::EPSILON {
            Some(Ordering::Equal)
          } else {
            None
          }
        }
        _ => None,
      },
      Self::U128(a) => match other {
        Self::U128(b) => Some(a.cmp(b)),
        _ => None,
      },
      Self::String(a) => match other {
        Self::String(b) => Some(a.cmp(b)),
        _ => None,
      },
      _ => None,
    }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nil => write!(f, "nil"),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Num(n) => write!(f, "{}", n),
      Self::U128(n) => write!(f, "{}", n),
      Self::String(s) => write!(f, "{}", s),
      Self::List(l) => write!(f, "{}", l),
      Self::Function(func) => write!(f, "<{}>", func),
      Self::Closure(_) => write!(f, "<closure>"),
      Self::NativeFunction(func) => write!(f, "<native '{}' @{:p}>", func.name, func.raw()),
      Self::Struct(obj) => write!(f, "{:?}", obj),
      Self::Method(_) => write!(f, "<method>"), // TODO consider class.name
      Self::Class(class) => write!(f, "<class {}>", class),
      Self::Instance(instance) => {
        write!(f, "<instance of {}> {}", instance.class.name, instance.data)
      }
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::String(s) => write!(f, "'{}'", s),
      v => Display::fmt(v, f),
    }
  }
}

#[derive(Clone)]
pub struct List(Vec<Value>);

impl List {
  pub fn new(values: Vec<Value>) -> Self {
    Self(values)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn get(&self, index: usize) -> Option<&Value> {
    self.0.get(index)
  }

  pub fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
    self.0.extend(iter);
  }

  pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) {
    self.0.drain(range);
  }

  pub fn exchange(&mut self, other: &mut Vec<Value>) {
    std::mem::swap(&mut self.0, other);
  }

  pub fn iter(&self) -> Iter<'_, Value> {
    self.0.iter()
  }
}

impl Index<usize> for List {
  type Output = Value;

  fn index(&self, idx: usize) -> &Self::Output {
    &self.0[idx]
  }
}

impl IndexMut<usize> for List {
  fn index_mut(&mut self, idx: usize) -> &mut Value {
    &mut self.0[idx]
  }
}

impl Display for List {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "[{}]",
      self
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}

#[derive(Clone)]
pub struct Function {
  airity: usize,
  locals: usize,
  ctx: SmartPtr<Context>,
}

impl Function {
  pub fn new(airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self {
      airity,
      locals,
      ctx,
    }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    mut args: Vec<Value>,
  ) -> Result<Value, Vec<String>> {
    if args.len() > self.airity {
      return Err(vec![format!(
        "too many arguments number of arguments, expected {}, got {}",
        self.airity,
        args.len()
      )]);
    }

    while args.len() < self.airity {
      args.push(Value::Nil);
    }

    let prev_ip = thread.ip;

    args.reserve(self.locals);

    let prev_stack = thread.stack_move(args);

    let res = thread
      .run(self.ctx.clone(), env)
      .map_err(|e| e.into_iter().map(|e| e.msg).collect());

    thread.ip = prev_ip;
    thread.stack_move(prev_stack);

    res
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.ctx
  }

  pub fn context(&self) -> &Context {
    &self.ctx
  }
}

impl Display for Function {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "function {} {}",
      self.context().id,
      self.context().name()
    )
  }
}

impl Debug for Function {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self)
  }
}

impl PartialEq for Function {
  fn eq(&self, other: &Self) -> bool {
    self.context_ptr().raw() == other.context_ptr().raw()
  }
}

#[derive(Clone)]
pub struct Closure {
  capture_count: usize,
  captures: SmartPtr<List>,
  pub function: SmartPtr<Function>,
}

impl Closure {
  pub fn new(captures: SmartPtr<List>, function: SmartPtr<Function>) -> Self {
    Self {
      capture_count: captures.len(),
      captures,
      function,
    }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    mut args: Vec<Value>,
  ) -> Result<Value, Vec<String>> {
    if args.len() > self.function.airity {
      return Err(vec![format!(
        "too many arguments number of arguments, expected {}, got {}",
        self.function.airity,
        args.len()
      )]);
    }

    while args.len() < self.function.airity {
      args.push(Value::Nil);
    }

    let prev_ip = thread.ip;

    self.captures.extend(args);

    let mut captures_with_args = Vec::default();
    self.captures.exchange(&mut captures_with_args);

    let prev_stack = thread.stack_move(captures_with_args);

    let res = thread
      .run(self.function.ctx.clone(), env)
      .map_err(|e| e.into_iter().map(|e| e.msg).collect());

    thread.ip = prev_ip;

    let mut captures = thread.stack_move(prev_stack);
    self.captures.exchange(&mut captures);

    // remove leftover elements in bulk in event of early return
    if self.captures.len() > self.capture_count {
      self.captures.drain(self.capture_count..);
    }

    res
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.function.ctx
  }

  pub fn context(&self) -> &Context {
    &self.function.ctx
  }
}

pub struct NativeFn {
  pub name: String,
  pub callee: Box<dyn FnMut(&mut Env, Vec<Value>) -> ValueOpResult>,
}

impl NativeFn {
  pub fn new<F: FnMut(&mut Env, Vec<Value>) -> ValueOpResult + 'static>(
    name: String,
    callee: F,
  ) -> Self {
    Self {
      name,
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, env: &mut Env, args: Vec<Value>) -> ValueOpResult {
    (self.callee)(env, args)
  }
}

#[derive(Debug, Default)]
pub struct Struct {
  members: BTreeMap<String, Value>,
}

impl Struct {
  pub fn set<T: ToString>(&mut self, name: T, value: Value) {
    self.members.insert(name.to_string(), value);
  }

  pub fn get<T: ToString>(&self, name: &T) -> Value {
    self
      .members
      .get(&name.to_string())
      .cloned()
      .unwrap_or(Value::Nil)
  }

  pub fn values(&self) -> Vec<Value> {
    self.members.values().cloned().collect()
  }
}

impl Display for Struct {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

#[derive(Clone)]
pub struct Method {
  this: SmartPtr<Instance>,
  pub function: SmartPtr<Function>,
}

impl Method {
  pub fn new(this: SmartPtr<Instance>, function: SmartPtr<Function>) -> Self {
    Self { this, function }
  }
}

impl Method {
  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    mut args: Vec<Value>,
  ) -> Result<Value, Vec<String>> {
    if args.len() > self.function.airity {
      return Err(vec![format!(
        "too many arguments number of arguments, expected {}, got {}",
        self.function.airity,
        args.len()
      )]);
    }

    while args.len() < self.function.airity {
      args.push(Value::Nil);
    }

    let prev_ip = thread.ip;

    let mut args_with_self = Vec::with_capacity(args.len() + 1);
    args_with_self.push(Value::Instance(self.this.clone()));
    args_with_self.extend(args);

    let prev_stack = thread.stack_move(args_with_self);

    let res = thread
      .run(self.function.ctx.clone(), env)
      .map_err(|e| e.into_iter().map(|e| e.msg).collect());

    thread.ip = prev_ip;

    thread.stack_move(prev_stack);

    res
  }
}

#[derive(Clone)]
pub struct Class {
  pub name: String,
  pub initializer: Option<SmartPtr<Function>>,
  pub methods: BTreeMap<String, SmartPtr<Function>>,
}

impl Class {
  pub fn new(name: String) -> Self {
    Self {
      name,
      initializer: None,
      methods: BTreeMap::default(),
    }
  }

  // TODO crate two types of classes,
  // one for default construction,
  // another for custom construction
  // to save a cpu cycle or two
  pub fn construct(
    mut class: SmartPtr<Self>,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Vec<Value>,
  ) -> Result<Value, Vec<String>> {
    let value = if let Some(initializer) = &mut class.initializer {
      initializer.call(thread, env, args)?
    } else {
      Value::new(Struct::default())
    };

    let mut instance = SmartPtr::new(Instance::new(value, class.clone()));

    for (name, function) in class.methods.iter() {
      let method = Method::new(instance.clone(), function.clone());
      instance.set_method(name.clone(), method);
    }

    Ok(Value::Instance(instance))
  }

  pub fn set_initializer(&mut self, value: Value) -> Result<(), String> {
    if let Value::Function(function) = value {
      self.initializer = Some(function);
      Ok(())
    } else {
      Err(format!("cannot set initializer to value {}", value))
    }
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: Value) -> Result<(), String> {
    if let Value::Function(function) = value {
      self.methods.insert(name.to_string(), function);
      Ok(())
    } else {
      Err(format!("cannot set method to value {}", value))
    }
  }
}

impl Display for Class {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} {:?}", self.name, self.methods)
  }
}

#[derive(Clone)]
pub struct Instance {
  data: Value,
  methods: BTreeMap<String, Method>,
  class: SmartPtr<Class>,
}

impl Instance {
  pub fn new(data: Value, class: SmartPtr<Class>) -> Self {
    Self {
      data,
      methods: BTreeMap::default(),
      class,
    }
  }

  pub fn set<N: ToString>(&mut self, name: N, value: Value) -> Result<(), String> {
    self.data.set(name, value)
  }

  pub fn get<N: ToString>(&self, name: &N) -> Value {
    match self.data.get(name) {
      Value::Nil => {
        if let Some(method) = self.methods.get(&name.to_string()) {
          Value::new(method.clone())
        } else {
          Value::Nil
        }
      }
      value => value,
    }
  }

  pub fn set_method<N: ToString>(&mut self, name: N, method: Method) {
    self.methods.insert(name.to_string(), method);
  }
}

impl Deref for Instance {
  type Target = Value;
  fn deref(&self) -> &Self::Target {
    &self.data
  }
}

#[cfg(test)]
mod test;
