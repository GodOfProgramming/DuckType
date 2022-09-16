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

#[cfg(test)]
mod test;

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
  ) -> Result<(), Vec<String>> {
    match self {
      Value::Function(f) => f.call(thread, args),
      Value::Closure(c) => c.call(thread, args),
      Value::NativeFunction(f) => f.call(thread, env, args).map_err(|e| vec![e]),
      Value::Method(m) => m.call(thread, env, args),
      Value::Class(c) => Class::construct(c.clone(), thread, env, args),
      _ => return Err(vec![format!("unable to call non callable '{}'", self)]),
    }
  }

  pub fn index(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    index: Value,
  ) -> Result<(), String> {
    match self {
      Value::List(values) => match index {
        Value::Num(n) => {
          if n == n as usize as f64 {
            if let Some(v) = values.get(n as usize) {
              thread.stack_push(v.clone());
            } else {
              thread.stack_push(Value::Nil);
            }
          } else {
            thread.stack_push(Value::Nil);
          }
          Ok(())
        }
        _ => Err(format!("cannot index list with {}", index)),
      },
      Value::String(string) => match index {
        Value::Num(n) => {
          if n == n as usize as f64 {
            if let Some(c) = string.chars().nth(n as usize) {
              thread.stack_push(Value::new(String::from(c)));
            } else {
              thread.stack_push(Value::Nil);
            }
          } else {
            thread.stack_push(Value::Nil);
          }
          Ok(())
        }
        _ => Err(format!("cannot index string with {}", index)),
      },
      Value::Instance(instance) => instance.call_method("__index__", thread, env, vec![index]),
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

  pub fn native<
    N: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static,
  >(
    name: N,
    call: F,
  ) -> Self {
    Self::new((name.to_string(), call))
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

impl<F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static>
  New<(String, F)> for Value
{
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
      Self::String(a) => Ok(Self::String(format!("{}{}", a, other.to_string()))),
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
      Self::Class(class) => write!(f, "<{}>", class),
      Self::Instance(instance) => write!(f, "<{}>", instance),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::String(s) => write!(f, "\"{}\"", s),
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
        .map(|v| format!("{:?}", v))
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
    mut args: Vec<Value>,
  ) -> Result<(), Vec<String>> {
    if args.len() > self.airity {
      return Err(vec![format!(
        "<function> too many arguments, expected {}, got {}",
        self.airity,
        args.len()
      )]);
    }

    while args.len() < self.airity {
      args.push(Value::Nil);
    }

    args.reserve(self.locals);

    thread.new_frame(self.ctx.clone());

    thread.set_stack(args);

    Ok(())
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
  captures: Vec<Value>,
  function: SmartPtr<Function>,
}

impl Closure {
  pub fn new(captures: SmartPtr<List>, function: SmartPtr<Function>) -> Self {
    Self {
      captures: captures.0.clone(),
      function,
    }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    mut args: Vec<Value>,
  ) -> Result<(), Vec<String>> {
    if args.len() > self.function.airity {
      return Err(vec![format!(
        "<closure> too many arguments, expected {}, got {}",
        self.function.airity,
        args.len()
      )]);
    }

    while args.len() < self.function.airity {
      args.push(Value::Nil);
    }

    let mut captures_with_args = Vec::with_capacity(self.captures.len() + args.len());
    captures_with_args.extend(self.captures.clone());
    captures_with_args.extend(args);

    thread.new_frame(self.function.ctx.clone());

    thread.set_stack(captures_with_args);

    Ok(())
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.function.ctx
  }

  pub fn context(&self) -> &Context {
    &self.function.ctx
  }
}

type RawNative = dyn FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String>;

pub struct NativeFn {
  pub name: String,
  pub callee: Box<RawNative>,
}

impl NativeFn {
  pub fn new<
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static,
  >(
    name: String,
    callee: F,
  ) -> Self {
    Self {
      name,
      callee: Box::new(callee),
    }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Vec<Value>,
  ) -> Result<(), String> {
    let result = (self.callee)(thread, env, args)?;
    thread.stack_push(result);
    Ok(())
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
  pub function: Value,
}

impl Method {
  pub fn new(this: SmartPtr<Instance>, function: Value) -> Self {
    Self { this, function }
  }
}

impl Method {
  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    mut args: Vec<Value>,
  ) -> Result<(), Vec<String>> {
    match &mut self.function {
      Value::Function(function) => {
        if args.len() > function.airity {
          return Err(vec![format!(
            "<method> too many arguments, expected {}, got {}",
            function.airity,
            args.len()
          )]);
        }

        while args.len() < function.airity {
          args.push(Value::Nil);
        }

        thread.new_frame(function.ctx.clone());

        let mut args_with_self = Vec::with_capacity(1 + args.len());
        args_with_self.push(Value::Instance(self.this.clone()));
        args_with_self.extend(args);

        thread.set_stack(args_with_self);

        Ok(())
      }
      Value::NativeFunction(native) => {
        let mut args_with_self = Vec::with_capacity(args.len() + 1);
        args_with_self.push(Value::Instance(self.this.clone()));
        args_with_self.extend(args);

        native
          .call(thread, env, args_with_self)
          .map_err(|e| vec![e])
      }
      v => Err(vec![format!("invalid type for class initializer {}", v)]),
    }
  }
}

#[derive(Clone)]
pub struct Class {
  pub name: String,
  pub initializer: Option<Value>,
  pub methods: BTreeMap<String, Value>,
  pub static_members: BTreeMap<String, Value>,
}

impl Class {
  pub fn new<N: ToString>(name: N) -> Self {
    Self {
      name: name.to_string(),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  pub fn construct(
    mut class: SmartPtr<Self>,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Vec<Value>,
  ) -> Result<(), Vec<String>> {
    let mut instance = SmartPtr::new(Instance::new(Value::Nil, class.clone()));

    for (name, function) in class.methods.iter() {
      let method = Method::new(instance.clone(), function.clone());
      instance.set_method(name.clone(), method);
    }

    if let Some(initializer) = &mut class.initializer {
      let mut args_with_self = Vec::with_capacity(args.len() + 1);
      args_with_self.push(Value::Instance(instance));
      args_with_self.extend(args);
      initializer.call(thread, env, args_with_self)
    } else {
      thread.stack_push(Value::Instance(instance));
      Ok(())
    }
  }

  pub fn set_initializer(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: Value) {
    self.methods.insert(name.to_string(), value);
  }

  pub fn set_method_fn<
    N: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static,
  >(
    &mut self,
    name: N,
    value: F,
  ) {
    self
      .methods
      .insert(name.to_string(), Value::native(name, value));
  }

  pub fn set_static<N: ToString>(&mut self, name: N, value: Value) {
    self.static_members.insert(name.to_string(), value);
  }

  pub fn set_static_fn<
    N: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Result<Value, String> + 'static,
  >(
    &mut self,
    name: N,
    value: F,
  ) {
    self.set_static(name.to_string(), Value::native(name, value));
  }

  pub fn get_static<N: ToString>(&self, name: &N) -> Value {
    self
      .static_members
      .get(&name.to_string())
      .cloned()
      .unwrap_or(Value::Nil)
  }
}

impl Display for Class {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if cfg!(debug_assertions) && cfg!(feature = "verbose-debug") {
      write!(
        f,
        "class {} initializer = {:?} <method> {:?} <static> {:?}",
        self.name, self.initializer, self.methods, self.static_members
      )
    } else {
      write!(f, "class {}", self.name)
    }
  }
}

#[derive(Clone)]
pub struct Instance {
  pub data: Value,
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

  pub fn assign(&mut self, value: Value) {
    self.data = value;
  }

  pub fn deref(&self) -> Value {
    self.data.clone()
  }

  pub fn call_method<N: ToString>(
    &mut self,
    name: N,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Vec<Value>,
  ) -> Result<(), String> {
    if let Some(method) = self.methods.get_mut(&name.to_string()) {
      method.call(thread, env, args).map_err(|e| e.join(", "))
    } else {
      Err(format!(
        "no method found on object with name {}",
        name.to_string()
      ))
    }
  }
}

impl Deref for Instance {
  type Target = Value;
  fn deref(&self) -> &Self::Target {
    &self.data
  }
}

impl Display for Instance {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if cfg!(debug_assertions) && cfg!(feature = "verbose-debug") {
      write!(
        f,
        "instance of {}> <data {} > <methods {:#?}",
        self.class.name,
        self.data,
        self.methods.keys()
      )
    } else {
      write!(f, "instance of {}", self.class.name)
    }
  }
}

#[derive(Default, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl Error {
  pub fn from_ref<M: ToString>(msg: M, opcode: &OpCode, opcode_ref: OpCodeReflection) -> Self {
    let mut e = Self {
      msg: msg.to_string(),
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
