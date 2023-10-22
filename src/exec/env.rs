use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
};
use ptr::SmartPtr;
use std::{
  collections::{btree_map::Iter, BTreeMap},
  env,
};

pub mod prelude {
  pub use super::{Context, Env};
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

  pub(crate) fn new_child(name: Option<String>, id: usize, ctx: SmartPtr<Context>, reflection: Reflection) -> Self {
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

  pub(crate) fn write(&mut self, op: Opcode, line: usize, column: usize) {
    #[cfg(test)]
    {
      println!("emitting {:?}", op);
    }
    self.instructions.push(op);
    self.meta.add(line, column);
  }

  pub(crate) fn write_const(&mut self, c: ConstantValue, line: usize, column: usize) {
    let c = self.add_const(c);
    self.write(Opcode::Const(c), line, column);
  }

  pub(crate) fn add_const(&mut self, c: ConstantValue) -> usize {
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

  pub(crate) fn replace_instruction(&mut self, index: usize, op: Opcode) -> bool {
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
