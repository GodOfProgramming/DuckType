use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
};
use ptr::SmartPtr;
use std::collections::BTreeMap;

pub mod prelude {
  pub use super::Context;
}

pub struct Context {
  pub name: Option<String>,
  pub id: usize, // the function id within the local file

  global: SmartPtr<Context>,

  pub(crate) instructions: Vec<Opcode>,

  consts: Vec<ConstantValue>,
  // map of string to const vec location to save memory
  strings: BTreeMap<String, usize>,

  pub meta: Reflection,
}

impl Context {
  pub(crate) fn new(name: Option<impl Into<String>>, reflection: Reflection) -> Self {
    Self {
      name: name.map(|n| n.into()),
      id: Default::default(),
      global: Default::default(),
      instructions: Default::default(),
      consts: Default::default(),
      strings: Default::default(),
      meta: reflection,
    }
  }

  pub fn trace_all(&self, marks: &mut Marker) {
    if self.global.valid() {
      self.global.trace(marks);
    }
  }

  pub(crate) fn new_child(name: Option<String>, id: usize, ctx: SmartPtr<Context>, reflection: Reflection) -> Self {
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
      if let ConstantValue::Fn(f) = value {
        f.ctx.disassemble();
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
      Opcode::Invoke(count) => println!("{} {}", Self::opcode_column("Call"), Self::value_column(*count)),
      Opcode::CreateVec(count) => println!("{} {}", Self::opcode_column("CreateList"), Self::value_column(*count)),
      Opcode::Define(ident) => println!(
        "{} {} {}",
        Self::opcode_column("Define"),
        Self::value_column(*ident),
        self.const_at_column(*ident)
      ),
      Opcode::Resolve(ident) => println!(
        "{} {} {}",
        Self::opcode_column("Resolve"),
        Self::value_column(*ident),
        self.const_at_column(*ident)
      ),
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
