use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
};
use std::mem;

use super::Register;

pub mod prelude {
  pub use super::{BitsRepr, Context, Program};
}

pub type BitsRepr = u32;

static_assertions::const_assert!(mem::size_of::<BitsRepr>() <= mem::size_of::<usize>());

#[derive(Default)]
pub struct Program {
  consts: Vec<ConstantValue>,
  strings: bimap::BiBTreeMap<BitsRepr, String>,
}

impl Program {
  pub fn const_at(&self, index: BitsRepr) -> Option<&ConstantValue> {
    self.consts.get(index as usize)
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub(crate) fn add_const(&mut self, c: ConstantValue) -> Option<BitsRepr> {
    let string = if let ConstantValue::String(string) = &c {
      if let Some(index) = self.strings.get_by_right(string.as_str()) {
        return Some(*index);
      }
      Some(string.clone())
    } else {
      None
    };

    let index = self.consts.len().try_into().ok()?;
    self.consts.push(c);

    if let Some(string) = string {
      self.strings.insert(index, string);
    }

    Some(index)
  }
}

pub struct Context {
  pub id: usize, // the function id within the local file
  pub(crate) instructions: Vec<Opcode>,
  pub meta: Reflection,
}

impl Context {
  pub(crate) fn new(id: usize, reflection: Reflection) -> Self {
    Self {
      id,
      instructions: Default::default(),
      meta: reflection,
    }
  }

  pub fn next(&self, index: usize) -> Option<Opcode> {
    self.instructions.get(index).cloned()
  }

  pub(crate) fn write(&mut self, op: Opcode, line: usize, column: usize) {
    #[cfg(test)]
    {
      println!("emitting {:?}", op);
    }
    self.instructions.push(op);
    self.meta.add(line, column);
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
  pub fn disassemble(&self, program: &Program) {
    self.display_opcodes(program);
  }

  pub fn display_opcodes(&self, program: &Program) {
    for (i, op) in self.instructions.iter().enumerate() {
      self.display_instruction(program, op, i);
    }
  }

  pub fn display_instruction(&self, program: &Program, op: &Opcode, offset: usize) {
    print!("{} ", Self::address_of(offset));
    if let Some(curr) = self.meta.info(offset) {
      if offset > 0 {
        if let Some(prev) = self.meta.info(offset - 1) {
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
          self.const_at_column(program, *index)
        );
      }
      Opcode::PopN(count) => println!("{} {}", Self::opcode_column("PopN"), Self::value_column(*count)),
      Opcode::Load(storage) => match storage {
        crate::exec::Storage::Local(index) => println!("{} {}", Self::opcode_column("Load Local"), Self::value_column(*index)),
        crate::exec::Storage::Global(index) => println!(
          "{} {} {}",
          Self::opcode_column("Load Global"),
          Self::value_column(*index),
          self.const_at_column(program, *index),
        ),
        crate::exec::Storage::Reg(reg) => println!("{} {}", Self::opcode_column("Load Reg"), Self::reg_column(*reg)),
      },
      Opcode::Store(storage) => match storage {
        crate::exec::Storage::Local(index) => println!("{} {}", Self::opcode_column("Store Local"), Self::value_column(*index)),
        crate::exec::Storage::Global(index) => println!(
          "{} {} {}",
          Self::opcode_column("Store Global"),
          Self::value_column(*index),
          self.const_at_column(program, *index),
        ),
        crate::exec::Storage::Reg(reg) => println!("{} {}", Self::opcode_column("Store Reg"), Self::reg_column(*reg)),
      },
      Opcode::Define(ident) => println!(
        "{} {} {}",
        Self::opcode_column("Define"),
        Self::value_column(*ident),
        self.const_at_column(program, *ident)
      ),
      Opcode::AssignMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(*index),
          self.const_at_column(program, *index)
        );
      }
      Opcode::LookupMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(*index),
          self.const_at_column(program, *index)
        );
      }
      Opcode::Jump(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Jump"),
        Self::address_of(offset + *count as usize)
      ),
      Opcode::JumpIfFalse(count) => {
        println!(
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + *count as usize)
        )
      }
      Opcode::Loop(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Loop"),
        Self::address_of(offset - *count as usize)
      ),
      Opcode::Or(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Or"),
        Self::address_of(offset + *count as usize)
      ),
      Opcode::And(count) => println!(
        "{} {: >14}",
        Self::opcode_column("And"),
        Self::address_of(offset + *count as usize)
      ),
      Opcode::Invoke(count) => println!("{} {}", Self::opcode_column("Call"), Self::value_column(*count)),
      Opcode::CreateVec(count) => println!("{} {}", Self::opcode_column("CreateList"), Self::value_column(*count)),
      Opcode::Resolve(ident) => println!(
        "{} {} {}",
        Self::opcode_column("Resolve"),
        Self::value_column(*ident),
        self.const_at_column(program, *ident)
      ),
      x => println!("{}", Self::opcode_column(format!("{:?}", x))),
    }
  }

  fn opcode_column<O: ToString>(opcode: O) -> String {
    format!("{:<20}", opcode.to_string())
  }

  fn reg_column(value: Register) -> String {
    format!("{: >4?}", value)
  }

  fn value_column(value: BitsRepr) -> String {
    format!("{: >4}", value)
  }

  fn const_at_column(&self, program: &Program, index: BitsRepr) -> String {
    let cval = &ConstantValue::StaticString("????");
    let value = program.const_at(index).unwrap_or(cval);
    format!("{value: >4?}")
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}
