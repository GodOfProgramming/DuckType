use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
};

pub mod prelude {
  pub use super::{Context, Program};
}

#[derive(Default)]
pub struct Program {
  consts: Vec<ConstantValue>,
  strings: bimap::BiBTreeMap<usize, String>,
}

impl Program {
  pub fn const_at(&self, index: usize) -> Option<&ConstantValue> {
    self.consts.get(index)
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub(crate) fn add_const(&mut self, c: ConstantValue) -> usize {
    let string = if let ConstantValue::String(string) = &c {
      if let Some(index) = self.strings.get_by_right(string.as_str()) {
        return *index;
      }
      Some(string.clone())
    } else {
      None
    };

    let index = self.consts.len();
    self.consts.push(c);

    if let Some(string) = string {
      self.strings.insert(index, string);
    }

    index
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
      Opcode::LookupGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("LookupGlobal"),
        Self::value_column(*name),
        self.const_at_column(program, *name),
      ),
      Opcode::DefineGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("DefineGlobal"),
        Self::value_column(*name),
        self.const_at_column(program, *name),
      ),
      Opcode::AssignGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("AssignGlobal"),
        Self::value_column(*name),
        self.const_at_column(program, *name),
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
        self.const_at_column(program, *ident)
      ),
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

  fn value_column(value: usize) -> String {
    format!("{: >4}", value)
  }

  fn const_at_column(&self, program: &Program, index: usize) -> String {
    let cval = &ConstantValue::StaticString("????");
    let value = program.const_at(index).unwrap_or(cval);
    format!("{value: >4?}")
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}
