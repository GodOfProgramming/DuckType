#[cfg(test)]
use crate::code::gen::{CAPTURE_OPS, GENERATED_OPS};
use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
  util::UnwrapAnd,
};
pub mod prelude {
  pub use super::{Context, Program};
}

#[derive(Default)]
pub struct Program {
  consts: Vec<ConstantValue>,
  pub(crate) strings: bimap::BiBTreeMap<usize, String>,
}

impl Program {
  pub fn const_at(&self, index: impl Into<usize>) -> Option<&ConstantValue> {
    self.consts.get(index.into())
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub(crate) fn add_const(&mut self, c: ConstantValue) -> Option<usize> {
    let string = if let ConstantValue::String(string) = &c {
      if let Some(index) = self.strings.get_by_right(string.as_str()) {
        return Some(*index);
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

    Some(index)
  }
}

pub struct Context {
  pub id: usize, // the function id within the local file
  pub(crate) instructions: Vec<Instruction>,
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

  pub fn next(&self, index: usize) -> Option<Instruction> {
    self.instructions.get(index).cloned()
  }

  pub(crate) fn write(&mut self, inst: Instruction, line: usize, column: usize) {
    #[cfg(test)]
    {
      CAPTURE_OPS.with_borrow(|should_cap| {
        if *should_cap {
          GENERATED_OPS.with_borrow_mut(|ops| {
            ops.insert(inst.opcode().unwrap());
          });
        }
      });
    }
    self.instructions.push(inst);
    self.meta.add(line, column);
  }

  pub fn num_instructions(&self) -> usize {
    self.instructions.len()
  }

  pub(crate) fn replace_instruction<D>(&mut self, index: usize, op: Opcode, data: D) -> bool
  where
    D: InstructionData,
  {
    #[cfg(test)]
    {
      CAPTURE_OPS.with_borrow(|should_cap| {
        if *should_cap {
          GENERATED_OPS.with_borrow_mut(|ops| {
            ops.insert(op);
          });
        }
      });
    }

    self
      .instructions
      .get_mut(index)
      .zip(Instruction::new(op, data))
      .unwrap_and(|(existing, inst)| {
        *existing = inst;
      })
  }

  #[cfg(debug_assertions)]
  pub fn disassemble(&self, program: &Program) {
    self.display_opcodes(program);
  }

  pub fn display_opcodes(&self, program: &Program) {
    for (i, op) in self.instructions.iter().cloned().enumerate() {
      self.display_instruction(program, op, i);
    }
  }

  pub fn display_instruction(&self, program: &Program, inst: Instruction, offset: usize) {
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

    match inst.opcode().unwrap_or_default() {
      Opcode::Const => {
        let index: usize = inst.display_data();
        println!(
          "{} {} {}",
          Self::opcode_column("Const"),
          Self::value_column(index),
          Self::const_at_column(program, index)
        );
      }
      Opcode::PopN => {
        let n: usize = inst.display_data();
        println!("{} {}", Self::opcode_column("PopN"), Self::value_column(n))
      }
      Opcode::Load => {
        let (storage, index): (Storage, LongAddr) = inst.display_data();
        match storage {
          Storage::Local => println!(
            "{} {}",
            Self::opcode_column("Load Local"),
            Self::storage_column(program, storage, index)
          ),
          Storage::Global => println!(
            "{} {}",
            Self::opcode_column("Load Global"),
            Self::storage_column(program, storage, index)
          ),
          Storage::Reg => println!(
            "{} {}",
            Self::opcode_column("Load Reg"),
            Self::storage_column(program, storage, index)
          ),
        }
      }
      Opcode::Store => {
        let (storage, index): (Storage, LongAddr) = inst.display_data();
        match storage {
          Storage::Local => println!(
            "{} {}",
            Self::opcode_column("Store Local"),
            Self::storage_column(program, storage, index)
          ),
          Storage::Global => println!(
            "{} {}",
            Self::opcode_column("Store Global"),
            Self::storage_column(program, storage, index)
          ),
          Storage::Reg => println!(
            "{} {}",
            Self::opcode_column("Store Reg"),
            Self::storage_column(program, storage, index)
          ),
        }
      }
      Opcode::Define => {
        let ident: usize = inst.display_data();
        println!(
          "{} {} {}",
          Self::opcode_column("Define"),
          Self::value_column(ident),
          Self::const_at_column(program, ident)
        )
      }
      Opcode::AssignMember => {
        let index: usize = inst.display_data();
        println!(
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(index),
          Self::const_at_column(program, index)
        );
      }
      Opcode::LookupMember => {
        let index: usize = inst.display_data();
        println!(
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(index),
          Self::const_at_column(program, index)
        );
      }
      Opcode::Jump => {
        let forward: usize = inst.display_data();
        println!("{} {: >14}", Self::opcode_column("Jump"), Self::address_of(offset + forward))
      }
      Opcode::JumpIfFalse => {
        let forward: usize = inst.display_data();
        println!(
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::Loop => {
        let backward: usize = inst.display_data();
        println!("{} {: >14}", Self::opcode_column("Loop"), Self::address_of(offset - backward))
      }
      Opcode::Or => {
        let forward: usize = inst.display_data();
        println!("{} {: >14}", Self::opcode_column("Or"), Self::address_of(offset + forward))
      }
      Opcode::And => {
        let forward: usize = inst.display_data();
        println!("{} {: >14}", Self::opcode_column("And"), Self::address_of(offset + forward))
      }
      Opcode::Invoke => {
        let args: usize = inst.display_data();
        println!("{} {}", Self::opcode_column("Call"), Self::value_column(args))
      }
      Opcode::CreateStruct => {
        let items: usize = inst.display_data();
        println!("{} {}", Self::opcode_column("CreateStruct"), Self::value_column(items))
      }
      Opcode::CreateVec => {
        let items: usize = inst.display_data();
        println!("{} {}", Self::opcode_column("CreateVec"), Self::value_column(items))
      }
      Opcode::Resolve => {
        let ident: usize = inst.display_data();
        println!(
          "{} {} {}",
          Self::opcode_column("Resolve"),
          Self::value_column(ident),
          Self::const_at_column(program, ident)
        )
      }
      Opcode::Add if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        println!(
          "{} {} {}",
          Self::opcode_column("Add"),
          Self::storage_column(program, st_a, addr_a),
          Self::storage_column(program, st_b, addr_b)
        )
      }
      Opcode::Sub if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        println!(
          "{} {} {}",
          Self::opcode_column("Sub"),
          Self::storage_column(program, st_a, addr_a),
          Self::storage_column(program, st_b, addr_b)
        )
      }
      Opcode::Mul if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        println!(
          "{} {} {}",
          Self::opcode_column("Mul"),
          Self::storage_column(program, st_a, addr_a),
          Self::storage_column(program, st_b, addr_b)
        )
      }
      Opcode::Div if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        println!(
          "{} {} {}",
          Self::opcode_column("Div"),
          Self::storage_column(program, st_a, addr_a),
          Self::storage_column(program, st_b, addr_b)
        )
      }
      Opcode::Rem if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        println!(
          "{} {} {}",
          Self::opcode_column("Rem"),
          Self::storage_column(program, st_a, addr_a),
          Self::storage_column(program, st_b, addr_b)
        )
      }
      x => println!("{}", Self::opcode_column(format!("{:?}", x))),
    }
  }

  fn opcode_column<O: ToString>(opcode: O) -> String {
    format!("{:<20}", opcode.to_string())
  }

  fn value_column(value: impl Into<usize>) -> String {
    format!("{: >4}", value.into())
  }

  fn const_at_column(program: &Program, index: impl Into<usize>) -> String {
    let cval = &ConstantValue::StaticString("????");
    let value = program.const_at(index).unwrap_or(cval);
    format!("{value: >4?}")
  }

  fn storage_column(program: &Program, storage: Storage, index: impl Into<usize>) -> String {
    let index = index.into();
    match storage {
      Storage::Local => Self::value_column(index),
      Storage::Global => format!("{} {}", Self::value_column(index), Self::const_at_column(program, index),),
      Storage::Reg => Self::value_column(index),
    }
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}
