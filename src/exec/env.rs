use std::{
  collections::{btree_map, hash_map, BTreeMap},
  fmt::{self, Display, Formatter},
  iter::Chain,
};

#[cfg(test)]
use crate::code::gen::{CAPTURE_OPS, GENERATED_OPS};
use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
  util::FileIdType,
  FastHashMap,
};

use super::Stack;
pub mod prelude {
  pub use super::{Cache, Context};
}

type ConstIndex = usize;
type ValueBits = u64;

#[derive(Default)]
pub struct Cache {
  // Static
  pub(crate) consts: Vec<ConstantValue>,
  pub(crate) strings: bimap::BiBTreeMap<ConstIndex, String>,
  globals: FastHashMap<ConstIndex, Value>,
  libs: BTreeMap<FileIdType, Value>,

  // Can be cleared from gc cleaning
  mods: FastHashMap<ValueBits, FastHashMap<ConstIndex, Value>>,
}

impl Cache {
  pub fn const_at(&self, index: impl Into<usize>) -> Option<&ConstantValue> {
    self.consts.get(index.into())
  }

  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub(crate) fn find_var(&self, env: &UsertypeHandle<ModuleValue>, key: impl Into<ConstIndex>) -> Option<Value> {
    let key = key.into();
    self
      .mods
      .get(&env.value().bits)
      .and_then(|m| m.get(&key))
      .or_else(|| self.globals.get(&key))
      .cloned()
  }

  pub(crate) fn resolve_mod(&self, value: Value, key: impl Into<ConstIndex>) -> Option<Value> {
    self.mods.get(&value.bits).and_then(|m| m.get(&key.into())).cloned()
  }

  pub(crate) fn add_lib(&mut self, id: FileIdType, value: Value) {
    self.libs.insert(id, value);
  }

  pub(crate) fn get_lib(&self, id: FileIdType) -> Option<Value> {
    self.libs.get(&id).cloned()
  }

  pub(crate) fn add_to_mod(&mut self, module: Value, key: impl Into<ConstIndex>, value: Value) {
    self.mods.entry(module.bits).or_default().insert(key.into(), value);
  }

  pub(crate) fn add_global(&mut self, id: impl Into<ConstIndex>, value: Value) {
    self.globals.insert(id.into(), value);
  }

  pub(crate) fn invalidate_global(&mut self, name: &str) {
    if let Some(index) = self.strings.get_by_right(name) {
      self.globals.remove(index);
    }
  }

  pub(crate) fn values(&self) -> Chain<hash_map::Values<'_, usize, Value>, btree_map::Values<'_, FileIdType, Value>> {
    self.globals.values().chain(self.libs.values())
  }

  pub(crate) fn forget(&mut self, addr: &u64) {
    self.mods.remove(addr);
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

  pub fn fetch(&self, index: usize) -> Option<Instruction> {
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

    if let Some((existing, inst)) = self.instructions.get_mut(index).zip(Instruction::new(op, data)) {
      *existing = inst;
      true
    } else {
      false
    }
  }

  pub fn disassemble(&self, stack: &Stack, cache: &Cache) -> String {
    ContextDisassembler { ctx: self, stack, cache }.to_string()
  }
}

pub(crate) struct ContextDisassembler<'a> {
  pub(crate) ctx: &'a Context,
  pub(crate) stack: &'a Stack,
  pub(crate) cache: &'a Cache,
}

impl<'a> Display for ContextDisassembler<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    for (i, inst) in self.ctx.instructions.iter().cloned().enumerate() {
      InstructionDisassembler {
        ctx: self,
        inst,
        offset: i,
      }
      .fmt(f)?;
      if i != self.ctx.instructions.len() - 1 {
        writeln!(f)?;
      }
    }
    Ok(())
  }
}

pub(crate) struct InstructionDisassembler<'p> {
  pub(crate) ctx: &'p ContextDisassembler<'p>,
  pub(crate) inst: Instruction,
  pub(crate) offset: usize,
}

impl<'p> InstructionDisassembler<'p> {
  fn opcode_column<O: ToString>(opcode: O) -> String {
    format!("{:<20}", opcode.to_string())
  }

  fn value_column(value: impl Display) -> String {
    format!("{: >4}", value)
  }

  fn const_at_column(cache: &Cache, index: impl Into<usize>) -> String {
    let cval = &ConstantValue::StaticString("????");
    let value = cache.const_at(index).unwrap_or(cval);
    format!("{value: >4?}")
  }

  fn storage_column(cache: &Cache, storage: Storage, index: impl Into<usize>) -> String {
    let index = index.into();
    match storage {
      Storage::Stack => format!("{} {}", Self::value_column("st"), Self::value_column(index)),
      Storage::Local => format!("{} {}", Self::value_column("lc"), Self::value_column(index)),
      Storage::Global => format!(
        "{} {} {}",
        Self::value_column("gb"),
        Self::value_column(index),
        Self::const_at_column(cache, index),
      ),
    }
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}

impl<'p> Display for InstructionDisassembler<'p> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let InstructionDisassembler { ctx, inst, offset } = self;
    let ContextDisassembler { ctx, stack, cache } = ctx;

    write!(f, "{} ", Self::address_of(*offset))?;
    if let Some(curr) = ctx.meta.info(*offset) {
      if *offset > 0 {
        if let Some(prev) = ctx.meta.info(offset - 1) {
          if curr.line == prev.line {
            write!(f, "   | ")?;
          } else {
            write!(f, "{:#04} ", curr.line)?;
          }
        } else {
          write!(f, "?????")?;
        }
      } else {
        write!(f, "{:#04} ", curr.line)?;
      }
    } else {
      write!(f, "?????")?;
    }

    match inst.opcode().unwrap_or_default() {
      Opcode::Const => {
        let index: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Const"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::PopN => {
        let n: usize = inst.display_data();
        write!(f, "{} {}", Self::opcode_column("PopN"), Self::value_column(n))
      }
      Opcode::Load => {
        let (storage, index): (Storage, LongAddr) = inst.display_data();
        match storage {
          Storage::Stack => write!(f, "{}", Self::opcode_column("Load Stack")),
          Storage::Local => write!(
            f,
            "{} {}",
            Self::opcode_column("Load Local"),
            Self::storage_column(cache, storage, index)
          ),
          Storage::Global => write!(
            f,
            "{} {}",
            Self::opcode_column("Load Global"),
            Self::storage_column(cache, storage, index)
          ),
        }
      }
      Opcode::Store => {
        let (storage, index): (Storage, LongAddr) = inst.display_data();
        match storage {
          Storage::Stack => write!(f, "{}", Self::opcode_column("Store Stack")),
          Storage::Local => write!(
            f,
            "{} {}",
            Self::opcode_column("Store Local"),
            Self::storage_column(cache, storage, index)
          ),
          Storage::Global => write!(
            f,
            "{} {}",
            Self::opcode_column("Store Global"),
            Self::storage_column(cache, storage, index)
          ),
        }
      }
      Opcode::DefineGlobal => {
        let ident: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Define Global"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::DefineScope => {
        let ident: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Define Scope"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::AssignMember => {
        let index: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::LookupMember => {
        let index: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::Jump => {
        let forward: usize = inst.display_data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("Jump"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::JumpIfFalse => {
        let forward: usize = inst.display_data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::Loop => {
        let backward: usize = inst.display_data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("Loop"),
          Self::address_of(offset - backward)
        )
      }
      Opcode::Or => {
        let forward: usize = inst.display_data();
        write!(f, "{} {: >14}", Self::opcode_column("Or"), Self::address_of(offset + forward))
      }
      Opcode::And => {
        let forward: usize = inst.display_data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("And"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::Invoke => {
        let args: usize = inst.display_data();
        write!(f, "{} {}", Self::opcode_column("Call"), Self::value_column(args))
      }
      Opcode::CreateStruct => {
        let items: usize = inst.display_data();
        write!(f, "{} {}", Self::opcode_column("CreateStruct"), Self::value_column(items))
      }
      Opcode::CreateVec => {
        let items: usize = inst.display_data();
        write!(f, "{} {}", Self::opcode_column("CreateVec"), Self::value_column(items))
      }
      Opcode::Resolve => {
        let ident: usize = inst.display_data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Resolve"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::Add if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Add"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Sub if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Sub"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Mul if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Mul"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Div if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Div"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Rem if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.display_data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Rem"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Swap => {
        let (addr_a, addr_b) = inst.display_data::<(ShortAddr, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Swap"),
          Self::value_column(stack.len() - 1 - addr_a.0),
          Self::value_column(stack.len() - 1 - addr_b.0)
        )
      }
      x => write!(f, "{}", Self::opcode_column(format!("{:?}", x))),
    }
  }
}
