use crate::{util::FileIdType, Context, Instruction, SourceLocation};
use std::rc::Rc;

#[derive(Debug)]
pub struct Reflection {
  pub name: Option<String>,
  pub file_id: Option<FileIdType>,
  pub source: Rc<String>,
  pub opcode_info: Vec<SourceLocation>,
}

impl Reflection {
  pub(crate) fn new(name: Option<impl ToString>, file_id: Option<FileIdType>, source: Rc<String>) -> Self {
    Reflection {
      name: name.map(|n| n.to_string()),
      file_id,
      source,
      opcode_info: Default::default(),
    }
  }

  pub(crate) fn add(&mut self, line: usize, column: usize) {
    self.opcode_info.push(SourceLocation { line, column });
  }

  pub fn location(&self, offset: usize) -> Option<SourceLocation> {
    self.opcode_info.get(offset).cloned()
  }

  pub fn reflect(&self, inst: Instruction, offset: usize) -> Option<InstructionReflection<'_>> {
    self.location(offset).map(|info| InstructionReflection {
      inst,
      file_id: self.file_id,
      source: &self.source,
      line: info.line,
      column: info.column,
    })
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionReflection<'src> {
  pub inst: Instruction,
  pub file_id: Option<FileIdType>,
  pub source: &'src str,
  pub line: usize,
  pub column: usize,
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
    let value = cache.const_at(index);
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

    match inst.opcode() {
      Opcode::Const => {
        let index: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Const"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::PopN => {
        let n: usize = inst.data();
        write!(f, "{} {}", Self::opcode_column("PopN"), Self::value_column(n))
      }
      Opcode::Load => {
        let (storage, index): (Storage, LongAddr) = inst.data();
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
        let (storage, index): (Storage, LongAddr) = inst.data();
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
        let ident: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Define Global"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::DefineScope => {
        let ident: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Define Scope"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::AssignMember => {
        let index: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::LookupMember => {
        let index: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(index),
          Self::const_at_column(cache, index)
        )
      }
      Opcode::Jump => {
        let forward: usize = inst.data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("Jump"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::JumpIfFalse => {
        let forward: usize = inst.data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::Loop => {
        let backward: usize = inst.data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("Loop"),
          Self::address_of(offset - backward)
        )
      }
      Opcode::Or => {
        let forward: usize = inst.data();
        write!(f, "{} {: >14}", Self::opcode_column("Or"), Self::address_of(offset + forward))
      }
      Opcode::And => {
        let forward: usize = inst.data();
        write!(
          f,
          "{} {: >14}",
          Self::opcode_column("And"),
          Self::address_of(offset + forward)
        )
      }
      Opcode::Invoke => {
        let args: usize = inst.data();
        write!(f, "{} {}", Self::opcode_column("Call"), Self::value_column(args))
      }
      Opcode::CreateStruct => {
        let items: usize = inst.data();
        write!(f, "{} {}", Self::opcode_column("CreateStruct"), Self::value_column(items))
      }
      Opcode::CreateVec => {
        let items: usize = inst.data();
        write!(f, "{} {}", Self::opcode_column("CreateVec"), Self::value_column(items))
      }
      Opcode::Resolve => {
        let ident: usize = inst.data();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Resolve"),
          Self::value_column(ident),
          Self::const_at_column(cache, ident)
        )
      }
      Opcode::Add if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Add"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Sub if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Sub"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Mul if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Mul"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Div if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Div"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Rem if inst.has_data() => {
        let (st_a, addr_a, st_b, addr_b) = inst.data::<(Storage, ShortAddr, Storage, ShortAddr)>();
        write!(
          f,
          "{} {} {}",
          Self::opcode_column("Rem"),
          Self::storage_column(cache, st_a, addr_a),
          Self::storage_column(cache, st_b, addr_b)
        )
      }
      Opcode::Swap => {
        let (addr_a, addr_b) = inst.data::<(ShortAddr, ShortAddr)>();
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
