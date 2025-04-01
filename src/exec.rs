pub mod prelude {
  #[allow(unused_imports)]
  pub use super::{
    Cache, Context, ContextDisassembler, Instruction, InstructionData, LongAddr, Opcode, ShortAddr, Storage, TryIntoInstruction,
  };
}

use crate::{
  RapidHashMap,
  code::{ConstantValue, InstructionMetadata},
  util::FileIdType,
};
use crate::{prelude::*, util};
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  fmt::{self, Debug, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
};
use strum::EnumCount;
use strum_macros::{EnumCount, EnumIter, FromRepr};

#[cfg(test)]
use crate::code::bytecode::{CAPTURE_OPS, GENERATED_OPS};

type ConstIndex = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Instruction(u64);

impl Instruction {
  const DATA_BIT: u64 = 1 << Opcode::BITS;
  const DATA_OFFSET: u64 = (Self::DATA_BIT << 1).ilog2() as u64;

  pub fn new<D>(opcode: Opcode, data: D) -> Option<Self>
  where
    D: InstructionData,
  {
    let inst = data.encode()? << Self::DATA_OFFSET | if D::BITS > 0 { Self::DATA_BIT } else { 0 } | opcode.encode()?;
    Some(Self(inst))
  }

  pub fn opcode(&self) -> Opcode {
    Opcode::decode(self.0)
  }

  pub fn data<T>(&self) -> T
  where
    T: InstructionData,
  {
    T::decode(self.0 >> Self::DATA_OFFSET)
  }

  pub fn has_data(&self) -> bool {
    self.0 & Self::DATA_BIT == Self::DATA_BIT
  }
}

impl Display for Instruction {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:064b}", self.0)
  }
}

pub trait TryIntoInstruction {
  fn try_into_inst(self) -> Result<Instruction, Opcode>;
}

impl TryIntoInstruction for Opcode {
  fn try_into_inst(self) -> Result<Instruction, Opcode> {
    Instruction::new(self, ()).ok_or(self)
  }
}

impl<D> TryIntoInstruction for (Opcode, D)
where
  D: InstructionData,
{
  fn try_into_inst(self) -> Result<Instruction, Opcode> {
    Instruction::new(self.0, self.1).ok_or(self.0)
  }
}

pub trait InstructionData
where
  Self: Sized,
{
  const BITS: u64;
  const MASK: u64 = 2u64.pow(Self::BITS as u32) - 1;

  fn to_bits(self) -> u64;

  fn decode_checked(inst: u64) -> Option<Self>;

  fn decode(inst: u64) -> Self;

  fn encode(self) -> Option<u64> {
    Self::valid_bits(self.to_bits())
  }

  fn valid_bits(bits: u64) -> Option<u64> {
    (bits <= Self::MASK).then_some(bits)
  }
}

#[derive(Hash, Default, Clone, Copy, Debug, PartialEq, Eq, EnumCount, FromRepr, EnumIter)]
#[repr(u8)]
#[cfg_attr(all(target_os = "linux", feature = "jtbl"), macros::opcode_bindings(crate::bindings))]
pub enum Opcode {
  /// Pops a value off the stack
  ///
  /// Encoding: None
  Pop,
  /// Pops N values off the stack.
  ///
  /// Encoding: | usize |
  PopN,
  /// Looks up a constant value at the specified location.
  ///
  /// Encoding: | usize |
  Const,
  /// Store the value on the stack in the given location
  ///
  /// Encoding: | Storage | LongAddr |
  Store,
  /// Load a value and push it onto the stack
  ///
  /// Encoding: | Storage | LongAddr |
  Load,
  /// Pushes a nil value on to the stack
  ///
  /// Encoding: None
  Nil,
  /// Pushes true on the stack
  ///
  /// Encoding: None
  True,
  /// Pushes false on the stack
  ///
  /// Encoding: None
  False,
  /// Pops two values off the stack, calculates the sum, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Add,
  /// Pops two values off the stack, calculates the difference, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Sub,
  /// Pops two values off the stack, calculates the product, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Mul,
  /// Pops two values off the stack, calculates the quotient, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Div,
  /// Pops two values off the stack, calculates the remainder, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Rem,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up, compared, and the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Equal,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  NotEqual,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Greater,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  GreaterEqual,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Less,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  LessEqual,
  /// Pop two items off the stack, using the first as the index and the second as the indexable, pushing the result back on
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  Index,
  /// Pop three items off the stack, the first is the value, the second is the index, and the third is the indexable. Push the third back on after assignment
  ///
  /// With data two locations are looked up and the operation applied, the result placed back on the stack
  ///
  /// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
  AssignIndex,
  /// Pops a value off the stack, inverts its numerical value, then pushes that back on
  ///
  /// Encoding: None
  Negate,
  /// Pops a value off the stack, inverts its truthy value, then pushes that back on
  ///
  /// Encoding: None
  Not,
  /// Peeks at the stack. If the top value is true, the ip in incremented
  ///
  /// Encoding: | usize |
  Or,
  /// Peeks at the stack. If the top value is false, the ip is incremented
  ///
  /// Encoding: | usize |
  And,
  /// Initializes a member of an object, keeping the object on the stack for further assignments
  ///
  /// Encoding: | usize |
  InitializeMember,
  /// Assigns a value to a member on an object
  ///
  /// \[ Value \] \
  /// \[ Object \]
  ///
  /// Encoding: | usize |
  AssignMember,
  /// Looks up the member on the next value on the stack, replacing it with the member's value
  ///
  /// Encoding: | usize |
  LookupMember,
  /// Looks up the member of the next value on the stack, pushing the value
  ///
  /// Encoding: | usize |
  PeekMember,
  /// Initializes the constructor on a class, keeping the class on the stack for further assignments
  ///
  /// Encoding: None
  InitializeConstructor,
  /// Initializes a method on a class, keeping the class on the stack for further assignments
  ///
  /// Encoding: | usize |
  InitializeMethod,
  /// Create a vec of values and push it on the stack.
  /// Items come off the top of the stack.
  /// The number of items is specified in the encoding
  ///
  /// Encoding: | usize |
  CreateVec,
  /// Create a vec of values and push it on the stack.
  /// The last item on the stack is copied as many times as the size indicates
  ///
  /// Encoding: | usize |
  CreateSizedVec,
  /// Create a vec of values and push it on the stack.
  /// The last item is the size.
  /// The next is the item to be copied the amount of times specified
  ///
  /// Encoding: | usize |
  CreateDynamicVec,
  /// Create a closure. The first item on the stack is the function itself, the second is the capture list
  ///
  /// Encoding: None
  CreateClosure,
  /// Create a new struct with the number of members as the bits
  /// Values are popped off the stack as key values in that order
  ///
  /// Encoding: | usize |
  CreateStruct,
  /// Create a new class.
  /// The const in the encoding is the name
  ///
  /// Encoding: | usize |
  CreateClass,
  /// Create a new module.
  /// The const in the encoding is the name
  ///
  /// Encoding: | usize |
  CreateModule,
  /// Pops a value off the stack, and compares it with the peeked value, pushing the new value on
  ///
  /// Encoding: None
  Check,
  /// Pops a value off the stack and prints it to the screen
  ///
  /// Encoding: None
  Println,
  /// Jumps the ip forward unconditionally
  ///
  /// Encoding: | usize |
  Jump,
  /// Jumps the ip forward if the value on the stack is falsy
  ///
  /// Encoding: | usize |
  JumpIfFalse,
  /// Jumps the instruction pointer backwards a number of instructions
  ///
  /// Encoding: | usize |
  Loop,
  /// Calls the value on the stack. Number of arguments is specified by the modifying bits
  ///
  /// Encoding: | usize |
  Invoke,
  /// Load an external file, or pull from the cache if already loaded.
  /// The file name is the value on the stack
  ///
  /// Encoding: None
  Req,
  /// Exits from a function, returning nil on the previous frame. Number of locals to pop is specified by the modifying bits
  ///
  /// Encoding: | usize |
  Ret,
  /// Mark the current value as exported
  ///
  /// Encoding: None
  Export,
  /// Defines a variable in the global environment
  ///
  /// The encoding points to a const which is the name
  ///
  /// Encoding: | usize |
  DefineGlobal,
  /// Defines a variable in the current environment
  ///
  /// The encoding points to a const which is the name
  ///
  /// Encoding: | usize |
  DefineScope,
  /// Resolve the specified identifier
  /// The const in the encoding is the name
  ///
  /// Encoding: | usize |
  Resolve,
  /// Push the last value on the stack as a module
  ///
  /// Encoding: None
  EnableModule,
  /// Pop an env
  ///
  /// Encoding: None
  PopScope,
  /// Swaps the two locations on the stack
  ///
  /// Encoding: | ShortAddr | ShortAddr |
  Swap,
  /// Swaps the last two items on the stack and pops
  ///
  /// Encoding: None
  SwapPop,
  /// Pop two values off the stack, check if the second is of the first's type
  ///
  /// For instances this will get it's underlying class's type id and compare that with the right's type id
  ///
  /// For everything else this will compare the type ids directly
  ///
  /// Encoding: None
  Is,
  /// Panic
  ///
  /// Encoding: None
  Quack,
  /// Unknown instruction
  /// Value given when one cannot be interpreted
  ///
  /// Encoding: None
  #[default]
  Unknown,
  /// Halt the VM when this instruction is reached and enter the debugger
  ///
  /// Encoding: None
  Breakpoint,
}

static_assertions::const_assert!(Opcode::COUNT < 2usize.pow(Opcode::BITS as u32));

impl Display for Opcode {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{self:?}")
  }
}

impl InstructionData for Opcode {
  const BITS: u64 = 7;

  fn to_bits(self) -> u64 {
    self as u8 as u64
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    Self::from_repr((inst & Self::MASK).try_into().ok()?)
  }

  fn decode(inst: u64) -> Self {
    unsafe { mem::transmute((inst & Self::MASK) as u8) }
  }
}

impl InstructionData for usize {
  const BITS: u64 = (mem::size_of::<usize>() * 8 - Opcode::BITS as usize - 1) as u64;

  fn to_bits(self) -> u64 {
    self as u64
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    (inst & Self::MASK).try_into().ok()
  }

  fn decode(inst: u64) -> Self {
    (inst & Self::MASK) as usize
  }
}

#[derive(Debug, PartialEq, Eq, strum_macros::EnumCount, strum_macros::FromRepr)]
#[repr(u8)]
pub enum Storage {
  Stack,
  Local,
  Global,
}

static_assertions::const_assert!(Storage::COUNT < 2usize.pow(Storage::BITS as u32));

impl InstructionData for Storage {
  const BITS: u64 = 2;

  fn to_bits(self) -> u64 {
    self as u8 as u64
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    Self::from_repr((inst & Self::MASK).try_into().ok()?)
  }

  fn decode(inst: u64) -> Self {
    unsafe { mem::transmute((inst & Self::MASK) as u8) }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LongAddr(pub(crate) usize);

impl InstructionData for LongAddr {
  const BITS: u64 = 32;

  fn to_bits(self) -> u64 {
    self.0 as u64
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    Some(Self::decode(inst))
  }

  fn decode(inst: u64) -> Self {
    Self((inst & Self::MASK) as usize)
  }
}

impl From<usize> for LongAddr {
  fn from(value: usize) -> Self {
    Self(value)
  }
}

impl From<LongAddr> for usize {
  fn from(value: LongAddr) -> Self {
    value.0
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ShortAddr(pub(crate) usize);

impl InstructionData for ShortAddr {
  const BITS: u64 = 16;

  fn to_bits(self) -> u64 {
    self.0 as u64
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    (inst & Self::MASK).try_into().map(Self).ok()
  }

  fn decode(inst: u64) -> Self {
    Self((inst & Self::MASK) as usize)
  }
}

impl From<usize> for ShortAddr {
  fn from(value: usize) -> Self {
    Self(value)
  }
}

impl From<ShortAddr> for usize {
  fn from(value: ShortAddr) -> Self {
    value.0
  }
}

impl InstructionData for () {
  const BITS: u64 = 0;

  fn to_bits(self) -> u64 {
    0
  }

  fn decode_checked(_: u64) -> Option<Self> {
    Some(())
  }

  fn decode(_: u64) -> Self {}
}

impl<T0, T1> InstructionData for (T0, T1)
where
  T0: InstructionData,
  T1: InstructionData,
{
  const BITS: u64 = T0::BITS + T1::BITS;

  fn to_bits(self) -> u64 {
    self.0.to_bits() | self.1.to_bits() << T0::BITS
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    T0::decode_checked(inst & T0::MASK).zip(T1::decode_checked(inst >> T0::BITS & T1::MASK))
  }

  fn decode(inst: u64) -> Self {
    (T0::decode(inst & T0::MASK), T1::decode(inst >> T0::BITS & T1::MASK))
  }
}

impl<T0, T1, T2> InstructionData for (T0, T1, T2)
where
  T0: InstructionData,
  T1: InstructionData,
  T2: InstructionData,
{
  const BITS: u64 = T0::BITS + T1::BITS + T2::BITS;

  fn to_bits(self) -> u64 {
    self.0.to_bits() | self.1.to_bits() << T0::BITS | self.2.to_bits() << (T0::BITS + T1::BITS)
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    match (
      T0::decode_checked(inst & T0::MASK),
      T1::decode_checked(inst >> T0::BITS & T1::MASK),
      T2::decode_checked(inst >> (T0::BITS + T1::BITS) & T2::MASK),
    ) {
      (Some(t0), Some(t1), Some(t2)) => Some((t0, t1, t2)),
      _ => None,
    }
  }

  fn decode(inst: u64) -> Self {
    (
      T0::decode(inst & T0::MASK),
      T1::decode(inst >> T0::BITS & T1::MASK),
      T2::decode(inst >> (T0::BITS + T1::BITS) & T2::MASK),
    )
  }
}

impl<T0, T1, T2, T3> InstructionData for (T0, T1, T2, T3)
where
  T0: InstructionData,
  T1: InstructionData,
  T2: InstructionData,
  T3: InstructionData,
{
  const BITS: u64 = T0::BITS + T1::BITS + T2::BITS + T3::BITS;

  fn to_bits(self) -> u64 {
    self.0.to_bits()
      | self.1.to_bits() << T0::BITS
      | self.2.to_bits() << (T0::BITS + T1::BITS)
      | self.3.to_bits() << (T0::BITS + T1::BITS + T2::BITS)
  }

  fn decode_checked(inst: u64) -> Option<Self> {
    match (
      T0::decode_checked(inst & T0::MASK),
      T1::decode_checked(inst >> T0::BITS & T1::MASK),
      T2::decode_checked(inst >> (T0::BITS + T1::BITS) & T2::MASK),
      T3::decode_checked(inst >> (T0::BITS + T1::BITS + T2::BITS) & T3::MASK),
    ) {
      (Some(t0), Some(t1), Some(t2), Some(t3)) => Some((t0, t1, t2, t3)),
      _ => None,
    }
  }

  fn decode(inst: u64) -> Self {
    (
      T0::decode(inst & T0::MASK),
      T1::decode(inst >> T0::BITS & T1::MASK),
      T2::decode(inst >> (T0::BITS + T1::BITS) & T2::MASK),
      T3::decode(inst >> (T0::BITS + T1::BITS + T2::BITS) & T3::MASK),
    )
  }
}

#[derive(Default)]
pub struct Context {
  pub(crate) instructions: Vec<Instruction>,
  pub meta: InstructionMetadata,
}

impl Context {
  pub(crate) fn new(reflection: InstructionMetadata) -> Self {
    Self {
      instructions: Default::default(),
      meta: reflection,
    }
  }

  pub fn fetch(&self, index: usize) -> Instruction {
    self.instructions[index]
  }

  pub(crate) fn write(&mut self, inst: Instruction, line: usize, column: usize) {
    #[cfg(test)]
    {
      CAPTURE_OPS.with_borrow(|should_cap| {
        if *should_cap {
          GENERATED_OPS.with_borrow_mut(|ops| {
            ops.insert(inst.opcode());
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

    match self.instructions.get_mut(index).zip(Instruction::new(op, data)) {
      Some((existing, inst)) => {
        *existing = inst;
        true
      }
      _ => false,
    }
  }

  pub fn disassemble(&self, stack: &Stack, cache: &Cache) -> String {
    ContextDisassembler { ctx: self, stack, cache }.to_string()
  }
}

pub struct ContextDisassembler<'a> {
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
    if let Some(curr) = ctx.meta.src_loc_at(*offset) {
      if *offset > 0 {
        if let Some(prev) = ctx.meta.src_loc_at(offset - 1) {
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
      Opcode::Ret => {
        let local_count = inst.data::<usize>();
        write!(f, "{} {}", Self::opcode_column("Ret"), Self::value_column(local_count))
      }
      x => write!(f, "{}", Self::opcode_column(format!("{:?}", x))),
    }
  }
}

#[derive(Default)]
pub struct Stack(Vec<Value>);

impl Stack {
  pub(crate) fn with_capacity(sz: usize) -> Self {
    Self(Vec::with_capacity(sz))
  }
}

impl Display for Stack {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if self.is_empty() {
      write!(f, "               | [ ]")
    } else {
      let cols = util::terminal_width().map(|c| c.saturating_sub(22)).unwrap_or(64);
      let formatted = self.iter().rev().enumerate().map(|(index, item)| {
        let mut vs = format!("{item:?}");
        if vs.len() > cols {
          vs = format!("{}...", &vs[0..cols.saturating_sub(3)]);
        }
        format!("{:#15}| [ {} ]", self.len() - 1 - index, vs)
      });

      let look = itertools::join(formatted, "\n");

      write!(f, "{look}")
    }
  }
}

impl Deref for Stack {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Stack {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

// A structure representing stack frame information
#[derive(Default)]
pub struct StackFrame {
  /// when the jtbl feature is enabled this needs to be a pointer so that when the vector of stack frames is reallocated C++ doesn't reference invalid memory
  #[cfg(feature = "jtbl")]
  ip: Box<usize>,

  /// The instruction pointer of the current stack frame
  #[cfg(not(feature = "jtbl"))]
  ip: usize,

  /// The base pointer to where arguments to functions, or local variables if there are no arguments, are located on the stack
  pub bp: usize,

  /// The context of the current function or file source
  pub ctx: SmartPtr<Context>,

  /// The export of the current function or file
  pub export: Option<Value>,

  pub is_req: bool,

  pub module_index: usize,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>, bp: usize, is_req: bool, module_index: usize) -> Self {
    Self {
      ip: Default::default(),
      bp,
      ctx,
      export: None,
      is_req,
      module_index,
    }
  }

  /// Convenience function to return the ip as a usize regardless of implementation
  pub fn ip(&self) -> usize {
    #[cfg(feature = "jtbl")]
    {
      *self.ip
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip
    }
  }

  /// Convenience function to increment the ip regardless of implementation
  pub fn ip_inc(&mut self, offset: usize) {
    #[cfg(feature = "jtbl")]
    {
      *self.ip += offset;
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip += offset;
    }
  }

  /// Convenience function to decrement the ip regardless of implementation
  pub fn ip_dec(&mut self, offset: usize) {
    #[cfg(feature = "jtbl")]
    {
      *self.ip -= offset;
    }

    #[cfg(not(feature = "jtbl"))]
    {
      self.ip -= offset;
    }
  }

  /// Convenience function to get the ip as a pointer, for use when passing it to C++
  #[cfg(feature = "jtbl")]
  pub fn ip_ptr(&mut self) -> *mut usize {
    &mut *self.ip as *mut usize
  }
}

/// The stack of modules in use, with the last being the parent of all future modules
///
/// Functions mostly like a vector otherwise
#[derive(Default)]
pub(crate) struct ModuleStack {
  envs: Vec<UsertypeHandle<ModuleValue>>,
}

impl ModuleStack {
  pub(crate) fn iter(&self) -> std::slice::Iter<UsertypeHandle<ModuleValue>> {
    self.envs.iter()
  }

  pub(crate) fn len(&self) -> usize {
    self.envs.len()
  }

  pub(crate) fn push(&mut self, entry: UsertypeHandle<ModuleValue>) {
    self.envs.push(entry);
  }

  pub(crate) fn pop(&mut self) -> UsertypeHandle<ModuleValue> {
    self.envs.pop().expect("pop: the env stack should never be empty")
  }

  pub(crate) fn last(&self) -> &UsertypeHandle<ModuleValue> {
    self.envs.last().expect("last: the env stack should never be empty")
  }

  pub(crate) fn last_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    self.envs.last_mut().expect("last_mut: the env stack should never be empty")
  }

  pub(crate) fn truncate(&mut self, to: usize) {
    self.envs.truncate(to);
  }
}

#[derive(Default)]
pub struct Cache {
  // Static
  pub(crate) consts: Vec<ConstantValue>,
  pub(crate) strings: bimap::BiBTreeMap<ConstIndex, String>,
  globals: RapidHashMap<ConstIndex, Value>,
  libs: BTreeMap<FileIdType, Value>,

  // Can be cleared from gc cleaning
  mods: RapidHashMap<Value, RapidHashMap<ConstIndex, Value>>,
}

impl Cache {
  pub fn add_const(&mut self, const_val: impl Into<ConstantValue>) -> usize {
    let c = const_val.into();

    let string = match &c {
      ConstantValue::String(string) => {
        if let Some(index) = self.strings.get_by_right(string.as_str()) {
          return *index;
        }
        Some(string.clone())
      }
      _ => None,
    };

    let index = self.consts.len();
    self.consts.push(c);

    if let Some(string) = string {
      self.strings.insert(index, string);
    }

    index
  }

  pub fn const_at(&self, index: impl Into<usize>) -> &ConstantValue {
    &self.consts[index.into()]
  }

  pub fn get_const(&self, index: impl Into<usize>) -> Option<&ConstantValue> {
    self.consts.get(index.into())
  }

  pub fn consts(&self) -> &Vec<ConstantValue> {
    &self.consts
  }

  pub(crate) fn set_global(&mut self, id: impl Into<ConstIndex>, value: Value) -> bool {
    self.globals.insert(id.into(), value).is_none()
  }

  pub(crate) fn get_global_by_name(&self, name: &str) -> Option<Value> {
    self.strings.get_by_right(name).and_then(|idx| self.globals.get(idx)).cloned()
  }

  pub(crate) fn find_var(&self, env: &UsertypeHandle<ModuleValue>, key: impl Into<ConstIndex>) -> Option<Value> {
    let key = key.into();
    self
      .mods
      .get(&env.value())
      .and_then(|m| m.get(&key))
      .or_else(|| self.globals.get(&key))
      .cloned()
  }

  pub(crate) fn resolve_mod(&self, value: Value, key: impl Into<ConstIndex>) -> Option<Value> {
    self.mods.get(&value).and_then(|m| m.get(&key.into())).cloned()
  }

  pub(crate) fn add_lib(&mut self, id: FileIdType, value: Value) {
    self.libs.insert(id, value);
  }

  pub(crate) fn get_lib(&self, id: FileIdType) -> Option<Value> {
    self.libs.get(&id).cloned()
  }

  pub(crate) fn add_to_mod(&mut self, module: Value, key: impl Into<ConstIndex>, value: Value) {
    self.mods.entry(module).or_default().insert(key.into(), value);
  }

  pub(crate) fn deep_trace(&self, marker: &mut Tracer) {
    for value in self.globals.values().chain(self.libs.values()).cloned() {
      marker.deep_trace(value);
    }
  }

  pub(crate) fn incremental_trace(&self, marker: &mut Tracer) {
    for value in self.globals.values().chain(self.libs.values()).cloned() {
      marker.try_mark_gray(value);
    }
  }

  pub(crate) fn forget(&mut self, value: Value) {
    self.mods.remove(&value);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const ADDR: usize = 123;
  const CONST: usize = 1;

  #[test]
  fn opcode_serde_simple_inst() {
    Instruction::new(Opcode::Const, CONST).unwrap();
  }

  #[test]
  fn opcode_serde_addr() {
    let addr = LongAddr(ADDR);
    let bits = addr.encode().unwrap();
    assert_eq!(bits, ADDR as u64);
    let addr = LongAddr::decode_checked(bits).unwrap();
    assert_eq!(addr, LongAddr(ADDR));
  }

  #[test]
  fn opcode_serde_cplx_inst() {
    let inst = Instruction::new(Opcode::Load, (Storage::Local, LongAddr(ADDR))).unwrap();

    let op = inst.opcode();
    let (storage, addr) = inst.data::<(Storage, LongAddr)>();

    assert_eq!(op, Opcode::Load);
    assert_eq!(storage, Storage::Local);
    assert_eq!(addr, LongAddr(ADDR));
  }

  #[test]
  fn opcode_serde_v_cplx_inst() {
    const A_ADDR: usize = 12;
    const B_ADDR: usize = 34;
    let inst = Instruction::new(
      Opcode::Add,
      (Storage::Local, ShortAddr(A_ADDR), Storage::Global, ShortAddr(B_ADDR)),
    )
    .unwrap();

    let op = inst.opcode();
    let ((a_store, a_addr), (b_store, b_addr)) = inst.data::<((Storage, ShortAddr), (Storage, ShortAddr))>();

    assert!(inst.has_data());
    assert_eq!(op, Opcode::Add);
    assert_eq!(a_store, Storage::Local);
    assert_eq!(a_addr, ShortAddr(A_ADDR));
    assert_eq!(b_store, Storage::Global);
    assert_eq!(b_addr, ShortAddr(B_ADDR));
  }
}
