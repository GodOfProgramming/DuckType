mod env;
pub mod memory;

use crate::{prelude::*, util::FileIdType};
use dlopen2::wrapper::WrapperApi;
use enum_map::{Enum, EnumMap};
use ptr::SmartPtr;
use std::{
  fmt::{self, Debug, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
  path::PathBuf,
};
use strum::EnumCount;
use strum_macros::{EnumCount, EnumIter, FromRepr};

pub mod prelude {
  pub use super::{env::prelude::*, memory::*};
  #[allow(unused_imports)]
  pub(crate) use super::{Instruction, InstructionData, LongAddr, Opcode, ShortAddr, Storage, TryIntoInstruction};
}

#[derive(WrapperApi)]
pub(crate) struct NativeApi {
  simple_script_load_module: fn(vm: &mut Vm) -> Result<Value, Error>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount, EnumIter, Enum, FromRepr)]
#[repr(u8)]
pub enum Register {
  A,
  B,
  C,
  D,
  E,
  F,
  G,
  H,
}

impl InstructionData for Register {
  const BITS: u64 = 8;

  fn to_bits(self) -> u64 {
    self as u8 as u64
  }

  fn checked_data(inst: u64) -> Option<Self> {
    Self::from_repr((inst & Self::MASK).try_into().ok()?)
  }

  fn unchecked_data(inst: u64) -> Self {
    unsafe { mem::transmute((inst & Self::MASK) as u8) }
  }
}

#[derive(Default)]
pub struct StackFrame {
  pub ip: usize,
  pub sp: usize,
  pub ctx: SmartPtr<Context>,
  pub registers: Vec<EnumMap<Register, Value>>,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>, sp: usize) -> Self {
    Self {
      ip: Default::default(),
      sp,
      ctx,
      registers: Default::default(),
    }
  }

  /**
   * Clear the current stack frame, returning the previous
   */
  pub fn clear_out(&mut self) -> Self {
    let mut old = Self::default();
    mem::swap(&mut old, self);
    old
  }

  pub fn reg_store(&mut self, reg: Register, value: Value) {
    let sz = self.registers.len();
    self.registers[sz - 1][reg] = value;
  }

  pub fn reg_load(&self, reg: Register) -> Value {
    self.registers[self.registers.len() - 1][reg].clone()
  }

  pub fn new_reg_ctx(&mut self) {
    self.registers.push(Default::default());
  }

  pub fn pop_reg_ctx(&mut self) {
    self.registers.pop();
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
      let formatted = self
        .iter()
        .enumerate()
        .map(|(index, item)| format!("{:#15}| [ {:?} ]", index, item));
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

pub(crate) struct FileInfo {
  pub(crate) path: PathBuf,
  pub(crate) id: FileIdType,
}

impl FileInfo {
  pub(crate) fn new(path: impl Into<PathBuf>, id: FileIdType) -> Self {
    Self { path: path.into(), id }
  }
}

#[derive(Default)]
pub(crate) struct EnvStack {
  envs: Vec<EnvEntry>,
}

impl EnvStack {
  pub(crate) fn len(&self) -> usize {
    self.envs.len()
  }

  pub(crate) fn push(&mut self, entry: EnvEntry) {
    self.envs.push(entry);
  }

  pub(crate) fn pop(&mut self) -> EnvEntry {
    self.envs.pop().expect("pop: the env stack should never be empty")
  }

  pub(crate) fn last(&self) -> &UsertypeHandle<ModuleValue> {
    match self.envs.last().expect("last: the env stack should never be empty") {
      EnvEntry::Fn(e) => e,
      EnvEntry::Mod(e) => e,
      EnvEntry::File(e) => e,
      EnvEntry::Block(e) => e,
      EnvEntry::String(e) => e,
    }
  }

  pub(crate) fn last_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    match self.envs.last_mut().expect("last_mut: the env stack should never be empty") {
      EnvEntry::Fn(e) => e,
      EnvEntry::Mod(e) => e,
      EnvEntry::File(e) => e,
      EnvEntry::Block(e) => e,
      EnvEntry::String(e) => e,
    }
  }
}

pub(crate) enum EnvEntry {
  Fn(UsertypeHandle<ModuleValue>),
  Mod(UsertypeHandle<ModuleValue>),
  File(UsertypeHandle<ModuleValue>),
  Block(UsertypeHandle<ModuleValue>),
  String(UsertypeHandle<ModuleValue>),
}

impl Debug for EnvEntry {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fn(_) => f.debug_tuple("Fn").finish(),
      Self::Mod(_) => f.debug_tuple("Mod").finish(),
      Self::File(_) => f.debug_tuple("File").finish(),
      Self::Block(_) => f.debug_tuple("Block").finish(),
      Self::String(_) => f.debug_tuple("String").finish(),
    }
  }
}

pub(crate) enum ExecType {
  String,
  File,
  Fn,
}

pub trait InstructionData
where
  Self: Sized,
{
  const BITS: u64;
  const MASK: u64 = 2u64.pow(Self::BITS as u32) - 1;

  fn to_bits(self) -> u64;

  fn checked_data(inst: u64) -> Option<Self>;

  fn unchecked_data(inst: u64) -> Self;

  fn bits(self) -> Option<u64> {
    Self::valid_bits(self.to_bits())
  }

  fn valid_bits(bits: u64) -> Option<u64> {
    (bits <= Self::MASK).then_some(bits)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction(u64);

impl Instruction {
  pub fn new<D>(opcode: Opcode, data: D) -> Option<Self>
  where
    D: InstructionData,
  {
    let inst = opcode.bits()? | data.bits()? << Opcode::BITS;
    Some(Self(inst))
  }

  pub fn opcode(&self) -> Option<Opcode> {
    Opcode::checked_data(self.0 & Opcode::MASK)
  }

  #[cfg(not(debug_assertions))]
  pub fn data<T>(&self) -> T
  where
    T: InstructionData,
  {
    T::unchecked_data(self.0 >> Opcode::BITS)
  }

  #[cfg(debug_assertions)]
  pub fn data<T>(&self) -> Option<T>
  where
    T: InstructionData,
  {
    T::checked_data(self.0 >> Opcode::BITS)
  }

  /// Returns unchecked data meant for display and debugging purposes
  pub fn display_data<T>(&self) -> T
  where
    T: InstructionData,
  {
    T::unchecked_data(self.0 >> Opcode::BITS)
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
    Instruction::new(self, 0).ok_or(self)
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

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, strum_macros::EnumCount, strum_macros::FromRepr)]
#[repr(u8)]
pub enum Opcode {
  /// Unknown instruction
  /// Value given when one cannot be interpreted
  ///
  /// Encoding: None
  #[default]
  Unknown,
  /// No operation instruction
  ///
  /// Encoding: None
  NoOp,
  /// Looks up a constant value at the specified location.
  ///
  /// Encoding: | usize |
  Const,
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
  /// Pops a value off the stack
  ///
  /// Encoding: None
  Pop,
  /// Pops N values off the stack.
  ///
  /// Encoding: | usize |
  PopN,
  /// Store the value on the stack in the given location
  ///
  /// Encoding: | Storage | LongAddr |
  Store,
  /// Load a value and push it onto the stack
  ///
  /// Encoding: | Storage | LongAddr |
  Load,
  /// Assigns a value to a member on an object
  ///
  /// \[ Value \] \
  /// \[ Object \]
  ///
  /// Encoding: | usize |
  AssignMember,
  /// Initializes a member of an object, keeping the object on the stack for further assignments
  ///
  /// Encoding: | usize |
  InitializeMember,
  /// Initializes a method on a class, keeping the class on the stack for further assignments
  ///
  /// Encoding: | usize |
  InitializeMethod,
  /// Initializes the constructor on a class, keeping the class on the stack for further assignments
  ///
  /// Encoding: None
  InitializeConstructor,
  /// Looks up the member on the next value on the stack, replacing it with the member's value
  ///
  /// Encoding: | usize |
  LookupMember,
  /// Looks up the member of the next value on the stack, pushing the value
  ///
  /// Encoding: | usize |
  PeekMember,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  Equal,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  NotEqual,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  Greater,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  GreaterEqual,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  Less,
  /// Pops two values off the stack, compares, then pushes the result back on
  ///
  /// Encoding: None
  LessEqual,
  /// Pops a value off the stack, and compares it with the peeked value, pushing the new value on
  ///
  /// Encoding: None
  Check,
  /// Pops two values off the stack, calculates the sum, then pushes the result back on
  ///
  /// Encoding: None
  Add,
  /// Pops two values off the stack, calculates the difference, then pushes the result back on
  ///
  /// Encoding: None
  Sub,
  /// Pops two values off the stack, calculates the product, then pushes the result back on
  ///
  /// Encoding: None
  Mul,
  /// Pops two values off the stack, calculates the quotient, then pushes the result back on
  ///
  /// Encoding: None
  Div,
  /// Pops two values off the stack, calculates the remainder, then pushes the result back on
  ///
  /// Encoding: None
  Rem,
  /// Peeks at the stack. If the top value is true, the ip in incremented
  ///
  /// Encoding: | usize |
  Or,
  /// Peeks at the stack. If the top value is false, the ip is incremented
  ///
  /// Encoding: | usize |
  And,
  /// Pops a value off the stack, inverts its truthy value, then pushes that back on
  ///
  /// Encoding: None
  Not,
  /// Pops a value off the stack, inverts its numerical value, then pushes that back on
  ///
  /// Encoding: None
  Negate,
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
  /// Swaps the last two items on the stack and pops
  ///
  /// Encoding: None
  SwapPop,
  /// Exits from a function, returning nil on the previous frame
  ///
  /// Encoding: None
  Ret,
  /// Load an external file, or pull from the cache if already loaded.
  /// The file name is the value on the stack
  ///
  /// Encoding: None
  Req,
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
  /// Halt the VM when this instruction is reached and enter the debugger
  ///
  /// Encoding: None
  Breakpoint,
  /// Mark the current value as exported
  ///
  /// Encoding: None
  Export,
  /// Defines the identifier on the variable
  /// The const in the encoding is the name
  ///
  /// Encoding: | usize |
  Define,
  /// Resolve the specified identifier
  /// The const in the encoding is the name
  ///
  /// Encoding: | usize |
  Resolve,
  /// Push a new env
  ///
  /// Encoding: None
  EnterBlock,
  /// Pop an env
  ///
  /// Encoding: None
  PopScope,
  /// Push a register context
  /// TODO remove this, it's a crap solution
  ///
  /// Encoding: None
  PushRegCtx,
  /// Pop a register context
  ///
  /// Encoding: None
  PopRegCtx,
  /// Panic duck style
  ///
  /// Encoding: None
  Quack,
}

static_assertions::const_assert!(Opcode::COUNT - 1 < 2usize.pow(Opcode::BITS as u32));

impl InstructionData for Opcode {
  const BITS: u64 = 8;

  fn to_bits(self) -> u64 {
    self as u8 as u64
  }

  fn checked_data(inst: u64) -> Option<Self> {
    Self::from_repr((inst & Self::MASK).try_into().ok()?)
  }

  fn unchecked_data(inst: u64) -> Self {
    unsafe { mem::transmute((inst & Self::MASK) as u8) }
  }
}

impl InstructionData for usize {
  const BITS: u64 = (mem::size_of::<usize>() * 8 - Opcode::BITS as usize) as u64;

  fn to_bits(self) -> u64 {
    self as u64
  }

  fn checked_data(inst: u64) -> Option<Self> {
    (inst & Self::MASK).try_into().ok()
  }

  fn unchecked_data(inst: u64) -> Self {
    (inst & Self::MASK) as usize
  }
}

#[derive(Debug, PartialEq, Eq, strum_macros::EnumCount, strum_macros::FromRepr)]
#[repr(u8)]
pub enum Storage {
  Local,
  Global,
  Reg,
}

static_assertions::const_assert!(Storage::COUNT - 1 < 2usize.pow(Storage::BITS as u32));

impl InstructionData for Storage {
  const BITS: u64 = 2;

  fn to_bits(self) -> u64 {
    self as u8 as u64
  }

  fn checked_data(inst: u64) -> Option<Self> {
    Self::from_repr((inst & Self::MASK).try_into().ok()?)
  }

  fn unchecked_data(inst: u64) -> Self {
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

  fn checked_data(inst: u64) -> Option<Self> {
    Some(Self::unchecked_data(inst))
  }

  fn unchecked_data(inst: u64) -> Self {
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

  fn checked_data(inst: u64) -> Option<Self> {
    (inst & Self::MASK).try_into().map(Self).ok()
  }

  fn unchecked_data(inst: u64) -> Self {
    Self((inst & Self::MASK) as usize)
  }
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

  fn checked_data(inst: u64) -> Option<Self> {
    T0::checked_data(inst & T0::MASK).zip(T1::checked_data(inst >> T0::BITS & T1::MASK))
  }

  fn unchecked_data(inst: u64) -> Self {
    (
      T0::unchecked_data(inst & T0::MASK),
      T1::unchecked_data(inst >> T0::BITS & T1::MASK),
    )
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

  fn checked_data(inst: u64) -> Option<Self> {
    match (
      T0::checked_data(inst & T0::MASK),
      T1::checked_data(inst >> T0::BITS & T1::MASK),
      T2::checked_data(inst >> (T0::BITS + T1::BITS) & T2::MASK),
    ) {
      (Some(t0), Some(t1), Some(t2)) => Some((t0, t1, t2)),
      _ => None,
    }
  }

  fn unchecked_data(inst: u64) -> Self {
    (
      T0::unchecked_data(inst & T0::MASK),
      T1::unchecked_data(inst >> T0::BITS & T1::MASK),
      T2::unchecked_data(inst >> (T0::BITS + T1::BITS) & T2::MASK),
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

  fn checked_data(inst: u64) -> Option<Self> {
    match (
      T0::checked_data(inst & T0::MASK),
      T1::checked_data(inst >> T0::BITS & T1::MASK),
      T2::checked_data(inst >> (T0::BITS + T1::BITS) & T2::MASK),
      T3::checked_data(inst >> (T0::BITS + T1::BITS + T2::BITS) & T3::MASK),
    ) {
      (Some(t0), Some(t1), Some(t2), Some(t3)) => Some((t0, t1, t2, t3)),
      _ => None,
    }
  }

  fn unchecked_data(inst: u64) -> Self {
    (
      T0::unchecked_data(inst & T0::MASK),
      T1::unchecked_data(inst >> T0::BITS & T1::MASK),
      T2::unchecked_data(inst >> (T0::BITS + T1::BITS) & T2::MASK),
      T3::unchecked_data(inst >> (T0::BITS + T1::BITS + T2::BITS) & T3::MASK),
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn opcode_serde() {
    const ADDR: usize = 123;
    const CONST: usize = 1;
    let addr = LongAddr(ADDR);

    {
      Instruction::new(Opcode::Const, CONST).unwrap();
    }

    {
      let bits = addr.bits().unwrap();
      assert_eq!(bits, ADDR as u64);
      let addr = LongAddr::checked_data(bits).unwrap();
      assert_eq!(addr, LongAddr(ADDR));
    }

    {
      let inst = Instruction::new(Opcode::Load, (Storage::Local, LongAddr(ADDR))).unwrap();

      let op = inst.opcode().unwrap();
      let (storage, addr) = inst.data::<(Storage, LongAddr)>().unwrap();

      assert_eq!(op, Opcode::Load);
      assert_eq!(storage, Storage::Local);
      assert_eq!(addr, LongAddr(ADDR));
    }

    {
      const A_ADDR: usize = 12;
      const B_ADDR: usize = 34;
      let inst = Instruction::new(
        Opcode::Add,
        (Storage::Local, ShortAddr(A_ADDR), Storage::Global, ShortAddr(B_ADDR)),
      )
      .unwrap();

      let op = inst.opcode().unwrap();
      let ((a_store, a_addr), (b_store, b_addr)) = inst.data::<((Storage, ShortAddr), (Storage, ShortAddr))>().unwrap();

      assert_eq!(op, Opcode::Add);
      assert_eq!(a_store, Storage::Local);
      assert_eq!(a_addr, ShortAddr(A_ADDR));
      assert_eq!(b_store, Storage::Global);
      assert_eq!(b_addr, ShortAddr(B_ADDR));
    }
  }
}
