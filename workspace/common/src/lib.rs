pub mod errors;
pub mod reflect;
pub mod util;
pub mod value;

use ahash::RandomState;
pub use errors::Error;
use ptr::SmartPtr;
use reflect::{ContextDisassembler, Reflection};
use strum::EnumCount;
use strum_macros::{EnumCount, EnumIter, FromRepr};
use util::FileIdType;
use value::Value;

use std::{
  collections::{BTreeMap, HashMap, HashSet},
  fmt::{self, Debug, Display, Formatter},
  mem,
  ops::{Deref, DerefMut},
  str::Utf8Error,
};

pub type FastHashSet<T> = HashSet<T, RandomState>;
pub type FastHashMap<K, V> = HashMap<K, V, RandomState>;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,
  Comma,
  Dot,
  Semicolon,
  At,
  Pipe,

  // One or two character tokens.
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Arrow,
  BackArrow,
  Plus,
  PlusEqual,
  Minus,
  MinusEqual,
  Asterisk,
  AsteriskEqual,
  Slash,
  SlashEqual,
  Percent,
  PercentEqual,
  Colon,
  ColonColon,

  // Literals.
  Identifier(String),
  String(String),
  Number(NumberToken),

  // Keywords.
  And,
  As,
  Break,
  Class,
  Cont,
  Else,
  Export,
  False,
  For,
  Fn,
  If,
  Is,
  Let,
  Loop,
  Match,
  Mod,
  New,
  Nil,
  Or,
  Println,
  Quack,
  Req,
  Ret,
  Struct,
  True,
  Use,
  While,

  // Special
  Breakpoint,
}

impl Token {
  /// Gets the char repr of a single character token
  ///
  /// Returns None if not a single char token
  pub fn chr(&self) -> Option<char> {
    match self {
      Self::RightParen => Some('('),
      Self::Pipe => Some('|'),
      _ => None,
    }
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Identifier(i) => write!(f, "Identifier ({})", i),
      Self::String(s) => write!(f, "String ({})", s),
      Self::Number(n) => write!(
        f,
        "Number ({})",
        match n {
          NumberToken::I32(i) => i.to_string(),
          NumberToken::F64(f) => f.to_string(),
        }
      ),
      _ => write!(f, "{:?}", self),
    }
  }
}

impl From<f64> for Token {
  fn from(v: f64) -> Self {
    Self::Number(NumberToken::F64(v))
  }
}

impl From<i32> for Token {
  fn from(v: i32) -> Self {
    Self::Number(NumberToken::I32(v))
  }
}

impl TryFrom<&[u8]> for Token {
  type Error = Utf8Error;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Ok(match std::str::from_utf8(bytes)? {
      "and" => Self::And,
      "as" => Self::As,
      "break" => Self::Break,
      "class" => Self::Class,
      "cont" => Self::Cont,
      "else" => Self::Else,
      "export" => Self::Export,
      "false" => Self::False,
      "fn" => Self::Fn,
      "for" => Self::For,
      "if" => Self::If,
      "is" => Self::Is,
      "let" => Self::Let,
      "loop" => Self::Loop,
      "match" => Self::Match,
      "mod" => Self::Mod,
      "new" => Self::New,
      "nil" => Self::Nil,
      "or" => Self::Or,
      "println" => Self::Println,
      "quack" => Self::Quack,
      "req" => Self::Req,
      "ret" => Self::Ret,
      "struct" => Self::Struct,
      "true" => Self::True,
      "use" => Self::Use,
      "while" => Self::While,
      "__breakpoint__" => Self::Breakpoint,
      word => Self::Identifier(String::from(word)),
    })
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberToken {
  I32(i32),
  F64(f64),
}

macro_rules! opstr {
  ($op:ident) => {
    concat!("__", stringify!($op), "__")
  };
}

pub mod ops {
  pub const NOT: &str = opstr!(not);
  pub const NEG: &str = opstr!(neg);

  pub const ADD: &str = opstr!(add);
  pub const SUB: &str = opstr!(sub);
  pub const MUL: &str = opstr!(mul);
  pub const DIV: &str = opstr!(div);
  pub const REM: &str = opstr!(rem);

  pub const EQUALITY: &str = opstr!(eq);
  pub const NOT_EQUAL: &str = opstr!(neq);
  pub const LESS: &str = opstr!(less);
  pub const LESS_EQUAL: &str = opstr!(leq);
  pub const GREATER: &str = opstr!(greater);
  pub const GREATER_EQUAL: &str = opstr!(geq);

  pub const INDEX: &str = opstr!(index);
  pub const INDEX_ASSIGN: &str = opstr!(idxeq);

  pub const CALL: &str = opstr!(call);
}

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
#[cfg_attr(feature = "jtbl", macros::opcode_bindings(crate::bindings))]
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
  /// Exits from a function, returning nil on the previous frame
  ///
  /// Encoding: None
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
pub struct LongAddr(pub usize);

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
pub struct ShortAddr(pub usize);

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

#[derive(Debug, Clone)]
pub enum ConstantValue {
  Integer(i32),
  Float(f64),
  String(String),
  StaticString(&'static str),
  Fn(FunctionConstant),
}

impl Display for ConstantValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Integer(v) => write!(f, "{}", v),
      Self::Float(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "{}", v),
      Self::StaticString(v) => write!(f, "{}", v),
      Self::Fn(v) => write!(f, "{}", v.name()),
    }
  }
}

impl From<i32> for ConstantValue {
  fn from(value: i32) -> Self {
    Self::Integer(value)
  }
}

impl From<f64> for ConstantValue {
  fn from(value: f64) -> Self {
    Self::Float(value)
  }
}

impl From<String> for ConstantValue {
  fn from(value: String) -> Self {
    Self::String(value)
  }
}

impl From<&String> for ConstantValue {
  fn from(value: &String) -> Self {
    Self::String(value.clone())
  }
}

impl From<&'static str> for ConstantValue {
  fn from(value: &'static str) -> Self {
    Self::StaticString(value)
  }
}

impl From<FunctionConstant> for ConstantValue {
  fn from(value: FunctionConstant) -> Self {
    Self::Fn(value)
  }
}

#[derive(Clone)]
pub struct FunctionConstant {
  pub airity: usize,
  pub ctx: SmartPtr<Context>,
}

impl FunctionConstant {
  pub fn new(airity: usize, ctx: SmartPtr<Context>) -> Self {
    Self { airity, ctx }
  }

  fn name(&self) -> &str {
    self.ctx.meta.name.as_deref().unwrap_or("<lambda>")
  }
}

impl Debug for FunctionConstant {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name())
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

  pub fn fetch(&self, index: usize) -> Instruction {
    self.instructions[index]
  }

  pub(crate) fn write(&mut self, inst: Instruction, line: usize, column: usize) {
    #[cfg(test)]
    {
      assertions::CAPTURE_OPS.with_borrow(|should_cap| {
        if *should_cap {
          assertions::GENERATED_OPS.with_borrow_mut(|ops| {
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
      assertions::CAPTURE_OPS.with_borrow(|should_cap| {
        if *should_cap {
          assertions::GENERATED_OPS.with_borrow_mut(|ops| {
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

#[cfg(test)]
mod assertions {
  use super::Opcode;
  use std::{cell::RefCell, collections::HashSet};
  thread_local! {
    pub static CAPTURE_OPS: RefCell<bool> = RefCell::new(false);
    pub static GENERATED_OPS: RefCell<HashSet<Opcode>> = RefCell::new(Default::default());
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
      let cols = termion::terminal_size()
        .map(|(c, _)| c.saturating_sub(22) as usize)
        .unwrap_or(64);
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

type ConstIndex = usize;

#[derive(Default)]
pub struct Cache {
  // Static
  pub(crate) consts: Vec<ConstantValue>,
  pub(crate) strings: bimap::BiBTreeMap<ConstIndex, String>,
  globals: FastHashMap<ConstIndex, Value>,
  libs: BTreeMap<FileIdType, Value>,

  // Can be cleared from gc cleaning
  mods: FastHashMap<Value, FastHashMap<ConstIndex, Value>>,

  pub(crate) native_handles: FastHashSet<Value>,
}

impl Cache {
  pub fn add_const(&mut self, const_val: impl Into<ConstantValue>) -> usize {
    let c = const_val.into();

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

  pub fn const_at(&self, index: impl Into<usize>) -> &ConstantValue {
    &self.consts[index.into()]
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
    for value in self.globals.values().chain(self.libs.values()).chain(&self.native_handles) {
      marker.deep_trace(value);
    }
  }

  pub(crate) fn incremental_trace(&self, marker: &mut Tracer) {
    for value in self.globals.values().chain(self.libs.values()).chain(&self.native_handles) {
      marker.try_mark_gray(value);
    }
  }

  pub(crate) fn make_handle(&mut self, value: Value) -> ValueHandle {
    self.native_handles.insert(value);
    ValueHandle::new(value)
  }

  pub(crate) fn forget(&mut self, value: Value) {
    self.mods.remove(&value);
  }
}

pub struct UsertypeHandle<T>
where
  T: Usertype,
{
  pub(crate) usertype: MutPtr<T>,
  pub handle: ValueHandle,
}

impl<T> UsertypeHandle<T>
where
  T: Usertype,
{
  pub fn new(mut handle: ValueHandle) -> Self {
    Self {
      usertype: MutPtr::new(handle.value.reinterpret_cast_to_mut::<T>()),
      handle,
    }
  }

  pub fn value(&self) -> Value {
    self.handle.value
  }
}

impl<T> Clone for UsertypeHandle<T>
where
  T: Usertype,
{
  fn clone(&self) -> Self {
    Self {
      usertype: self.usertype,
      handle: self.handle.clone(),
    }
  }
}

impl<T> From<UsertypeHandle<T>> for ValueHandle
where
  T: Usertype,
{
  fn from(utype: UsertypeHandle<T>) -> Self {
    utype.handle
  }
}

impl<T> MaybeFrom<ValueHandle> for UsertypeHandle<T>
where
  T: Usertype,
{
  fn maybe_from(handle: ValueHandle) -> Option<Self> {
    if handle.value.is::<T>() {
      Some(UsertypeHandle::new(handle))
    } else {
      None
    }
  }
}

impl<T> Deref for UsertypeHandle<T>
where
  T: Usertype,
{
  type Target = T;
  fn deref(&self) -> &Self::Target {
    &self.usertype
  }
}

impl<T> DerefMut for UsertypeHandle<T>
where
  T: Usertype,
{
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.usertype
  }
}

impl<T> Display for UsertypeHandle<T>
where
  T: Usertype,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.handle)
  }
}

pub struct ValueHandle {
  pub value: Value,
}

impl ValueHandle {
  pub fn new(mut value: Value) -> ValueHandle {
    if value.is_ptr() {
      value.meta_mut().ref_count.fetch_add(1, Ordering::Relaxed);
    }
    Self { value }
  }
}

impl Deref for ValueHandle {
  type Target = Value;
  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl DerefMut for ValueHandle {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.value
  }
}

impl From<ValueHandle> for Value {
  fn from(handle: ValueHandle) -> Self {
    handle.value
  }
}

impl Clone for ValueHandle {
  fn clone(&self) -> Self {
    if self.value.is_ptr() {
      self.value.meta().ref_count.fetch_add(1, Ordering::Relaxed);
    }

    Self { value: self.value }
  }
}

impl Display for ValueHandle {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}

impl Drop for ValueHandle {
  fn drop(&mut self) {
    if self.value.is_ptr() {
      let meta = self.value.meta();

      #[cfg(debug_assertions)]
      let before = meta.ref_count.load(Ordering::Relaxed);

      meta.ref_count.fetch_sub(1, Ordering::Relaxed);

      #[cfg(debug_assertions)]
      {
        let after = meta.ref_count.load(Ordering::Relaxed);

        debug_assert!(before > after);
      }
    }
  }
}

trait BasicVm {}
