use std::{
  collections::BTreeMap,
  fmt::{self, Display, Formatter},
  ops::{Deref, DerefMut},
  sync::atomic::Ordering,
};

use ptr::MutPtr;

#[cfg(test)]
use crate::code::gen::{CAPTURE_OPS, GENERATED_OPS};
use crate::{
  code::{ConstantValue, Reflection},
  prelude::*,
  util::FileIdType,
  FastHashMap, FastHashSet,
};

use super::Stack;
pub mod prelude {
  pub use super::{Cache, Context, UsertypeHandle, ValueHandle};
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
