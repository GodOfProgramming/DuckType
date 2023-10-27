use crate::prelude::*;
use std::{
  collections::{btree_map::Entry, BTreeMap},
  env,
};

pub const PATHS_ENV_VAR: &str = "SS_LIBRARY_PATHS";
pub const LIB_GLOBAL: &str = "$G";
pub const PATHS_MEMBER: &str = "paths";
pub const PATH_SEPARATOR: char = ';';

#[derive(Default, Usertype)]
#[uuid("fc79ffad-9286-4188-9905-76ae73108f9e")]
pub struct ModuleValue {
  #[trace]
  pub members: BTreeMap<String, Value>,
  #[trace]
  pub env: BTreeMap<String, Value>,
  #[trace]
  pub parent: Value,
}

impl ModuleValue {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn new_global_module(gc: &mut SmartPtr<Gc>) -> UsertypeHandle<Self> {
    let mut this = Gc::allocate_handle(gc, Self::default());

    let mut lib_paths = Vec::default();

    if let Ok(paths) = env::var(PATHS_ENV_VAR) {
      lib_paths.extend(paths.split_terminator(PATH_SEPARATOR).map(|v| gc.allocate(v)));
    }

    let module = ModuleBuilder::initialize(gc, Some(this.handle.value.clone()), |gc, mut lib| {
      let lib_paths = gc.allocate(lib_paths);
      lib.define(PATHS_MEMBER, lib_paths);
    });

    this.define(LIB_GLOBAL, module);

    this
  }

  pub(crate) fn new_child(parent: Value) -> Self {
    Self {
      members: Default::default(),
      env: Default::default(),
      parent,
    }
  }

  /// Defines a new variable. Returns true if the variable is new, false otherwise
  pub fn define(&mut self, name: impl Into<String>, value: impl Into<Value>) -> bool {
    self.env.insert(name.into(), value.into()).is_none()
  }

  /// Assigns to an existing variable. Returns true if the variable already exists, false otherwise
  pub fn assign(&mut self, name: impl Into<String>, value: impl Into<Value>) -> bool {
    let name = name.into();
    if let Entry::Occupied(mut e) = self.env.entry(name.clone()) {
      e.insert(value.into());
      true
    } else {
      self
        .parent
        .cast_to_mut::<Self>()
        .map(|m| m.assign(name, value))
        .unwrap_or(false)
    }
  }

  /// Looks up the value specified in the current module
  pub fn resolve<T: AsRef<str>>(&self, name: T) -> Option<Value> {
    self.env.get(name.as_ref()).cloned()
  }

  pub fn search_for(&self, depth: usize, name: impl Into<String>) -> Option<usize> {
    let name = name.into();
    if self.env.contains_key(&name) {
      Some(depth)
    } else {
      self.parent.cast_to::<Self>().and_then(|m| m.search_for(depth + 1, name))
    }
  }

  /// Looks up the value specified, and if not found traverses the ancestry
  pub fn lookup<T: AsRef<str>>(&self, name: T) -> Option<Value> {
    self
      .env
      .get(name.as_ref())
      .cloned()
      .or_else(|| self.parent.cast_to::<Self>().and_then(|module| module.lookup(name)))
  }
}

impl UsertypeFields for ModuleValue {
  fn get_field(&self, _gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
    self
      .members
      .get(field)
      .cloned()
      .map(Ok)
      .or_else(|| Some(Err(ValueError::UndefinedMember(field.to_string()))))
      .transpose()
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
    self.members.insert(field.to_string(), value);
    Ok(())
  }
}

#[methods]
impl ModuleValue {
  fn __def__(&mut self, field: &str, value: Value) -> ValueResult<bool> {
    Ok(self.define(field, value))
  }

  fn __res__(&self, field: &str) -> ValueResult {
    self.resolve(field).ok_or(ValueError::NameError(field.to_string()))
  }

  fn __dbg__(&self) -> String {
    format!("mod {:#?} {:#?}", self.env, self.members)
  }
}

pub struct ModuleBuilder;

impl ModuleBuilder {
  pub fn initialize<F>(gc: &mut SmartPtr<Gc>, parent: Option<Value>, f: F) -> UsertypeHandle<ModuleValue>
  where
    F: FnOnce(&mut SmartPtr<Gc>, UsertypeHandle<ModuleValue>),
  {
    let module = parent
      .map(|parent| Gc::allocate_handle(gc, ModuleValue::new_child(parent)))
      .unwrap_or_else(|| ModuleValue::new_global_module(gc));
    f(gc, module.clone());
    module
  }
}
