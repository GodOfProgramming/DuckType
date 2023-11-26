use crate::{prelude::*, FastHashMap};
use std::{
  collections::hash_map::Entry,
  iter::{self, Once},
};

#[derive(Default, Usertype, NoMethods)]
#[uuid("fc79ffad-9286-4188-9905-76ae73108f9e")]
pub struct ModuleValue {
  name: Option<String>,

  #[trace]
  pub env: FastHashMap<String, Value>,
  #[trace]
  pub parent: Value,
}

impl ModuleValue {
  pub fn new(name: impl ToString) -> Self {
    Self {
      name: Some(name.to_string()),
      ..Default::default()
    }
  }

  fn new_global_module(name: impl ToString) -> Self {
    Self::new(name)
  }

  pub(crate) fn new_child(name: impl ToString, parent: Value) -> Self {
    Self {
      name: Some(name.to_string()),
      env: Default::default(),
      parent,
    }
  }

  pub(crate) fn new_scope(parent: Value) -> Self {
    Self {
      name: None,
      env: Default::default(),
      parent,
    }
  }

  /// Defines a new variable. Returns true if the variable is new, false otherwise
  ///
  /// Calling this after the module becomes in use will cause caching issues
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
        .map(|m| m.assign(&name, value))
        .unwrap_or(false)
    }
  }

  /// Looks up the value specified in the current module
  pub fn resolve<T: AsRef<str>>(&self, name: T) -> Option<Value> {
    self.env.get(name.as_ref()).cloned()
  }

  /// Looks up the value specified in the current module
  pub fn lookup_path<'t, I, T>(&'t self, path: &'t I) -> UsageResult<Option<Value>>
  where
    I: IntoModPath<'t, T> + 't,
    T: AsRef<str> + 't,
  {
    let mut retval: Option<Value> = None;

    for part in path.mod_path_iter() {
      match retval {
        Some(rt) => retval = Some(rt.resolve(part)?),
        None => retval = self.lookup(part),
      }
    }

    Ok(retval)
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
  fn get_field(&self, _: &mut Vm, _field: Field) -> UsageResult<Option<Value>> {
    Err(UsageError::Immutable(self.__str__()))
  }

  fn set_field(&mut self, _: &mut Vm, _: Field, _: Value) -> UsageResult<()> {
    Err(UsageError::Immutable(self.__str__()))
  }
}

impl Operators for ModuleValue {
  fn __def__(&mut self, field: &str, value: Value) -> UsageResult<bool> {
    Ok(self.define(field, value))
  }

  fn __res__(&self, field: &str) -> UsageResult {
    self.resolve(field).ok_or(UsageError::NameError(field.to_string()))
  }

  fn __str__(&self) -> String {
    self
      .name
      .as_ref()
      .map(|name| format!("<mod {}>", name))
      .unwrap_or_else(|| String::from("<anonymous mod>"))
  }

  fn __dbg__(&self) -> String {
    self
      .name
      .as_ref()
      .map(|name| {
        format!(
          "<mod {} {{ {} }}>",
          name,
          itertools::join(self.env.iter().map(|(k, v)| format!("{k}: {v}")), ", "),
        )
      })
      .unwrap_or_else(|| String::from("<anonymous mod>"))
  }
}

pub enum ModuleType<T>
where
  T: ToString,
{
  Global { name: T },
  Child { name: T, parent: Value },
}

impl<T> ModuleType<T>
where
  T: ToString,
{
  pub fn new_global(name: T) -> Self {
    Self::Global { name }
  }

  pub fn new_child(name: T, parent: Value) -> Self {
    Self::Child { name, parent }
  }
}

pub struct ModuleBuilder;

impl ModuleBuilder {
  pub fn initialize<T, F>(vm: &mut Vm, module_type: ModuleType<T>, f: F) -> UsertypeHandle<ModuleValue>
  where
    T: ToString,
    F: FnOnce(&mut Vm, UsertypeHandle<ModuleValue>),
  {
    let module = match module_type {
      ModuleType::Global { name } => ModuleValue::new_global_module(name),
      ModuleType::Child { name, parent } => ModuleValue::new_child(name, parent),
    };

    let module = vm.make_usertype_handle_from(module);

    f(vm, module.clone());

    module
  }
}

pub trait IntoModPath<'t, T>
where
  T: AsRef<str> + 't,
{
  type Iter: Iterator<Item = &'t T> + 't;
  fn mod_path_iter(&'t self) -> Self::Iter;
}

impl<'t, T> IntoModPath<'t, T> for T
where
  T: AsRef<str> + 't,
{
  type Iter = Once<&'t T>;
  fn mod_path_iter(&'t self) -> Self::Iter {
    iter::once(self)
  }
}

impl<'t, T> IntoModPath<'t, T> for [T]
where
  T: AsRef<str> + 't,
{
  type Iter = std::slice::Iter<'t, T>;
  fn mod_path_iter(&'t self) -> Self::Iter {
    self.iter()
  }
}

impl<'t, T, const N: usize> IntoModPath<'t, T> for [T; N]
where
  T: AsRef<str> + 't,
{
  type Iter = std::slice::Iter<'t, T>;
  fn mod_path_iter(&'t self) -> Self::Iter {
    self.iter()
  }
}
