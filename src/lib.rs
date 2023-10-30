pub(crate) mod code;
pub(crate) mod dbg;
pub(crate) mod exec;
pub(crate) mod value;

pub mod stdlib;

mod util;

#[cfg(test)]
mod test;

pub mod prelude {
  pub use super::dbg::prelude::*;
  pub use super::exec::prelude::*;
  pub use super::stdlib;
  pub use super::value::prelude::*;
  pub use macros::*;
  pub use ptr::SmartPtr;
}

pub mod macro_requirements {
  pub use crate::prelude::{
    methods, native, Args, DebugValue, DisplayValue, Fields, MaybeFrom, ModuleBuilder, TryUnwrapArg, Usertype, UsertypeFields,
    UsertypeMethods, Value, ValueError, ValueResult, Vm,
  };
  pub use uuid;
}

pub(crate) trait UnwrapAnd<T> {
  fn unwrap_and(self, f: impl FnOnce(T));
}

impl<T> UnwrapAnd<T> for Option<T> {
  fn unwrap_and(self, f: impl FnOnce(T)) {
    if let Some(inner) = self {
      f(inner);
    }
  }
}
