pub(crate) mod code;
pub(crate) mod dbg;
pub(crate) mod exec;
pub(crate) mod stdlib;
pub(crate) mod value;

mod util;

#[cfg(test)]
mod test;

pub mod prelude {
  pub use super::dbg::prelude::*;
  pub use super::exec::prelude::*;
  pub(crate) use super::stdlib;
  pub use super::stdlib::prelude::*;
  pub use super::value::prelude::*;
  pub use macros::*;
}

pub mod macro_requirements {
  pub use crate::prelude::{
    methods, native, Args, DebugValue, DisplayValue, Env, Fields, LockableValue, LockedModule, MaybeFrom, TryUnwrapArg,
    Usertype, UsertypeFields, UsertypeMethods, Value, ValueError, ValueResult, Vm,
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
