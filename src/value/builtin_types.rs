

use super::{AllocatedObject, Value, META_OFFSET};
pub use array::Array;
pub use error::Error;
pub use native::NativeFn;
pub use r#struct::Struct;
use std::{
  any::TypeId,
  fmt::{Display, Formatter, Result as FmtResult},
};
pub use string::Str;

mod array;
mod error;
mod native;
mod string;
mod r#struct;

pub struct Nil;

pub trait Object
where
  Self: Sized + 'static,
{
  #[allow(unused_variables)]
  fn set(&mut self, name: &str, value: Value) -> Result<(), Error> {
    Err(UnimplementedFunction::Set.to_string().into())
  }

  #[allow(unused_variables)]
  fn get(&self, name: &str) -> Value {
    Value::new_err(UnimplementedFunction::Get.to_string())
  }

  #[allow(unused_variables)]
  fn add(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Add.to_string())
  }

  #[allow(unused_variables)]
  fn sub(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Sub.to_string())
  }

  #[allow(unused_variables)]
  fn mul(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Mul.to_string())
  }

  #[allow(unused_variables)]
  fn div(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Div.to_string())
  }

  #[allow(unused_variables)]
  fn rem(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Rem.to_string())
  }

  fn drop(&mut self) {}

  // override this only if necessary
  fn dealloc(this: *mut Self) {
    consume::<Self>(this);
  }

  // below this line should not to be reimplemented by the user

  fn type_id() -> TypeId {
    TypeId::of::<Self>()
  }

  fn basic_desc() -> &'static str {
    std::any::type_name::<Self>()
  }
}

pub enum UnimplementedFunction {
  Set,
  Get,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Custom(String),
}

impl Display for UnimplementedFunction {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{} is unimplemented",
      match self {
        UnimplementedFunction::Set => "set",
        UnimplementedFunction::Get => "get",
        UnimplementedFunction::Add => "add",
        UnimplementedFunction::Sub => "sub",
        UnimplementedFunction::Mul => "mul",
        UnimplementedFunction::Div => "div",
        UnimplementedFunction::Rem => "rem",
        UnimplementedFunction::Custom(s) => s,
      }
    )
  }
}

fn consume<T: Object>(this: *mut T) {
  unsafe {
    Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>);
  }
}
