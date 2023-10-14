#[test]
fn user_tests() {
  let t = trybuild::TestCases::new();
  t.pass("tests/pass/*.rs")
}

#[cfg(test)]
mod tests {
  use macros::{methods, Class};
  use std::{error::Error, fmt::Debug};

  struct Vm;
  struct Env;

  struct Args {
    pub this: Option<Value>,
    pub list: Vec<Value>,
  }

  type ValueResult = Result<Value, ValueError>;

  enum ValueError {
    // fn name
    MissingSelf(&'static str),
    // fn name, argument index, error
    WrongType(&'static str, usize, Box<dyn Error>),
    // fn name, type, actual/this
    BadCast(&'static str, &'static str, Value),
    // given, expected
    ArgumentError(usize, usize),
  }

  trait Class {
    fn id(&self) -> &'static str;
    fn get(&self, field: &str) -> Option<Value>;
    fn set(&mut self, field: &str, value: Value) -> Result<(), ValueError>;
  }

  trait ClassBody {
    fn lookup(name: &str) -> Option<Value>;
  }

  enum Value {
    I32(i32),
    F32(f32),
    Fn(fn(&mut Vm, &mut Env, Args) -> ValueResult),
    Class(Box<dyn Class>),
    Nil,
  }

  impl Value {
    fn native(f: fn(&mut Vm, &mut Env, Args) -> ValueResult) -> Value {
      Value::Fn(f)
    }

    fn cast_to<T: Default>(&self) -> Option<&T> {
      Some(Box::leak(Box::new(T::default())))
    }

    fn cast_to_mut<T: Default>(&mut self) -> Option<&mut T> {
      Some(Box::leak(Box::new(T::default())))
    }
  }

  impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Self::I32(arg0) => f.debug_tuple("I32").field(arg0).finish(),
        Self::F32(arg0) => f.debug_tuple("F32").field(arg0).finish(),
        Self::Fn(_arg0) => write!(f, "Fn"),
        Self::Nil => write!(f, "Nil"),
      }
    }
  }

  impl From<()> for Value {
    fn from(_value: ()) -> Self {
      Self::Nil
    }
  }

  impl From<i32> for Value {
    fn from(value: i32) -> Self {
      Self::from(&value)
    }
  }

  impl From<&i32> for Value {
    fn from(value: &i32) -> Self {
      Self::I32(*value)
    }
  }

  impl From<f32> for Value {
    fn from(value: f32) -> Self {
      Self::from(&value)
    }
  }

  impl From<&f32> for Value {
    fn from(value: &f32) -> Self {
      Self::F32(*value)
    }
  }

  impl<T> From<T> for Value
  where
    T: Class,
  {
    fn from(value: T) -> Self {
      Self::Class(Box::new(value))
    }
  }

  impl TryFrom<Value> for i32 {
    type Error = Box<dyn Error>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
      if let Value::I32(i) = value {
        Ok(i)
      } else {
        Err("")?
      }
    }
  }

  impl TryFrom<Value> for f32 {
    type Error = Box<dyn Error>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
      if let Value::F32(f) = value {
        Ok(f)
      } else {
        Err("")?
      }
    }
  }

  impl Default for Value {
    fn default() -> Self {
      Self::Nil
    }
  }

  #[derive(Default, Class)]
  struct Foo {
    #[field]
    foo: i32,

    #[field]
    foo2: f32,
  }

  #[methods]
  impl Foo {
    fn foo(x: i32) -> Result<i32, ValueError> {
      Ok(x)
    }

    fn bar(&self) -> Result<i32, ValueError> {
      Ok(1)
    }

    fn baz(&self) -> Result<f32, ValueError> {
      Ok(1.0)
    }

    fn barbaz(&mut self, a: i32, b: f32) -> Result<(), ValueError> {
      self.foo = a;
      self.foo2 = b;
      Ok(())
    }
  }
}
