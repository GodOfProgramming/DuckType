#[test]
fn user_tests() {
  let t = trybuild::TestCases::new();
  t.pass("tests/pass/*.rs")
}

#[cfg(test)]
mod tests {
  use macros::{class_body, Class};
  use std::error::Error;

  type NativeFn = fn(&mut Vm, &mut Env, Args) -> ValueResult;

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
    fn get(&self, field: &str) -> Option<Value>;
    fn set(&mut self, field: &str, value: Value) -> Result<(), Box<dyn Error>>;
  }

  trait ClassBody {
    fn lookup(name: &str) -> Option<Value>;
  }

  #[derive(Debug)]
  enum Value {
    I32(i32),
    F32(f32),
    Fn(fn(&mut Vm, &mut Env, Args) -> ValueResult),
    Nil,
  }

  impl Value {
    fn native(f: fn(&mut Vm, &mut Env, Args) -> ValueResult) -> Value {
      Value::Fn(f)
    }

    fn cast_to_mut<T: Default>(&mut self) -> Option<&mut T> {
      Some(&mut T::default())
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

  impl Foo {
    fn new(foo: i32, foo2: f32) -> Self {
      Self { foo, foo2 }
    }
  }

  #[class_body]
  impl Foo {
    fn bar(&mut self) -> Result<i32, ValueError> {
      Ok(1)
    }

    fn baz(&mut self) -> Result<f32, ValueError> {
      Ok(1.0)
    }

    fn barbaz(&mut self, a: i32, b: f32) -> Result<f32, ValueError> {
      Ok(a as f32 + b)
    }
  }

  #[test]
  fn try_compile() {
    Foo::new(1, 1.0);
  }
}
