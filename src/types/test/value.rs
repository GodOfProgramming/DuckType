use super::*;
use tfix::{fixture, TestFixture};

#[derive(Default)]
struct ValueTest;

impl ValueTest {
  fn run_assertions(&self, funcs: Vec<fn(Value)>, values: Vec<Value>) {
    for func in funcs {
      let values = values.clone();
      for value in values {
        func(value);
      }
    }
  }
}

impl TestFixture for ValueTest {
  fn set_up() -> Self {
    Self::default()
  }
}

#[fixture(ValueTest)]
mod tests {
  use super::*;

  fn is_truthy(t: &mut ValueTest) {
    let true_expectations = |t: Value| {
      assert!(t.truthy());
    };

    let false_expectations = |t: Value| {
      assert!(!t.truthy());
    };

    t.run_assertions(
      vec![true_expectations],
      vec![
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Vec::new()),
      ],
    );

    t.run_assertions(
      vec![false_expectations],
      vec![Value::Nil, Value::new(false)],
    );
  }

  fn can_add(_: &mut ValueTest) {
    let x = Value::new(1.0);
    let y = Value::new(2.0);

    assert_eq!(x + y, Ok(Value::new(3.0)));

    let x = Value::new("x");
    let y = Value::new("y");

    assert_eq!(x + y, Ok(Value::new("xy")));

    let x = Value::new(1.0);
    let y = Value::new("y");

    assert_eq!(x + y, Ok(Value::new("1y")));

    let x = Value::new("x");
    let y = Value::new(2.0);

    assert_eq!(x + y, Ok(Value::new("x2")));
  }

  fn cannot_add_invalid(t: &mut ValueTest) {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num + t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t + num, Err(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s + t.clone(), Err(_)));
      let s = Value::new("a");
      assert!(matches!(t + s, Err(_)));
    };

    t.run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new(Vec::new()),
      ],
    );
  }

  fn can_sub(_: &mut ValueTest) {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x - y, Ok(Value::new(1.0)));
  }

  fn cannot_sub_invalid(t: &mut ValueTest) {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num - t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t - num, Err(_)));
    };

    t.run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Vec::new()),
      ],
    );
  }

  fn can_mul(_: &mut ValueTest) {
    let x = Value::new(2.0);
    let y = Value::new(3.0);

    assert_eq!(x * y, Ok(Value::new(6.0)));

    let x = Value::new(2.0);
    let y = Value::new("a");

    assert_eq!(x * y, Ok(Value::new("aa")));

    let x = Value::new(2.0);
    let y = Value::new("a");

    assert_eq!(x * y, Ok(Value::new("aa")));
  }

  fn cannot_mul_invalid(t: &mut ValueTest) {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num * t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t * num, Err(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s * t.clone(), Err(_)));
      let s = Value::new("a");
      assert!(matches!(t * s, Err(_)));
    };

    t.run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new(Vec::new()),
      ],
    );

    t.run_assertions(
      vec![assert_err_with_str],
      vec![Value::new(-1.0), Value::new("test")],
    );
  }

  fn can_div(_: &mut ValueTest) {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x / y, Ok(Value::new(1.5)));
  }

  fn cannot_div_invalid(t: &mut ValueTest) {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num / t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t / num, Err(_)));
    };

    t.run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Vec::new()),
      ],
    );
  }

  fn can_mod(_: &mut ValueTest) {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x % y, Ok(Value::new(1.0)));
  }

  fn cannot_mod_invalid(t: &mut ValueTest) {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num % t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t % num, Err(_)));
    };

    t.run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Vec::new()),
      ],
    );
  }

  fn not_a_value_returns_opposite_truthiness(t: &mut ValueTest) {
    let true_expectations = |t: Value| {
      assert_eq!(Value::new(true), !t);
    };

    let false_expectations = |t: Value| {
      assert_eq!(Value::new(false), !t);
    };

    t.run_assertions(vec![true_expectations], vec![Value::Nil, Value::new(false)]);

    t.run_assertions(
      vec![false_expectations],
      vec![
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Vec::new()),
      ],
    );
  }

  fn can_negate(_: &mut ValueTest) {
    let x = Value::new(1.0);
    assert_eq!(-x, Ok(Value::new(-1.0)));
  }

  fn cannot_negate_invalid(t: &mut ValueTest) {
    let assert_err = |t: Value| {
      assert!(matches!(-t, Err(_)));
    };

    t.run_assertions(
      vec![assert_err],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Vec::new()),
      ],
    );
  }
}
