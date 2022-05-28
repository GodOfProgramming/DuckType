use super::*;

#[test]
fn is_truthy() {
    let true_expectations = |t: Value| {
        assert!(t.truthy());
    };

    let false_expectations = |t: Value| {
        assert!(!t.truthy());
    };

    run_assertions(
        vec![true_expectations],
        vec![
            Value::new(true),
            Value::new(0.0),
            Value::new(1.0),
            Value::new(-1.0),
            Value::new("some string"),
            Value::new(Values::new(Vec::new())),
        ],
    );

    run_assertions(
        vec![false_expectations],
        vec![Value::Nil, Value::new(false)],
    );
}

fn run_assertions(funcs: Vec<fn(Value)>, values: Vec<Value>) {
    for func in funcs {
        let values = values.clone();
        for value in values {
            func(value);
        }
    }
}

#[test]
fn can_add() {
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

#[test]
fn cannot_add_invalid() {
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

    run_assertions(
        vec![assert_err_with_num, assert_err_with_str],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
        ],
    );
}

#[test]
fn can_sub() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x - y, Ok(Value::new(1.0)));
}

#[test]
fn cannot_sub_invalid() {
    let assert_err_with_num = |t: Value| {
        let num = Value::new(1.0);
        assert!(matches!(num - t.clone(), Err(_)));
        let num = Value::new(1.0);
        assert!(matches!(t - num, Err(_)));
    };

    run_assertions(
        vec![assert_err_with_num],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
        ],
    );
}

#[test]
fn can_mul() {
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

#[test]
fn cannot_mul_invalid() {
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

    run_assertions(
        vec![assert_err_with_num, assert_err_with_str],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
        ],
    );

    run_assertions(
        vec![assert_err_with_str],
        vec![Value::new(-1.0), Value::new("test")],
    );
}

#[test]
fn can_div() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x / y, Ok(Value::new(1.5)));
}

#[test]
fn cannot_div_invalid() {
    let assert_err_with_num = |t: Value| {
        let num = Value::new(1.0);
        assert!(matches!(num / t.clone(), Err(_)));
        let num = Value::new(1.0);
        assert!(matches!(t / num, Err(_)));
    };

    run_assertions(
        vec![assert_err_with_num],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
        ],
    );
}

#[test]
fn can_mod() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x % y, Ok(Value::new(1.0)));
}

#[test]
fn cannot_mod_invalid() {
    let assert_err_with_num = |t: Value| {
        let num = Value::new(1.0);
        assert!(matches!(num % t.clone(), Err(_)));
        let num = Value::new(1.0);
        assert!(matches!(t % num, Err(_)));
    };

    run_assertions(
        vec![assert_err_with_num],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
        ],
    );
}

#[test]
fn not_a_value_returns_opposite_truthiness() {
    let true_expectations = |t: Value| {
        assert_eq!(Value::new(true), !t);
    };

    let false_expectations = |t: Value| {
        assert_eq!(Value::new(false), !t);
    };

    run_assertions(vec![true_expectations], vec![Value::Nil, Value::new(false)]);

    run_assertions(
        vec![false_expectations],
        vec![
            Value::new(true),
            Value::new(0.0),
            Value::new(1.0),
            Value::new(-1.0),
            Value::new("some string"),
            Value::new(Values::new(Vec::new())),
        ],
    );
}

#[test]
fn can_negate() {
    let x = Value::new(1.0);
    assert_eq!(-x, Ok(Value::new(-1.0)));
}

#[test]
fn cannot_negate_invalid() {
    let assert_err = |t: Value| {
        assert!(matches!(-t, Err(_)));
    };

    run_assertions(
        vec![assert_err],
        vec![
            Value::Nil,
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
        ],
    );
}
