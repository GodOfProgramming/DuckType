use super::*;

const EXAMPLE_SCRIPT: &str = include_str!("parser_example_script.ss");
const SYNC_TEST_SCRIPT: &str = include_str!("sync_test_script.ss");

#[cfg(test)]
fn do_with_parser<F: FnOnce(Parser)>(script: &str, f: F) {
  let mut scanner = Scanner::new("test", script);

  let (tokens, meta) = scanner.scan().unwrap();

  let code_meta = Reflection::new(String::from("test"), String::from(script));
  let mut ctx = Context::new(code_meta);

  let parser = Parser::new(tokens, meta, &mut ctx);

  f(parser);
}

#[test]
fn current_returns_the_current_token() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let expected = Token::RightParen;
    parser.index = 3; // increment the index
    let actual = parser.current().unwrap();
    assert_eq!(actual, expected);
  });
}

#[test]
fn previous_returns_the_last_token() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let expected = Token::LeftParen;
    parser.index = 3; // increment the index
    let actual = parser.previous().unwrap();
    assert_eq!(actual, expected);
  });
}

#[test]
fn advance_increments_the_index_by_1() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let curr = parser.index;
    parser.advance();
    assert_eq!(curr + 1, parser.index);
  });
}

#[test]
fn advance_if_matches_advances_only_if_theres_a_match() {
  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 3;
    assert!(parser.advance_if_matches(Token::RightParen));
    assert_eq!(parser.index, 4);
  });

  // does not match
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 3;
    assert!(!parser.advance_if_matches(Token::LeftParen));
    assert_eq!(parser.index, 3);
  });

  // index too large
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 999;
    assert!(!parser.advance_if_matches(Token::RightParen));
    assert_eq!(parser.index, 999);
  });
}

#[test]
fn consume_advances_only_if_expected_is_accurate() {
  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    assert!(parser.consume(Token::Fn, String::from("unused")));
    assert_eq!(parser.index, 1);
  });

  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    assert!(!parser.consume(Token::LeftParen, String::from("used")));
    assert_eq!(parser.index, 0);
    let errors = parser.errors.unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(
      Error {
        msg: String::from("used"),
        file: String::from("test"),
        line: 1,
        column: 1,
      },
      errors[0],
    )
  });
}

#[test]
fn emit_creates_expected_opcode() {
  do_with_parser("1 + 1", |mut parser| {
    parser.emit(1, OpCode::Add);

    assert_eq!(parser.ctx.instructions[0], OpCode::Add);
    assert_eq!(
      OpCodeReflection {
        file: String::from("test"),
        source_line: String::from("1 + 1"),
        line: 1,
        column: 3,
      },
      parser.ctx.meta.get(0).unwrap(),
    );
  });
}

#[test]
fn emit_const_creates_expected_opcode() {
  do_with_parser("foo + 1", |mut parser| {
    parser.emit_const(2, Value::Num(1.0));
    assert_eq!(parser.ctx.instructions[0], OpCode::Const(0));
    assert_eq!(parser.ctx.consts[0], Value::Num(1.0));
    assert_eq!(
      OpCodeReflection {
        file: String::from("test"),
        source_line: String::from("foo + 1"),
        line: 1,
        column: 7,
      },
      parser.ctx.meta.get(0).unwrap(),
    );
  });
}

#[test]
fn let_stmt() {
  do_with_parser("let foo = true;", |mut parser| {
    parser.index = 1;
    parser.let_stmt();

    let expected_consts = vec![Value::new(String::from("foo"))];
    assert_eq!(expected_consts, parser.ctx.consts);

    let ident_loc = parser.ctx.identifiers.get("foo").cloned().unwrap();
    assert_eq!(ident_loc, 0usize);

    let expected_opcodes = vec![OpCode::True, OpCode::DefineGlobal(0)];
    assert_eq!(expected_opcodes, parser.ctx.instructions);
  })
}

#[test]
fn print_stmt() {
  do_with_parser("print 1;", |mut parser| {
    parser.index = 1;
    parser.print_stmt();
    let expected = vec![OpCode::Const(0), OpCode::Print];
    assert_eq!(expected.len(), parser.ctx.instructions.len());
    for (e, a) in expected.iter().zip(parser.ctx.instructions.iter()) {
      assert_eq!(e, a);
    }
  });
}

#[test]
fn basic_pemdas_rules_are_enforced() {
  do_with_parser("1 + 2 * 3 - 4 / 5 + 6 % 7", |mut parser| {
    parser.expression();
    assert!(parser.errors.is_none(), "errors: {:?}", parser.errors);
    let expected_instructions = vec![
      OpCode::Const(0),
      OpCode::Const(1),
      OpCode::Const(2),
      OpCode::Mul,
      OpCode::Add,
      OpCode::Const(3),
      OpCode::Const(4),
      OpCode::Div,
      OpCode::Sub,
      OpCode::Const(5),
      OpCode::Const(6),
      OpCode::Mod,
      OpCode::Add,
    ];
    assert_eq!(expected_instructions.len(), parser.ctx.instructions.len());
    for (e, a) in expected_instructions
      .iter()
      .zip(parser.ctx.instructions.iter())
    {
      assert_eq!(e, a);
    }

    let expected_constants = vec![
      Value::new(1),
      Value::new(2),
      Value::new(3),
      Value::new(4),
      Value::new(5),
      Value::new(6),
      Value::new(7),
    ];

    assert_eq!(expected_constants.len(), parser.ctx.consts.len());
    for (e, a) in expected_constants.iter().zip(parser.ctx.consts.iter()) {
      assert_eq!(e, a);
    }
  });
}

#[test]
fn sync_advances_to_expected_index() {
  do_with_parser(SYNC_TEST_SCRIPT, |mut parser| {
    let check_current = |parser: &mut Parser, e: &mut f64| {
      if let Token::Number(n) = parser.current().unwrap() {
        assert_eq!(n, *e);
        *e += 1.0;
      } else {
        panic!("position incorrect: {}", parser.index);
      }
    };

    let mut check = 1.0;

    parser.sync();
    assert_eq!(parser.index, 3);
    check_current(&mut parser, &mut check);
    parser.advance();

    for n in 0..11 {
      parser.sync();
      assert_eq!(parser.index, 6 + n * 4);
      parser.advance();
      check_current(&mut parser, &mut check);
      parser.advance();
    }
  });
}