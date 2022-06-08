use super::{SourceLocation, Token};
use crate::{
  types::{Error, Value},
  New,
};
use std::{
  collections::BTreeSet,
  fmt::{Display, Formatter},
  mem,
};

pub struct Ast {
  pub statements: Vec<Statement>,
}

impl Ast {
  pub fn from(tokens: Vec<Token>, meta: Vec<SourceLocation>) -> Result<Ast, Vec<Error>> {
    AstGenerator::new(tokens, meta).generate()
  }
}

struct AstGenerator {
  tokens: Vec<Token>,
  meta: Vec<SourceLocation>,

  statements: Vec<Statement>,
  errors: Vec<Error>,

  index: usize,

  in_loop: bool,

  test: usize,
}

impl AstGenerator {
  fn new(tokens: Vec<Token>, meta: Vec<SourceLocation>) -> Self {
    Self {
      tokens,
      meta,
      statements: Default::default(),
      errors: Default::default(),
      index: Default::default(),
      in_loop: Default::default(),
      test: Default::default(),
    }
  }

  fn generate(mut self) -> Result<Ast, Vec<Error>> {
    while let Some(current) = self.current() {
      self.statement(current);
    }

    if self.errors.is_empty() {
      Ok(Ast {
        statements: self.statements,
      })
    } else {
      Err(self.errors)
    }
  }

  fn statement(&mut self, token: Token) {
    match token {
      Token::Break => {
        self.advance();
        self.break_stmt();
      }
      Token::Cont => {
        self.advance();
        self.cont_stmt();
      }
      Token::Class => {
        self.advance();
        self.class_stmt();
      }
      Token::Fn => {
        self.advance();
        self.fn_stmt();
      }
      Token::For => {
        self.advance();
        self.for_stmt();
      }
      Token::If => {
        self.advance();
        self.if_stmt();
      }
      Token::LeftBrace => {
        self.advance();
        self.block_stmt();
      }
      Token::Let => {
        self.advance();
        self.let_stmt();
      }
      Token::Loop => {
        self.advance();
        self.loop_stmt();
      }
      Token::Match => {
        self.advance();
        self.match_stmt();
      }
      Token::Print => {
        self.advance();
        self.print_stmt();
      }
      Token::Req => {
        self.advance();
        self.req_stmt();
      }
      Token::Ret => {
        self.advance();
        self.ret_stmt();
      }
      Token::While => {
        self.advance();
        self.while_stmt();
      }
      _ => self.expression_stmt(),
    }
  }

  /* Statements */

  fn block_stmt(&mut self) {
    if let Some(block_loc) = self.meta_at::<1>() {
      if let Some(block) = self.block(block_loc) {
        self.statements.push(Statement::new(block));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn break_stmt(&mut self) {
    if !self.in_loop {
      self.error::<1>(String::from(
        "break statements can only be used within loops",
      ));
      return;
    }

    if !self.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    if let Some(loc) = self.meta_at::<2>() {
      self
        .statements
        .push(Statement::new(BreakStatement::new(loc)));
    } else {
      // sanity check
      self.error::<1>(String::from("could not find original token"));
    }
  }

  fn cont_stmt(&mut self) {
    if !self.in_loop {
      self.error::<1>(String::from(
        "cont statements can only be used within loops",
      ));
      return;
    }

    if !self.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    if let Some(loc) = self.meta_at::<2>() {
      self
        .statements
        .push(Statement::new(ContStatement::new(loc)));
    } else {
      // sanity check
      self.error::<1>(String::from("could not find original token"));
    }
  }

  fn class_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<0>() {
      if let Some(Token::Identifier(class_name)) = self.current() {
        self.advance();

        if !self.consume(Token::LeftBrace, "expected '{' after class name") {
          return;
        }

        let mut initializer = None;
        let mut methods = Vec::default();
        let mut declared_functions = BTreeSet::default();

        while let Some(token) = self.current() {
          match token {
            Token::New => {
              self.advance();
              if initializer.is_none() {
                if self.consume(Token::LeftParen, "expected '(' after 'new'") {
                  initializer = self.lambda_expr(|params, body| {
                    Some(Expression::new(MethodExpression::new(
                      params,
                      Statement::new(body),
                      loc,
                    )))
                  });
                }
              } else {
                self.error::<0>(String::from("duplicate initializer found"));
              }
            }
            Token::Fn => {
              self.advance();
              if let Some(Token::Identifier(ident)) = self.current() {
                if !declared_functions.contains(&ident) {
                  self.advance();
                  if self.consume(Token::LeftParen, "expected '(' after identifier") {
                    if let Some(function) = self.lambda_expr(|params, body| {
                      declared_functions.insert(ident.clone());
                      Some(Expression::new(MethodExpression::new(
                        params,
                        Statement::new(body),
                        loc,
                      )))
                    }) {
                      let ident = Ident::new(ident.clone());
                      methods.push((ident, function));
                    }
                  }
                } else {
                  self.error::<0>(String::from("duplicate method definition"));
                }
              }
            }
            _ => break,
          }
        }

        if !self.consume(Token::RightBrace, "expected '}' after class body") {
          return;
        }

        self.statements.push(Statement::new(ClassStatement::new(
          Ident::new(class_name),
          initializer,
          methods,
          loc,
        )))
      } else {
        self.error::<0>(String::from("expected an identifier"));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn fn_stmt(&mut self) {
    if let Some(stmt) = self.parse_fn() {
      self.statements.push(stmt);
    }
  }

  fn for_stmt(&mut self) {
    if let Some(for_loc) = self.meta_at::<1>() {
      if let Some(initializer_loc) = self.meta_at::<0>() {
        let initializer = if self.advance_if_matches(Token::Let) {
          if let Some(declaration) = self.declaration() {
            Statement::new(declaration)
          } else {
            return;
          }
        } else if let Some(expr) = self.expression() {
          Statement::new(ExpressionStatement::new(expr, initializer_loc))
        } else {
          return;
        };

        if !self.consume(Token::Semicolon, "expected ';' after expression") {
          return;
        }

        if let Some(comparison) = self.expression() {
          if !self.consume(Token::Semicolon, "expected ';' after comparison") {
            return;
          }

          if let Some(increment) = self.expression() {
            if self.consume(Token::LeftBrace, "expected '{' after increment") {
              let prev = self.in_loop;
              self.in_loop = true;
              if let Some(block_loc) = self.meta_at::<1>() {
                if let Some(block) = self.block(block_loc) {
                  self.statements.push(Statement::new(ForStatement::new(
                    initializer,
                    comparison,
                    increment,
                    Statement::new(block),
                    for_loc,
                  )));
                }
              } else {
                // sanity check
                self.error::<0>(String::from("could not find original token"));
              }
              self.in_loop = prev;
            }
          } else {
            self.error::<0>(String::from("expected increment after comparison"));
          }
        } else {
          self.error::<0>(String::from("expected comparison after initializer"));
        }
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn if_stmt(&mut self) {
    if let Some(if_stmt) = self.branch() {
      self.statements.push(Statement::new(if_stmt));
    }
  }

  fn let_stmt(&mut self) {
    if let Some(declaration) = self.declaration() {
      if !self.consume(Token::Semicolon, "expected ';' after expression") {
        return;
      }

      self.statements.push(Statement::new(declaration));
    }
  }

  fn req_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<1>() {
      if let Some(expr) = self.expression() {
        let ident = if self.advance_if_matches(Token::Arrow) {
          if let Some(token) = self.current() {
            if let Token::Identifier(ident) = token {
              self.advance();
              Some(Ident::new(ident))
            } else {
              self.error::<0>(String::from("identifier expected after require"));
              None
            }
          } else {
            self.error::<0>(String::from("unexpected end of file"));
            None
          }
        } else {
          None
        };

        if !self.consume(Token::Semicolon, "expected ';' after expression") {
          return;
        }

        self
          .statements
          .push(Statement::new(ReqStatement::new(expr, ident, loc)));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn loop_stmt(&mut self) {
    if !self.consume(Token::LeftBrace, "expect '{' after loop") {
      return;
    }

    let prev = self.in_loop;
    self.in_loop = true;

    if let Some(loc) = self.meta_at::<1>() {
      if let Some(block) = self.block(loc) {
        self.statements.push(Statement::new(LoopStatement::new(
          Statement::new(block),
          loc,
        )));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }

    self.in_loop = prev;
  }

  fn match_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<1>() {
      if let Some(expr) = self.expression() {
        if !self.consume(Token::LeftBrace, "expected '{' after expression") {
          return;
        }

        let mut branches = Vec::default();

        while let Some(token) = self.current() {
          if token == Token::RightBrace {
            break;
          }

          if let Some(condition) = self.expression() {
            if !self.consume(Token::Arrow, "expected => after expression") {
              break;
            }

            if let Some(eval_loc) = self.meta_at::<0>() {
              let stmt = if self.advance_if_matches(Token::LeftBrace) {
                if let Some(block) = self.block(eval_loc) {
                  Statement::new(block)
                } else {
                  break;
                }
              } else if let Some(eval_loc) = self.meta_at::<0>() {
                if let Some(eval) = self.expression() {
                  if !self.consume(Token::Comma, "expected ',' after expression") {
                    break;
                  }
                  Statement::new(ExpressionStatement::new(eval, eval_loc))
                } else {
                  break;
                }
              } else {
                break;
              };

              branches.push((condition, stmt));
            } else {
              break; // error but need to restore statements
            }
          } else {
            // sanity check
            self.error::<0>(String::from("could not find original token"));
          }
        }

        if !self.consume(Token::RightBrace, "expected '}' after match") {
          return;
        }

        let default =
          if self.advance_if_matches(Token::Else) && self.advance_if_matches(Token::LeftBrace) {
            if let Some(else_loc) = self.meta_at::<2>() {
              self.block(else_loc).map(Statement::new)
            } else {
              // sanity check
              self.error::<0>(String::from("could not find original token"));
              None
            }
          } else {
            None
          };

        self.statements.push(Statement::new(MatchStatement::new(
          expr, branches, default, loc,
        )))
      }
    }
  }

  fn print_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<1>() {
      if let Some(expr) = self.expression() {
        if !self.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }
        self
          .statements
          .push(Statement::new(PrintStatement::new(expr, loc)));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn ret_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<1>() {
      if let Some(current) = self.current() {
        let expr = if current == Token::Semicolon {
          None
        } else if let Some(expr) = self.expression() {
          Some(expr)
        } else {
          return;
        };

        if !self.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }

        self
          .statements
          .push(Statement::new(RetStatement::new(expr, loc)));
      }
    }
  }

  fn while_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(Token::LeftBrace, "expected '{' after expression") {
        return;
      }

      let prev = self.in_loop;
      self.in_loop = true;

      if let Some(loc) = self.meta_at::<1>() {
        if let Some(block) = self.block(loc) {
          self.statements.push(Statement::new(WhileStatement::new(
            expr,
            Statement::new(block),
            loc,
          )));
        }
      } else {
        // sanity check
        self.error::<0>(String::from("could not find original token"));
      }

      self.in_loop = prev;
    }
  }

  fn expression_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<0>() {
      if let Some(expr) = self.expression() {
        if !self.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }
        self
          .statements
          .push(Statement::new(ExpressionStatement::new(expr, loc)));
      }
    }
  }

  /* Expressions */

  fn expression(&mut self) -> Option<Expression> {
    self.parse_precedence(Precedence::Assignment)
  }

  fn literal_expr(&mut self) -> Option<Expression> {
    let mut expr = None;

    if let Some(prev) = self.previous() {
      match prev {
        Token::Nil => {
          expr = Some(Expression::new(LiteralExpression::new(
            Value::Nil,
            self.meta_at::<1>()?,
          )));
        }
        Token::True => {
          expr = Some(Expression::new(LiteralExpression::new(
            Value::new(true),
            self.meta_at::<1>()?,
          )));
        }
        Token::False => {
          expr = Some(Expression::new(LiteralExpression::new(
            Value::new(false),
            self.meta_at::<1>()?,
          )));
        }
        Token::String(s) => {
          expr = Some(Expression::new(LiteralExpression::new(
            Value::new(s),
            self.meta_at::<1>()?,
          )))
        }
        Token::Number(n) => {
          expr = Some(Expression::new(LiteralExpression::new(
            Value::new(n),
            self.meta_at::<1>()?,
          )))
        }
        _ => {
          self.error::<1>(String::from(
            "sanity check, invalid literal, very bad logic error",
          ));
        }
      }
    } else {
      self.error::<1>(String::from(
        "sanity check, no previous token, very bad logic error",
      ));
    }

    expr
  }

  fn unary_expr(&mut self) -> Option<Expression> {
    if let Some(operator_token) = self.previous() {
      let op_meta = self.meta_at::<1>()?;
      let op = match operator_token {
        Token::Bang => UnaryOperator::Not,
        Token::Minus => UnaryOperator::Negate,
        Token::At => UnaryOperator::Deref,
        _ => {
          self.error::<1>(String::from("invalid unary operator"));
          return None;
        }
      };

      let expr = self.parse_precedence(Precedence::Unary)?;
      Some(Expression::new(UnaryExpression::new(op, expr, op_meta)))
    } else {
      self.error::<1>(String::from(
        "tried to make unary expression without operator",
      ));
      None
    }
  }

  fn binary_expr(&mut self, left: Expression) -> Option<Expression> {
    if let Some(operator_token) = self.previous() {
      let op_meta = self.meta_at::<1>()?;
      let op = match operator_token {
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEq,
        Token::Greater => BinaryOperator::Greater,
        Token::GreaterEqual => BinaryOperator::GreaterEq,
        Token::Less => BinaryOperator::Less,
        Token::LessEqual => BinaryOperator::LessEq,
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Sub,
        Token::Asterisk => BinaryOperator::Mul,
        Token::Slash => BinaryOperator::Div,
        Token::Percent => BinaryOperator::Mod,
        _ => {
          self.error::<1>(String::from("invalid binary operator"));
          return None;
        }
      };

      let rule = Self::rule_for(&operator_token);

      if let Some(next_precedence) = rule.precedence.next() {
        let expr = self.parse_precedence(next_precedence)?;
        Some(Expression::new(BinaryExpression::new(
          left, op, expr, op_meta,
        )))
      } else {
        self.error::<1>(String::from(""));
        None
      }
    } else {
      self.error::<2>(String::from("unexpected end of token stream"));
      None
    }
  }

  fn and_expr(&mut self, left: Expression) -> Option<Expression> {
    let rule = Self::rule_for(&Token::And);

    if let Some(next_precedence) = rule.precedence.next() {
      let op_meta = self.meta_at::<1>()?;
      let expr = self.parse_precedence(next_precedence)?;
      Some(Expression::new(AndExpression::new(left, expr, op_meta)))
    } else {
      self.error::<1>(String::from("unable to retrieve precedence for and expr")); // this may not be an error?
      None
    }
  }

  fn or_expr(&mut self, left: Expression) -> Option<Expression> {
    let rule = Self::rule_for(&Token::Or);

    if let Some(next_precedence) = rule.precedence.next() {
      let op_meta = self.meta_at::<1>()?;
      let expr = self.parse_precedence(next_precedence)?;
      Some(Expression::new(OrExpression::new(left, expr, op_meta)))
    } else {
      self.error::<1>(String::from("unable to retrieve precedence for or expr")); // this may not be an error?
      None
    }
  }

  fn group_expr(&mut self) -> Option<Expression> {
    let paren_meta = self.meta_at::<1>()?;

    let is_lambda = if let Some(token) = self.current() {
      if matches!(token, Token::Identifier(_)) {
        if let Some(after_ident) = self.peek_after::<1>() {
          after_ident == Token::RightParen || after_ident == Token::Comma
        } else {
          false
        }
      } else {
        token == Token::RightParen
      }
    } else {
      false
    };

    if is_lambda {
      return self.lambda_expr(|params, body| {
        Some(Expression::new(LambdaExpression::new(
          params,
          Statement::new(body),
          paren_meta,
        )))
      });
    }

    let expr = self.expression()?;
    if self.consume(Token::RightParen, "expected ')' after expression") {
      Some(Expression::new(GroupExpression::new(expr, paren_meta)))
    } else {
      None
    }
  }

  fn ident_expr(&mut self) -> Option<Expression> {
    if let Some(ident_token) = self.previous() {
      if let Token::Identifier(ident_name) = ident_token {
        Some(Expression::new(IdentExpression::new(
          Ident::new(ident_name),
          self.meta_at::<1>()?,
        )))
      } else {
        self.error::<2>(String::from("variable name is not an identifier"));
        None
      }
    } else {
      self.error::<2>(String::from("unexpected end of token stream"));
      None
    }
  }

  fn assign_expr(&mut self, left: Expression) -> Option<Expression> {
    if let Expression::Ident(ident_expr) = left {
      let op = self.previous()?;
      let op_meta = self.meta_at::<1>()?;
      let value = self.expression()?;

      let op = match op {
        Token::Equal => AssignOperator::Assign,
        Token::BackArrow => AssignOperator::DerefAssign,
        Token::PlusEqual => AssignOperator::Add,
        Token::MinusEqual => AssignOperator::Sub,
        Token::AsteriskEqual => AssignOperator::Mul,
        Token::SlashEqual => AssignOperator::Div,
        Token::PercentEqual => AssignOperator::Mod,
        t => {
          self.error::<1>(format!("invalid token {}", t));
          return None;
        }
      };

      Some(Expression::new(AssignExpression::new(
        ident_expr.ident,
        op,
        value,
        op_meta,
      )))
    } else {
      self.error::<1>(String::from("can only assign to variables"));
      None
    }
  }

  fn call_expr(&mut self, expr: Expression) -> Option<Expression> {
    let paren_meta = self.meta_at::<1>()?;
    let mut args = Vec::default();

    if let Some(token) = self.current() {
      if token != Token::RightParen {
        loop {
          args.push(self.expression()?);
          if !self.advance_if_matches(Token::Comma) {
            break;
          }
        }
      }
    }

    if self.consume(Token::RightParen, "expect ')' after arguments") {
      Some(Expression::new(CallExpression::new(expr, args, paren_meta)))
    } else {
      None
    }
  }

  fn list_expr(&mut self) -> Option<Expression> {
    let bracket_meta = self.meta_at::<1>()?;
    let mut items = Vec::default();

    if let Some(token) = self.current() {
      if token != Token::RightParen {
        loop {
          items.push(self.expression()?);
          if !self.advance_if_matches(Token::Comma) {
            break;
          }
        }
      }
    }

    if self.consume(Token::RightBracket, "expect ']' after arguments") {
      Some(Expression::new(ListExpression::new(items, bracket_meta)))
    } else {
      None
    }
  }

  fn index_expr(&mut self, expr: Expression) -> Option<Expression> {
    let bracket_meta = self.meta_at::<1>()?;

    let index = self.expression()?;

    if self.consume(Token::RightBracket, "expected ']' after expression") {
      Some(Expression::new(IndexExpression::new(
        expr,
        index,
        bracket_meta,
      )))
    } else {
      None
    }
  }

  fn member_expr(&mut self, obj: Expression) -> Option<Expression> {
    if let Some(token) = self.current() {
      if let Token::Identifier(ident) = token {
        let ident_meta = self.meta_at::<0>()?;

        self.advance();

        if self.advance_if_matches(Token::Equal) {
          let value = self.expression()?;
          Some(Expression::new(MemberAssignExpression::new(
            obj,
            Ident::new(ident),
            value,
            ident_meta,
          )))
        } else {
          Some(Expression::new(MemberAccessExpression::new(
            obj,
            Ident::new(ident),
            ident_meta,
          )))
        }
      } else {
        self.error::<1>(String::from("expected identifier"));
        None
      }
    } else {
      self.error::<1>(String::from("unexpected end of file"));
      None
    }
  }

  fn struct_expr(&mut self) -> Option<Expression> {
    let struct_meta = self.meta_at::<1>()?;
    let mut members = Vec::default();

    while let Some(token) = self.current() {
      if token == Token::RightBrace {
        break;
      }

      if let Token::Identifier(ident) = token {
        self.advance();
        if self.advance_if_matches(Token::Comma) || self.check_peek_after::<0>(Token::RightBrace) {
          members.push((
            Ident::new(ident.clone()),
            Expression::new(IdentExpression::new(Ident::new(ident), struct_meta)),
          ))
        } else if self.consume(Token::Colon, "expected ':' after identifier") {
          let value = self.expression()?;
          members.push((Ident::new(ident), value));
          self.advance_if_matches(Token::Comma);
        } else {
          return None;
        }
      } else {
        self.error::<0>(String::from("expected identifier"));
        return None;
      }
    }

    if self.consume(Token::RightBrace, "expected '}' after struct") {
      if self.advance_if_matches(Token::LeftParen) {
        self.closure_expr(StructExpression::new(members, struct_meta))
      } else {
        Some(Expression::new(StructExpression::new(members, struct_meta)))
      }
    } else {
      self.error::<1>(String::from("expected token"));
      None
    }
  }

  fn closure_expr(&mut self, captures: StructExpression) -> Option<Expression> {
    let loc = self.meta_at::<0>()?;

    self.lambda_expr(|params, body| {
      Some(Expression::new(ClosureExpression::new(
        captures,
        params,
        Statement::new(body),
        loc,
      )))
    })
  }

  fn lambda_expr<F: FnOnce(Vec<Ident>, BlockStatement) -> Option<Expression>>(
    &mut self,
    f: F,
  ) -> Option<Expression> {
    let params = self.parse_parameters()?;

    if !self.consume(Token::RightParen, "expected ')' after parameters") {
      return None;
    }

    if !self.consume(Token::LeftBrace, "expected '{' after paren") {
      return None;
    }

    if let Some(block_loc) = self.meta_at::<1>() {
      let body = self.block(block_loc)?;
      f(params, body)
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
      None
    }
  }

  /* Utility functions */

  fn current(&self) -> Option<Token> {
    self.peek_after::<0>()
  }

  fn previous(&self) -> Option<Token> {
    self.peek_before::<1>()
  }

  fn peek_after<const I: usize>(&self) -> Option<Token> {
    self.tokens.get(self.index + I).cloned()
  }

  fn peek_before<const I: usize>(&self) -> Option<Token> {
    if self.index > 0 {
      self.tokens.get(self.index - I).cloned()
    } else {
      None
    }
  }

  fn check_peek_after<const I: usize>(&self, expected: Token) -> bool {
    if let Some(token) = self.peek_after::<I>() {
      expected == token
    } else {
      false
    }
  }

  fn advance(&mut self) {
    self.index += 1;
  }

  fn advance_if_matches(&mut self, token: Token) -> bool {
    if let Some(curr) = self.current() {
      if curr == token {
        self.advance();
        return true;
      }
    }
    false
  }

  fn consume<E: ToString>(&mut self, expected: Token, err: E) -> bool {
    if let Some(curr) = self.current() {
      if curr == expected {
        self.advance();
        true
      } else {
        self.error::<0>(err.to_string());
        false
      }
    } else {
      self.error::<1>(format!(
        "tried to lookup a token at an invalid index: {}",
        err.to_string()
      ));
      false
    }
  }

  fn parse_fn(&mut self) -> Option<Statement> {
    if let Some(loc) = self.meta_at::<0>() {
      if let Some(Token::Identifier(fn_name)) = self.current() {
        let ident = Ident::new(fn_name);

        self.advance();
        if !self.consume(Token::LeftParen, "expect '(' after function name") {
          return None;
        }

        if let Some(params) = self.parse_parameters() {
          if !self.consume(Token::RightParen, "expected ')' after arguments") {
            return None;
          }

          if !self.consume(Token::LeftBrace, "expected '{' after paren") {
            return None;
          }

          if let Some(block_loc) = self.meta_at::<1>() {
            self.block(block_loc).map(|body| {
              Statement::new(FnStatement::new(ident, params, Statement::new(body), loc))
            })
          } else {
            // sanity check
            self.error::<0>(String::from("could not find original token"));
            None
          }
        } else {
          None
        }
      } else {
        self.error::<0>(String::from("expected an identifier"));
        None
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
      None
    }
  }

  fn meta_at<const I: usize>(&mut self) -> Option<SourceLocation> {
    self.meta.get(self.index - I).cloned().or_else(|| {
      self.error::<I>(String::from("unable to get meta at position"));
      None
    })
  }

  fn error<const I: usize>(&mut self, msg: String) {
    if let Some(meta) = self.meta_at::<I>() {
      if cfg!(debug_assertions) {
        println!(
          "{} ({}, {}): {}",
          "TODO IMPLEMENT FILE", meta.line, meta.column, msg
        );
      }
      self.errors.push(Error {
        msg,
        file: String::default(), // TODO get file when loading is supported
        line: meta.line,
        column: meta.column,
      });
    } else {
      self.errors.push(Error {
        msg: format!("could not find location of token for msg '{}'", msg),
        file: String::default(), // TODO get file when loading is supported
        line: 0,
        column: 0,
      });
    }

    self.sync();
  }

  fn sync(&mut self) {
    while let Some(curr) = self.current() {
      if let Some(prev) = self.previous() {
        if prev == Token::Semicolon {
          return;
        }
      }

      if matches!(
        curr,
        Token::Class
          | Token::Fn
          | Token::Let
          | Token::For
          | Token::If
          | Token::While
          | Token::Print
          | Token::Ret
          | Token::Match
          | Token::Loop
      ) {
        return;
      }

      self.advance();
    }
  }

  fn scope<F: FnOnce(&mut Self)>(&mut self, f: F) -> Vec<Statement> {
    let mut statements = Vec::default();
    mem::swap(&mut statements, &mut self.statements);
    f(self);
    mem::swap(&mut statements, &mut self.statements);
    statements
  }

  fn block(&mut self, loc: SourceLocation) -> Option<BlockStatement> {
    self.test += 1;
    let statements = self.scope(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
    });

    if self.consume(Token::RightBrace, "expected '}' after block") {
      self.test -= 1;
      Some(BlockStatement::new(statements, loc))
    } else {
      self.test -= 1;
      None
    }
  }

  fn branch(&mut self) -> Option<IfStatement> {
    if let Some(expr) = self.expression() {
      if !self.consume(Token::LeftBrace, "expected '{' after condition") {
        return None;
      }

      if let Some(block_loc) = self.meta_at::<1>() {
        if let Some(block) = self.block(block_loc) {
          let else_block = if self.advance_if_matches(Token::Else) {
            if let Some(else_meta) = self.meta_at::<1>() {
              if let Some(token) = self.current() {
                match token {
                  Token::LeftBrace => {
                    self.advance();
                    Some(Statement::new(self.block(else_meta)?))
                  }
                  Token::If => {
                    self.advance();
                    Some(Statement::new(self.branch()?))
                  }
                  _ => {
                    self.error::<0>(String::from("unexpected token after 'else'"));
                    return None;
                  }
                }
              } else {
                self.error::<1>(String::from("unexpected end of file"));
                return None;
              }
            } else {
              // sanity check
              self.error::<0>(String::from("could not find original token"));
              return None;
            }
          } else {
            None
          };

          return Some(IfStatement::new(
            expr,
            Statement::new(block),
            else_block,
            block_loc,
          ));
        }
      } else {
        // sanity check
        self.error::<0>(String::from("could not find original token"));
      }
    }

    None
  }

  fn parse_parameters(&mut self) -> Option<Vec<Ident>> {
    let mut params = Vec::default();
    if let Some(mut token) = self.current() {
      if token != Token::RightParen {
        loop {
          if let Token::Identifier(ident) = token {
            params.push(Ident::new(ident));
          } else {
            self.error::<0>(String::from("invalid token in parameter list"));
            return None;
          }

          self.advance();

          if !self.advance_if_matches(Token::Comma) {
            break;
          }

          if let Some(next) = self.current() {
            token = next;
          } else {
            break;
          }
        }
      }
    }

    Some(params)
  }

  fn declaration(&mut self) -> Option<LetStatement> {
    if let Some(let_loc) = self.meta_at::<1>() {
      if let Some(Token::Identifier(ident)) = self.current() {
        let ident = Ident::new(ident);
        self.advance();

        let value = if self.advance_if_matches(Token::Equal) {
          if let Some(expr) = self.expression() {
            Some(expr)
          } else {
            return None;
          }
        } else {
          None
        };

        Some(LetStatement::new(ident, value, let_loc))
      } else {
        self.error::<0>(String::from("expected variable name"));
        None
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
      None
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) -> Option<Expression> {
    self.advance();

    if let Some(prev) = self.previous() {
      let mut expr: Expression;
      let rule = Self::rule_for(&prev);

      if let Some(prefix) = rule.prefix {
        match prefix(self) {
          Some(e) => expr = e,
          None => return None,
        }
      } else {
        self.error::<1>(String::from("expected an expression"));
        return None;
      }

      while let Some(curr) = self.current() {
        let rule = Self::rule_for(&curr);
        if precedence <= rule.precedence {
          self.advance();
          if let Some(infix) = rule.infix {
            match infix(self, expr) {
              Some(e) => expr = e,
              None => return None,
            }
          } else {
            self.error::<0>(format!("no rule for {:?}", prev));
            return None;
          }
        } else {
          break;
        }
      }

      Some(expr)
    } else {
      self.error::<2>(String::from(
        "unexpected end of token stream (parse_precedence 3)",
      ));
      None
    }
  }

  fn rule_for(token: &Token) -> ParseRule {
    match token {
      Token::LeftParen => ParseRule::new(
        Some(Self::group_expr),
        Some(Self::call_expr),
        Precedence::Call,
      ),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(Some(Self::struct_expr), None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::LeftBracket => ParseRule::new(
        Some(Self::list_expr),
        Some(Self::index_expr),
        Precedence::Call,
      ),
      Token::RightBracket => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, Some(Self::member_expr), Precedence::Call),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Colon => ParseRule::new(None, None, Precedence::None),
      Token::At => ParseRule::new(Some(Self::unary_expr), None, Precedence::Unary),
      Token::Plus => ParseRule::new(None, Some(Self::binary_expr), Precedence::Term),
      Token::PlusEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Minus => ParseRule::new(
        Some(Self::unary_expr),
        Some(Self::binary_expr),
        Precedence::Term,
      ),
      Token::MinusEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Asterisk => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::AsteriskEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Slash => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::SlashEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Percent => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::PercentEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Bang => ParseRule::new(Some(Self::unary_expr), None, Precedence::None),
      Token::BangEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Equality),
      Token::Equal => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::EqualEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Equality),
      Token::Greater => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::GreaterEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::Less => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::LessEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::Arrow => ParseRule::new(None, None, Precedence::None),
      Token::BackArrow => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Identifier(_) => ParseRule::new(Some(Self::ident_expr), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::Number(_) => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::And => ParseRule::new(None, Some(Self::and_expr), Precedence::And),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(None, None, Precedence::None),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::New => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::Or => ParseRule::new(None, Some(Self::or_expr), Precedence::Or),
      Token::Print => ParseRule::new(None, None, Precedence::None),
      Token::Req => ParseRule::new(None, None, Precedence::None),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::True => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::Yield => ParseRule::new(None, None, Precedence::None),
    }
  }
}

#[derive(Clone)]
pub struct Ident {
  pub name: String,
}

impl Ident {
  pub fn new<N: ToString>(name: N) -> Self {
    Self {
      name: name.to_string(),
    }
  }

  pub fn global(&self) -> bool {
    matches!(self.name.chars().next(), Some('$'))
  }
}

pub enum Statement {
  Block(BlockStatement),
  Break(BreakStatement),
  Cont(ContStatement),
  Class(ClassStatement),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Let(LetStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Print(PrintStatement),
  Req(ReqStatement),
  Ret(RetStatement),
  While(WhileStatement),
  Expression(ExpressionStatement),
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Self::Block(_) => write!(f, "block"),
      Self::Break(_) => write!(f, "break"),
      Self::Cont(_) => write!(f, "cont"),
      Self::Class(c) => write!(f, "class {}", c.ident.name),
      Self::Fn(function) => write!(f, "fn {}", function.ident.name),
      Self::For(_) => write!(f, "for"),
      Self::If(_) => write!(f, "if"),
      Self::Let(_) => write!(f, "let"),
      Self::Loop(_) => write!(f, "loop"),
      Self::Match(_) => write!(f, "match"),
      Self::Print(_) => write!(f, "print"),
      Self::Req(_) => write!(f, "req"),
      Self::Ret(_) => write!(f, "ret"),
      Self::While(_) => write!(f, "while"),
      Self::Expression(_) => write!(f, "expression"),
    }
  }
}

impl New<BlockStatement> for Statement {
  fn new(stmt: BlockStatement) -> Self {
    Self::Block(stmt)
  }
}

impl New<BreakStatement> for Statement {
  fn new(stmt: BreakStatement) -> Self {
    Self::Break(stmt)
  }
}

impl New<ContStatement> for Statement {
  fn new(stmt: ContStatement) -> Self {
    Self::Cont(stmt)
  }
}

impl New<ClassStatement> for Statement {
  fn new(stmt: ClassStatement) -> Self {
    Self::Class(stmt)
  }
}

impl New<FnStatement> for Statement {
  fn new(stmt: FnStatement) -> Self {
    Self::Fn(stmt)
  }
}

impl New<ForStatement> for Statement {
  fn new(stmt: ForStatement) -> Self {
    Self::For(stmt)
  }
}

impl New<IfStatement> for Statement {
  fn new(stmt: IfStatement) -> Self {
    Self::If(stmt)
  }
}

impl New<LetStatement> for Statement {
  fn new(stmt: LetStatement) -> Self {
    Self::Let(stmt)
  }
}

impl New<ReqStatement> for Statement {
  fn new(stmt: ReqStatement) -> Self {
    Self::Req(stmt)
  }
}

impl New<LoopStatement> for Statement {
  fn new(stmt: LoopStatement) -> Self {
    Self::Loop(stmt)
  }
}

impl New<MatchStatement> for Statement {
  fn new(stmt: MatchStatement) -> Self {
    Self::Match(stmt)
  }
}

impl New<PrintStatement> for Statement {
  fn new(stmt: PrintStatement) -> Self {
    Self::Print(stmt)
  }
}

impl New<RetStatement> for Statement {
  fn new(stmt: RetStatement) -> Self {
    Self::Ret(stmt)
  }
}

impl New<WhileStatement> for Statement {
  fn new(stmt: WhileStatement) -> Self {
    Self::While(stmt)
  }
}

impl New<ExpressionStatement> for Statement {
  fn new(stmt: ExpressionStatement) -> Self {
    Self::Expression(stmt)
  }
}

pub struct BlockStatement {
  pub statements: Vec<Statement>,
  pub loc: SourceLocation,
}

impl BlockStatement {
  fn new(statements: Vec<Statement>, loc: SourceLocation) -> Self {
    Self { statements, loc }
  }
}

pub struct BreakStatement {
  pub loc: SourceLocation,
}

impl BreakStatement {
  fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

pub struct ContStatement {
  pub loc: SourceLocation,
}

impl ContStatement {
  fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

pub struct ClassStatement {
  pub ident: Ident,
  pub initializer: Option<Expression>,
  pub methods: Vec<(Ident, Expression)>,

  pub loc: SourceLocation,
}

impl ClassStatement {
  fn new(
    ident: Ident,
    initializer: Option<Expression>,
    methods: Vec<(Ident, Expression)>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      ident,
      initializer,
      methods,
      loc,
    }
  }
}

pub struct FnStatement {
  pub ident: Ident,
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl FnStatement {
  fn new(ident: Ident, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      ident,
      params,
      body: Box::new(body),
      loc,
    }
  }
}

pub struct ForStatement {
  pub initializer: Box<Statement>,
  pub comparison: Expression,
  pub increment: Expression,
  pub block: Box<Statement>,

  pub loc: SourceLocation,
}

impl ForStatement {
  fn new(
    initializer: Statement,
    comparison: Expression,
    increment: Expression,
    block: Statement,
    loc: SourceLocation,
  ) -> Self {
    Self {
      initializer: Box::new(initializer),
      comparison,
      increment,
      block: Box::new(block),
      loc,
    }
  }
}

pub struct IfStatement {
  pub comparison: Expression,
  pub block: Box<Statement>,
  pub else_block: Option<Box<Statement>>,
  pub loc: SourceLocation,
}

impl IfStatement {
  fn new(
    comparison: Expression,
    block: Statement,
    else_block: Option<Statement>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      comparison,
      block: Box::new(block),
      else_block: else_block.map(Box::new),
      loc,
    }
  }
}

pub struct LetStatement {
  pub ident: Ident,
  pub value: Option<Expression>,

  pub loc: SourceLocation, // location of the let
}

impl LetStatement {
  fn new(ident: Ident, value: Option<Expression>, loc: SourceLocation) -> Self {
    Self { ident, value, loc }
  }
}

pub struct ReqStatement {
  pub file: Expression,
  pub ident: Option<Ident>,
  pub loc: SourceLocation,
}

impl ReqStatement {
  fn new(file: Expression, ident: Option<Ident>, loc: SourceLocation) -> Self {
    Self { file, ident, loc }
  }
}

pub struct LoopStatement {
  pub block: Box<Statement>,
  pub loc: SourceLocation,
}

impl LoopStatement {
  fn new(block: Statement, loc: SourceLocation) -> Self {
    Self {
      block: Box::new(block),
      loc,
    }
  }
}

pub struct MatchStatement {
  pub expr: Expression,
  pub branches: Vec<(Expression, Statement)>,
  pub default: Option<Box<Statement>>,
  pub loc: SourceLocation,
}

impl MatchStatement {
  fn new(
    expr: Expression,
    branches: Vec<(Expression, Statement)>,
    default: Option<Statement>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      expr,
      branches,
      default: default.map(Box::new),
      loc,
    }
  }
}

pub struct PrintStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl PrintStatement {
  fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

pub struct RetStatement {
  pub expr: Option<Expression>,
  pub loc: SourceLocation,
}

impl RetStatement {
  fn new(expr: Option<Expression>, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

pub struct WhileStatement {
  pub comparison: Expression,
  pub block: Box<Statement>,
  pub loc: SourceLocation,
}

impl WhileStatement {
  fn new(comparison: Expression, block: Statement, loc: SourceLocation) -> Self {
    Self {
      comparison,
      block: Box::new(block),
      loc,
    }
  }
}

pub struct ExpressionStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl ExpressionStatement {
  fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

pub enum Expression {
  Literal(LiteralExpression),
  Unary(UnaryExpression),
  Binary(BinaryExpression),
  And(AndExpression),
  Or(OrExpression),
  Group(GroupExpression),
  Ident(IdentExpression),
  Assign(AssignExpression),
  Call(CallExpression),
  List(ListExpression),
  Index(IndexExpression),
  Struct(StructExpression),
  MemberAccess(MemberAccessExpression),
  MemberAssign(MemberAssignExpression),
  Lambda(LambdaExpression),
  Closure(ClosureExpression),
  Method(MethodExpression),
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Self::Literal(l) => write!(f, "literal {}", l.value),
      Self::Unary(u) => write!(f, "unary {:?}", u.op),
      Self::Binary(_) => write!(f, "binary"),
      Self::And(_) => write!(f, "and"),
      Self::Or(_) => write!(f, "or"),
      Self::Group(_) => write!(f, "group"),
      Self::Ident(i) => write!(f, "ident {}", i.ident.name),
      Self::Assign(_) => write!(f, "assign"),
      Self::MemberAccess(_) => write!(f, "member access"),
      Self::MemberAssign(_) => write!(f, "member assign"),
      Self::Call(_) => write!(f, "call"),
      Self::List(_) => write!(f, "list"),
      Self::Index(_) => write!(f, "index"),
      Self::Struct(_) => write!(f, "struct"),
      Self::Lambda(_) => write!(f, "lambda"),
      Self::Closure(_) => write!(f, "closure"),
      Self::Method(_) => write!(f, "method"),
    }
  }
}

impl New<LiteralExpression> for Expression {
  fn new(expr: LiteralExpression) -> Self {
    Self::Literal(expr)
  }
}

impl New<UnaryExpression> for Expression {
  fn new(expr: UnaryExpression) -> Self {
    Self::Unary(expr)
  }
}

impl New<BinaryExpression> for Expression {
  fn new(expr: BinaryExpression) -> Self {
    Self::Binary(expr)
  }
}

impl New<AndExpression> for Expression {
  fn new(expr: AndExpression) -> Self {
    Self::And(expr)
  }
}

impl New<OrExpression> for Expression {
  fn new(expr: OrExpression) -> Self {
    Self::Or(expr)
  }
}

impl New<GroupExpression> for Expression {
  fn new(expr: GroupExpression) -> Self {
    Self::Group(expr)
  }
}

impl New<IdentExpression> for Expression {
  fn new(expr: IdentExpression) -> Self {
    Self::Ident(expr)
  }
}

impl New<AssignExpression> for Expression {
  fn new(expr: AssignExpression) -> Self {
    Self::Assign(expr)
  }
}

impl New<CallExpression> for Expression {
  fn new(expr: CallExpression) -> Self {
    Self::Call(expr)
  }
}

impl New<ListExpression> for Expression {
  fn new(expr: ListExpression) -> Self {
    Self::List(expr)
  }
}

impl New<IndexExpression> for Expression {
  fn new(expr: IndexExpression) -> Self {
    Self::Index(expr)
  }
}

impl New<StructExpression> for Expression {
  fn new(expr: StructExpression) -> Self {
    Self::Struct(expr)
  }
}

impl New<MemberAccessExpression> for Expression {
  fn new(expr: MemberAccessExpression) -> Self {
    Self::MemberAccess(expr)
  }
}

impl New<MemberAssignExpression> for Expression {
  fn new(expr: MemberAssignExpression) -> Self {
    Self::MemberAssign(expr)
  }
}

impl New<LambdaExpression> for Expression {
  fn new(expr: LambdaExpression) -> Self {
    Self::Lambda(expr)
  }
}

impl New<ClosureExpression> for Expression {
  fn new(expr: ClosureExpression) -> Self {
    Self::Closure(expr)
  }
}

impl New<MethodExpression> for Expression {
  fn new(expr: MethodExpression) -> Self {
    Self::Method(expr)
  }
}

pub struct LiteralExpression {
  pub value: Value,

  pub loc: SourceLocation, // location of the literal
}

impl LiteralExpression {
  fn new(value: Value, loc: SourceLocation) -> Self {
    Self { value, loc }
  }
}

#[derive(Debug)]
pub enum UnaryOperator {
  Not,
  Negate,
  Deref,
}

pub struct UnaryExpression {
  pub op: UnaryOperator,
  pub expr: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl UnaryExpression {
  fn new(op: UnaryOperator, expr: Expression, loc: SourceLocation) -> Self {
    Self {
      op,
      expr: Box::new(expr),
      loc,
    }
  }
}

pub struct AndExpression {
  pub left: Box<Expression>,
  pub right: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl AndExpression {
  fn new(left: Expression, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      right: Box::new(right),
      loc,
    }
  }
}

pub struct OrExpression {
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub loc: SourceLocation, // location of the operator
}

impl OrExpression {
  fn new(left: Expression, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      right: Box::new(right),
      loc,
    }
  }
}

pub enum BinaryOperator {
  Equal,
  NotEq,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
}

pub struct BinaryExpression {
  pub left: Box<Expression>,
  pub op: BinaryOperator,
  pub right: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl BinaryExpression {
  fn new(left: Expression, op: BinaryOperator, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      op,
      right: Box::new(right),
      loc,
    }
  }
}

pub struct GroupExpression {
  pub expr: Box<Expression>,

  pub loc: SourceLocation, // location of the left paren
}

impl GroupExpression {
  fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self {
      expr: Box::new(expr),
      loc,
    }
  }
}

pub struct IdentExpression {
  pub ident: Ident,

  pub loc: SourceLocation, // location of the identifier
}

impl IdentExpression {
  fn new(ident: Ident, loc: SourceLocation) -> Self {
    Self { ident, loc }
  }
}

pub enum AssignOperator {
  Assign,
  DerefAssign,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
}

pub struct AssignExpression {
  pub ident: Ident,
  pub op: AssignOperator,
  pub value: Box<Expression>,

  pub loc: SourceLocation, // location of the =
}

impl AssignExpression {
  fn new(ident: Ident, op: AssignOperator, value: Expression, loc: SourceLocation) -> Self {
    Self {
      ident,
      op,
      value: Box::new(value),
      loc,
    }
  }
}

pub struct CallExpression {
  pub callable: Box<Expression>,
  pub args: Vec<Expression>,

  pub loc: SourceLocation,
}

impl CallExpression {
  fn new(callable: Expression, args: Vec<Expression>, loc: SourceLocation) -> Self {
    Self {
      callable: Box::new(callable),
      args,
      loc,
    }
  }
}

pub struct ListExpression {
  pub items: Vec<Expression>,
  pub loc: SourceLocation,
}

impl ListExpression {
  fn new(items: Vec<Expression>, loc: SourceLocation) -> Self {
    Self { items, loc }
  }
}

pub struct IndexExpression {
  pub indexable: Box<Expression>,
  pub index: Box<Expression>,

  pub loc: SourceLocation,
}

impl IndexExpression {
  fn new(indexable: Expression, index: Expression, loc: SourceLocation) -> Self {
    Self {
      indexable: Box::new(indexable),
      index: Box::new(index),
      loc,
    }
  }
}

pub struct MemberAccessExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub loc: SourceLocation,
}

impl MemberAccessExpression {
  fn new(obj: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      loc,
    }
  }
}

pub struct MemberAssignExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub value: Box<Expression>,
  pub loc: SourceLocation,
}

impl MemberAssignExpression {
  fn new(obj: Expression, ident: Ident, value: Expression, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      value: Box::new(value),
      loc,
    }
  }
}

pub struct StructExpression {
  pub members: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl StructExpression {
  fn new(members: Vec<(Ident, Expression)>, loc: SourceLocation) -> Self {
    Self { members, loc }
  }
}

pub struct LambdaExpression {
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl LambdaExpression {
  fn new(params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      params,
      body: Box::new(body),
      loc,
    }
  }
}

impl From<ClosureExpression> for LambdaExpression {
  fn from(expr: ClosureExpression) -> Self {
    Self {
      params: expr.params,
      body: expr.body,
      loc: expr.loc,
    }
  }
}

pub struct ClosureExpression {
  pub captures: StructExpression,
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl ClosureExpression {
  fn new(
    captures: StructExpression,
    params: Vec<Ident>,
    body: Statement,
    loc: SourceLocation,
  ) -> Self {
    Self {
      captures,
      params,
      body: Box::new(body),
      loc,
    }
  }
}

pub struct MethodExpression {
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl MethodExpression {
  fn new(params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      params,
      body: Box::new(body),
      loc,
    }
  }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
  None,
  Assignment, // = <-
  Or,         // or
  And,        // and
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // / *
  Unary,      // - ! @
  Call,       // . () []
  Primary,
}

impl Precedence {
  fn next(&self) -> Option<Self> {
    match self {
      Precedence::None => Some(Precedence::Assignment),
      Precedence::Assignment => Some(Precedence::Or),
      Precedence::Or => Some(Precedence::And),
      Precedence::And => Some(Precedence::Equality),
      Precedence::Equality => Some(Precedence::Comparison),
      Precedence::Comparison => Some(Precedence::Term),
      Precedence::Term => Some(Precedence::Factor),
      Precedence::Factor => Some(Precedence::Unary),
      Precedence::Unary => Some(Precedence::Call),
      Precedence::Call => Some(Precedence::Primary),
      Precedence::Primary => None,
    }
  }
}

type PrefixRule = fn(&mut AstGenerator) -> Option<Expression>;
type InfixRule = fn(&mut AstGenerator, Expression) -> Option<Expression>;

struct ParseRule {
  prefix: Option<PrefixRule>,
  infix: Option<InfixRule>,
  precedence: Precedence,
}

impl ParseRule {
  fn new(prefix: Option<PrefixRule>, infix: Option<InfixRule>, precedence: Precedence) -> Self {
    Self {
      prefix,
      infix,
      precedence,
    }
  }
}
