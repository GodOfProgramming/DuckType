use super::*;
use lex::{NumberToken, Token};
use std::{
  collections::BTreeSet,
  fmt::{Display, Formatter, Result as FmtResult},
  mem,
};

#[cfg(feature = "visit-ast")]
use horrorshow::{helper::doctype, html, prelude::*};

pub struct Ast {
  pub statements: Vec<Statement>,
}

impl Ast {
  pub fn from(tokens: Vec<Token>, meta: Vec<SourceLocation>) -> (Ast, Vec<RuntimeError>) {
    AstGenerator::new(tokens, meta).generate()
  }

  #[cfg(feature = "visit-ast")]
  pub fn dump(&self, file: &str) {
    let out = html! {
      : doctype::HTML;
      head {
        title: "AST";
        link(rel="stylesheet", href="ast.css");
        script(src="ast.js");
      }
      body(class="vertically-centered") {
        |tmpl| {
          for statement in &self.statements {
            statement.dump(tmpl);
          }
        }
      }
    };
    std::fs::write(
      format!("assets/{}.html", Path::new(file).file_name().unwrap().to_string_lossy()),
      format!("{}", out),
    )
    .unwrap();
  }
}

struct AstGenerator {
  tokens: Vec<Token>,
  meta: Vec<SourceLocation>,

  statements: Vec<Statement>,
  errors: Vec<RuntimeError>,

  index: usize,

  in_loop: bool,
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
    }
  }

  fn generate(mut self) -> (Ast, Vec<RuntimeError>) {
    while let Some(current) = self.current() {
      self.statement(current);
    }

    let ast = Ast {
      statements: self.statements,
    };

    (ast, self.errors)
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
      Token::Use => {
        self.advance();
        self.use_stmt();
      }
      Token::While => {
        self.advance();
        self.while_stmt();
      }
      Token::Yield => {
        self.advance();
        self.yield_stmt();
      }
      _ => self.expression_stmt(),
    }
  }

  /* Statements */

  fn block_stmt(&mut self) {
    if let Some(block_loc) = self.meta_at::<1>() {
      if let Some(block) = self.block(block_loc) {
        self.statements.push(Statement::from(block));
      }
    } else {
      // sanity check
      self.error::<0>(String::from("could not find original token"));
    }
  }

  fn break_stmt(&mut self) {
    if !self.in_loop {
      self.error::<1>(String::from("break statements can only be used within loops"));
      return;
    }

    if !self.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    if let Some(loc) = self.meta_at::<2>() {
      self.statements.push(Statement::from(BreakStatement::new(loc)));
    } else {
      // sanity check
      self.error::<1>(String::from("could not find original token"));
    }
  }

  fn cont_stmt(&mut self) {
    if !self.in_loop {
      self.error::<1>(String::from("cont statements can only be used within loops"));
      return;
    }

    if !self.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    if let Some(loc) = self.meta_at::<2>() {
      self.statements.push(Statement::from(ContStatement::new(loc)));
    } else {
      // sanity check
      self.error::<1>(String::from("could not find original token"));
    }
  }

  fn class_stmt(&mut self) {
    if let Some(class_loc) = self.meta_at::<0>() {
      if let Some(Token::Identifier(class_name)) = self.current() {
        self.advance();

        if !self.consume(Token::LeftBrace, "expected '{' after class name") {
          return;
        }

        let mut initializer = None;
        let mut class_members = Vec::default();
        let mut declared_functions = BTreeSet::default();

        while let Some(token) = self.current() {
          let class_loc = class_loc.clone();
          match token {
            Token::New => {
              self.advance();
              if initializer.is_none() {
                if self.consume(Token::LeftParen, "expected '(' after 'new'") {
                  initializer = self.lambda_expr(true, Token::RightParen, |this, params, mut body| {
                    if !params.found_self {
                      this.error::<0>(String::from("missing self arg in initializer"));
                      None
                    } else {
                      body
                        .statements
                        .push(Statement::from(DefaultConstructorRet::new(class_loc.clone())));
                      Some(Expression::from(LambdaExpression::new(
                        params.list,
                        Statement::from(body),
                        class_loc,
                      )))
                    }
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
                    if let Some(function) = self.lambda_expr(true, Token::RightParen, |_this, params, body| {
                      declared_functions.insert(ident.clone());
                      if params.found_self {
                        Some(Expression::from(MethodExpression::new(
                          params.list,
                          Statement::from(body),
                          class_loc,
                        )))
                      } else {
                        Some(Expression::from(LambdaExpression::new(
                          params.list,
                          Statement::from(body),
                          class_loc,
                        )))
                      }
                    }) {
                      class_members.push((Ident::new(ident), function));
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

        self.statements.push(Statement::from(ClassStatement::new(
          Ident::new(class_name),
          initializer,
          class_members,
          class_loc,
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
            Statement::from(declaration)
          } else {
            return;
          }
        } else if let Some(expr) = self.expression() {
          Statement::from(ExpressionStatement::new(expr, initializer_loc))
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
                  self.statements.push(Statement::from(ForStatement::new(
                    initializer,
                    comparison,
                    increment,
                    Statement::from(block),
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
      self.statements.push(Statement::from(if_stmt));
    }
  }

  fn let_stmt(&mut self) {
    if let Some(declaration) = self.declaration() {
      if !self.consume(Token::Semicolon, "expected ';' after expression") {
        return;
      }

      self.statements.push(Statement::from(declaration));
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

        self.statements.push(Statement::from(ReqStatement::new(expr, ident, loc)));
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
      if let Some(block) = self.block(loc.clone()) {
        self
          .statements
          .push(Statement::from(LoopStatement::new(Statement::from(block), loc)));
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
                  Statement::from(block)
                } else {
                  break;
                }
              } else if let Some(eval_loc) = self.meta_at::<0>() {
                if let Some(eval) = self.expression() {
                  if !self.consume(Token::Comma, "expected ',' after expression") {
                    break;
                  }
                  Statement::from(ExpressionStatement::new(eval, eval_loc))
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

        let default = if self.advance_if_matches(Token::Else) && self.advance_if_matches(Token::LeftBrace) {
          if let Some(else_loc) = self.meta_at::<2>() {
            self.block(else_loc).map(Statement::from)
          } else {
            // sanity check
            self.error::<0>(String::from("could not find original token"));
            None
          }
        } else {
          None
        };

        self
          .statements
          .push(Statement::from(MatchStatement::new(expr, branches, default, loc)))
      }
    }
  }

  fn print_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<1>() {
      if let Some(expr) = self.expression() {
        if !self.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }
        self.statements.push(Statement::from(PrintStatement::new(expr, loc)));
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

        self.statements.push(Statement::from(RetStatement::new(expr, loc)));
      }
    }
  }

  fn use_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<0>() {
      let path = self.resolve();
      if path.is_empty() {
        self.error::<1>(String::from("use must have a type following it"));
      } else if self.consume(Token::Semicolon, "expecte ';' after use") {
        self.statements.push(Statement::from(UseStatement::new(path, loc)));
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
        if let Some(block) = self.block(loc.clone()) {
          self
            .statements
            .push(Statement::from(WhileStatement::new(expr, Statement::from(block), loc)));
        }
      } else {
        // sanity check
        self.error::<0>(String::from("could not find original token"));
      }

      self.in_loop = prev;
    }
  }

  fn yield_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<0>() {
      if self.consume(Token::Semicolon, "expected ';' after yield") {
        self.statements.push(Statement::from(YieldStatement::new(loc)));
      }
    }
  }

  fn expression_stmt(&mut self) {
    if let Some(loc) = self.meta_at::<0>() {
      if let Some(expr) = self.expression() {
        if !self.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }
        self.statements.push(Statement::from(ExpressionStatement::new(expr, loc)));
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
          expr = Some(Expression::from(LiteralExpression::new(Value::nil, self.meta_at::<1>()?)));
        }
        Token::True => {
          expr = Some(Expression::from(LiteralExpression::new(
            Value::from(true),
            self.meta_at::<1>()?,
          )));
        }
        Token::False => {
          expr = Some(Expression::from(LiteralExpression::new(
            Value::from(false),
            self.meta_at::<1>()?,
          )));
        }
        Token::String(s) => expr = Some(Expression::from(LiteralExpression::new(Value::from(s), self.meta_at::<1>()?))),
        Token::Number(n) => {
          expr = Some(Expression::from(LiteralExpression::new(
            match n {
              NumberToken::I32(i) => Value::from(i),
              NumberToken::F64(f) => Value::from(f),
            },
            self.meta_at::<1>()?,
          )))
        }
        _ => {
          self.error::<1>(String::from("sanity check, invalid literal, very bad logic error"));
        }
      }
    } else {
      self.error::<1>(String::from("sanity check, no previous token, very bad logic error"));
    }

    expr
  }

  fn unary_expr(&mut self) -> Option<Expression> {
    if let Some(operator_token) = self.previous() {
      let op_meta = self.meta_at::<1>()?;
      let op = match operator_token {
        Token::Bang => UnaryOperator::Not,
        Token::Minus => UnaryOperator::Negate,
        _ => {
          self.error::<1>(String::from("invalid unary operator"));
          return None;
        }
      };

      let expr = self.parse_precedence(Precedence::Unary)?;
      Some(Expression::from(UnaryExpression::new(op, expr, op_meta)))
    } else {
      self.error::<1>(String::from("tried to make unary expression without operator"));
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
        Some(Expression::from(BinaryExpression::new(left, op, expr, op_meta)))
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
      Some(Expression::from(AndExpression::new(left, expr, op_meta)))
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
      Some(Expression::from(OrExpression::new(left, expr, op_meta)))
    } else {
      self.error::<1>(String::from("unable to retrieve precedence for or expr")); // this may not be an error?
      None
    }
  }

  fn group_expr(&mut self) -> Option<Expression> {
    let paren_meta = self.meta_at::<1>()?;
    let expr = self.expression()?;
    if self.consume(Token::RightParen, "expected ')' after expression") {
      Some(Expression::from(GroupExpression::new(expr, paren_meta)))
    } else {
      None
    }
  }

  fn ident_expr(&mut self) -> Option<Expression> {
    if let Some(ident_token) = self.previous() {
      if let Token::Identifier(ident_name) = ident_token {
        Some(Expression::from(IdentExpression::new(
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
    if let Expression::Ident(expr) = &left {
      if expr.ident.name == "self" {
        self.error::<0>(String::from("cannot change the value of self"));
      }
    }

    if let Expression::Ident(ident_expr) = left {
      let op = self.previous()?;
      let op_meta = self.meta_at::<1>()?;
      let value = self.expression()?;

      let op = match op {
        Token::Equal => AssignOperator::Assign,
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

      Some(Expression::from(AssignExpression::new(ident_expr.ident, op, value, op_meta)))
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
      Some(Expression::from(CallExpression::new(expr, args, paren_meta)))
    } else {
      None
    }
  }

  fn list_expr(&mut self) -> Option<Expression> {
    let bracket_meta = self.meta_at::<1>()?;
    let mut items = Vec::default();

    if let Some(token) = self.current() {
      if token != Token::RightBracket {
        loop {
          items.push(self.expression()?);
          if !self.advance_if_matches(Token::Comma) {
            break;
          }
        }
      }
    }

    if self.consume(Token::RightBracket, "expect ']' after arguments") {
      Some(Expression::from(ListExpression::new(items, bracket_meta)))
    } else {
      None
    }
  }

  fn index_expr(&mut self, expr: Expression) -> Option<Expression> {
    let bracket_meta = self.meta_at::<1>()?;

    let index = self.expression()?;

    if self.consume(Token::RightBracket, "expected ']' after expression") {
      Some(Expression::from(IndexExpression::new(expr, index, bracket_meta)))
    } else {
      None
    }
  }

  fn member_expr(&mut self, obj: Expression) -> Option<Expression> {
    if let Some(token) = self.current() {
      if let Token::Identifier(ident) = token {
        let ident_meta = self.meta_at::<0>()?;

        self.advance();

        if let Some(current) = self.current() {
          match current {
            Token::Equal => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Assign,
                value,
                ident_meta,
              )))
            }
            Token::PlusEqual => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Add,
                value,
                ident_meta,
              )))
            }
            Token::MinusEqual => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Sub,
                value,
                ident_meta,
              )))
            }
            Token::AsteriskEqual => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Mul,
                value,
                ident_meta,
              )))
            }
            Token::SlashEqual => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Div,
                value,
                ident_meta,
              )))
            }
            Token::PercentEqual => {
              self.advance();
              let value = self.expression()?;
              Some(Expression::from(MemberAssignExpression::new(
                obj,
                Ident::new(ident),
                AssignOperator::Mod,
                value,
                ident_meta,
              )))
            }
            _ => Some(Expression::from(MemberAccessExpression::new(
              obj,
              Ident::new(ident),
              ident_meta,
            ))),
          }
        } else {
          self.error::<1>(String::from("expected token following member access"));
          None
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
      let struct_meta = struct_meta.clone();
      if token == Token::RightBrace {
        break;
      }

      if let Token::Identifier(ident) = token {
        self.advance();
        if self.advance_if_matches(Token::Comma) || self.check_peek_after::<0>(Token::RightBrace) {
          members.push((
            Ident::new(ident.clone()),
            Expression::from(IdentExpression::new(Ident::new(ident), struct_meta)),
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
      if self.advance_if_matches(Token::Pipe) {
        self.closure_expr(Token::Pipe, StructExpression::new(members, struct_meta))
      } else {
        Some(Expression::from(StructExpression::new(members, struct_meta)))
      }
    } else {
      self.error::<1>(String::from("expected token"));
      None
    }
  }

  fn anon_fn_expr(&mut self) -> Option<Expression> {
    let loc = self.meta_at::<0>()?;

    self.lambda_expr(false, Token::Pipe, |_, params, body| {
      Some(Expression::from(LambdaExpression::new(
        params.list,
        Statement::from(body),
        loc,
      )))
    })
  }

  fn closure_expr(&mut self, param_term: Token, captures: StructExpression) -> Option<Expression> {
    let loc = self.meta_at::<0>()?;

    self.lambda_expr(false, param_term, |_, params, body| {
      Some(Expression::from(ClosureExpression::new(
        captures,
        params.list,
        Statement::from(body),
        loc,
      )))
    })
  }

  fn lambda_expr<F>(&mut self, allow_self: bool, param_term: Token, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut Self, Params, BlockStatement) -> Option<Expression>,
  {
    let mut params = self.parse_parameters(param_term.clone())?;

    if !allow_self && params.found_self {
      self.error::<0>(String::from("found 'self' in invalid context"));
      return None;
    } else if params.found_self {
      params.list.push(Ident::new("self"));
    }

    if !self.consume(param_term, "expected terminator after parameters") {
      return None;
    }

    if !self.consume(Token::LeftBrace, "expected '{' after paren") {
      return None;
    }

    if let Some(block_loc) = self.meta_at::<1>() {
      let body = self.block(block_loc)?;
      f(self, params, body)
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
      self.error::<1>(format!("tried to lookup a token at an invalid index: {}", err.to_string()));
      false
    }
  }

  fn parse_fn(&mut self) -> Option<Statement> {
    if let Some(loc) = self.meta_at::<0>() {
      if let Some(current) = self.current() {
        self.advance();
        if !self.consume(Token::LeftParen, "expect '(' after function name") {
          return None;
        }

        if let Some(params) = self.parse_parameters(Token::RightParen) {
          if params.found_self {
            self.error::<0>(String::from("found 'self' in invalid context"));
            return None;
          }

          if !self.consume(Token::RightParen, "expected ')' after arguments") {
            return None;
          }

          if !self.consume(Token::LeftBrace, "expected '{' after paren") {
            return None;
          }

          if let Some(block_loc) = self.meta_at::<1>() {
            let ident = Ident::new(match current {
              Token::Identifier(fn_name) => fn_name,
              other => match other {
                Token::Bang => ops::NOT,
                Token::Plus => ops::ADD,
                Token::Minus => {
                  if params.list.is_empty() {
                    ops::NEG
                  } else {
                    ops::SUB
                  }
                }
                Token::Asterisk => ops::MUL,
                Token::Slash => ops::DIV,
                Token::Percent => ops::REM,
                Token::EqualEqual => ops::EQUALITY,
                Token::BangEqual => ops::NOT_EQUAL,
                Token::Less => ops::LESS,
                Token::LessEqual => ops::LESS_EQUAL,
                Token::Greater => ops::GREATER,
                Token::GreaterEqual => ops::GREATER_EQUAL,
                _ => None?,
              }
              .to_string(),
            });
            self
              .block(block_loc)
              .map(|body| Statement::from(FnStatement::new(ident, params.list, Statement::from(body), loc)))
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

  fn resolve(&mut self) -> Vec<Ident> {
    let mut parts = Vec::default();

    while let Some(current) = self.current() {
      match current {
        Token::Identifier(ident) => parts.push(Ident::new(ident)),
        Token::Dot => (/* intentionally do nothing */),
        _ => break,
      }
      self.advance();
    }

    parts
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
        println!("{} ({}, {}): {}", meta.file, meta.line, meta.column, msg);
      }
      self.errors.push(RuntimeError {
        msg,
        file: meta.file.deref().deref().clone(),
        line: meta.line,
        column: meta.column,
      });
    } else {
      self.errors.push(RuntimeError {
        msg: format!("could not find location of token for msg '{}'", msg),
        file: String::default(),
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
    let statements = self.scope(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
    });

    if self.consume(Token::RightBrace, "expected '}' after block") {
      Some(BlockStatement::new(statements, loc))
    } else {
      None
    }
  }

  fn branch(&mut self) -> Option<IfStatement> {
    if let Some(expr) = self.expression() {
      if !self.consume(Token::LeftBrace, "expected '{' after condition") {
        return None;
      }

      if let Some(block_loc) = self.meta_at::<1>() {
        if let Some(block) = self.block(block_loc.clone()) {
          let else_block = if self.advance_if_matches(Token::Else) {
            if let Some(else_meta) = self.meta_at::<1>() {
              if let Some(token) = self.current() {
                match token {
                  Token::LeftBrace => {
                    self.advance();
                    Some(Statement::from(self.block(else_meta)?))
                  }
                  Token::If => {
                    self.advance();
                    Some(Statement::from(self.branch()?))
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

          return Some(IfStatement::new(expr, Statement::from(block), else_block, block_loc));
        }
      } else {
        // sanity check
        self.error::<0>(String::from("could not find original token"));
      }
    }

    None
  }

  fn parse_parameters(&mut self, terminator: Token) -> Option<Params> {
    let mut found_self = false;
    let mut params = Vec::default();

    if let Some(mut token) = self.current() {
      if token != terminator {
        loop {
          if let Token::Identifier(ident) = token {
            if params.contains(&ident) {
              self.error::<0>(format!("duplicate identifier '{}' found in parameter list", ident));
              return None;
            }

            if ident == "self" {
              if found_self {
                self.error::<0>(String::from("self found twice in parameter list"));
                return None;
              } else {
                found_self = true;
              }
            } else {
              params.push(ident);
            }
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

    Some(Params::from((found_self, params.into_iter().map(Ident::new).collect())))
  }

  fn declaration(&mut self) -> Option<LetStatement> {
    if let Some(let_loc) = self.meta_at::<1>() {
      if let Some(Token::Identifier(ident)) = self.current() {
        let ident = Ident::new(ident);
        self.advance();

        let value = if self.advance_if_matches(Token::Equal) {
          self.expression()
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

  fn parse_precedence(&mut self, root_token_precedence: Precedence) -> Option<Expression> {
    self.advance();

    if let Some(prev) = self.previous() {
      let mut expr: Expression;
      let prev_token_rule = Self::rule_for(&prev);

      if let Some(prefix) = prev_token_rule.prefix {
        match prefix(self) {
          Some(e) => expr = e,
          None => {
            self.error::<1>(format!("no prefix rule for {}", prev));
            return None;
          }
        }
      } else {
        self.error::<1>(String::from("expected an expression"));
        return None;
      }

      while let Some(curr) = self.current() {
        let current_token_rule = Self::rule_for(&curr);
        if root_token_precedence <= current_token_rule.precedence {
          if let Some(infix) = current_token_rule.infix {
            self.advance();
            match infix(self, expr) {
              Some(e) => expr = e,
              None => return None,
            }
          } else {
            // TODO if weird results start happening, this could be the cause
            // allows for structs to be made and braces to still work for scoping purposes
            //   self.error::<0>(format!("no infix rule for {:?}", curr));
            //   return None;
            break;
          }
        } else {
          break;
        }
      }

      Some(expr)
    } else {
      self.error::<2>(String::from("unexpected end of token stream (parse_precedence 3)"));
      None
    }
  }

  fn rule_for(token: &Token) -> ParseRule {
    match token {
      Token::LeftParen => ParseRule::new(Some(Self::group_expr), Some(Self::call_expr), Precedence::Call),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(Some(Self::struct_expr), None, Precedence::Primary),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::LeftBracket => ParseRule::new(Some(Self::list_expr), Some(Self::index_expr), Precedence::Call),
      Token::RightBracket => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, Some(Self::member_expr), Precedence::Call),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Colon => ParseRule::new(None, None, Precedence::None),
      Token::At => ParseRule::new(None, None, Precedence::None),
      Token::Pipe => ParseRule::new(Some(Self::anon_fn_expr), None, Precedence::Primary),
      Token::Plus => ParseRule::new(None, Some(Self::binary_expr), Precedence::Term),
      Token::PlusEqual => ParseRule::new(None, Some(Self::assign_expr), Precedence::Assignment),
      Token::Minus => ParseRule::new(Some(Self::unary_expr), Some(Self::binary_expr), Precedence::Term),
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
      Token::BackArrow => ParseRule::new(None, None, Precedence::None),
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
      Token::Use => ParseRule::new(None, None, Precedence::None),
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
    Self { name: name.to_string() }
  }

  pub fn is_global(&self) -> bool {
    matches!(self.name.chars().next(), Some('$'))
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name)
  }
}

pub enum Statement {
  Block(BlockStatement),
  Break(BreakStatement),
  Cont(ContStatement),
  Class(ClassStatement),
  DefaultConstructorRet(DefaultConstructorRet),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Let(LetStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Print(PrintStatement),
  Req(ReqStatement),
  Ret(RetStatement),
  Use(UseStatement),
  While(WhileStatement),
  Yield(YieldStatement),
  Expression(ExpressionStatement),
}

impl Statement {
  #[cfg(feature = "visit-ast")]
  fn dump(&self, tmpl: &mut TemplateBuffer) {
    html! {
      div(class="node vertically-centered") {
        span(class="bubble", onclick="click_node(this)") : self.to_string();
        div(id="child", class="hidden") {
          |tmpl| self.dump_children(tmpl);
        }
      }
    }
    .render(tmpl);
  }

  #[cfg(feature = "visit-ast")]
  fn dump_children(&self, tmpl: &mut TemplateBuffer) {
    match self {
      Statement::Block(blk) => {
        for statement in &blk.statements {
          statement.dump(tmpl)
        }
      }
      Statement::Break(_) => (),
      Statement::Cont(_) => (),
      Statement::Class(c) => {
        if let Some(init) = &c.initializer {
          html! {
            div(class="children") {
              div(class="vertically-centered") {
                div(class="bubble") : "new";
                |tmpl| init.dump(tmpl);
              }
              @ for (ident, method) in c.methods.iter() {
                div(class="vertically-centered") {
                  div(class="bubble") : format_args!("fn {}", ident.to_string());
                  |tmpl| method.dump(tmpl);
                }
              }
            }
          }
          .render(tmpl);
        } else {
          html! {
            div(class="children") {
              @ for (ident, method) in c.methods.iter() {
                div(class="vertically-centered") {
                  div(class="bubble") : format_args!("fn {}", ident.to_string());
                  |tmpl| method.dump(tmpl);
                }
              }
            }
          }
          .render(tmpl);
        }
      }
      Statement::DefaultConstructorRet(_) => (),
      Statement::Fn(_) => (),
      Statement::For(_) => (),
      Statement::If(_) => (),
      Statement::Let(l) => {
        if let Some(expr) = &l.value {
          html! {
            div(class="children") {
              div(class="bubble child-node") : l.ident.to_string();
              div(class="bubble child-node") { |tmpl| expr.dump(tmpl); }
            }
          }
          .render(tmpl);
        } else {
          html! {
            div(class="bubble unary") : l.ident.to_string();
          }
          .render(tmpl);
        }
      }
      Statement::Loop(_) => (),
      Statement::Match(_) => (),
      Statement::Print(_) => (),
      Statement::Req(_) => (),
      Statement::Ret(_) => (),
      Statement::Use(u) => {
        html! {
          span(class="bubble") : itertools::join(u.path.iter(), ".");
        }
        .render(tmpl);
      }
      Statement::While(_) => (),
      Statement::Yield(_) => (),
      Statement::Expression(e) => e.expr.dump(tmpl),
    }
  }
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      Self::Block(_) => write!(f, "block"),
      Self::Break(_) => write!(f, "break"),
      Self::Cont(_) => write!(f, "cont"),
      Self::Class(c) => write!(f, "class {}", c.ident.name),
      Self::DefaultConstructorRet(_) => write!(f, "default constructor ret"),
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
      Self::Use(_) => write!(f, "use"),
      Self::Yield(_) => write!(f, "yield"),
      Self::Expression(_) => write!(f, "expression"),
    }
  }
}

impl From<BlockStatement> for Statement {
  fn from(stmt: BlockStatement) -> Self {
    Self::Block(stmt)
  }
}

impl From<BreakStatement> for Statement {
  fn from(stmt: BreakStatement) -> Self {
    Self::Break(stmt)
  }
}

impl From<ContStatement> for Statement {
  fn from(stmt: ContStatement) -> Self {
    Self::Cont(stmt)
  }
}

impl From<ClassStatement> for Statement {
  fn from(stmt: ClassStatement) -> Self {
    Self::Class(stmt)
  }
}

impl From<DefaultConstructorRet> for Statement {
  fn from(stmt: DefaultConstructorRet) -> Self {
    Self::DefaultConstructorRet(stmt)
  }
}

impl From<FnStatement> for Statement {
  fn from(stmt: FnStatement) -> Self {
    Self::Fn(stmt)
  }
}

impl From<ForStatement> for Statement {
  fn from(stmt: ForStatement) -> Self {
    Self::For(stmt)
  }
}

impl From<IfStatement> for Statement {
  fn from(stmt: IfStatement) -> Self {
    Self::If(stmt)
  }
}

impl From<LetStatement> for Statement {
  fn from(stmt: LetStatement) -> Self {
    Self::Let(stmt)
  }
}

impl From<ReqStatement> for Statement {
  fn from(stmt: ReqStatement) -> Self {
    Self::Req(stmt)
  }
}

impl From<LoopStatement> for Statement {
  fn from(stmt: LoopStatement) -> Self {
    Self::Loop(stmt)
  }
}

impl From<MatchStatement> for Statement {
  fn from(stmt: MatchStatement) -> Self {
    Self::Match(stmt)
  }
}

impl From<PrintStatement> for Statement {
  fn from(stmt: PrintStatement) -> Self {
    Self::Print(stmt)
  }
}

impl From<RetStatement> for Statement {
  fn from(stmt: RetStatement) -> Self {
    Self::Ret(stmt)
  }
}

impl From<UseStatement> for Statement {
  fn from(stmt: UseStatement) -> Self {
    Self::Use(stmt)
  }
}

impl From<WhileStatement> for Statement {
  fn from(stmt: WhileStatement) -> Self {
    Self::While(stmt)
  }
}

impl From<YieldStatement> for Statement {
  fn from(stmt: YieldStatement) -> Self {
    Self::Yield(stmt)
  }
}

impl From<ExpressionStatement> for Statement {
  fn from(stmt: ExpressionStatement) -> Self {
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
  fn new(ident: Ident, initializer: Option<Expression>, methods: Vec<(Ident, Expression)>, loc: SourceLocation) -> Self {
    Self {
      ident,
      initializer,
      methods,
      loc,
    }
  }
}

pub struct DefaultConstructorRet {
  pub loc: SourceLocation,
}

impl DefaultConstructorRet {
  fn new(loc: SourceLocation) -> Self {
    Self { loc }
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
  fn new(initializer: Statement, comparison: Expression, increment: Expression, block: Statement, loc: SourceLocation) -> Self {
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
  fn new(comparison: Expression, block: Statement, else_block: Option<Statement>, loc: SourceLocation) -> Self {
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
  fn new(expr: Expression, branches: Vec<(Expression, Statement)>, default: Option<Statement>, loc: SourceLocation) -> Self {
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

pub struct UseStatement {
  pub path: Vec<Ident>,
  pub loc: SourceLocation,
}

impl UseStatement {
  fn new(path: Vec<Ident>, loc: SourceLocation) -> Self {
    Self { path, loc }
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

pub struct YieldStatement {
  pub loc: SourceLocation,
}

impl YieldStatement {
  fn new(loc: SourceLocation) -> Self {
    Self { loc }
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

impl Expression {
  #[cfg(feature = "visit-ast")]
  fn dump(&self, tmpl: &mut TemplateBuffer) {
    match self {
      Expression::Literal(l) => {
        html! {
          : l.value.to_string();
        }
        .render(tmpl);
      }
      Expression::Unary(_) => (),
      Expression::Binary(_) => (),
      Expression::And(_) => (),
      Expression::Or(_) => (),
      Expression::Group(_) => (),
      Expression::Ident(_) => (),
      Expression::Assign(_) => (),
      Expression::Call(_) => (),
      Expression::List(_) => (),
      Expression::Index(_) => (),
      Expression::Struct(_) => (),
      Expression::MemberAccess(_) => (),
      Expression::MemberAssign(_) => (),
      Expression::Lambda(l) => l.body.dump(tmpl),
      Expression::Closure(c) => c.body.dump(tmpl),
      Expression::Method(m) => m.body.dump(tmpl),
    }
  }
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

impl From<LiteralExpression> for Expression {
  fn from(expr: LiteralExpression) -> Self {
    Self::Literal(expr)
  }
}

impl From<UnaryExpression> for Expression {
  fn from(expr: UnaryExpression) -> Self {
    Self::Unary(expr)
  }
}

impl From<BinaryExpression> for Expression {
  fn from(expr: BinaryExpression) -> Self {
    Self::Binary(expr)
  }
}

impl From<AndExpression> for Expression {
  fn from(expr: AndExpression) -> Self {
    Self::And(expr)
  }
}

impl From<OrExpression> for Expression {
  fn from(expr: OrExpression) -> Self {
    Self::Or(expr)
  }
}

impl From<GroupExpression> for Expression {
  fn from(expr: GroupExpression) -> Self {
    Self::Group(expr)
  }
}

impl From<IdentExpression> for Expression {
  fn from(expr: IdentExpression) -> Self {
    Self::Ident(expr)
  }
}

impl From<AssignExpression> for Expression {
  fn from(expr: AssignExpression) -> Self {
    Self::Assign(expr)
  }
}

impl From<CallExpression> for Expression {
  fn from(expr: CallExpression) -> Self {
    Self::Call(expr)
  }
}

impl From<ListExpression> for Expression {
  fn from(expr: ListExpression) -> Self {
    Self::List(expr)
  }
}

impl From<IndexExpression> for Expression {
  fn from(expr: IndexExpression) -> Self {
    Self::Index(expr)
  }
}

impl From<StructExpression> for Expression {
  fn from(expr: StructExpression) -> Self {
    Self::Struct(expr)
  }
}

impl From<MemberAccessExpression> for Expression {
  fn from(expr: MemberAccessExpression) -> Self {
    Self::MemberAccess(expr)
  }
}

impl From<MemberAssignExpression> for Expression {
  fn from(expr: MemberAssignExpression) -> Self {
    Self::MemberAssign(expr)
  }
}

impl From<LambdaExpression> for Expression {
  fn from(expr: LambdaExpression) -> Self {
    Self::Lambda(expr)
  }
}

impl From<ClosureExpression> for Expression {
  fn from(expr: ClosureExpression) -> Self {
    Self::Closure(expr)
  }
}

impl From<MethodExpression> for Expression {
  fn from(expr: MethodExpression) -> Self {
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
  pub op: AssignOperator,
  pub value: Box<Expression>,
  pub loc: SourceLocation,
}

impl MemberAssignExpression {
  fn new(obj: Expression, ident: Ident, op: AssignOperator, value: Expression, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      op,
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
  fn new(captures: StructExpression, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
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

struct Params {
  found_self: bool,
  list: Vec<Ident>,
}

impl From<(bool, Vec<Ident>)> for Params {
  fn from((found_self, list): (bool, Vec<Ident>)) -> Self {
    Self { found_self, list }
  }
}

impl From<Vec<Ident>> for Params {
  fn from(list: Vec<Ident>) -> Self {
    Self { found_self: false, list }
  }
}
