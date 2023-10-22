mod expr;
mod stmt;

use super::{lex::Token, SourceLocation};
use crate::{prelude::*, UnwrapAnd};
pub use expr::*;
use std::{
  collections::BTreeSet,
  fmt::{Binary, Display, Formatter, Result as FmtResult},
  mem,
  rc::Rc,
};
pub use stmt::*;
#[cfg(feature = "visit-ast")]
use {
  horrorshow::{helper::doctype, html},
  std::path::Path,
};

trait AstStatement {
  fn stmt(ast: &mut AstGenerator);
}

trait AstExpression {
  #[allow(unused_variables)]
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    None
  }

  #[allow(unused_variables)]
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    None
  }
}

pub struct Ast {
  pub statements: Vec<Statement>,
}

impl Ast {
  pub fn from(tokens: Vec<Token>, meta: Vec<SourceLocation>) -> (Ast, Vec<RuntimeError>) {
    AstGenerator::new(tokens, meta).generate()
  }

  #[cfg(feature = "visit-ast")]
  pub fn dump(&self, file: &Path) {
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
      format!("assets/{}.html", file.file_name().unwrap().to_string_lossy()),
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
  export_found: bool,
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
      export_found: false,
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
        BreakStatement::stmt(self);
      }
      Token::Cont => {
        self.advance();
        ContStatement::stmt(self);
      }
      Token::Class => {
        self.advance();
        ClassStatement::stmt(self);
      }
      Token::Export => {
        self.advance();
        ExportStatement::stmt(self);
      }
      Token::Fn => {
        self.advance();
        FnStatement::stmt(self);
      }
      Token::For => {
        self.advance();
        ForStatement::stmt(self);
      }
      Token::If => {
        self.advance();
        IfStatement::stmt(self);
      }
      Token::LeftBrace => {
        self.advance();
        BlockStatement::stmt(self);
      }
      Token::Let => {
        self.advance();
        LetStatement::stmt(self);
      }
      Token::Loop => {
        self.advance();
        LoopStatement::stmt(self);
      }
      Token::Match => {
        self.advance();
        MatchStatement::stmt(self);
      }
      Token::Mod => {
        self.advance();
        ModStatement::stmt(self);
      }
      Token::Print => {
        self.advance();
        PrintStatement::stmt(self);
      }
      Token::Req => {
        self.advance();
        ReqStatement::stmt(self);
      }
      Token::Ret => {
        self.advance();
        RetStatement::stmt(self);
      }
      Token::Use => {
        self.advance();
        UseStatement::stmt(self);
      }
      Token::While => {
        self.advance();
        WhileStatement::stmt(self);
      }
      Token::Yield => {
        self.advance();
        YieldStatement::stmt(self);
      }
      Token::Breakpoint => {
        self.advance();
        self.breakpoint_stmt();
      }
      _ => ExpressionStatement::stmt(self),
    }
  }

  /* Statements */

  fn breakpoint_stmt(&mut self) {
    self
      .meta_at::<0>()
      .unwrap_and(|meta| self.statements.push(Statement::Breakpoint(meta)));
    if !self.consume(Token::Semicolon, "expected ';' after value") {
      return;
    }
  }

  /* Expressions */

  fn expression(&mut self) -> Option<Expression> {
    self.parse_precedence(Precedence::Assignment)
  }

  fn class_expr(&mut self, name: Option<Ident>) -> Option<Expression> {
    if let Some(class_loc) = self.meta_at::<0>() {
      // needed here because infix advances to '{' after seeing "class"
      if !self.consume(Token::LeftBrace, "expected '{' to begin class body") {
        return None;
      }

      let mut initializer = None;
      let mut class_members = Vec::default();
      let mut declared_functions = BTreeSet::default();

      while let Some(token) = self.current() {
        if let Some(member_loc) = self.meta_at::<0>() {
          match token {
            Token::New => {
              self.advance();
              if initializer.is_none() {
                if self.consume(Token::LeftParen, "expected '(' after 'new'") {
                  initializer = self.lambda_expr(SelfRules::Require, Token::RightParen, |_this, params, mut body| {
                    body
                      .statements
                      .push(Statement::from(DefaultConstructorRet::new(member_loc.clone())));
                    Some(Expression::from(LambdaExpression::new(
                      params.list,
                      Statement::from(body),
                      member_loc,
                    )))
                  });
                }
              } else {
                self.error::<0>(String::from("duplicate initializer found"));
              }
            }
            Token::Fn => {
              self.advance();
              if let Some(fn_name) = self.current() {
                self.advance();
                if self.consume(Token::LeftParen, "expected '(' after identifier") {
                  if let Some(params) = self.parse_parameters(Token::RightParen) {
                    if let Some(ident) = self.fn_ident(fn_name, &params) {
                      if !declared_functions.contains(&ident.name) {
                        if let Some(function) = self.parse_lambda(params, |_this, params, body| {
                          declared_functions.insert(ident.name.clone());
                          if params.found_self {
                            Some(Expression::from(MethodExpression::new(
                              ident.clone(),
                              params.list,
                              Statement::from(body),
                              member_loc,
                            )))
                          } else {
                            Some(Expression::from(LambdaExpression::new(
                              params.list,
                              Statement::from(body),
                              member_loc,
                            )))
                          }
                        }) {
                          class_members.push((ident, function));
                        }
                      } else {
                        self.error::<0>("duplicate method definition");
                      }
                    }
                  }
                }
              }
            }
            Token::RightBrace => break,
            t => self.error::<0>(format!("unexpected token in class {t}")),
          }
        } else {
          self.error::<0>("no meta found at location");
        }
      }

      if !self.consume(Token::RightBrace, "expected '}' after class body") {
        return None;
      }

      Some(Expression::from(ClassExpression::new(
        name,
        initializer,
        class_members,
        class_loc,
      )))
    } else {
      self.error::<0>("no meta found at location");
      None
    }
  }

  fn mod_expr(&mut self, name: Option<Ident>) -> Option<Expression> {
    if let Some(mod_loc) = self.meta_at::<0>() {
      if !self.consume(Token::LeftBrace, "expected '{' after mod name") {
        return None;
      }

      let mut module_items = Vec::default();
      let mut declared_items = BTreeSet::default();

      while let Some(token) = self.current() {
        if let Some(member_loc) = self.meta_at::<0>() {
          match token {
            Token::Identifier(ident) => {
              declared_items.insert(ident.clone());
              self.advance();

              if self.advance_if_matches(Token::Colon) {
                let ident = Ident::new(ident);
                if let Some(expr) = self.expression() {
                  module_items.push((ident, expr));
                }
              } else {
                module_items.push((
                  Ident::new(ident.clone()),
                  Expression::from(IdentExpression::new(Ident::new(ident), member_loc)),
                ))
              }
              if !matches!(self.current(), Some(Token::RightBrace)) {
                if !self.consume(Token::Comma, "expected ',' after expression") {
                  return None;
                }
              }
            }
            Token::Mod => {
              self.advance();
              if let Some(Token::Identifier(ident)) = self.current() {
                declared_items.insert(ident.clone());
                self.advance();
                let ident = Ident::new(ident);
                if let Some(module) = self.mod_expr(Some(ident.clone())) {
                  module_items.push((ident, module));
                }
              } else {
                self.error::<0>("mod name is invalid");
              }
            }
            Token::Class => {
              self.advance();
              if let Some(Token::Identifier(ident)) = self.current() {
                declared_items.insert(ident.clone());
                self.advance();
                let ident = Ident::new(ident);
                if let Some(class) = self.class_expr(Some(ident.clone())) {
                  module_items.push((ident, class));
                }
              } else {
                self.error::<0>("class name is invalid");
              }
            }
            Token::Fn => {
              self.advance();
              if let Some(fn_name) = self.current() {
                self.advance();
                if self.consume(Token::LeftParen, "expected '(' after identifier") {
                  if let Some(params) = self.parse_parameters(Token::RightParen) {
                    if let Some(ident) = self.fn_ident(fn_name, &params) {
                      if !declared_items.contains(&ident.name) {
                        if let Some(function) = self.parse_lambda(params, |this, params, body| {
                          declared_items.insert(ident.name.clone());
                          if params.found_self {
                            this.error::<0>("cannot use self in module function");
                            None
                          } else {
                            Some(Expression::from(LambdaExpression::new(
                              params.list,
                              Statement::from(body),
                              member_loc,
                            )))
                          }
                        }) {
                          module_items.push((ident, function));
                        }
                      } else {
                        self.error::<0>("duplicate method definition");
                      }
                    }
                  }
                }
              }
            }
            Token::RightBrace => break,
            t => self.error::<0>(format!("unexpected token in class {t}")),
          }
        } else {
          self.error::<0>("no meta found at location");
        }
      }

      if !self.consume(Token::RightBrace, "expected '}' after class body") {
        return None;
      }

      Some(Expression::from(ModExpression::new(name, module_items, mod_loc)))
    } else {
      self.error::<0>("no meta found at location");
      None
    }
  }

  fn struct_expr(&mut self) -> Option<Expression> {
    if let Some(struct_meta) = self.meta_at::<1>() {
      // needed here because infix advances to '{' after seeing "struct"
      if !self.consume(Token::LeftBrace, "expected '{' to begin struct body") {
        return None;
      }

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
          self.error::<0>("expected identifier");
          return None;
        }
      }

      if !self.consume(Token::RightBrace, "expected '}' after struct") {
        return None;
      }

      Some(Expression::from(StructExpression::new(members, struct_meta)))
    } else {
      self.error::<1>("cannot find meta");
      None
    }
  }

  fn req_expr(&mut self) -> Option<Expression> {
    let loc = self.meta_at::<1>()?;
    let expr = self.expression()?;
    Some(Expression::from(ReqExpression::new(expr, loc)))
  }

  fn anon_fn_expr(&mut self) -> Option<Expression> {
    let loc = self.meta_at::<0>()?;

    self.lambda_expr(SelfRules::Disallow, Token::Pipe, |_this, params, body| {
      Some(Expression::from(LambdaExpression::new(
        params.list,
        Statement::from(body),
        loc,
      )))
    })
  }

  fn closure_expr(&mut self, param_term: Token, captures: ListExpression) -> Option<Expression> {
    let loc = self.meta_at::<0>()?;

    let mut vetted_captures = Vec::new();
    for capture in captures.items {
      match capture {
        Expression::Ident(expr) => vetted_captures.push(expr),
        _ => {
          self.error::<0>("invalid lambda capture");
          return None;
        }
      }
    }

    self.lambda_expr(SelfRules::Disallow, param_term, |_this, params, body| {
      Some(Expression::from(ClosureExpression::new(
        vetted_captures,
        params.list,
        Statement::from(body),
        loc,
      )))
    })
  }

  fn lambda_expr<F>(&mut self, self_rules: SelfRules, param_term: Token, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut Self, Params, BlockStatement) -> Option<Expression>,
  {
    let params = self.parse_parameters(param_term)?;

    if self_rules == SelfRules::Disallow && params.found_self {
      self.error::<0>(String::from("found 'self' in invalid context"));
      return None;
    }

    if self_rules == SelfRules::Require && !params.found_self {
      self.error::<0>(String::from("missing 'self' in function"));
      return None;
    }

    self.parse_lambda(params, f)
  }

  fn parse_lambda<F>(&mut self, params: Params, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut Self, Params, BlockStatement) -> Option<Expression>,
  {
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

          if !self.consume(Token::LeftBrace, "expected '{' after paren") {
            return None;
          }

          if let Some(block_loc) = self.meta_at::<1>() {
            let ident = self.fn_ident(current, &params)?;
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

  fn meta_at<const OFFSET: usize>(&mut self) -> Option<SourceLocation> {
    self.meta.get(self.index - OFFSET).cloned().or_else(|| {
      self.error::<OFFSET>(String::from("unable to get meta at position"));
      None
    })
  }

  fn error<const I: usize>(&mut self, msg: impl AsRef<str> + Into<String>) {
    if let Some(meta) = self.meta_at::<I>() {
      if cfg!(debug_assertions) {
        println!("{} ({}, {}): {}", meta.file.display(), meta.line, meta.column, msg.as_ref());
      }
      self.errors.push(RuntimeError {
        msg: msg.into(),
        file: Rc::clone(&meta.file),
        line: meta.line,
        column: meta.column,
      });
    } else {
      self.errors.push(RuntimeError {
        msg: format!("could not find location of token for msg '{}'", msg.as_ref()),
        file: Default::default(),
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

    if self.consume(terminator, "expected terminator after parameters") {
      Some(Params::from((found_self, params.into_iter().map(Ident::new).collect())))
    } else {
      return None;
    }
  }

  fn declaration(&mut self) -> Option<LetStatement> {
    self.meta_at::<1>().and_then(|let_loc| {
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
    })
  }

  fn parse_precedence(&mut self, root_token_precedence: Precedence) -> Option<Expression> {
    self.advance();

    if let Some(prev) = self.previous() {
      let mut expr: Expression;
      let prev_token_rule = Self::rule_for(&prev);

      let prefix_rule = prev_token_rule.prefix.or_else(|| {
        self.error::<1>(String::from("expected an expression"));
        None
      })?;

      expr = prefix_rule(self).or_else(|| {
        self.error::<1>(format!("no prefix rule for {}", prev));
        None
      })?;

      while let Some(curr) = self.current() {
        let current_token_rule = Self::rule_for(&curr);
        if root_token_precedence <= current_token_rule.precedence {
          match current_token_rule.infix {
            Some(infix) => {
              self.advance();
              expr = infix(self, expr)?;
            }
            None => {
              self.error::<0>(format!("no infix rule for {:?}", curr));
              return None;
            }
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
      Token::LeftParen => ParseRule::new(Some(GroupExpression::prefix), Some(CallExpression::infix), Precedence::Call),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(None, None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::LeftBracket => ParseRule::new(Some(ListExpression::prefix), Some(IndexExpression::infix), Precedence::Call),
      Token::RightBracket => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, Some(MemberAccessExpression::infix), Precedence::Call),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Colon => ParseRule::new(None, None, Precedence::None),
      Token::At => ParseRule::new(None, None, Precedence::None),
      Token::Pipe => ParseRule::new(Some(Self::anon_fn_expr), None, Precedence::Primary),
      Token::Plus => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Term),
      Token::PlusEqual => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::Minus => ParseRule::new(Some(UnaryExpression::prefix), Some(BinaryExpression::infix), Precedence::Term),
      Token::MinusEqual => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::Asterisk => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Factor),
      Token::AsteriskEqual => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::Slash => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Factor),
      Token::SlashEqual => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::Percent => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Factor),
      Token::PercentEqual => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::Bang => ParseRule::new(Some(UnaryExpression::prefix), None, Precedence::None),
      Token::BangEqual => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Equality),
      Token::Equal => ParseRule::new(None, Some(AssignExpression::infix), Precedence::Assignment),
      Token::EqualEqual => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Equality),
      Token::Greater => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Comparison),
      Token::GreaterEqual => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Comparison),
      Token::Less => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Comparison),
      Token::LessEqual => ParseRule::new(None, Some(BinaryExpression::infix), Precedence::Comparison),
      Token::Arrow => ParseRule::new(None, None, Precedence::None),
      Token::BackArrow => ParseRule::new(None, None, Precedence::None),
      Token::Identifier(_) => ParseRule::new(Some(IdentExpression::prefix), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Number(_) => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::And => ParseRule::new(None, Some(AndExpression::infix), Precedence::And),
      Token::As => ParseRule::new(None, None, Precedence::None),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(Some(|this| Self::class_expr(this, None)), None, Precedence::Primary),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::Export => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::Mod => ParseRule::new(Some(|this| Self::mod_expr(this, None)), None, Precedence::Primary),
      Token::New => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Or => ParseRule::new(None, Some(OrExpression::infix), Precedence::Or),
      Token::Print => ParseRule::new(None, None, Precedence::None),
      Token::Req => ParseRule::new(Some(Self::req_expr), None, Precedence::Primary),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::Struct => ParseRule::new(Some(Self::struct_expr), None, Precedence::Primary),
      Token::True => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Use => ParseRule::new(None, None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::Yield => ParseRule::new(None, None, Precedence::None),
      Token::Breakpoint => ParseRule::new(None, None, Precedence::None),
    }
  }

  fn fn_ident(&mut self, current: Token, params: &Params) -> Option<Ident> {
    macro_rules! check_missing_self {
      ($p:ident, $op:literal) => {
        if !$p.found_self {
          self.error::<0>(format!("must have self in '{}' overload", $op));
          None?
        }
      };
    }

    macro_rules! check_not_binary {
      ($p:ident, $op:literal) => {
        if $p.list.len() != 2 {
          self.error::<0>(format!("invalid number of arguments in '{}' overload", $op));
          None?
        }
      };
    }

    macro_rules! check_not_unary {
      ($p:ident, $op:literal) => {
        if $p.list.len() != 1 {
          self.error::<0>(format!("cannot have arguments in '{}' overload", $op));
          None?
        }
      };
    }

    Some(Ident::new(match current {
      Token::Identifier(fn_name) => fn_name,
      other => match other {
        Token::Bang => {
          check_missing_self!(params, "not");
          check_not_unary!(params, "not");
          ops::NOT
        }
        Token::Plus => {
          check_missing_self!(params, "add");
          check_not_binary!(params, "add");
          ops::ADD
        }
        Token::Minus => {
          if params.list.is_empty() {
            check_missing_self!(params, "negate");
            ops::NEG
          } else {
            check_missing_self!(params, "sub");
            check_not_binary!(params, "sub");
            ops::SUB
          }
        }
        Token::Asterisk => {
          check_missing_self!(params, "mul");
          check_not_binary!(params, "mul");
          ops::MUL
        }
        Token::Slash => {
          check_missing_self!(params, "div");
          check_not_binary!(params, "div");
          ops::DIV
        }
        Token::Percent => {
          check_missing_self!(params, "rem");
          check_not_binary!(params, "rem");
          ops::REM
        }
        Token::EqualEqual => {
          check_missing_self!(params, "eq");
          check_not_binary!(params, "eq");
          ops::EQUALITY
        }
        Token::BangEqual => {
          check_missing_self!(params, "neq");
          check_not_binary!(params, "neq");
          ops::NOT_EQUAL
        }
        Token::Less => {
          check_missing_self!(params, "less");
          check_not_binary!(params, "less");
          ops::LESS
        }
        Token::LessEqual => {
          check_missing_self!(params, "leq");
          check_not_binary!(params, "leq");
          ops::LESS_EQUAL
        }
        Token::Greater => {
          check_missing_self!(params, "greater");
          check_not_binary!(params, "greater");
          ops::GREATER
        }
        Token::GreaterEqual => {
          check_missing_self!(params, "geq");
          check_not_binary!(params, "geq");
          ops::GREATER_EQUAL
        }
        _ => None?,
      }
      .to_string(),
    }))
  }
}

#[derive(Debug, Clone)]
pub struct Ident {
  pub name: String,
  pub global: bool,
}

impl Ident {
  pub fn new(name: impl Into<String>) -> Self {
    let name = name.into();
    Self {
      global: matches!(name.chars().next(), Some('$')),
      name: name,
    }
  }

  pub fn new_global(name: impl Into<String>) -> Self {
    Self {
      name: name.into(),
      global: true,
    }
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name)
  }
}

impl PartialEq for Ident {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
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
  Primary,    // literal
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
  fn from((found_self, mut list): (bool, Vec<Ident>)) -> Self {
    if found_self {
      list.push(Ident::new("self"));
    }
    Self { found_self, list }
  }
}

impl From<Vec<Ident>> for Params {
  fn from(list: Vec<Ident>) -> Self {
    Self { found_self: false, list }
  }
}

#[derive(PartialEq, Eq)]
enum SelfRules {
  Disallow,
  Require,
}
