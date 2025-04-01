mod expr;
mod opt;
mod stmt;

use crate::{
  error::{AstGenerationError, AstGenerationErrorMsg, CompilerError},
  ops,
  util::{FileIdType, UnwrapAnd},
};

pub use expr::*;
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  mem,
};
pub use stmt::*;
#[cfg(feature = "visit-ast")]
use {
  horrorshow::{helper::doctype, html},
  std::path::Path,
};

const SELF_IDENT: &str = "self";

type Validator = dyn FnOnce(&mut AstGenerator, &Params) -> Option<Ident>;

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

pub fn generate(file_id: Option<FileIdType>, tokens: Vec<Token>, meta: Vec<SourceLocation>) -> Result<Ast, CompilerError> {
  AstGenerator::new(file_id, tokens, meta).generate()
}

pub use opt::optimize;

use super::{lex::Token, SourceLocation};

pub struct Ast {
  pub statements: Vec<Statement>,
}

impl Ast {
  #[cfg(feature = "visit-ast")]
  #[allow(dead_code)]
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

pub(crate) struct AstGenerator {
  file_id: Option<FileIdType>,
  tokens: Vec<Token>,
  token_locations: Vec<SourceLocation>,

  statements: Vec<Statement>,
  errors: Vec<AstGenerationError>,

  index: usize,

  in_loop: bool,
  export_found: bool,
  returnable: bool,
  scope_depth: usize,
}

impl AstGenerator {
  fn new(file_id: Option<FileIdType>, tokens: Vec<Token>, meta: Vec<SourceLocation>) -> Self {
    Self {
      file_id,
      tokens,
      token_locations: meta,
      statements: Default::default(),
      errors: Default::default(),
      index: Default::default(),
      in_loop: Default::default(),
      export_found: false,
      returnable: false,
      scope_depth: 0,
    }
  }

  fn generate(mut self) -> Result<Ast, CompilerError> {
    while let Some(current) = self.current() {
      self.statement(current);
    }

    let ast = Ast {
      statements: self.statements,
    };

    if self.errors.is_empty() {
      Ok(ast)
    } else {
      Err(CompilerError::AstGeneration(self.errors))
    }
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
      Token::Println => {
        self.advance();
        PrintlnStatement::stmt(self);
      }
      Token::Quack => {
        self.advance();
        QuackStatement::stmt(self);
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
      Token::Breakpoint => {
        self.advance();
        self.breakpoint_stmt();
      }
      _ => ExpressionStatement::stmt(self),
    }
  }

  /* Statements */

  fn breakpoint_stmt(&mut self) {
    if self.consume(Token::Semicolon) {
      self
        .token_location::<1>()
        .unwrap_and(|meta| self.statements.push(Statement::Breakpoint(meta)));
    }
  }

  /* Expressions */

  fn expression(&mut self) -> Option<Expression> {
    self.parse_precedence(Precedence::Assignment)
  }

  fn parse_lambda<F>(&mut self, can_return: bool, params: Params, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut Self, Params, BlockStatement) -> Option<Expression>,
  {
    if self.advance_if_matches(Token::Arrow) {
      self.expression()?;
    }

    if !self.consume(Token::LeftBrace) {
      return None;
    }

    let block_loc = self.token_location::<1>()?;
    let body = self.block(can_return, block_loc)?;
    f(self, params, body)
  }

  /* Utility functions */

  fn add(&mut self, stmt: impl Into<Statement>) {
    self.statements.push(stmt.into())
  }

  fn current(&self) -> Option<Token> {
    self.peek_after::<0>()
  }

  fn expect_current(&mut self) -> Option<Token> {
    let current = self.current();
    if current.is_none() {
      self.error::<0>(AstGenerationErrorMsg::UnexpectedEof);
    }
    current
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

  fn consume(&mut self, expected: Token) -> bool {
    if let Some(curr) = self.current() {
      if curr == expected {
        self.advance();
        return true;
      }
    }

    self.error::<0>(AstGenerationErrorMsg::FailedConsumption(expected));
    false
  }

  fn parse_fn(&mut self) -> Option<Statement> {
    let loc = self.token_location::<0>()?;

    let validator = self.fn_ident_validator()?;

    if !self.consume(Token::LeftParen) {
      return None;
    }

    let params = self.parse_parameters(Token::RightParen)?;
    if params.found_self {
      self.error::<0>(AstGenerationErrorMsg::SelfNotAllowed);
      return None;
    }

    if self.advance_if_matches(Token::Arrow) {
      self.expression()?;
    }

    if !self.consume(Token::LeftBrace) {
      return None;
    }

    let block_loc = self.token_location::<1>()?;
    let ident = validator(self, &params)?;
    self
      .block(true, block_loc)
      .map(|body| Statement::from(FnStatement::new(ident, params.list, Statement::from(body), loc)))
  }

  fn resolve(&mut self) -> Vec<Ident> {
    let mut parts = Vec::default();

    while let Some(current) = self.current() {
      match current {
        Token::Identifier(ident) => parts.push(Ident::new(ident)),
        Token::ColonColon => (/* intentionally do nothing */),
        _ => break,
      }
      self.advance();
    }

    parts
  }

  fn token_location<const OFFSET: usize>(&mut self) -> Option<SourceLocation> {
    self.token_locations.get(self.index - OFFSET).cloned()
  }

  fn add_error(&mut self, msg: AstGenerationErrorMsg, loc: SourceLocation) {
    self
      .errors
      .push(AstGenerationError::new(msg, self.file_id, loc.line, loc.column));
  }

  fn error<const I: usize>(&mut self, msg: AstGenerationErrorMsg) {
    let mut index = I;
    'bt: loop {
      if let Some(loc) = self.token_locations.get(index).cloned() {
        self.add_error(msg, loc);
        break 'bt;
      }

      match index.checked_sub(1) {
        Some(i) => index = i,
        None => {
          self.add_error(
            AstGenerationErrorMsg::NoLocationForError(Box::new(msg)),
            SourceLocation { line: 0, column: 0 },
          );
          break 'bt;
        }
      }
    }

    self.sync();
  }

  fn sync(&mut self) {
    let start_scope = self.scope_depth;
    while let Some(curr) = self.current() {
      if curr == Token::LeftBrace {
        self.scope_depth += 1;
      }

      if curr == Token::RightBrace {
        match self.scope_depth.checked_sub(1) {
          Some(v) => self.scope_depth = v,
          None => {
            self.advance();
            continue;
          }
        }
      }

      if self.scope_depth == start_scope {
        // only if the ast gets past the entire body of something that has gone wrong
        // can it resume parsing, otherwise it'll have a ton of cascading errors
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
            | Token::Println
            | Token::Ret
            | Token::Match
            | Token::Loop
        ) {
          return;
        }
      }

      self.advance();
    }
  }

  fn scope<F: FnOnce(&mut Self)>(&mut self, f: F) -> Vec<Statement> {
    self.scope_depth += 1;
    let mut statements = Vec::default();
    mem::swap(&mut statements, &mut self.statements);
    f(self);
    mem::swap(&mut statements, &mut self.statements);

    match self.scope_depth.checked_sub(1) {
      Some(v) => self.scope_depth = v,
      None => self.error::<0>(AstGenerationErrorMsg::UnclosedScope),
    }

    statements
  }

  fn block(&mut self, returnable: bool, loc: SourceLocation) -> Option<BlockStatement> {
    let prev_returnable = self.returnable;
    self.returnable = returnable;
    let statements = self.scope(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
    });
    self.returnable = prev_returnable;

    if self.consume(Token::RightBrace) {
      Some(BlockStatement::new(statements, loc))
    } else {
      None
    }
  }

  fn normal_block(&mut self, loc: SourceLocation) -> Option<BlockStatement> {
    self.block(self.returnable, loc)
  }

  fn parse_parameters(&mut self, terminator: Token) -> Option<Params> {
    let mut first_token = true;
    let mut found_self = false;
    let mut params = Vec::new();

    'params: while let Some(token) = self.current() {
      if token == terminator {
        break 'params;
      }

      self.advance();

      match token {
        Token::Identifier(ident) => {
          if params.contains(&ident) {
            self.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
            return None;
          }

          if ident == SELF_IDENT {
            if first_token {
              if found_self {
                self.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
                return None;
              } else {
                found_self = true;
              }
            } else {
              self.error::<0>(AstGenerationErrorMsg::InvalidSelfPosition);
              return None;
            }
          } else {
            params.push(ident);
            if self.advance_if_matches(Token::Colon) {
              self.expression()?;
            }
          }

          first_token = false;
        }

        t => self.error::<0>(AstGenerationErrorMsg::InvalidParameter(t)),
      }

      self.advance_if_matches(Token::Comma);
    }

    if self.consume(terminator) {
      Some(Params::from((found_self, params.into_iter().map(Ident::new).collect())))
    } else {
      None
    }
  }

  fn declaration(&mut self) -> Option<LetStatement> {
    self.token_location::<1>().and_then(|let_loc| match self.current() {
      Some(Token::Identifier(ident)) => {
        let ident = Ident::new(ident);
        self.advance();

        let value = if self.advance_if_matches(Token::Equal) {
          self.expression()
        } else {
          None
        };

        Some(LetStatement::new(ident, value, let_loc))
      }
      Some(t) => {
        self.error::<0>(AstGenerationErrorMsg::InvalidIdentifier(t));
        None
      }
      None => {
        self.error::<0>(AstGenerationErrorMsg::UnexpectedEof);
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
        self.error::<1>(AstGenerationErrorMsg::InvalidExpression);
        None
      })?;

      expr = prefix_rule(self)?;

      while let Some(curr) = self.current() {
        let current_token_rule = Self::rule_for(&curr);
        if root_token_precedence <= current_token_rule.precedence {
          let infix_rule = current_token_rule.infix.or_else(|| {
            self.error::<0>(AstGenerationErrorMsg::InvalidExpression);
            None
          })?;
          self.advance();
          expr = infix_rule(self, expr)?;
        } else {
          break;
        }
      }

      Some(expr)
    } else {
      self.error::<2>(AstGenerationErrorMsg::UnexpectedEof);
      None
    }
  }

  fn rule_for(token: &Token) -> ParseRule {
    match token {
      Token::LeftParen => ParseRule::new(Some(GroupExpression::prefix), Some(CallExpression::infix), Precedence::Call),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(None, None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::LeftBracket => ParseRule::new(Some(VecExpression::prefix), Some(IndexExpression::infix), Precedence::Call),
      Token::RightBracket => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, Some(MemberAccessExpression::infix), Precedence::Call),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::At => ParseRule::new(None, None, Precedence::None),
      Token::Pipe => ParseRule::new(Some(LambdaExpression::prefix), None, Precedence::Primary),
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
      Token::Colon => ParseRule::new(None, None, Precedence::None),
      Token::ColonColon => ParseRule::new(None, Some(ScopeResolutionExpression::infix), Precedence::Call),
      Token::Identifier(_) => ParseRule::new(Some(IdentExpression::prefix), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Number(_) => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::And => ParseRule::new(None, Some(AndExpression::infix), Precedence::And),
      Token::As => ParseRule::new(None, None, Precedence::None),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(Some(ClassExpression::prefix), None, Precedence::Primary),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::Export => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Is => ParseRule::new(None, Some(IsExpression::infix), Precedence::Assignment),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::Mod => ParseRule::new(Some(ModExpression::prefix), None, Precedence::Primary),
      Token::New => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Or => ParseRule::new(None, Some(OrExpression::infix), Precedence::Or),
      Token::Println => ParseRule::new(None, None, Precedence::None),
      Token::Quack => ParseRule::new(None, None, Precedence::None),
      Token::Req => ParseRule::new(Some(ReqExpression::prefix), None, Precedence::Primary),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::Struct => ParseRule::new(Some(StructExpression::prefix), None, Precedence::Primary),
      Token::True => ParseRule::new(Some(LiteralExpression::prefix), None, Precedence::Primary),
      Token::Use => ParseRule::new(None, None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::Breakpoint => ParseRule::new(None, None, Precedence::None),
    }
  }

  /// Produces a validator function to test if an operator overload is valid with the paramters
  fn fn_ident_validator(&mut self) -> Option<Box<Validator>> {
    macro_rules! check_missing_self {
      ($this:ident, $p:ident) => {
        if !$p.found_self {
          $this.error::<0>(AstGenerationErrorMsg::SelfParameterUndefined);
          None?
        }
      };
    }

    macro_rules! check_nargs {
      ($this:ident, $p:ident, $expargs:literal) => {{
        let nargs = $p.list.len();
        if nargs != $expargs {
          $this.error::<0>(AstGenerationErrorMsg::OperatorOverloadInvalidNumberOfParameters(
            nargs, $expargs,
          ));
          None?
        }
      }};
    }

    macro_rules! check_not_ternary {
      ($this:ident, $p:ident) => {
        check_nargs!($this, $p, 3);
      };
    }

    macro_rules! check_not_binary {
      ($this:ident, $p:ident) => {
        check_nargs!($this, $p, 2);
      };
    }

    macro_rules! check_not_unary {
      ($this:ident, $p:ident) => {
        check_nargs!($this, $p, 1);
      };
    }

    macro_rules! validator {
      ($f:expr_2021) => {
        Box::new($f)
      };
    }

    let mut auto_advance = true;

    let validator: Box<Validator> = match self.current()? {
      Token::Identifier(fn_name) => validator!(|_, _| Some(Ident::new(fn_name))),
      other => match other {
        Token::Bang => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_unary!(this, params);
          Some(ops::NOT.into())
        }),
        Token::Plus => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::ADD.into())
        }),
        Token::Minus => validator!(|this, params: &Params| {
          if params.list.is_empty() {
            check_missing_self!(this, params);
            Some(ops::NEG.into())
          } else {
            check_missing_self!(this, params);
            check_not_binary!(this, params);
            Some(ops::SUB.into())
          }
        }),
        Token::Asterisk => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::MUL.into())
        }),
        Token::Slash => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::DIV.into())
        }),
        Token::Percent => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::REM.into())
        }),
        Token::EqualEqual => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::EQUALITY.into())
        }),
        Token::BangEqual => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::NOT_EQUAL.into())
        }),
        Token::Less => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::LESS.into())
        }),
        Token::LessEqual => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::LESS_EQUAL.into())
        }),
        Token::Greater => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::GREATER.into())
        }),
        Token::GreaterEqual => validator!(|this, params: &Params| {
          check_missing_self!(this, params);
          check_not_binary!(this, params);
          Some(ops::GREATER_EQUAL.into())
        }),
        Token::LeftBracket => {
          auto_advance = false;
          self.advance();
          if self.consume(Token::RightBracket) {
            if self.advance_if_matches(Token::Equal) {
              validator!(|this, params: &Params| {
                check_missing_self!(this, params);
                check_not_ternary!(this, params);
                Some(ops::INDEX_ASSIGN.into())
              })
            } else {
              validator!(|this, params: &Params| {
                check_missing_self!(this, params);
                check_not_binary!(this, params);
                Some(ops::INDEX.into())
              })
            }
          } else {
            None?
          }
        }
        t => {
          self.error::<0>(AstGenerationErrorMsg::InvalidFnIdentifier(t));
          None?
        }
      },
    };

    if auto_advance {
      self.advance();
    }

    Some(validator)
  }
}

#[derive(Debug, Clone, Copy)]
pub enum IdentScope {
  Module,
  Global,
}

#[derive(Debug, Clone)]
pub struct Ident {
  pub name: String,
  pub scope: Option<IdentScope>,
  pub global_name: bool,
}

impl Ident {
  pub fn new(name: impl Into<String>) -> Self {
    let name = name.into();
    let global_name = matches!(name.chars().next(), Some('$'));
    Self {
      name,
      scope: global_name.then_some(IdentScope::Global),
      global_name,
    }
  }

  pub fn new_module(name: impl Into<String>) -> Self {
    let name = name.into();
    let global_name = matches!(name.chars().next(), Some('$'));
    Self {
      name,
      scope: Some(IdentScope::Module),
      global_name,
    }
  }

  pub fn new_global(name: impl Into<String>) -> Self {
    let name = name.into();
    let global_name = matches!(name.chars().next(), Some('$'));
    Self {
      name,
      scope: Some(IdentScope::Global),
      global_name,
    }
  }

  pub fn has_global_name(&self) -> bool {
    self.global_name
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

impl From<&str> for Ident {
  fn from(value: &str) -> Self {
    Self::new(value)
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
      list.push(Ident::new(SELF_IDENT));
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
