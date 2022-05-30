use super::{Token, TokenMeta};
use crate::{
  types::{Error, Value},
  New,
};
use std::mem;

pub struct Ast {
  pub statements: Vec<Statement>,
}

impl Ast {
  pub fn from(tokens: Vec<Token>, meta: Vec<TokenMeta>) -> Result<Ast, Vec<Error>> {
    AstGenerator::new(tokens, meta).generate()
  }
}

struct AstGenerator {
  tokens: Vec<Token>,
  meta: Vec<TokenMeta>,

  statements: Vec<Statement>,
  errors: Vec<Error>,

  index: usize,

  in_loop: bool,
}

impl AstGenerator {
  fn new(tokens: Vec<Token>, meta: Vec<TokenMeta>) -> Self {
    Self {
      tokens,
      meta,
      statements: Default::default(),
      errors: Default::default(),
      index: Default::default(),
      in_loop: Default::default(),
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
      Token::End => {
        self.advance();
        self.end_stmt();
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
      Token::Load => {
        self.advance();
        self.load_stmt();
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
    let block = self.block();
    self.statements.push(Statement::new(block));
  }

  fn break_stmt(&mut self) {
    if !self.in_loop {
      self.error(
        self.index - 1,
        String::from("break statements can only be used within loops"),
      );
      return;
    }

    if !self.consume(Token::Semicolon, String::from("expect ';' after break")) {
      return;
    }

    self.statements.push(Statement::Break);
  }

  fn cont_stmt(&mut self) {
    if !self.in_loop {
      self.error(
        self.index - 1,
        String::from("cont statements can only be used within loops"),
      );
      return;
    }

    if !self.consume(Token::Semicolon, String::from("expect ';' after cont")) {
      return;
    }

    self.statements.push(Statement::Cont);
  }

  fn end_stmt(&mut self) {
    let expr = if !self.advance_if_matches(Token::Semicolon) {
      let expr = self.expression();

      if expr.is_none() {
        return;
      }

      expr
    } else {
      None
    };

    if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
      return;
    }

    self
      .statements
      .push(Statement::new(EndStatement::new(expr)));
  }

  fn fn_stmt(&mut self) {
    if let Some(Token::Identifier(fn_name)) = self.current() {
      let ident = Ident::new(fn_name);

      self.advance();
      if !self.consume(
        Token::LeftParen,
        String::from("expect '(' after function name"),
      ) {
        return;
      }

      let mut params = Vec::default();
      if let Some(mut token) = self.current() {
        if token != Token::RightParen {
          loop {
            if let Token::Identifier(ident) = token {
              params.push(Ident::new(ident));
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

      if !self.consume(
        Token::RightParen,
        String::from("expected ')' after arguments"),
      ) {
        return;
      }

      if !self.consume(Token::LeftBrace, String::from("expected '}' after paren")) {
        return;
      }

      let body = self.block();

      self
        .statements
        .push(Statement::new(FnStatement::new(ident, params, body)));
    } else {
      self.error(self.index, String::from("expected an identifier"));
    }
  }

  fn for_stmt(&mut self) {
    let mut statements = self.scope(|this| {
      if this.advance_if_matches(Token::Let) {
        this.let_stmt();
      } else {
        this.expression_stmt();
      }

      if let Some(comparison) = this.expression() {
        if let Some(increment) = this.expression() {
          this
            .statements
            .push(Statement::new(ExpressionStatement::new(comparison)));
          this
            .statements
            .push(Statement::new(ExpressionStatement::new(increment)));

          if this.consume(
            Token::LeftBrace,
            String::from("expected '{' after increment"),
          ) {
            let block = this.block();
            this.statements.push(Statement::new(block));
          }
        } else {
          this.error(
            this.index,
            String::from("expected increment after comparison"),
          );
        }
      } else {
        this.error(
          this.index,
          String::from("expected comparison after initializer"),
        );
      }
    });

    if statements.len() == 4 {
      let block = statements.swap_remove(3);
      let increment = statements.swap_remove(2);
      let comparison = statements.swap_remove(1);
      let initializer = statements.swap_remove(0);
      self.statements.push(Statement::new(ForStatement::new(
        initializer,
        comparison,
        increment,
        block,
      )));
    }
  }

  fn if_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(
        Token::LeftBrace,
        String::from("expected '{' after condition"),
      ) {
        return;
      }

      let block = self.block();
      self
        .statements
        .push(Statement::new(IfStatement::new(expr, block)));
    } else {
      return;
    }

    if self.advance_if_matches(Token::Else) {
      if let Some(token) = self.current() {
        match token {
          Token::LeftBrace => self.block_stmt(),
          Token::If => self.if_stmt(),
          _ => self.error(self.index, String::from("unexpected token after 'else'")),
        }
      } else {
        self.error(self.index - 1, String::from("unexpected end of file"));
      }
    }
  }

  fn let_stmt(&mut self) {
    if let Some(Token::Identifier(ident)) = self.current() {
      let ident = Ident::new(ident);

      let mut value = None;
      if self.advance_if_matches(Token::Equal) {
        if let Some(expr) = self.expression() {
          value = Some(expr);
        } else {
          return;
        }
      }

      self
        .statements
        .push(Statement::new(LetStatement::new(ident, value)));
    } else {
      self.error(self.index, String::from("expected variable name"));
    }
  }

  fn load_stmt(&mut self) {
    unimplemented!();
  }

  fn loop_stmt(&mut self) {
    if !self.consume(Token::LeftBrace, String::from("expect '{' after loop")) {
      return;
    }

    let block = self.block();

    self
      .statements
      .push(Statement::new(LoopStatement::new(block)));
  }

  fn match_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(
        Token::LeftBrace,
        String::from("expected '{' after expression"),
      ) {
        return;
      }

      let mut branches = Vec::default();

      while let Some(token) = self.current() {
        if token == Token::RightBrace {
          break;
        }

        if let Some(condition) = self.expression() {
          if !self.consume(Token::Arrow, String::from("expected => after expression")) {
            break;
          }

          let stmt = if self.advance_if_matches(Token::LeftBrace) {
            Statement::new(self.block())
          } else if let Some(eval) = self.expression() {
            if !self.consume(Token::Comma, String::from("expected ',' after expression")) {
              break;
            }
            Statement::new(ExpressionStatement::new(eval))
          } else {
            break;
          };

          branches.push((condition, stmt));
        } else {
          break; // error but need to restore statements
        }
      }

      self
        .statements
        .push(Statement::new(MatchStatement::new(expr, branches)))
    }
  }

  fn print_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
        return;
      }
      self
        .statements
        .push(Statement::new(PrintStatement::new(expr)));
    }
  }

  fn ret_stmt(&mut self) {
    if let Some(current) = self.current() {
      let expr = if current == Token::Semicolon {
        None
      } else if let Some(expr) = self.expression() {
        Some(expr)
      } else {
        return;
      };

      if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
        return;
      }

      self
        .statements
        .push(Statement::new(RetStatement::new(expr)));
    }
  }

  fn while_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(
        Token::LeftBrace,
        String::from("expected '{' after expression"),
      ) {
        return;
      }

      let block = self.block();

      self
        .statements
        .push(Statement::new(WhileStatement::new(expr, block)));
    }
  }

  fn expression_stmt(&mut self) {
    if let Some(expr) = self.expression() {
      if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
        return;
      }
      self
        .statements
        .push(Statement::new(ExpressionStatement::new(expr)));
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
          expr = Some(Expression::new(LiteralExpression::new(Value::Nil)));
        }
        Token::True => {
          expr = Some(Expression::new(LiteralExpression::new(Value::new(true))));
        }
        Token::False => {
          expr = Some(Expression::new(LiteralExpression::new(Value::new(false))));
        }
        Token::String(s) => expr = Some(Expression::new(LiteralExpression::new(Value::new(s)))),
        Token::Number(n) => expr = Some(Expression::new(LiteralExpression::new(Value::new(n)))),
        _ => {
          self.error(
            self.index - 1,
            String::from("sanity check, invalid literal, very bad logic error"),
          );
        }
      }
    } else {
      self.error(
        self.index - 1,
        String::from("sanity check, no previous token, very bad logic error"),
      );
    }

    expr
  }

  fn unary_expr(&mut self) -> Option<Expression> {
    if let Some(operator_token) = self.previous() {
      let op = match operator_token {
        Token::Bang => UnaryOperator::Not,
        Token::Minus => UnaryOperator::Negate,
        _ => {
          self.error(self.index - 1, String::from("invalid unary operator"));
          return None;
        }
      };

      let expr = self.parse_precedence(Precedence::Unary)?;
      Some(Expression::new(UnaryExpression::new(op, expr)))
    } else {
      self.error(
        self.index - 1,
        String::from("tried to make unary expression without operator"),
      );
      None
    }
  }

  fn binary_expr(&mut self, left: Expression) -> Option<Expression> {
    if let Some(operator_token) = self.previous() {
      let op = match operator_token {
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEq,
        Token::Greater => BinaryOperator::Greater,
        Token::GreaterEqual => BinaryOperator::GreaterEq,
        Token::Less => BinaryOperator::Less,
        Token::LessEqual => BinaryOperator::LessEq,
        Token::And => BinaryOperator::And,
        Token::Or => BinaryOperator::Or,
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Sub,
        Token::Asterisk => BinaryOperator::Mul,
        Token::Slash => BinaryOperator::Div,
        Token::Modulus => BinaryOperator::Mod,
        _ => {
          self.error(self.index - 1, String::from("invalid binary operator"));
          return None;
        }
      };

      let rule = Self::rule_for(&operator_token);

      if let Some(next_precedence) = rule.precedence.next() {
        let expr = self.parse_precedence(next_precedence)?;
        Some(Expression::new(BinaryExpression::new(left, op, expr)))
      } else {
        self.error(self.index - 1, String::from(""));
        None
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream"),
      );
      None
    }
  }

  fn group_expr(&mut self) -> Option<Expression> {
    let expr = self.expression()?;
    if self.consume(
      Token::RightParen,
      String::from("expected ')' after expression"),
    ) {
      Some(Expression::new(GroupExpression::new(expr)))
    } else {
      None
    }
  }

  fn ident_expr(&mut self) -> Option<Expression> {
    if let Some(ident_token) = self.previous() {
      if let Token::Identifier(ident_name) = ident_token {
        Some(Expression::new(IdentExpression::new(Ident::new(
          ident_name,
        ))))
      } else {
        self.error(
          self.index - 2,
          String::from("variable name is not an identifier"),
        );
        None
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream"),
      );
      None
    }
  }

  fn call_expr(&mut self, ident: Expression) -> Option<Expression> {
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

      if !self.consume(
        Token::RightParen,
        String::from("expect ')' after arguments"),
      ) {
        return None;
      }
    }

    Some(Expression::new(CallExpression::new(ident, args)))
  }

  /* Utility functions */

  fn current(&self) -> Option<Token> {
    self.tokens.get(self.index).cloned()
  }

  fn previous(&self) -> Option<Token> {
    if self.index > 0 {
      self.tokens.get(self.index - 1).cloned()
    } else {
      None
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

  fn consume(&mut self, expected: Token, err: String) -> bool {
    if let Some(curr) = self.current() {
      if curr == expected {
        self.advance();
        true
      } else {
        self.error(self.index, err);
        false
      }
    } else {
      self.error(
        self.index - 1,
        format!("tried to lookup a token at an invalid index: {}", err),
      );
      false
    }
  }

  fn error(&mut self, pos: usize, msg: String) {
    let meta = self.meta.get(pos);

    if let Some(meta) = meta {
      if cfg!(debug_assertions) {
        println!("{} ({}, {}): {}", meta.file, meta.line, meta.column, msg);
      }
      self.errors.push(Error {
        msg,
        file: meta.file.access().clone(),
        line: meta.line,
        column: meta.column,
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
          | Token::End
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

  fn block(&mut self) -> BlockStatement {
    let statements = self.scope(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
    });

    BlockStatement::new(statements)
  }

  fn parse_precedence(&mut self, precedence: Precedence) -> Option<Expression> {
    self.advance();

    if let Some(prev) = self.previous() {
      let mut expr: Expression;
      let rule = Self::rule_for(&prev);
      let can_assign = precedence <= Precedence::Assignment;

      if let Some(prefix) = rule.prefix {
        match prefix(self) {
          Some(e) => expr = e,
          None => return None,
        }
      } else {
        self.error(self.index - 1, String::from("expected an expression"));
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
            self.error(self.index, format!("no rule for {:?}", prev));
            return None;
          }
        } else {
          break;
        }
      }

      if can_assign && self.advance_if_matches(Token::Equal) {
        self.error(self.index - 1, String::from("invalid assignment target"));
        None
      } else {
        Some(expr)
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (parse_precedence 3)"),
      );
      None
    }
  }

  fn rule_for(token: &Token) -> ParseRule {
    match token {
      Token::Invalid => panic!("invalid token read"),
      Token::LeftParen => ParseRule::new(
        Some(Self::group_expr),
        Some(Self::call_expr),
        Precedence::Call,
      ),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(None, None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, None, Precedence::None),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Plus => ParseRule::new(None, Some(Self::binary_expr), Precedence::Term),
      Token::Minus => ParseRule::new(
        Some(Self::unary_expr),
        Some(Self::binary_expr),
        Precedence::Term,
      ),
      Token::Asterisk => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::Slash => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::Modulus => ParseRule::new(None, Some(Self::binary_expr), Precedence::Factor),
      Token::Bang => ParseRule::new(Some(Self::unary_expr), None, Precedence::None),
      Token::BangEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Equality),
      Token::Equal => ParseRule::new(None, None, Precedence::None),
      Token::EqualEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Equality),
      Token::Greater => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::GreaterEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::Less => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::LessEqual => ParseRule::new(None, Some(Self::binary_expr), Precedence::Comparison),
      Token::Arrow => ParseRule::new(None, None, Precedence::None),
      Token::Identifier(_) => ParseRule::new(Some(Self::ident_expr), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::Number(_) => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::And => ParseRule::new(None, Some(Self::binary_expr), Precedence::And),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(None, None, Precedence::None),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Load => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::Or => ParseRule::new(None, Some(Self::binary_expr), Precedence::Or),
      Token::Print => ParseRule::new(None, None, Precedence::None),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::True => ParseRule::new(Some(Self::literal_expr), None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::End => ParseRule::new(None, None, Precedence::None),
    }
  }
}

pub struct Ident {
  pub name: String,
}

impl Ident {
  fn new(name: String) -> Self {
    Self { name }
  }
}

pub enum Statement {
  Break,
  Cont,
  End(EndStatement),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Block(BlockStatement),
  Let(LetStatement),
  Load(LoadStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Print(PrintStatement),
  Ret(RetStatement),
  While(WhileStatement),
  Expression(ExpressionStatement),
}

impl New<EndStatement> for Statement {
  fn new(stmt: EndStatement) -> Self {
    Self::End(stmt)
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

impl New<BlockStatement> for Statement {
  fn new(stmt: BlockStatement) -> Self {
    Self::Block(stmt)
  }
}

impl New<LetStatement> for Statement {
  fn new(stmt: LetStatement) -> Self {
    Self::Let(stmt)
  }
}

impl New<LoadStatement> for Statement {
  fn new(stmt: LoadStatement) -> Self {
    Self::Load(stmt)
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

pub struct EndStatement {
  pub expr: Option<Expression>,
}

impl EndStatement {
  fn new(expr: Option<Expression>) -> Self {
    Self { expr }
  }
}

pub struct FnStatement {
  pub ident: Ident,
  pub params: Vec<Ident>,
  pub body: Box<BlockStatement>,
}

impl FnStatement {
  fn new(ident: Ident, params: Vec<Ident>, body: BlockStatement) -> Self {
    Self {
      ident,
      params,
      body: Box::new(body),
    }
  }
}

pub struct ForStatement {
  pub initializer: Box<Statement>,
  pub comparison: Box<Statement>,
  pub increment: Box<Statement>,
  pub block: Box<Statement>,
}

impl ForStatement {
  fn new(
    initializer: Statement,
    comparison: Statement,
    increment: Statement,
    block: Statement,
  ) -> Self {
    Self {
      initializer: Box::new(initializer),
      comparison: Box::new(comparison),
      increment: Box::new(increment),
      block: Box::new(block),
    }
  }
}

pub struct IfStatement {
  pub comparison: Expression,
  pub block: Box<BlockStatement>,
}

impl IfStatement {
  fn new(comparison: Expression, block: BlockStatement) -> Self {
    Self {
      comparison,
      block: Box::new(block),
    }
  }
}

pub struct BlockStatement {
  pub statements: Vec<Statement>,
}

impl BlockStatement {
  fn new(statements: Vec<Statement>) -> Self {
    Self { statements }
  }
}

pub struct LetStatement {
  pub ident: Ident,
  pub value: Option<Expression>,
}

impl LetStatement {
  fn new(ident: Ident, value: Option<Expression>) -> Self {
    Self { ident, value }
  }
}

pub struct LoadStatement {
  pub file: String,
}

pub struct LoopStatement {
  pub block: Box<BlockStatement>,
}

impl LoopStatement {
  fn new(block: BlockStatement) -> Self {
    Self {
      block: Box::new(block),
    }
  }
}

pub struct MatchStatement {
  pub expr: Expression,
  pub branches: Vec<(Expression, Statement)>,
}

impl MatchStatement {
  fn new(expr: Expression, branches: Vec<(Expression, Statement)>) -> Self {
    Self { expr, branches }
  }
}

pub struct PrintStatement {
  pub expr: Expression,
}

impl PrintStatement {
  fn new(expr: Expression) -> Self {
    Self { expr }
  }
}

pub struct RetStatement {
  pub expr: Option<Expression>,
}

impl RetStatement {
  fn new(expr: Option<Expression>) -> Self {
    Self { expr }
  }
}

pub struct WhileStatement {
  pub comparison: Expression,
  pub block: Box<BlockStatement>,
}

impl WhileStatement {
  fn new(comparison: Expression, block: BlockStatement) -> Self {
    Self {
      comparison,
      block: Box::new(block),
    }
  }
}

pub struct ExpressionStatement {
  pub expr: Expression,
}

impl ExpressionStatement {
  fn new(expr: Expression) -> Self {
    Self { expr }
  }
}

pub enum Expression {
  Literal(LiteralExpression),
  Unary(UnaryExpression),
  Binary(BinaryExpression),
  Group(GroupExpression),
  Ident(IdentExpression),
  Call(CallExpression),
}

impl Expression {
  fn append(&mut self, other: Expression) -> Option<String> {
    None
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

impl New<CallExpression> for Expression {
  fn new(expr: CallExpression) -> Self {
    Self::Call(expr)
  }
}

pub struct LiteralExpression {
  pub value: Value,
}

impl LiteralExpression {
  fn new(value: Value) -> Self {
    Self { value }
  }
}

pub enum UnaryOperator {
  Not,
  Negate,
}

pub struct UnaryExpression {
  pub op: UnaryOperator,
  pub expr: Box<Expression>,
}

impl UnaryExpression {
  fn new(op: UnaryOperator, expr: Expression) -> Self {
    Self {
      op,
      expr: Box::new(expr),
    }
  }
}

pub enum BinaryOperator {
  Or,
  And,
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
}

impl BinaryExpression {
  fn new(left: Expression, op: BinaryOperator, right: Expression) -> Self {
    Self {
      left: Box::new(left),
      op,
      right: Box::new(right),
    }
  }
}

pub struct GroupExpression {
  pub expr: Box<Expression>,
}

impl GroupExpression {
  fn new(expr: Expression) -> Self {
    Self {
      expr: Box::new(expr),
    }
  }
}

pub struct IdentExpression {
  pub ident: Ident,
}

impl IdentExpression {
  fn new(ident: Ident) -> Self {
    Self { ident }
  }
}

pub struct CallExpression {
  pub callable: Box<Expression>,
  pub args: Vec<Expression>,
}

impl CallExpression {
  fn new(callable: Expression, args: Vec<Expression>) -> Self {
    Self {
      callable: Box::new(callable),
      args,
    }
  }
}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
  None,
  Assignment, // =
  Or,         // or
  And,        // and
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // / *
  Unary,      // - !
  Call,       // . ()
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
type InfixRule = fn(&mut AstGenerator, prefix: Expression) -> Option<Expression>;

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
