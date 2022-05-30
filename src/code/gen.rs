use super::*;
use crate::{
  code::ast::*,
  types::{Error, Function, Value},
  New,
};
use ptr::SmartPtr;
use std::collections::BTreeMap;

#[cfg(test)]
mod test;

pub struct BytecodeGenerator {
  ctx: SmartPtr<Context>,

  identifiers: BTreeMap<String, usize>,
  locals: Vec<Local>,

  function_id: usize,
  current_fn: Option<SmartPtr<Context>>,

  scope_depth: usize,
  loop_depth: usize,

  breaks: Vec<usize>,
  cont_jump: usize,

  errors: Option<Vec<Error>>,
}

impl BytecodeGenerator {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ctx,

      identifiers: BTreeMap::new(),
      locals: Vec::new(),

      function_id: 0,
      current_fn: None,

      scope_depth: 0,
      loop_depth: 0,

      breaks: Vec::new(),
      cont_jump: 0,

      errors: None,
    }
  }

  pub fn generate(mut self, ast: Ast) -> Result<SmartPtr<Context>, Vec<Error>> {
    for stmt in ast.statements {
      self.emit_stmt(stmt);
    }

    Ok(self.ctx)
  }

  /* Statements */

  fn break_stmt(&mut self) {
    self.reduce_locals_to_depth(self.loop_depth);
    let jmp = self.emit_jump();
    self.breaks.push(jmp);
  }

  fn cont_stmt(&mut self) {
    self.reduce_locals_to_depth(self.loop_depth);
    let distance = self.current_ctx().num_instructions() - self.cont_jump;
    self.emit(OpCode::Loop(distance));
  }

  fn end_stmt(&mut self, stmt: EndStatement) {
    if let Some(expr) = stmt.expr {
      self.emit_expr(expr);
    } else {
      self.emit(OpCode::Nil);
    }
    self.emit(OpCode::End);
  }

  fn print_stmt(&mut self, stmt: PrintStatement) {
    self.emit_expr(stmt.expr);
    self.emit(OpCode::Print);
  }

  /* Expressions */

  fn literal_expr(&mut self, expr: LiteralExpression) {
    self.emit_const(expr.value);
  }

  fn unary_expr(&mut self, expr: UnaryExpression) {
    self.emit_expr(*expr.expr);

    match expr.op {
      UnaryOperator::Not => self.emit(OpCode::Not),
      UnaryOperator::Negate => self.emit(OpCode::Negate),
      _ => unimplemented!(),
    }
  }

  fn binary_expr(&mut self, expr: BinaryExpression) {
    self.emit_expr(*expr.left);
    self.emit_expr(*expr.right);
    match expr.op {
      BinaryOperator::Equal => self.emit(OpCode::Equal),
      BinaryOperator::NotEq => self.emit(OpCode::NotEqual),
      BinaryOperator::Less => self.emit(OpCode::Less),
      BinaryOperator::LessEq => self.emit(OpCode::LessEqual),
      BinaryOperator::Greater => self.emit(OpCode::Greater),
      BinaryOperator::GreaterEq => self.emit(OpCode::GreaterEqual),
      BinaryOperator::Add => self.emit(OpCode::Add),
      BinaryOperator::Sub => self.emit(OpCode::Sub),
      BinaryOperator::Mul => self.emit(OpCode::Mul),
      BinaryOperator::Div => self.emit(OpCode::Div),
      BinaryOperator::Mod => self.emit(OpCode::Mod),
    }
  }

  fn and_expr(&mut self, expr: AndExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_jump();
    self.emit_expr(*expr.right);
    self.patch_jump(short_circuit, OpCode::And);
  }

  fn or_expr(&mut self, expr: OrExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_jump();
    self.emit_expr(*expr.right);
    self.patch_jump(short_circuit, OpCode::Or);
  }

  fn group_expr(&mut self, expr: GroupExpression) {
    self.emit_expr(*expr.expr);
  }

  fn ident_expr(&mut self, expr: IdentExpression) {
    if let Some(lookup) = self.resolve_local(&expr.ident) {
      let get: OpCode;

      if lookup.kind == LookupKind::Local {
        get = OpCode::LookupLocal(lookup.index);
      } else {
        let index = self.add_ident(expr.ident);
        get = OpCode::LookupGlobal(index);
      }

      self.emit(get);
    }
  }

  fn assign_expr(&mut self, expr: AssignExpression) {
    if let Some(lookup) = self.resolve_local(&expr.ident) {
      let set: OpCode;

      if lookup.kind == LookupKind::Local {
        set = OpCode::AssignLocal(lookup.index);
      } else {
        let index = self.add_ident(expr.ident);
        set = OpCode::AssignGlobal(index);
      }

      self.emit_expr(*expr.value);

      self.emit(set);
    }
  }

  fn call_expr(&mut self, expr: CallExpression) {
    let arg_count = expr.args.len();

    for arg in expr.args {
      self.emit_expr(arg)
    }

    self.emit_expr(*expr.callable);
    self.emit(OpCode::Call(arg_count));
  }

  /* Utility Functions */

  fn emit(&mut self, op: OpCode) {
    self.current_ctx().write(op, 0, 0); // TODO get line/col
  }

  fn emit_stmt(&mut self, stmt: Statement) {
    match stmt {
      Statement::Break => self.break_stmt(),
      Statement::Cont => self.cont_stmt(),
      Statement::End(stmt) => self.end_stmt(stmt),
      Statement::Fn(stmt) => {}
      Statement::For(stmt) => {}
      Statement::If(stmt) => {}
      Statement::Block(stmt) => {}
      Statement::Let(stmt) => {}
      Statement::Load(stmt) => {}
      Statement::Loop(stmt) => {}
      Statement::Match(stmt) => {}
      Statement::Print(stmt) => self.print_stmt(stmt),
      Statement::Ret(stmt) => {}
      Statement::While(stmt) => {}
      Statement::Expression(stmt) => {}
    }
  }

  fn emit_expr(&mut self, expr: Expression) {
    match expr {
      Expression::Literal(expr) => self.literal_expr(expr),
      Expression::Unary(expr) => self.unary_expr(expr),
      Expression::Binary(expr) => self.binary_expr(expr),
      Expression::And(expr) => self.and_expr(expr),
      Expression::Or(expr) => self.or_expr(expr),
      Expression::Group(expr) => self.group_expr(expr),
      Expression::Ident(expr) => self.ident_expr(expr),
      Expression::Assign(expr) => self.assign_expr(expr),
      Expression::Call(expr) => self.call_expr(expr),
    }
  }

  fn emit_const(&mut self, c: Value) {
    self.current_ctx().write_const(c, 0, 0); // TODO get line/col
  }

  /**
   * Emits a no op instruction and returns its index, the "jump" is made later with a patch
   */
  fn emit_jump(&mut self) -> usize {
    let offset = self.current_ctx().num_instructions();
    self.emit(OpCode::NoOp);
    offset
  }

  fn patch_jump<F: FnOnce(usize) -> OpCode>(&mut self, index: usize, f: F) -> bool {
    let offset = self.current_ctx().num_instructions() - index;
    let opcode = f(offset);
    self.current_ctx().replace_instruction(index, opcode)
  }

  /**
   * Returns the index of the identifier name, and creates it if it doesn't already exist
   */
  fn add_ident(&mut self, ident: Ident) -> usize {
    if let Some(index) = self.identifiers.get(&ident.name).cloned() {
      index
    } else {
      let index = self.current_ctx().add_const(Value::new(ident.name.clone()));
      self.identifiers.insert(ident.name, index);
      index
    }
  }

  fn current_ctx(&mut self) -> SmartPtr<Context> {
    if let Some(ctx) = &mut self.current_fn {
      ctx.clone()
    } else {
      self.ctx.clone()
    }
  }

  fn resolve_local(&mut self, ident: &Ident) -> Option<Lookup> {
    if !self.locals.is_empty() {
      let mut index = self.locals.len() - 1;

      for local in self.locals.iter().rev() {
        if ident.name == local.name {
          if !local.initialized {
            todo!("make error here");
            // return None;
          } else {
            return Some(Lookup {
              index,
              kind: LookupKind::Local,
            });
          }
        }

        if index > 0 {
          index -= 1;
        }
      }
    }

    Some(Lookup {
      index: 0,
      kind: LookupKind::Global,
    })
  }

  fn reduce_locals_to_depth(&mut self, depth: usize) {
    let count = self.num_locals_in_depth(depth);

    self
      .locals
      .truncate(self.locals.len().saturating_sub(count));

    if count > 0 {
      self.emit(OpCode::PopN(count));
    }
  }

  fn num_locals_in_depth(&self, depth: usize) -> usize {
    let mut count = 0;
    for local in self.locals.iter().rev() {
      if local.depth > depth {
        count += 1;
      } else {
        break;
      }
    }
    count
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

type ParseFn = fn(&mut Parser, bool) -> bool;

struct ParseRule {
  prefix: Option<ParseFn>,
  infix: Option<ParseFn>,
  precedence: Precedence,
}

impl ParseRule {
  fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> Self {
    Self {
      prefix,
      infix,
      precedence,
    }
  }
}

struct Local {
  name: String,
  depth: usize,
  initialized: bool,
}

#[derive(PartialEq)]
enum LookupKind {
  Local,
  Global,
}

struct Lookup {
  kind: LookupKind,
  index: usize,
}

pub struct Parser {
  tokens: Vec<Token>,
  meta: Vec<TokenMeta>,
  ctx: SmartPtr<Context>,

  current_fn: Option<SmartPtr<Context>>,

  index: usize,
  scope_depth: usize,

  function_id: usize,

  errors: Option<Vec<Error>>,
  locals: Vec<Local>,

  in_loop: bool,
  loop_depth: usize,
  cont_jump: usize,

  breaks: Vec<usize>,

  identifiers: BTreeMap<String, usize>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>, meta: Vec<TokenMeta>, ctx: SmartPtr<Context>) -> Self {
    Self {
      tokens,
      meta,
      ctx,
      current_fn: None,
      index: 0,
      scope_depth: 0,
      function_id: 0,
      errors: None,
      locals: Vec::new(),
      in_loop: false,
      loop_depth: 0,
      cont_jump: 0,
      breaks: Vec::new(),
      identifiers: BTreeMap::new(),
    }
  }

  pub fn parse(&mut self) -> Option<Vec<Error>> {
    while let Some(current) = self.current() {
      self.statement(current);
    }

    self.errors.take()
  }

  fn error(&mut self, pos: usize, msg: String) {
    if self.errors.is_none() {
      self.errors = Some(Vec::new());
    }

    let meta = self.meta.get(pos);

    if let Some(errs) = &mut self.errors {
      if let Some(meta) = meta {
        if cfg!(debug_assertions) {
          println!("{} ({}, {}): {}", meta.file, meta.line, meta.column, msg);
        }
        errs.push(Error {
          msg,
          file: meta.file.access().clone(),
          line: meta.line,
          column: meta.column,
        });
      }
    }

    self.sync();
  }

  fn current_ctx(&mut self) -> SmartPtr<Context> {
    if let Some(ctx) = &mut self.current_fn {
      ctx.clone()
    } else {
      self.ctx.clone()
    }
  }

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

  fn consume_identifier(&mut self, err: String) -> bool {
    if let Some(curr) = self.current() {
      if matches!(curr, Token::Identifier(_)) {
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

  fn emit(&mut self, pos: usize, op: OpCode) {
    if let Some(meta) = self.meta.get(pos).cloned() {
      self.current_ctx().write(op, meta.line, meta.column);
    } else {
      panic!("could not locate token meta at pos {}", pos);
    }
  }

  fn emit_const(&mut self, pos: usize, c: Value) {
    if let Some(meta) = self.meta.get(pos).cloned() {
      self.current_ctx().write_const(c, meta.line, meta.column);
    } else {
      panic!("could not locate token meta at pos {}", pos);
    }
  }

  /**
   * Emits a no op instruction and returns its index, the "jump" is made later with a patch
   */
  fn emit_jump(&mut self, pos: usize) -> usize {
    let offset = self.current_ctx().num_instructions();
    self.emit(pos, OpCode::NoOp);
    offset
  }

  fn patch_jump<F: FnOnce(usize) -> OpCode>(&mut self, index: usize, f: F) -> bool {
    let offset = self.current_ctx().num_instructions() - index;
    let opcode = f(offset);
    self.current_ctx().replace_instruction(index, opcode)
  }

  /**
   * Returns the index of the identifier name, and creates it if it doesn't already exist
   */
  fn add_ident(&mut self, name: String) -> usize {
    if let Some(index) = self.identifiers.get(&name).cloned() {
      index
    } else {
      let index = self.current_ctx().add_const(Value::new(name.clone()));
      self.identifiers.insert(name, index);
      index
    }
  }

  fn num_locals_in_depth(&self, depth: usize) -> usize {
    let mut count = 0;
    for local in self.locals.iter().rev() {
      if local.depth > depth {
        count += 1;
      } else {
        break;
      }
    }
    count
  }

  fn reduce_locals_to_depth(&mut self, index: usize, depth: usize) {
    let count = self.num_locals_in_depth(depth);

    self
      .locals
      .truncate(self.locals.len().saturating_sub(count));

    if count > 0 {
      self.emit(index, OpCode::PopN(count));
    }
  }

  fn patch_breaks(&mut self) -> bool {
    let breaks: Vec<usize> = self.breaks.drain(0..).collect();
    for br in breaks {
      if !self.patch_jump(br, OpCode::Jump) {
        return false;
      }
    }
    true
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

  fn break_stmt(&mut self) {
    let break_index = self.index - 1;
    if !self.in_loop {
      self.error(
        break_index,
        String::from("break statements can only be used within loops"),
      );
      return;
    }

    if !self.consume(Token::Semicolon, String::from("expect ';' after break")) {
      return;
    }

    self.reduce_locals_to_depth(break_index, self.loop_depth);

    let jmp = self.emit_jump(break_index);
    self.breaks.push(jmp);
  }

  fn cont_stmt(&mut self) {
    let cont_index = self.index - 1;
    if !self.in_loop {
      self.error(
        cont_index,
        String::from("cont statements can only be used within loops"),
      );
      return;
    }

    if !self.consume(Token::Semicolon, String::from("expect ';' after cont")) {
      return;
    }

    self.reduce_locals_to_depth(cont_index, self.loop_depth);
    let dist = self.current_ctx().num_instructions() - self.cont_jump;
    self.emit(cont_index, OpCode::Loop(dist));
  }

  fn end_stmt(&mut self) {
    let pos = self.index - 1;
    if !self.advance_if_matches(Token::Semicolon) && !self.expression() {
      return;
    }
    if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
      return;
    }
    self.emit(pos, OpCode::End)
  }

  fn expression_stmt(&mut self) {
    if !self.expression() {
      return;
    }
    if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
      return;
    }
    self.emit(self.index - 1, OpCode::Pop);
  }

  fn fn_stmt(&mut self) {
    let var_pos = self.index;
    if let Some(global) = self.parse_variable(String::from("expected function name")) {
      if self.scope_depth > 0 {
        if let Some(local) = self.locals.last_mut() {
          local.initialized = true;
        } else {
          self.error(
            var_pos,
            String::from("expected local variable instead of global, this is a parser error"),
          );
        }
      }

      let name = if let Some(Token::Identifier(name)) = self.previous() {
        name
      } else {
        panic!("unable to retrieve identifier, should not happen");
      };

      self.wrap_fn(name.clone(), self.index - 1, |this| {
        let mut airity = 0;
        if !this.consume(
          Token::LeftParen,
          String::from("expect '(' after function name"),
        ) {
          return None;
        }

        if let Some(token) = this.current() {
          if token != Token::RightParen {
            loop {
              if let Some(param) = this.parse_variable(String::from("expected parameter name")) {
                airity += 1;
                this.define_variable(param);
              } else {
                return None;
              }
              if !this.advance_if_matches(Token::Comma) {
                break;
              }
            }
          }

          if !this.consume(
            Token::RightParen,
            String::from("expected ')' after parameters"),
          ) {
            return None;
          }

          if !this.consume(
            Token::LeftBrace,
            String::from("expected '{' after function declaration"),
          ) {
            return None;
          }

          this.fn_block();
          let count = this.num_locals_in_depth(this.scope_depth);

          this
            .locals
            .truncate(this.locals.len().saturating_sub(count));

          if count > 0 {
            this.emit(this.index - 1, OpCode::PopN(count + airity));
          }
        } else {
          this.error(this.index - 1, String::from("unexpected EOF"));
          return None;
        }

        this.current_ctx().name = name;

        Some(airity)
      });
      self.define_variable(global);
    } else {
      self.error(var_pos, String::from("expected an identifier"));
    }
  }

  fn for_stmt(&mut self) {
    self.wrap_block(|this| {
      // for x; y; z { }
      //     ^
      if this.advance_if_matches(Token::Let) {
        this.let_stmt();
      } else {
        this.expression_stmt();
      }

      // for x; y; z { }
      //        ^
      let comparison_index = this.ctx.num_instructions();
      this.expression();
      let exit_jump = this.emit_jump(this.index); // jump to exit if clause is false

      if !this.consume(
        Token::Semicolon,
        String::from("expected ';' after expression"),
      ) {
        return false;
      }

      let body_jump = this.emit_jump(this.index); // jump over increment to the body

      // for x; y; z { }
      //           ^
      let increment_index = this.ctx.num_instructions();
      this.expression();
      this.emit(this.index, OpCode::Pop);

      if !this.consume(Token::LeftBrace, String::from("expect '{' after clauses")) {
        return false;
      }

      this.loop_to(comparison_index);
      this.patch_jump(body_jump, OpCode::Jump);

      this.wrap_loop(increment_index, |this| {
        this.block_stmt();
        this.loop_to(increment_index);
        true
      });

      this.patch_jump(exit_jump, OpCode::JumpIfFalse);

      true
    });
  }

  fn if_stmt(&mut self) {
    if !self.expression() {
      return;
    }
    if !self.consume(Token::LeftBrace, String::from("expect '{' after condition")) {
      return;
    }

    let if_end = self.emit_jump(self.index);
    self.block_stmt();

    if self.advance_if_matches(Token::Else) {
      let else_end = self.emit_jump(self.index - 1);
      if !self.patch_jump(if_end, OpCode::JumpIfFalse) {
        return;
      }

      if let Some(token) = self.current() {
        self.statement(token);
      } else {
        self.error(self.index - 1, String::from("unexpected end of file"));
        return;
      }

      self.patch_jump(else_end, OpCode::Jump);
    } else {
      self.patch_jump(if_end, OpCode::JumpIfFalse);
    }
  }

  fn block_stmt(&mut self) {
    self.wrap_block(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
      this.consume(Token::RightBrace, String::from("expected '}' after block"))
    });
  }

  fn fn_block(&mut self) {
    self.wrap_scope(|this| {
      while let Some(token) = this.current() {
        if token == Token::RightBrace {
          break;
        }
        this.statement(token);
      }
      this.consume(Token::RightBrace, String::from("expected '}' after block"))
    });
  }

  fn let_stmt(&mut self) {
    if let Some(global) = self.parse_variable(String::from("expect variable name")) {
      if self.advance_if_matches(Token::Equal) {
        if !self.expression() {
          return;
        }
      } else {
        self.emit(self.index, OpCode::Nil);
      }

      if self.consume(
        Token::Semicolon,
        String::from("expect ';' after variable declaration"),
      ) {
        self.define_variable(global);
      }
    }
  }

  fn load_stmt(&mut self) {
    unimplemented!();
  }

  fn loop_stmt(&mut self) {
    let start = self.current_ctx().num_instructions();
    if !self.consume(
      Token::LeftBrace,
      String::from("expect '{' after loop keyword"),
    ) {
      return;
    }
    self.wrap_loop(start, |this| {
      this.block_stmt();
      this.emit(
        this.index - 1,
        OpCode::Loop(this.ctx.num_instructions() - start),
      );
      true
    });
  }

  fn match_stmt(&mut self) {
    if !self.expression() {
      return;
    }

    if !self.consume(Token::LeftBrace, String::from("expect '{' after condition")) {
      return;
    }

    let mut jumps = Vec::new();

    let mut default_case_found = false;
    while let Some(curr) = self.current() {
      if curr == Token::RightBrace {
        break;
      }

      if self.advance_if_matches(Token::Arrow) {
        if default_case_found {
          self.error(
            self.index - 1,
            String::from("can only have one default case in match"),
          );
        }
        default_case_found = true;
      } else {
        if default_case_found {
          self.error(
            self.index,
            String::from("found case after default was created"),
          );
        }

        if !self.expression() {
          continue;
        }

        if !self.consume(Token::Arrow, String::from("expect '=>' after condition")) {
          continue;
        }

        self.emit(self.index - 1, OpCode::Check);
      }

      let next_jmp = self.emit_jump(self.index);

      if let Some(curr) = self.current() {
        self.statement(curr);
        jumps.push(self.emit_jump(self.index));
        if !self.patch_jump(next_jmp, OpCode::JumpIfFalse) {
          return;
        }
      } else {
        self.error(
          self.index - 1,
          String::from("expected statement after condition"),
        );
        return;
      }
    }

    if !self.consume(Token::RightBrace, String::from("expected '}' after match")) {
      return;
    }

    for jump in jumps {
      if !self.patch_jump(jump, OpCode::Jump) {
        return;
      }
    }

    self.emit(self.index - 1, OpCode::Pop);
  }

  fn print_stmt(&mut self) {
    let pos = self.index - 1;

    if !self.expression() {
      return;
    }

    if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
      return;
    }

    self.emit(pos, OpCode::Print);
  }

  fn ret_stmt(&mut self) {
    let pos = self.index - 1;

    if self.advance_if_matches(Token::Semicolon) {
      self.emit(pos, OpCode::Nil);
    } else {
      if !self.expression() {
        return;
      }

      if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
        return;
      }

      self.emit(pos, OpCode::Return);
    }
  }

  fn while_stmt(&mut self) {
    let loop_start = self.current_ctx().num_instructions();
    if !self.expression() {
      return;
    }
    if !self.consume(
      Token::LeftBrace,
      String::from("expected '{' after condition"),
    ) {
      return;
    }

    let exit_jump = self.emit_jump(self.index);

    self.wrap_loop(loop_start, |this| {
      this.block_stmt();
      this.emit(
        this.index - 1,
        OpCode::Loop(this.ctx.num_instructions() - loop_start),
      );
      this.patch_jump(exit_jump, OpCode::JumpIfFalse)
    });
  }

  fn expression(&mut self) -> bool {
    self.parse_precedence(Precedence::Assignment)
  }

  fn wrap_scope<F: FnOnce(&mut Parser) -> bool>(&mut self, f: F) -> bool {
    self.scope_depth += 1;
    if !f(self) {
      return false;
    }
    self.scope_depth -= 1;
    true
  }

  fn wrap_block<F: FnOnce(&mut Parser) -> bool>(&mut self, f: F) -> bool {
    if !self.wrap_scope(f) {
      return false;
    }

    self.reduce_locals_to_depth(self.index - 1, self.scope_depth);

    true
  }

  fn wrap_loop<F: FnOnce(&mut Parser) -> bool>(&mut self, start: usize, f: F) -> bool {
    let in_loop = self.in_loop;
    let loop_depth = self.loop_depth;
    let cont_jump = self.cont_jump;

    let mut breaks = Vec::default();
    std::mem::swap(&mut breaks, &mut self.breaks);

    self.in_loop = true;
    self.loop_depth = self.scope_depth;
    self.cont_jump = start;

    // don't have to worry about wrapping the block since all loops expect block statements after the condition
    let res = f(self);

    let break_res = self.patch_breaks();

    self.in_loop = in_loop;
    self.loop_depth = loop_depth;
    self.breaks = breaks;
    self.cont_jump = cont_jump;

    res && break_res
  }

  fn wrap_fn<F: FnOnce(&mut Parser) -> Option<usize>>(&mut self, name: String, index: usize, f: F) {
    self.function_id += 1;

    let mut locals = Vec::default();
    std::mem::swap(&mut locals, &mut self.locals);

    let prev_ctx = self.current_ctx();

    let reflection = Reflection::new(prev_ctx.meta.file.clone(), prev_ctx.meta.source.clone());

    let prev_fn = self.current_fn.take();
    self.current_fn = Some(SmartPtr::new(Context::new_child(
      prev_ctx,
      reflection,
      self.function_id,
    )));

    self.wrap_scope(|this| {
      let airity = f(this);
      let ctx = this.current_fn.take().unwrap();

      // restore here so const is emitted to correct place
      this.current_fn = prev_fn;
      this.locals = locals;

      if let Some(airity) = airity {
        this.emit_const(index, Value::Function(Function::new(name, airity, ctx)))
      }
      true
    });
  }

  fn rule_for(token: &Token) -> ParseRule {
    match token {
      Token::Invalid => panic!("invalid token read"),
      Token::LeftParen => ParseRule::new(
        Some(Parser::grouping_expr),
        Some(Parser::call_expr),
        Precedence::Call,
      ),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(None, None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, None, Precedence::None),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Plus => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Term),
      Token::Minus => ParseRule::new(
        Some(Parser::unary_expr),
        Some(Parser::binary_expr),
        Precedence::Term,
      ),
      Token::Asterisk => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Slash => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Modulus => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Bang => ParseRule::new(Some(Parser::unary_expr), None, Precedence::None),
      Token::BangEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Equality),
      Token::Equal => ParseRule::new(None, None, Precedence::None),
      Token::EqualEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Equality),
      Token::Greater => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::GreaterEqual => {
        ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison)
      }
      Token::Less => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::LessEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::Arrow => ParseRule::new(None, None, Precedence::None),
      Token::Identifier(_) => ParseRule::new(Some(Parser::make_variable), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(Parser::make_string), None, Precedence::None),
      Token::Number(_) => ParseRule::new(Some(Parser::make_number), None, Precedence::None),
      Token::And => ParseRule::new(None, Some(Parser::and_expr), Precedence::And),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(None, None, Precedence::None),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Load => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::Or => ParseRule::new(None, Some(Parser::or_expr), Precedence::Or),
      Token::Print => ParseRule::new(None, None, Precedence::None),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::True => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::End => ParseRule::new(None, None, Precedence::None),
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) -> bool {
    self.advance();
    if let Some(prev) = self.previous() {
      let rule = Parser::rule_for(&prev);
      let can_assign = precedence <= Precedence::Assignment;

      if let Some(prefix) = rule.prefix {
        if !prefix(self, can_assign) {
          return false;
        }
      } else {
        self.error(self.index - 1, String::from("expected an expression"));
        return false;
      }

      while let Some(curr) = self.current() {
        let rule = Parser::rule_for(&curr);
        if precedence <= rule.precedence {
          self.advance();
          if let Some(prev) = self.previous() {
            if let Some(infix) = Parser::rule_for(&prev).infix {
              if !infix(self, can_assign) {
                return false;
              }
            } else {
              self.error(self.index, format!("no rule for {:?}", prev));
              return false;
            }
          } else {
            self.error(
              self.index - 2,
              String::from("unexpected end of token stream (parse_precedence 1)"),
            );
            return false;
          }
        } else {
          break;
        }
      }

      if can_assign && self.advance_if_matches(Token::Equal) {
        self.error(self.index - 1, String::from("invalid assignment target"));
        false
      } else {
        true
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (parse_precedence 3)"),
      );
      false
    }
  }

  fn make_number(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if let Token::Number(n) = prev {
        self.emit_const(pos, Value::new(n));
        true
      } else {
        self.error(pos, format!("expected number, found {}", prev));
        false
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (make_number)"),
      );
      false
    }
  }

  fn make_string(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if let Token::String(s) = prev {
        self.emit_const(pos, Value::new(s));
        true
      } else {
        self.error(pos, format!("expected string, found {}", prev));
        false
      }
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (make_string)"),
      );
      false
    }
  }

  fn make_variable(&mut self, can_assign: bool) -> bool {
    if let Some(prev) = self.previous() {
      self.named_variable(prev, self.index - 1, can_assign)
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (make_variable)"),
      );
      false
    }
  }

  fn grouping_expr(&mut self, _: bool) -> bool {
    if !self.expression() {
      return false;
    }
    self.consume(
      Token::RightParen,
      String::from("expected ')' after expression"),
    )
  }

  fn call_expr(&mut self, _: bool) -> bool {
    let pos = self.index - 1;
    if let Some(arg_count) = self.parse_arg_list() {
      self.emit(pos, OpCode::Call(arg_count));
      true
    } else {
      false
    }
  }

  fn literal_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      match prev {
        Token::Nil => {
          self.emit(self.index - 1, OpCode::Nil);
          true
        }
        Token::True => {
          self.emit(self.index - 1, OpCode::True);
          true
        }
        Token::False => {
          self.emit(self.index - 1, OpCode::False);
          true
        }
        _ => {
          self.error(
            self.index - 1,
            String::from("reaching this means something is very screwed up"),
          );
          false
        }
      }
    } else {
      todo!("error here");
    }
  }

  fn unary_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if !self.parse_precedence(Precedence::Unary) {
        return false;
      }

      match prev {
        Token::Bang => self.emit(pos, OpCode::Not),
        Token::Minus => self.emit(pos, OpCode::Negate),
        _ => {
          self.error(pos, String::from("invalid unary operator"));
          return false;
        }
      }

      true
    } else {
      todo!("error here");
    }
  }

  fn binary_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      let rule = Self::rule_for(&prev);
      if let Some(precedence) = rule.precedence.next() {
        if !self.parse_precedence(precedence) {
          return false;
        }
      } else {
        self.error(pos, String::from("could not determine precedence order"));
        return false;
      }

      match prev {
        Token::EqualEqual => self.emit(pos, OpCode::Equal),
        Token::BangEqual => self.emit(pos, OpCode::NotEqual),
        Token::Greater => self.emit(pos, OpCode::Greater),
        Token::GreaterEqual => self.emit(pos, OpCode::GreaterEqual),
        Token::Less => self.emit(pos, OpCode::Less),
        Token::LessEqual => self.emit(pos, OpCode::LessEqual),
        Token::Plus => self.emit(pos, OpCode::Add),
        Token::Minus => self.emit(pos, OpCode::Sub),
        Token::Asterisk => self.emit(pos, OpCode::Mul),
        Token::Slash => self.emit(pos, OpCode::Div),
        Token::Modulus => self.emit(pos, OpCode::Mod),
        _ => {
          self.error(pos, String::from("invalid binary operator"));
          return false;
        }
      }

      true
    } else {
      self.error(
        self.index - 2,
        String::from("unexpected end of token stream (binary_expr)"),
      );
      false
    }
  }

  fn and_expr(&mut self, _: bool) -> bool {
    let jmp_pos = self.emit_jump(self.index);
    if !self.parse_precedence(Precedence::And) {
      return false;
    }
    self.patch_jump(jmp_pos, OpCode::And)
  }

  fn or_expr(&mut self, _: bool) -> bool {
    let jmp_pos = self.emit_jump(self.index);
    if !self.parse_precedence(Precedence::Or) {
      return false;
    }
    self.patch_jump(jmp_pos, OpCode::Or)
  }

  fn loop_to(&mut self, start: usize) {
    self.emit(
      self.index,
      OpCode::Loop(self.ctx.num_instructions() - start),
    );
  }

  fn named_variable(&mut self, token: Token, pos: usize, can_assign: bool) -> bool {
    if let Some(lookup) = self.resolve_local(&token, pos) {
      let get: OpCode;
      let set: OpCode;

      if lookup.kind == LookupKind::Local {
        get = OpCode::LookupLocal(lookup.index);
        set = OpCode::AssignLocal(lookup.index);
      } else if let Token::Identifier(name) = token {
        let index = self.add_ident(name);
        get = OpCode::LookupGlobal(index);
        set = OpCode::AssignGlobal(index);
      } else {
        self.error(pos, format!("unable to parse lookup of var '{}'", token));
        return false;
      }

      if can_assign && self.advance_if_matches(Token::Equal) {
        self.expression();
        self.emit(pos, set);
      } else {
        self.emit(pos, get);
      }

      true
    } else {
      false
    }
  }

  /**
   * Returns > 0 if the variable is global, otherwise it returns 0 to signify a local variable
   */
  fn parse_variable(&mut self, err: String) -> Option<usize> {
    if !self.consume_identifier(err) {
      return None;
    }

    if !self.declare_variable() {
      return None;
    }

    if self.scope_depth > 0 {
      Some(0)
    } else if let Some(name) = self.previous() {
      if let Token::Identifier(name) = name {
        Some(self.add_ident(name))
      } else {
        self.error(self.index - 1, String::from("expected identifier"));
        None
      }
    } else {
      self.error(
        self.index - 1,
        String::from("tried to lookup a token at an invalid index"),
      );
      None
    }
  }

  fn parse_arg_list(&mut self) -> Option<usize> {
    if let Some(token) = self.current() {
      let mut count = 0;
      if token != Token::RightParen {
        loop {
          if !self.expression() {
            return None;
          }
          count += 1;
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
      Some(count)
    } else {
      self.error(
        self.index - 1,
        String::from("no current token to parse arguments from"),
      );
      None
    }
  }

  fn declare_variable(&mut self) -> bool {
    if self.scope_depth > 0 {
      if let Some(prev) = self.previous() {
        if let Token::Identifier(name) = prev {
          for local in self.locals.iter().rev() {
            if local.initialized && local.depth < self.scope_depth {
              break;
            }

            if name == local.name {
              self.error(
                self.index - 1,
                String::from("variable with same name already declared"),
              );
              return false;
            }
          }
          self.add_local(name);
        } else {
          self.error(self.index - 1, String::from("expected identifier"));
          return false;
        }
      } else {
        self.error(
          self.index - 1,
          String::from("tried to lookup a token at an invalid index"),
        );
        return false;
      }
    }
    true
  }

  fn define_variable(&mut self, global: usize) -> bool {
    if self.scope_depth == 0 {
      self.emit(self.index - 1, OpCode::DefineGlobal(global));
      true
    } else if let Some(local) = self.locals.last_mut() {
      local.initialized = true;
      // TODO what did this fix?
      // self.emit(self.index - 1, OpCode::AssignLocal(global));
      true
    } else {
      self.error(self.index, String::from("could not define variable"));
      false
    }
  }

  fn resolve_local(&mut self, token: &Token, pos: usize) -> Option<Lookup> {
    if !self.locals.is_empty() {
      let mut index = self.locals.len() - 1;

      if let Token::Identifier(name) = token {
        for local in self.locals.iter().rev() {
          if *name == local.name {
            if !local.initialized {
              self.error(
                pos,
                String::from("can't read variable in it's own initializer"),
              );
              return None;
            } else {
              return Some(Lookup {
                index,
                kind: LookupKind::Local,
              });
            }
          }

          if index > 0 {
            index -= 1;
          }
        }
      } else {
        self.error(
          self.index,
          String::from("tried looking up local who is not an identifier"),
        );
        return None;
      }
    }

    Some(Lookup {
      index: 0,
      kind: LookupKind::Global,
    })
  }

  fn add_local(&mut self, name: String) {
    self.locals.push(Local {
      name,
      depth: self.scope_depth,
      initialized: false,
    });
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
}
