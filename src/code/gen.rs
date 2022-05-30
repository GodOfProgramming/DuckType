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

  errors: Vec<Error>,
}

impl BytecodeGenerator {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ctx,

      identifiers: Default::default(),
      locals: Default::default(),

      function_id: Default::default(),
      current_fn: Default::default(),

      scope_depth: Default::default(),
      loop_depth: Default::default(),

      breaks: Default::default(),
      cont_jump: Default::default(),

      errors: Default::default(),
    }
  }

  pub fn generate(mut self, ast: Ast) -> Result<SmartPtr<Context>, Vec<Error>> {
    for stmt in ast.statements {
      self.emit_stmt(stmt);
    }

    Ok(self.ctx)
  }

  /* Statements */

  fn block_stmt(&mut self, stmt: BlockStatement) {
    let loc = stmt.loc;

    self.new_scope(stmt.loc, |this| {
      for statement in stmt.statements {
        this.emit_stmt(statement);
      }
    });

    self.reduce_locals_to_depth(self.scope_depth, loc);
  }

  fn break_stmt(&mut self, stmt: BreakStatement) {
    self.reduce_locals_to_depth(self.loop_depth, stmt.loc);
    let jmp = self.emit_noop(stmt.loc);
    self.breaks.push(jmp);
  }

  fn cont_stmt(&mut self, stmt: ContStatement) {
    self.reduce_locals_to_depth(self.loop_depth, stmt.loc);
    self.emit_loop(self.cont_jump, stmt.loc);
  }

  fn end_stmt(&mut self, stmt: EndStatement) {
    if let Some(expr) = stmt.expr {
      self.emit_expr(expr);
    } else {
      self.emit(OpCode::Nil, stmt.loc);
    }
    self.emit(OpCode::End, stmt.loc);
  }

  fn fn_stmt(&mut self, stmt: FnStatement) {
    if let Some(var) = self.declare_variable(stmt.ident.clone(), stmt.loc) {
      self.emit_fn(stmt.ident, stmt.params, *stmt.body, stmt.loc);
      self.define_variable(var, stmt.loc);
      if self.scope_depth == 0 {
        self.emit(OpCode::Pop, stmt.loc);
      }
    }
  }

  fn for_stmt(&mut self, stmt: ForStatement) {
    let loc = stmt.loc;

    self.new_scope(stmt.loc, |this| {
      this.emit_stmt(*stmt.initializer);

      let before_compare = this.current_instruction_count();
      this.emit_expr(stmt.comparison);
      let exit = this.emit_noop(stmt.loc);

      let block = *stmt.block;
      let increment = stmt.increment;
      let loc = stmt.loc;
      this.emit_loop_block(before_compare, stmt.loc, |this| {
        this.emit_stmt(block);
        this.emit_expr(increment);
        this.emit(OpCode::Pop, loc);
      });

      this.patch_inst(exit, OpCode::JumpIfFalse);
    });

    self.reduce_locals_to_depth(self.scope_depth, loc);
  }

  fn if_stmt(&mut self, stmt: IfStatement) {
    self.emit_expr(stmt.comparison);
    let end = self.emit_noop(stmt.loc);
    self.emit_stmt(*stmt.block);

    if let Some(else_stmt) = stmt.else_block {
      let else_end = self.emit_noop(stmt.loc);
      self.patch_inst(end, OpCode::JumpIfFalse);
      self.emit_stmt(*else_stmt);
      self.patch_inst(else_end, OpCode::Jump);
    } else {
      self.patch_inst(end, OpCode::JumpIfFalse);
    }
  }

  fn let_stmt(&mut self, stmt: LetStatement) {
    if let Some(var) = self.declare_variable(stmt.ident, stmt.loc) {
      if let Some(value) = stmt.value {
        self.emit_expr(value);
      } else {
        self.emit(OpCode::Nil, stmt.loc);
      }

      self.define_variable(var, stmt.loc);
      if self.scope_depth == 0 {
        self.emit(OpCode::Pop, stmt.loc);
      }
    }
  }

  fn loop_stmt(&mut self, stmt: LoopStatement) {
    let start = self.current_instruction_count();
    self.emit_loop_block(start, stmt.loc, |this| {
      this.emit_stmt(*stmt.block);
    });
  }

  fn print_stmt(&mut self, stmt: PrintStatement) {
    self.emit_expr(stmt.expr);
    self.emit(OpCode::Print, stmt.loc);
  }

  fn while_stmt(&mut self, stmt: WhileStatement) {
    let before_compare = self.current_instruction_count();
    self.emit_expr(stmt.comparison);
    let end_jump = self.emit_noop(stmt.loc);

    let block = *stmt.block;
    self.emit_loop_block(before_compare, stmt.loc, |this| {
      this.emit_stmt(block);
    });

    self.patch_inst(end_jump, OpCode::JumpIfFalse);
  }

  fn expr_stmt(&mut self, stmt: ExpressionStatement) {
    self.emit_expr(stmt.expr);
    self.emit(OpCode::Pop, stmt.loc);
  }

  /* Expressions */

  fn literal_expr(&mut self, expr: LiteralExpression) {
    self.emit_const(expr.value, expr.loc);
  }

  fn unary_expr(&mut self, expr: UnaryExpression) {
    self.emit_expr(*expr.expr);

    match expr.op {
      UnaryOperator::Not => self.emit(OpCode::Not, expr.loc),
      UnaryOperator::Negate => self.emit(OpCode::Negate, expr.loc),
      _ => unimplemented!(),
    }
  }

  fn binary_expr(&mut self, expr: BinaryExpression) {
    self.emit_expr(*expr.left);
    self.emit_expr(*expr.right);
    match expr.op {
      BinaryOperator::Equal => self.emit(OpCode::Equal, expr.loc),
      BinaryOperator::NotEq => self.emit(OpCode::NotEqual, expr.loc),
      BinaryOperator::Less => self.emit(OpCode::Less, expr.loc),
      BinaryOperator::LessEq => self.emit(OpCode::LessEqual, expr.loc),
      BinaryOperator::Greater => self.emit(OpCode::Greater, expr.loc),
      BinaryOperator::GreaterEq => self.emit(OpCode::GreaterEqual, expr.loc),
      BinaryOperator::Add => self.emit(OpCode::Add, expr.loc),
      BinaryOperator::Sub => self.emit(OpCode::Sub, expr.loc),
      BinaryOperator::Mul => self.emit(OpCode::Mul, expr.loc),
      BinaryOperator::Div => self.emit(OpCode::Div, expr.loc),
      BinaryOperator::Mod => self.emit(OpCode::Mod, expr.loc),
    }
  }

  fn and_expr(&mut self, expr: AndExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_noop(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_inst(short_circuit, OpCode::And);
  }

  fn or_expr(&mut self, expr: OrExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_noop(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_inst(short_circuit, OpCode::Or);
  }

  fn group_expr(&mut self, expr: GroupExpression) {
    self.emit_expr(*expr.expr);
  }

  fn ident_expr(&mut self, expr: IdentExpression) {
    if let Some(lookup) = self.resolve_local(&expr.ident) {
      let get = if lookup.kind == LookupKind::Local {
        OpCode::LookupLocal(lookup.index)
      } else {
        let index = self.add_ident(expr.ident);
        OpCode::LookupGlobal(index)
      };

      self.emit(get, expr.loc);
    }
  }

  fn assign_expr(&mut self, expr: AssignExpression) {
    if let Some(lookup) = self.resolve_local(&expr.ident) {
      let set = if lookup.kind == LookupKind::Local {
        OpCode::AssignLocal(lookup.index)
      } else {
        let index = self.add_ident(expr.ident);
        OpCode::AssignGlobal(index)
      };

      self.emit_expr(*expr.value);

      self.emit(set, expr.loc);
    }
  }

  fn call_expr(&mut self, expr: CallExpression) {
    let arg_count = expr.args.len();

    for arg in expr.args {
      self.emit_expr(arg)
    }

    self.emit_expr(*expr.callable);
    self.emit(OpCode::Call(arg_count), expr.loc);
  }

  /* Utility Functions */

  fn emit(&mut self, op: OpCode, loc: SourceLocation) {
    self.current_ctx().write(op, loc.line, loc.column);
  }

  fn emit_stmt(&mut self, stmt: Statement) {
    match stmt {
      Statement::Block(stmt) => self.block_stmt(stmt),
      Statement::Break(stmt) => self.break_stmt(stmt),
      Statement::Cont(stmt) => self.cont_stmt(stmt),
      Statement::End(stmt) => self.end_stmt(stmt),
      Statement::Fn(stmt) => self.fn_stmt(stmt),
      Statement::For(stmt) => self.for_stmt(stmt),
      Statement::If(stmt) => self.if_stmt(stmt),
      Statement::Let(stmt) => self.let_stmt(stmt),
      Statement::Load(stmt) => {}
      Statement::Loop(stmt) => self.loop_stmt(stmt),
      Statement::Match(stmt) => {}
      Statement::Print(stmt) => self.print_stmt(stmt),
      Statement::Ret(stmt) => {}
      Statement::While(stmt) => self.while_stmt(stmt),
      Statement::Expression(stmt) => self.expr_stmt(stmt),
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

  fn emit_loop_block<F: FnOnce(&mut Self)>(&mut self, start: usize, loc: SourceLocation, f: F) {
    let mut breaks = Vec::default();
    std::mem::swap(&mut breaks, &mut self.breaks);

    let cont_jump = self.cont_jump;
    self.cont_jump = start;

    let loop_depth = self.loop_depth;
    self.loop_depth = self.scope_depth;

    self.new_scope(loc, |this| {
      f(this);
    });

    self.emit_loop(start, loc);

    std::mem::swap(&mut breaks, &mut self.breaks);
    self.patch_breaks(breaks);

    self.reduce_locals_to_depth(self.scope_depth, loc);

    self.cont_jump = cont_jump;

    self.loop_depth = loop_depth;
  }

  fn emit_fn(&mut self, ident: Ident, args: Vec<Ident>, body: Statement, loc: SourceLocation) {
    self.function_id += 1;

    let mut locals = Vec::default();
    std::mem::swap(&mut locals, &mut self.locals);

    let parent_ctx = self.current_ctx();
    let prev_fn = self.current_fn.take();

    let reflection = Reflection::new(parent_ctx.meta.file.clone(), parent_ctx.meta.source.clone());

    self.current_fn = Some(SmartPtr::new(Context::new_child(
      parent_ctx,
      reflection,
      self.function_id,
    )));

    self.new_scope(loc, |this| {
      let airity = args.len();

      for arg in args {
        if let Some(var) = this.declare_variable(arg, loc) {
          this.define_variable(var, loc);
        }
      }

      this.emit_stmt(body);

      let ctx = this.current_fn.take().unwrap();

      let num_locals = this.num_locals_in_depth(this.scope_depth);
      if num_locals != 0 {
        this.emit(OpCode::PopN(num_locals), loc);
      }

      // restore here so const is emitted to correct place
      this.current_fn = prev_fn;
      this.locals = locals;

      this.emit_const(Value::Function(Function::new(ident.name, airity, ctx)), loc)
    });
  }

  fn emit_loop(&mut self, start: usize, loc: SourceLocation) {
    let loop_back = self.current_instruction_count() - start;
    self.emit(OpCode::Loop(loop_back), loc);
  }

  fn emit_const(&mut self, c: Value, loc: SourceLocation) {
    self.current_ctx().write_const(c, loc.line, loc.column);
  }

  /**
   * Emits a no op instruction and returns its index, the "jump" is made later with a patch
   */
  fn emit_noop(&mut self, loc: SourceLocation) -> usize {
    let offset = self.current_ctx().num_instructions();
    self.emit(OpCode::NoOp, loc);
    offset
  }

  fn patch_inst<F: FnOnce(usize) -> OpCode>(&mut self, index: usize, f: F) -> bool {
    let offset = self.current_ctx().num_instructions() - index;
    let opcode = f(offset);
    self.current_ctx().replace_instruction(index, opcode)
  }

  fn patch_breaks(&mut self, breaks: Vec<usize>) {
    for br in breaks {
      if !self.patch_inst(br, OpCode::Jump) {
        break;
      }
    }
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

  fn current_instruction_count(&mut self) -> usize {
    self.current_ctx().num_instructions()
  }

  fn new_scope<F: FnOnce(&mut BytecodeGenerator)>(&mut self, loc: SourceLocation, f: F) {
    self.scope_depth += 1;
    f(self);
    self.scope_depth -= 1;
  }

  /**
   * Define a variable
   * If global, the location of its name will be specified by 'var'
   * If local, the last entry in the locals vector will be variable to mark as initialized
   */
  fn define_variable(&mut self, var: usize, loc: SourceLocation) -> bool {
    if self.scope_depth == 0 {
      self.emit(OpCode::DefineGlobal(var), loc);
      true
    } else if let Some(local) = self.locals.last_mut() {
      local.initialized = true;
      true
    } else {
      self.error(loc, String::from("could not define variable"));
      false
    }
  }

  fn declare_variable(&mut self, ident: Ident, loc: SourceLocation) -> Option<usize> {
    println!("declaring {} at {}", ident.name, self.scope_depth);
    if self.scope_depth == 0 {
      Some(self.declare_global(ident))
    } else if self.declare_local(ident, loc) {
      Some(0)
    } else {
      None
    }
  }

  fn declare_local(&mut self, ident: Ident, loc: SourceLocation) -> bool {
    for local in self.locals.iter().rev() {
      // declared already in parent scope
      if local.initialized && local.depth < self.scope_depth {
        break;
      }

      if ident.name == local.name {
        self.error(
          loc,
          String::from("variable with same name already declared"),
        );
        return false;
      }
    }

    self.add_local(ident.name);

    true
  }

  fn declare_global(&mut self, ident: Ident) -> usize {
    self.add_ident(ident)
  }

  fn add_local(&mut self, name: String) {
    self.locals.push(Local {
      name,
      depth: self.scope_depth,
      initialized: false,
    });
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

  fn reduce_locals_to_depth(&mut self, depth: usize, loc: SourceLocation) {
    let count = self.num_locals_in_depth(depth);

    self
      .locals
      .truncate(self.locals.len().saturating_sub(count));

    if count > 0 {
      self.emit(OpCode::PopN(count), loc);
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

  fn error(&mut self, loc: SourceLocation, msg: String) {
    if cfg!(debug_assertions) {
      println!(
        "{} ({}, {}): {}",
        "TODO GET FILE NAME", loc.line, loc.column, msg
      );
    }
    self.errors.push(Error {
      msg,
      file: String::default(),
      line: loc.line,
      column: loc.column,
    });
  }
}
