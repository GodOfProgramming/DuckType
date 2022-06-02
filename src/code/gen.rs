use super::*;
use crate::{
  code::ast::*,
  types::{Error, Function, Struct, Value},
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

    self.new_scope(|this| {
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

  fn fn_stmt(&mut self, stmt: FnStatement) {
    if let Some(var) = self.declare_variable(stmt.ident.clone(), stmt.loc) {
      let is_global = stmt.ident.global();
      self.emit_fn(stmt.ident, stmt.params, *stmt.body, stmt.loc);
      if self.define_variable(is_global, var, stmt.loc) && is_global {
        self.emit(OpCode::Pop, stmt.loc);
      }
    }
  }

  fn for_stmt(&mut self, stmt: ForStatement) {
    let loc = stmt.loc;

    self.new_scope(|this| {
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
    let is_global = stmt.ident.global();
    if let Some(var) = self.declare_variable(stmt.ident, stmt.loc) {
      if let Some(value) = stmt.value {
        self.emit_expr(value);
      } else {
        self.emit(OpCode::Nil, stmt.loc);
      };
      if self.define_variable(is_global, var, stmt.loc) && is_global {
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

  fn match_stmt(&mut self, stmt: MatchStatement) {
    self.emit_expr(stmt.expr);

    let mut jumps = Vec::default();

    for (branch_expr, branch_stmt) in stmt.branches {
      self.emit_expr(branch_expr);
      self.emit(OpCode::Check, stmt.loc);
      let next_jump = self.emit_noop(stmt.loc);
      self.emit_stmt(branch_stmt);
      jumps.push(self.emit_noop(stmt.loc));
      if !self.patch_inst(next_jump, OpCode::JumpIfFalse) {
        break;
      }
    }

    if let Some(default) = stmt.default {
      self.emit_stmt(*default);
    }

    self.patch_jumps(jumps);

    self.emit(OpCode::Pop, stmt.loc);
  }

  fn print_stmt(&mut self, stmt: PrintStatement) {
    self.emit_expr(stmt.expr);
    self.emit(OpCode::Print, stmt.loc);
  }

  fn req_stmt(&mut self, stmt: ReqStatement) {
    self.emit_expr(stmt.file);
    self.emit(OpCode::Req, stmt.loc);

    if let Some(var) = stmt.ident {
      let is_global = var.global();
      if let Some(var) = self.declare_variable(var, stmt.loc) {
        self.define_variable(is_global, var, stmt.loc);
      }
    } else {
      self.emit(OpCode::Pop, stmt.loc);
    }
  }

  fn ret_stmt(&mut self, stmt: RetStatement) {
    if let Some(expr) = stmt.expr {
      self.emit_expr(expr);
    }
    self.emit(OpCode::Ret, stmt.loc);
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
    if let Some(lookup) = self.resolve_local(&expr.ident, expr.loc) {
      let get = match lookup.kind {
        LookupKind::Local => OpCode::LookupLocal(lookup.index),
        LookupKind::Global => {
          let index = self.add_ident(expr.ident);
          OpCode::LookupGlobal(index)
        }
      };

      self.emit(get, expr.loc);
    }
  }

  fn assign_expr(&mut self, expr: AssignExpression) {
    if let Some(lookup) = self.resolve_local(&expr.ident, expr.loc) {
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

  fn list_expr(&mut self, expr: ListExpression) {
    let num_items = expr.items.len();
    for item in expr.items {
      self.emit_expr(item);
    }
    self.emit(OpCode::CreateList(num_items), expr.loc);
  }

  fn index_expr(&mut self, expr: IndexExpression) {
    self.emit_expr(*expr.indexable);
    self.emit_expr(*expr.index);
    self.emit(OpCode::Index, expr.loc);
  }

  fn struct_expr(&mut self, expr: StructExpression) {
    self.emit_const(Value::new(Struct::default()), expr.loc);
    for (member, assign) in expr.members {
      let ident = self.add_const_ident(member);
      self.emit_expr(assign);
      self.emit(OpCode::AssignMember(ident), expr.loc);
    }
  }

  fn member_access_expr(&mut self, expr: MemberAccessExpression) {
    let ident = self.add_const_ident(expr.ident);
    self.emit_expr(*expr.obj);
    self.emit(OpCode::LookupMember(ident), expr.loc);
  }

  fn member_assign_expr(&mut self, expr: MemberAssignExpression) {
    let ident = self.add_const_ident(expr.ident);
    self.emit_expr(*expr.obj);
    self.emit_expr(*expr.value);
    self.emit(OpCode::AssignMember(ident), expr.loc);
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
      Statement::Fn(stmt) => self.fn_stmt(stmt),
      Statement::For(stmt) => self.for_stmt(stmt),
      Statement::If(stmt) => self.if_stmt(stmt),
      Statement::Let(stmt) => self.let_stmt(stmt),
      Statement::Loop(stmt) => self.loop_stmt(stmt),
      Statement::Match(stmt) => self.match_stmt(stmt),
      Statement::Print(stmt) => self.print_stmt(stmt),
      Statement::Req(stmt) => self.req_stmt(stmt),
      Statement::Ret(stmt) => self.ret_stmt(stmt),
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
      Expression::List(expr) => self.list_expr(expr),
      Expression::Index(expr) => self.index_expr(expr),
      Expression::MemberAccess(expr) => self.member_access_expr(expr),
      Expression::MemberAssign(expr) => self.member_assign_expr(expr),
      Expression::Struct(expr) => self.struct_expr(expr),
    }
  }

  fn emit_loop_block<F: FnOnce(&mut Self)>(&mut self, start: usize, loc: SourceLocation, f: F) {
    let mut breaks = Vec::default();
    std::mem::swap(&mut breaks, &mut self.breaks);

    let cont_jump = self.cont_jump;
    self.cont_jump = start;

    let loop_depth = self.loop_depth;
    self.loop_depth = self.scope_depth;

    self.new_scope(|this| {
      f(this);
    });

    self.emit_loop(start, loc);

    std::mem::swap(&mut breaks, &mut self.breaks);
    self.patch_jumps(breaks);

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
      ident.name.clone(),
    )));

    self.new_scope(|this| {
      let airity = args.len();

      for arg in args {
        if arg.global() {
          this.error(loc, String::from("parameter cannot be a global variable"));
          continue;
        }

        if !this.declare_local(arg, loc) {
          continue;
        }

        if !this.define_local() {
          this.error(
            loc,
            String::from("failed to define local who was just declared (sanity check)"),
          )
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

      this.emit_const(Value::Function(Function::new(airity, ctx)), loc)
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

  fn patch_jumps(&mut self, breaks: Vec<usize>) {
    for br in breaks {
      if !self.patch_inst(br, OpCode::Jump) {
        break;
      }
    }
  }

  fn add_const_ident(&mut self, ident: Ident) -> usize {
    self.current_ctx().add_const(Value::new(ident.name))
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

  fn new_scope<F: FnOnce(&mut BytecodeGenerator)>(&mut self, f: F) {
    self.scope_depth += 1;
    f(self);
    self.scope_depth -= 1;
  }

  /**
   * Declare a variable to exist, but do not emit any instruction for assignment
   */
  fn declare_variable(&mut self, ident: Ident, loc: SourceLocation) -> Option<usize> {
    if ident.global() {
      Some(self.declare_global(ident))
    } else if self.declare_local(ident, loc) {
      Some(0)
    } else {
      None
    }
  }

  fn define_global(&mut self, var: usize, loc: SourceLocation) {
    self.emit(OpCode::DefineGlobal(var), loc);
  }

  fn define_local(&mut self) -> bool {
    if let Some(local) = self.locals.last_mut() {
      local.initialized = true;
      true
    } else {
      false
    }
  }

  /**
   * Define a variable. If global the value will come off the stack. If local value is determined in the assign expr
   * If global, the location of its name will be specified by 'var'
   * If local, the last entry in the locals vector will be variable to mark as initialized
   */
  fn define_variable(&mut self, global: bool, var: usize, loc: SourceLocation) -> bool {
    if global {
      self.define_global(var, loc);
      true
    } else if self.define_local() {
      true
    } else {
      self.error(loc, String::from("could not define variable"));
      false
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
          String::from("variable with the same name already declared"),
        );
        return false;
      }
    }

    self.add_local(ident);

    true
  }

  fn declare_global(&mut self, ident: Ident) -> usize {
    self.add_ident(ident)
  }

  fn add_local(&mut self, var: Ident) {
    self.locals.push(Local {
      name: var.name,
      depth: self.scope_depth,
      initialized: false,
    });
  }

  fn resolve_local(&mut self, ident: &Ident, loc: SourceLocation) -> Option<Lookup> {
    if ident.global() {
      Some(Lookup {
        index: 0,
        kind: LookupKind::Global,
      })
    } else {
      if !self.locals.is_empty() {
        let mut index = self.locals.len() - 1;

        for local in self.locals.iter().rev() {
          if ident.name == local.name {
            if !local.initialized {
              self.error(loc, String::from("tried to use an undeclared identifier"));
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
      }

      None
    }
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
