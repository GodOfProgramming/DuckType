use crate::code::{ast::*, Reflection, SourceLocation};
use crate::prelude::*;
use ptr::SmartPtr;
use std::collections::BTreeMap;

use super::{ClassConstant, ConstantValue, FunctionConstant};

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

  errors: Vec<RuntimeError>,
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

  pub fn generate(mut self, ast: Ast) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    for stmt in ast.statements {
      self.emit_stmt(stmt);
    }

    if self.errors.is_empty() {
      Ok(self.ctx)
    } else {
      Err(self.errors)
    }
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
    self.reduce_locals_to_depth(self.loop_depth, stmt.loc.clone());
    let jmp = self.emit_noop(stmt.loc);
    self.breaks.push(jmp);
  }

  fn cont_stmt(&mut self, stmt: ContStatement) {
    self.reduce_locals_to_depth(self.loop_depth, stmt.loc.clone());
    self.emit_loop(self.cont_jump, stmt.loc);
  }

  fn class_stmt(&mut self, stmt: ClassStatement) {
    if self.scope_depth > 0 {
      self.error(stmt.loc, String::from("classes must be declared at the surface scope"));
      return;
    }

    let var = self.declare_global(stmt.ident.clone());

    self.emit_expr(stmt.body);

    self.define_class(var, stmt.loc);
  }

  fn default_constructor_ret(&mut self, stmt: DefaultConstructorRet) {
    self.emit(Opcode::RetValue, stmt.loc);
  }

  fn fn_stmt(&mut self, stmt: FnStatement) {
    if self.scope_depth > 0 {
      self.error(stmt.loc, String::from("functions must be declared at the surface scope"));
      return;
    }

    let var = self.declare_global(stmt.ident.clone());

    self.emit_fn(Some(stmt.ident.name), stmt.params, *stmt.body, stmt.loc.clone());

    self.define_function(var, stmt.loc);
  }

  fn for_stmt(&mut self, stmt: ForStatement) {
    let loc = stmt.loc.clone();
    self.new_scope(|this| {
      this.emit_stmt(*stmt.initializer);

      let before_compare = this.current_instruction_count();
      this.emit_expr(stmt.comparison);
      let exit = this.emit_noop(stmt.loc.clone());

      let block = *stmt.block;
      let increment = stmt.increment;
      let loc = stmt.loc.clone();
      this.emit_loop_block(before_compare, stmt.loc, |this| {
        this.emit_stmt(block);
        this.emit_expr(increment);
        this.emit(Opcode::Pop, loc);
      });

      this.patch_inst(exit, Opcode::JumpIfFalse);
    });

    self.reduce_locals_to_depth(self.scope_depth, loc);
  }

  fn if_stmt(&mut self, stmt: IfStatement) {
    self.emit_expr(stmt.comparison);
    let end = self.emit_noop(stmt.loc.clone());
    self.emit_stmt(*stmt.block);

    if let Some(else_stmt) = stmt.else_block {
      let else_end = self.emit_noop(stmt.loc);
      self.patch_inst(end, Opcode::JumpIfFalse);
      self.emit_stmt(*else_stmt);
      self.patch_inst(else_end, Opcode::Jump);
    } else {
      self.patch_inst(end, Opcode::JumpIfFalse);
    }
  }

  fn let_stmt(&mut self, stmt: LetStatement) {
    let is_global = stmt.ident.global;
    if let Some(var) = self.declare_variable(stmt.ident.clone(), stmt.loc.clone()) {
      if let Some(value) = stmt.value {
        self.emit_expr(value);
      } else {
        self.emit(Opcode::Nil, stmt.loc.clone());
      };

      if self.define_variable(is_global, var, stmt.loc.clone()) && is_global {
        self.emit(Opcode::Pop, stmt.loc);
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
      let loc = stmt.loc.clone();
      self.emit_expr(branch_expr);
      self.emit(Opcode::Check, loc.clone());
      let next_jump = self.emit_noop(loc.clone());
      self.emit_stmt(branch_stmt);
      jumps.push(self.emit_noop(loc));
      if !self.patch_inst(next_jump, Opcode::JumpIfFalse) {
        break;
      }
    }

    if let Some(default) = stmt.default {
      self.emit_stmt(*default);
    }

    self.patch_jumps(jumps);

    self.emit(Opcode::Pop, stmt.loc);
  }

  fn print_stmt(&mut self, stmt: PrintStatement) {
    self.emit_expr(stmt.expr);
    self.emit(Opcode::Print, stmt.loc);
  }

  fn req_stmt(&mut self, stmt: ReqStatement) {
    self.emit_expr(stmt.file);
    self.emit(Opcode::Req, stmt.loc.clone());

    if let Some(var) = stmt.ident {
      let is_global = var.global;
      if let Some(var) = self.declare_variable(var, stmt.loc.clone()) {
        self.define_variable(is_global, var, stmt.loc.clone());
      }
      if is_global {
        self.emit(Opcode::Pop, stmt.loc);
      }
    } else {
      self.emit(Opcode::Pop, stmt.loc);
    }
  }

  fn ret_stmt(&mut self, stmt: RetStatement) {
    if let Some(expr) = stmt.expr {
      self.emit_expr(expr);
      self.emit(Opcode::RetValue, stmt.loc);
    } else {
      self.emit(Opcode::Ret, stmt.loc);
    }
  }

  fn use_stmt(&mut self, stmt: UseStatement) {
    let initial = stmt.path.first().cloned().unwrap(); // validated in ast
    let var = stmt.path.last().cloned().unwrap(); // validated in ast
    let var_idx = self.declare_global(var);

    if let Some(lookup) = self.resolve_ident(&initial, stmt.loc.clone()) {
      let get = match lookup.kind {
        LookupKind::Local => Opcode::LookupLocal(lookup.index),
        LookupKind::Global => Opcode::LookupGlobal(self.declare_global(initial)),
      };

      self.emit(get, stmt.loc.clone());

      for name in stmt.path.into_iter().skip(1) {
        let ident = self.add_const_ident(name);
        self.emit(Opcode::LookupMember(ident), stmt.loc.clone());
      }

      self.define_global(var_idx, stmt.loc.clone());
      self.emit(Opcode::Pop, stmt.loc);
    }
  }

  fn while_stmt(&mut self, stmt: WhileStatement) {
    let before_compare = self.current_instruction_count();
    self.emit_expr(stmt.comparison);
    let end_jump = self.emit_noop(stmt.loc.clone());

    let block = *stmt.block;
    self.emit_loop_block(before_compare, stmt.loc, |this| {
      this.emit_stmt(block);
    });

    self.patch_inst(end_jump, Opcode::JumpIfFalse);
  }

  fn yield_stmt(&mut self, stmt: YieldStatement) {
    self.emit(Opcode::Yield, stmt.loc);
  }

  fn expr_stmt(&mut self, stmt: ExpressionStatement) {
    self.emit_expr(stmt.expr);
    self.emit(Opcode::Pop, stmt.loc);
  }

  /* Expressions */

  fn literal_expr(&mut self, expr: LiteralExpression) {
    self.emit_const(expr.value, expr.loc);
  }

  fn unary_expr(&mut self, expr: UnaryExpression) {
    self.emit_expr(*expr.expr);
    match expr.op {
      UnaryOperator::Not => self.emit(Opcode::Not, expr.loc),
      UnaryOperator::Negate => self.emit(Opcode::Negate, expr.loc),
    }
  }

  fn binary_expr(&mut self, expr: BinaryExpression) {
    self.emit_expr(*expr.left);
    self.emit_expr(*expr.right);
    match expr.op {
      BinaryOperator::Equal => self.emit(Opcode::Equal, expr.loc),
      BinaryOperator::NotEq => self.emit(Opcode::NotEqual, expr.loc),
      BinaryOperator::Less => self.emit(Opcode::Less, expr.loc),
      BinaryOperator::LessEq => self.emit(Opcode::LessEqual, expr.loc),
      BinaryOperator::Greater => self.emit(Opcode::Greater, expr.loc),
      BinaryOperator::GreaterEq => self.emit(Opcode::GreaterEqual, expr.loc),
      BinaryOperator::Add => self.emit(Opcode::Add, expr.loc),
      BinaryOperator::Sub => self.emit(Opcode::Sub, expr.loc),
      BinaryOperator::Mul => self.emit(Opcode::Mul, expr.loc),
      BinaryOperator::Div => self.emit(Opcode::Div, expr.loc),
      BinaryOperator::Mod => self.emit(Opcode::Rem, expr.loc),
    }
  }

  fn and_expr(&mut self, expr: AndExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_noop(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_inst(short_circuit, Opcode::And);
  }

  fn or_expr(&mut self, expr: OrExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_noop(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_inst(short_circuit, Opcode::Or);
  }

  fn group_expr(&mut self, expr: GroupExpression) {
    self.emit_expr(*expr.expr);
  }

  fn ident_expr(&mut self, expr: IdentExpression) {
    if let Some(lookup) = self.resolve_ident(&expr.ident, expr.loc.clone()) {
      let get = match lookup.kind {
        LookupKind::Local => Opcode::LookupLocal(lookup.index),
        LookupKind::Global => Opcode::LookupGlobal(self.declare_global(expr.ident)),
      };

      self.emit(get, expr.loc);
    }
  }

  fn assign_expr(&mut self, expr: AssignExpression) {
    let mut op_assign = |expr: AssignExpression, op| {
      if let Some(lookup) = self.resolve_ident(&expr.ident, expr.loc.clone()) {
        let (set, get) = match lookup.kind {
          LookupKind::Local => (Opcode::AssignLocal(lookup.index), Opcode::LookupLocal(lookup.index)),
          LookupKind::Global => {
            let global_index = self.declare_global(expr.ident);
            (Opcode::AssignGlobal(global_index), Opcode::LookupGlobal(global_index))
          }
        };

        self.emit(get, expr.loc.clone());

        self.emit_expr(*expr.value);

        self.emit(op, expr.loc.clone());

        self.emit(set, expr.loc);
      }
    };
    match expr.op {
      AssignOperator::Assign => {
        if let Some(lookup) = self.resolve_ident(&expr.ident, expr.loc.clone()) {
          let set = match lookup.kind {
            LookupKind::Local => Opcode::AssignLocal(lookup.index),
            LookupKind::Global => Opcode::AssignGlobal(self.declare_global(expr.ident)),
          };

          self.emit_expr(*expr.value);

          self.emit(set, expr.loc);
        }
      }
      AssignOperator::Add => op_assign(expr, Opcode::Add),
      AssignOperator::Sub => op_assign(expr, Opcode::Sub),
      AssignOperator::Mul => op_assign(expr, Opcode::Mul),
      AssignOperator::Div => op_assign(expr, Opcode::Div),
      AssignOperator::Mod => op_assign(expr, Opcode::Rem),
    }
  }

  fn call_expr(&mut self, expr: CallExpression) {
    let arg_count = expr.args.len();

    self.emit_expr(*expr.callable);

    for arg in expr.args {
      self.emit_expr(arg)
    }

    self.emit(Opcode::Call(arg_count), expr.loc);
  }

  fn list_expr(&mut self, expr: ListExpression) {
    let num_items = expr.items.len();
    for item in expr.items {
      self.emit_expr(item);
    }
    self.emit(Opcode::CreateList(num_items), expr.loc);
  }

  fn index_expr(&mut self, expr: IndexExpression) {
    let ident = self.add_const_ident(Ident::new(ops::INDEX));
    self.emit_expr(*expr.indexable);
    self.emit(Opcode::LookupMember(ident), expr.loc.clone());
    self.emit_expr(*expr.index);
    self.emit(Opcode::Call(1), expr.loc);
  }

  fn struct_expr(&mut self, expr: StructExpression) {
    self.emit(Opcode::CreateStruct, expr.loc.clone());
    for (member, assign) in expr.members {
      let ident = self.add_const_ident(member);
      self.emit_expr(assign);
      self.emit(Opcode::InitializeMember(ident), expr.loc.clone());
    }
  }

  fn class_expr(&mut self, name: Option<String>, expr: ClassExpression) {
    let mut class = ClassConstant::new(name.clone());

    if let Some(initializer) = expr.initializer {
      if let Some((function, is_static)) = self.create_class_fn("<constructor>".to_string(), *initializer) {
        if is_static {
          class.set_constructor(function);
        } else {
          self.error(
            expr.loc.clone(),
            String::from("method was used as initializer somehow (logic error)"),
          );
        }
      } else {
        self.error(expr.loc.clone(), format!("unable to create initializer function for class"));
      }
    }

    for (method_name, method) in expr.methods {
      if let Some((function, is_static)) = self.create_class_fn(method_name.name.clone(), method) {
        if is_static {
          class.set_static(method_name.name, function);
        } else {
          class.set_method(method_name.name, function);
        }
      } else {
        self.error(expr.loc.clone(), format!("unable to create method {}", method_name.name));
      }
    }

    self.emit_const(ConstantValue::Class(class), expr.loc.clone());
  }

  fn mod_expr(&mut self, expr: ModExpression) {}

  fn lambda_expr(&mut self, expr: LambdaExpression) {
    self.emit_fn(None, expr.params, *expr.body, expr.loc);
  }

  fn closure_expr(&mut self, expr: ClosureExpression) {
    let num_captures = expr.captures.members.len();

    if num_captures == 0 {
      self.lambda_expr(LambdaExpression::from(expr));
    } else {
      let mut params = Vec::with_capacity(num_captures);
      for (member, assign) in expr.captures.members {
        params.push(member);
        self.emit_expr(assign);
      }

      self.emit(Opcode::CreateList(params.len()), expr.loc.clone());

      params.extend(expr.params);

      self.emit_fn(None, params, *expr.body, expr.loc.clone());

      self.emit(Opcode::CreateClosure, expr.loc);
    }
  }

  fn method_expr(&mut self, expr: MethodExpression) {
    self.emit_fn(Some(expr.name), expr.params, *expr.body, expr.loc);
  }

  fn member_access_expr(&mut self, expr: MemberAccessExpression) {
    let ident = self.add_const_ident(expr.ident);
    self.emit_expr(*expr.obj);
    self.emit(Opcode::LookupMember(ident), expr.loc);
  }

  fn member_assign_expr(&mut self, expr: MemberAssignExpression) {
    let mut op_assign = |expr: MemberAssignExpression, op| {
      let ident = self.add_const_ident(expr.ident);
      self.emit_expr(*expr.obj);
      self.emit(Opcode::PeekMember(ident), expr.loc.clone());
      self.emit_expr(*expr.value);
      self.emit(op, expr.loc.clone());
      self.emit(Opcode::AssignMember(ident), expr.loc);
    };

    match expr.op {
      AssignOperator::Assign => {
        let ident = self.add_const_ident(expr.ident);
        self.emit_expr(*expr.obj);
        self.emit_expr(*expr.value);
        self.emit(Opcode::AssignMember(ident), expr.loc);
      }
      AssignOperator::Add => op_assign(expr, Opcode::Add),
      AssignOperator::Sub => op_assign(expr, Opcode::Sub),
      AssignOperator::Mul => op_assign(expr, Opcode::Mul),
      AssignOperator::Div => op_assign(expr, Opcode::Div),
      AssignOperator::Mod => op_assign(expr, Opcode::Rem),
    }
  }

  /* Utility Functions */

  fn emit(&mut self, op: Opcode, loc: SourceLocation) {
    self.current_ctx().write(op, loc.line, loc.column);
  }

  fn emit_stmt(&mut self, stmt: Statement) {
    #[cfg(feature = "visit-ast")]
    {
      println!("stmt {}", stmt);
    }
    match stmt {
      Statement::Block(stmt) => self.block_stmt(stmt),
      Statement::Break(stmt) => self.break_stmt(stmt),
      Statement::Cont(stmt) => self.cont_stmt(stmt),
      Statement::Class(stmt) => self.class_stmt(stmt),
      Statement::DefaultConstructorRet(stmt) => self.default_constructor_ret(stmt),
      Statement::Fn(stmt) => self.fn_stmt(stmt),
      Statement::For(stmt) => self.for_stmt(stmt),
      Statement::If(stmt) => self.if_stmt(stmt),
      Statement::Let(stmt) => self.let_stmt(stmt),
      Statement::Loop(stmt) => self.loop_stmt(stmt),
      Statement::Match(stmt) => self.match_stmt(stmt),
      Statement::Print(stmt) => self.print_stmt(stmt),
      Statement::Req(stmt) => self.req_stmt(stmt),
      Statement::Ret(stmt) => self.ret_stmt(stmt),
      Statement::Use(stmt) => self.use_stmt(stmt),
      Statement::While(stmt) => self.while_stmt(stmt),
      Statement::Yield(stmt) => self.yield_stmt(stmt),
      Statement::Expression(stmt) => self.expr_stmt(stmt),
    }
  }

  fn emit_expr(&mut self, expr: Expression) {
    #[cfg(feature = "visit-ast")]
    {
      println!("expr {}", expr);
    }
    match expr {
      Expression::Literal(expr) => self.literal_expr(expr),
      Expression::Unary(expr) => self.unary_expr(expr),
      Expression::Binary(expr) => self.binary_expr(expr),
      Expression::And(expr) => self.and_expr(expr),
      Expression::Or(expr) => self.or_expr(expr),
      Expression::Group(expr) => self.group_expr(expr),
      Expression::Ident(expr) => self.ident_expr(expr),
      Expression::Assign(expr) => self.assign_expr(expr),
      Expression::MemberAccess(expr) => self.member_access_expr(expr),
      Expression::MemberAssign(expr) => self.member_assign_expr(expr),
      Expression::Call(expr) => self.call_expr(expr),
      Expression::List(expr) => self.list_expr(expr),
      Expression::Index(expr) => self.index_expr(expr),
      Expression::Struct(expr) => self.struct_expr(expr),
      Expression::Class(expr) => self.class_expr(None, expr),
      Expression::Mod(expr) => self.mod_expr(expr),
      Expression::Lambda(expr) => self.lambda_expr(expr),
      Expression::Closure(expr) => self.closure_expr(expr),
      Expression::Method(expr) => self.method_expr(expr),
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

    self.emit_loop(start, loc.clone());

    std::mem::swap(&mut breaks, &mut self.breaks);
    self.patch_jumps(breaks);

    self.reduce_locals_to_depth(self.scope_depth, loc);

    self.cont_jump = cont_jump;

    self.loop_depth = loop_depth;
  }

  fn emit_fn(&mut self, name: Option<String>, args: Vec<Ident>, body: Statement, loc: SourceLocation) {
    let function = self.create_fn(name, args, body, loc.clone());
    self.emit_const(ConstantValue::Fn(function), loc);
  }

  fn emit_loop(&mut self, start: usize, loc: SourceLocation) {
    let loop_back = self.current_instruction_count() - start;
    self.emit(Opcode::Loop(loop_back), loc);
  }

  fn emit_const(&mut self, c: ConstantValue, loc: SourceLocation) {
    self.current_ctx().write_const(c, loc.line, loc.column);
  }

  /**
   * Emits a no op instruction and returns its index, the "jump" is made later with a patch
   */
  fn emit_noop(&mut self, loc: SourceLocation) -> usize {
    let offset = self.current_ctx().num_instructions();
    self.emit(Opcode::NoOp, loc);
    offset
  }

  fn patch_inst<F: FnOnce(usize) -> Opcode>(&mut self, index: usize, f: F) -> bool {
    let offset = self.current_ctx().num_instructions() - index;
    let opcode = f(offset);
    self.current_ctx().replace_instruction(index, opcode)
  }

  fn patch_jumps(&mut self, breaks: Vec<usize>) {
    for br in breaks {
      if !self.patch_inst(br, Opcode::Jump) {
        break;
      }
    }
  }

  fn add_const_ident(&mut self, ident: Ident) -> usize {
    self.current_ctx().add_const(ConstantValue::String(ident.name))
  }

  fn current_ctx(&mut self) -> &mut Context {
    if let Some(ctx) = self.current_fn.as_mut() {
      ctx
    } else {
      &mut self.ctx
    }
  }

  fn current_ctx_ptr(&mut self) -> SmartPtr<Context> {
    if let Some(ctx) = &mut self.current_fn {
      ctx.clone()
    } else {
      self.ctx.clone()
    }
  }

  fn global_ctx(&mut self) -> &mut Context {
    self.current_ctx().global_ctx_mut()
  }

  fn current_instruction_count(&mut self) -> usize {
    self.current_ctx().num_instructions()
  }

  fn new_scope<R, F: FnOnce(&mut BytecodeGenerator) -> R>(&mut self, f: F) -> R {
    self.scope_depth += 1;
    let ret = f(self);
    self.scope_depth -= 1;
    ret
  }

  /**
   * Declare a variable to exist, but do not emit any instruction for assignment
   */
  fn declare_variable(&mut self, ident: Ident, loc: SourceLocation) -> Option<usize> {
    if ident.global {
      Some(self.declare_global(ident))
    } else if self.declare_local(ident, loc) {
      Some(0)
    } else {
      None
    }
  }

  /**
   * Returns the index of the identifier name
   */
  fn declare_global(&mut self, ident: Ident) -> usize {
    if let Some(index) = self.identifiers.get(&ident.name).cloned() {
      index
    } else {
      let index = self.global_ctx().add_const(ConstantValue::String(ident.name.clone()));
      self.identifiers.insert(ident.name, index);
      index
    }
  }

  fn declare_local(&mut self, ident: Ident, loc: SourceLocation) -> bool {
    for local in self.locals.iter().rev() {
      // declared already in parent scope, break to redeclare in current scope
      if local.initialized && local.depth < self.scope_depth {
        break;
      }

      if ident.name == local.name {
        self.error(loc, String::from("variable with the same name already declared"));
        return false;
      }
    }

    self.locals.push(Local {
      name: ident.name,
      depth: self.scope_depth,
      initialized: false,
    });

    true
  }

  fn define_global(&mut self, var: usize, loc: SourceLocation) {
    self.emit(Opcode::DefineGlobal(var), loc);
  }

  fn define_local(&mut self) -> bool {
    if let Some(local) = self.locals.last_mut() {
      local.initialized = true;
      true
    } else {
      false
    }
  }

  fn define_function(&mut self, var: usize, loc: SourceLocation) {
    self.emit(Opcode::ForceAssignGlobal(var), loc);
  }

  fn define_class(&mut self, var: usize, loc: SourceLocation) {
    self.emit(Opcode::ForceAssignGlobal(var), loc);
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

  fn resolve_ident(&mut self, ident: &Ident, loc: SourceLocation) -> Option<Lookup> {
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

    Some(Lookup {
      index: 0,
      kind: LookupKind::Global,
    })
  }

  fn reduce_locals_to_depth(&mut self, depth: usize, loc: SourceLocation) {
    let count = self.num_locals_in_depth(depth);

    self.locals.truncate(self.locals.len().saturating_sub(count));

    if count > 0 {
      self.emit(Opcode::PopN(count), loc);
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

  fn create_class_fn(&mut self, name: String, expr: Expression) -> Option<(FunctionConstant, bool)> {
    match expr {
      Expression::Method(m) => Some((self.create_fn(Some(name), m.params, *m.body, m.loc), false)),
      Expression::Lambda(l) => Some((self.create_fn(Some(name), l.params, *l.body, l.loc), true)),
      _ => None,
    }
  }

  fn create_fn(&mut self, name: Option<String>, args: Vec<Ident>, body: Statement, loc: SourceLocation) -> FunctionConstant {
    self.function_id += 1;

    let mut locals = Vec::default();
    std::mem::swap(&mut locals, &mut self.locals);

    let parent_ctx = self.current_ctx_ptr();
    let prev_fn = self.current_fn.take();

    let reflection = Reflection::new(parent_ctx.meta.file.clone(), parent_ctx.meta.source.clone());

    self.current_fn = Some(SmartPtr::new(Context::new_child(
      name,
      self.function_id,
      parent_ctx,
      reflection,
    )));

    self.new_scope(|this| {
      let airity = args.len();

      for arg in args {
        let loc = loc.clone();
        if arg.global {
          this.error(loc.clone(), String::from("parameter cannot be a global variable"));
          continue;
        }

        if !this.declare_local(arg, loc.clone()) {
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
        this.emit(Opcode::PopN(num_locals), loc);
      }

      let local_count = this.locals.len();
      // restore here so const is emitted to correct place
      this.current_fn = prev_fn;
      this.locals = locals;

      FunctionConstant::new(airity, local_count, ctx)
    })
  }

  fn error(&mut self, loc: SourceLocation, msg: String) {
    if cfg!(debug_assertions) {
      println!("{} ({}, {}): {}", loc.file, loc.line, loc.column, msg);
    }
    self.errors.push(RuntimeError {
      msg,
      file: String::default(),
      line: loc.line,
      column: loc.column,
    });
  }
}
