use super::{ConstantValue, FunctionConstant};
use crate::code::{ast::*, Reflection, SourceLocation};
use crate::error::CompiletimeErrors;
use crate::prelude::*;
use crate::util::FileIdType;
use ptr::SmartPtr;
use std::fmt::Debug;
#[cfg(test)]
use std::{cell::RefCell, collections::HashSet};

macro_rules! sanity_check {
  () => {
    format!("{} ({}): sanity check", file!(), line!())
  };
}

#[cfg(test)]
thread_local! {
  pub static CAPTURE_OPS: RefCell<bool> = RefCell::new(false);
  pub static GENERATED_OPS: RefCell<HashSet<Opcode>> = RefCell::new(Default::default());
}

struct Local {
  name: String,
  depth: usize,
  initialized: bool,
}

#[derive(PartialEq)]
enum Lookup {
  Local(usize),
  Global,
}

pub struct BytecodeGenerator<'p> {
  cache: &'p mut Cache,
  file_id: Option<FileIdType>,
  ctx: SmartPtr<Context>,

  locals: Vec<Local>,

  function_id: usize,
  current_fn: Option<SmartPtr<Context>>,

  scope_depth: usize,
  loop_depth: usize,
  fn_depth: usize,

  breaks: Vec<usize>,
  continues: Vec<usize>,

  errors: CompiletimeErrors,
}

impl<'p> BytecodeGenerator<'p> {
  pub fn new(cache: &'p mut Cache, file_id: Option<FileIdType>, ctx: SmartPtr<Context>) -> Self {
    Self {
      cache,
      file_id,
      ctx,

      locals: Default::default(),

      function_id: Default::default(),
      current_fn: Default::default(),

      scope_depth: Default::default(),
      loop_depth: Default::default(),
      fn_depth: Default::default(),

      breaks: Default::default(),
      continues: Default::default(),

      errors: Default::default(),
    }
  }

  pub fn generate(mut self, ast: Ast) -> Result<SmartPtr<Context>, CompiletimeErrors> {
    for stmt in ast.statements {
      self.emit_stmt(stmt);
    }

    self.emit(Opcode::Ret, SourceLocation { line: 0, column: 0 });

    if self.errors.is_empty() {
      Ok(self.ctx)
    } else {
      Err(self.errors)
    }
  }

  /* Statements */

  fn block_stmt(&mut self, stmt: BlockStatement) {
    let loc = stmt.loc;

    self.emit(Opcode::EnterBlock, loc);

    let this_scope = self.scope_depth;
    self.new_scope(|this| {
      for statement in stmt.statements {
        this.emit_stmt(statement);
      }
      this.reduce_locals_to_depth(this_scope, loc);
    });

    self.emit(Opcode::PopScope, loc);
  }

  fn break_stmt(&mut self, stmt: BreakStatement) {
    let jmp = self.emit_placeholder(stmt.loc);
    self.breaks.push(jmp);
  }

  fn cont_stmt(&mut self, stmt: ContStatement) {
    let jmp = self.emit_placeholder(stmt.loc);
    self.continues.push(jmp);
  }

  fn class_stmt(&mut self, stmt: ClassStatement) {
    if let Some(var) = self.declare_global(stmt.ident.clone()) {
      self.emit_expr(stmt.body);

      self.define_scope(var, stmt.loc);
      self.emit(Opcode::Pop, stmt.loc);
    } else {
      self.error(stmt.loc, "could not create class name");
    }
  }

  fn export_stmt(&mut self, stmt: ExportStatement) {
    if self.scope_depth == 0 {
      self.emit_expr(stmt.expr);
      self.emit(Opcode::Export, stmt.loc);
    } else {
      self.error(stmt.loc, "exports can only be made at the surface scope");
    }
  }

  fn fn_stmt(&mut self, stmt: FnStatement) {
    if let Some(var) = self.declare_global(stmt.ident.clone()) {
      let nargs = stmt.params.len();
      self.emit_fn(Some(stmt.ident), stmt.params, nargs, *stmt.body, stmt.loc);

      self.define_scope(var, stmt.loc);
      self.emit(Opcode::Pop, stmt.loc);
    } else {
      self.error(stmt.loc, "could not create fn name");
    }
  }

  fn for_stmt(&mut self, stmt: ForStatement) {
    let loc = stmt.loc;
    self.new_scope(|this| {
      this.emit_stmt(*stmt.initializer);

      let before_compare = this.current_instruction_count();
      this.emit_expr(stmt.comparison);
      let exit = this.emit_placeholder(stmt.loc);

      let block = *stmt.block;
      let increment = stmt.increment;
      let loc = stmt.loc;
      this.emit_loop_block(before_compare, stmt.loc, |this| {
        this.emit_stmt(block);
        let here = this.current_instruction_count();
        this.emit_expr(increment);
        this.emit(Opcode::Pop, loc);
        Some(here)
      });

      this.patch_jump_to_here(exit, Opcode::JumpIfFalse);
    });

    self.reduce_locals_to_depth(self.scope_depth, loc);
  }

  fn if_stmt(&mut self, stmt: IfStatement) {
    self.emit_expr(stmt.comparison);
    let end = self.emit_placeholder(stmt.loc);
    self.emit_stmt(*stmt.block);

    if let Some(else_stmt) = stmt.else_block {
      let else_end = self.emit_placeholder(stmt.loc);
      self.patch_jump_to_here(end, Opcode::JumpIfFalse);
      self.emit_stmt(*else_stmt);
      self.patch_jump_to_here(else_end, Opcode::Jump);
    } else {
      self.patch_jump_to_here(end, Opcode::JumpIfFalse);
    }
  }

  fn let_stmt(&mut self, stmt: LetStatement) {
    let is_global = stmt.ident.global;
    if let Some(var) = self.declare_variable(stmt.ident.clone(), stmt.loc) {
      if let Some(value) = stmt.value {
        self.emit_expr(value);
      } else {
        self.emit(Opcode::Nil, stmt.loc);
      };

      if self.define_variable(is_global, var, stmt.loc) && is_global {
        self.emit(Opcode::Pop, stmt.loc);
      }
    }
  }

  fn loop_stmt(&mut self, stmt: LoopStatement) {
    let start = self.current_instruction_count();
    self.emit_loop_block(start, stmt.loc, |this| {
      this.emit_stmt(*stmt.block);
      None
    });
  }

  fn match_stmt(&mut self, stmt: MatchStatement) {
    self.emit_expr(stmt.expr);

    let mut jumps = Vec::default();

    for (branch_expr, branch_stmt) in stmt.branches {
      let loc = stmt.loc;
      self.emit_expr(branch_expr);
      self.emit(Opcode::Check, loc);
      let next_jump = self.emit_placeholder(loc);
      self.emit_stmt(branch_stmt);
      jumps.push(self.emit_placeholder(loc));
      if !self.patch_jump_to_here(next_jump, Opcode::JumpIfFalse) {
        break;
      }
    }

    if let Some(default) = stmt.default {
      self.emit_stmt(*default);
    }

    self.patch_jumps_to_here(jumps);

    self.emit(Opcode::Pop, stmt.loc);
  }

  fn mod_stmt(&mut self, stmt: ModStatement) {
    if let Some(var) = self.declare_global(stmt.ident.clone()) {
      self.emit_expr(stmt.body);

      self.define_scope(var, stmt.loc);
      self.emit(Opcode::Pop, stmt.loc);
    } else {
      self.error(stmt.loc, "could not create mod name");
    }
  }

  fn println_stmt(&mut self, stmt: PrintlnStatement) {
    self.emit_expr(stmt.expr);
    self.emit(Opcode::Println, stmt.loc);
  }

  fn quack_stmt(&mut self, stmt: QuackStatement) {
    self.emit_expr(stmt.expr);
    self.emit(Opcode::Quack, stmt.loc);
  }

  fn req_stmt(&mut self, stmt: ReqStatement) {
    if let Some(var) = self.declare_global(stmt.ident) {
      self.emit_expr(stmt.expr);
      self.define_scope(var, stmt.loc);
      self.emit(Opcode::Pop, stmt.loc);
    } else {
      self.error(stmt.loc, "could not crate req storage name");
    }
  }

  fn ret_stmt(&mut self, stmt: RetStatement) {
    if self.fn_depth == 0 {
      self.error(stmt.loc, "ret can only be used within functions");
      return;
    }

    if let Some(expr) = stmt.expr {
      self.emit_expr(expr);
      self.emit(Opcode::Export, stmt.loc);
    }

    let locals = self.num_locals_in_exclusive_depth(self.fn_depth);
    self.emit((Opcode::PopN, locals), stmt.loc);
    self.emit(Opcode::Ret, stmt.loc);
  }

  fn use_stmt(&mut self, stmt: UseStatement) {
    let initial = stmt.path.first().cloned().unwrap(); // validated in ast
    let var = stmt.path.last().cloned().unwrap(); // validated in ast

    if let Some(var_idx) = self.declare_global(var) {
      if let Some(lookup) = self.resolve_ident(&initial, stmt.loc) {
        let get = match lookup {
          Lookup::Local(index) => (Opcode::Load, (Storage::Local, LongAddr(index))),
          Lookup::Global => {
            if let Some(var) = self.declare_global(initial) {
              (Opcode::Load, (Storage::Global, LongAddr(var)))
            } else {
              self.error(stmt.loc, "could not declare ident for use");
              return;
            }
          }
        };

        self.emit(get, stmt.loc);

        for name in stmt.path.into_iter().skip(1) {
          if let Some(ident) = self.add_const_ident(name) {
            self.emit((Opcode::Resolve, ident), stmt.loc);
          } else {
            self.error(stmt.loc, "could not create path ident");
            return;
          }
        }

        self.define_scope(var_idx, stmt.loc);
        self.emit(Opcode::Pop, stmt.loc);
      } else {
        self.error(stmt.loc, "could not declare final name in use path");
      }
    }
  }

  fn while_stmt(&mut self, stmt: WhileStatement) {
    let before_compare = self.current_instruction_count();
    self.emit_expr(stmt.comparison);
    let end_jump = self.emit_placeholder(stmt.loc);

    let block = *stmt.block;
    self.emit_loop_block(before_compare, stmt.loc, |this| {
      this.emit_stmt(block);
      None
    });

    self.patch_jump_to_here(end_jump, Opcode::JumpIfFalse);
  }

  fn breakpoint_stmt(&mut self, loc: SourceLocation) {
    self.emit(Opcode::Breakpoint, loc)
  }

  fn expr_stmt(&mut self, stmt: ExpressionStatement) {
    self.emit_expr(stmt.expr);
    self.emit(Opcode::Pop, stmt.loc);
  }

  /* Expressions */

  fn literal_expr(&mut self, expr: LiteralExpression) {
    match expr.value {
      LiteralValue::Nil => self.emit(Opcode::Nil, expr.loc),
      LiteralValue::Bool(b) => self.emit(if b { Opcode::True } else { Opcode::False }, expr.loc),
      LiteralValue::I32(i) => {
        if !self.emit_const(ConstantValue::Integer(i), expr.loc) {
          self.error(expr.loc, format!("failed to add const {i}"));
        }
      }
      LiteralValue::F64(f) => {
        if !self.emit_const(ConstantValue::Float(f), expr.loc) {
          self.error(expr.loc, format!("failed to add const {f}"));
        }
      }
      LiteralValue::String(s) => {
        if !self.emit_const(ConstantValue::String(s), expr.loc) {
          self.error(expr.loc, "failed to add const string");
        }
      }
    };
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

  fn binary_register_expr(&mut self, expr: BinaryRegisterExpression) {
    let left = match expr.left {
      VarStorage::Stack(expr) => {
        self.emit_expr(*expr);
        (Storage::Stack, ShortAddr(0))
      }
      VarStorage::Ident(ident) => {
        if let Some(var) = self.resolve_ident(&ident, expr.loc) {
          match var {
            Lookup::Local(index) => (Storage::Local, ShortAddr(index)),
            Lookup::Global => {
              if let Some(var) = self.declare_global(ident) {
                (Storage::Global, ShortAddr(var))
              } else {
                self.error(expr.loc, "could not declare global ident");
                return;
              }
            }
          }
        } else {
          return;
        }
      }
    };

    let right = match expr.right {
      VarStorage::Stack(expr) => {
        self.emit_expr(*expr);
        (Storage::Stack, ShortAddr(0))
      }
      VarStorage::Ident(ident) => {
        if let Some(var) = self.resolve_ident(&ident, expr.loc) {
          match var {
            Lookup::Local(index) => (Storage::Local, ShortAddr(index)),
            Lookup::Global => {
              if let Some(var) = self.declare_global(ident) {
                (Storage::Global, ShortAddr(var))
              } else {
                self.error(expr.loc, "could not declare global ident");
                return;
              }
            }
          }
        } else {
          return;
        }
      }
    };

    match expr.op {
      BinaryOperator::Equal => self.emit((Opcode::Equal, (left, right)), expr.loc),
      BinaryOperator::NotEq => self.emit((Opcode::NotEqual, (left, right)), expr.loc),
      BinaryOperator::Less => self.emit((Opcode::Less, (left, right)), expr.loc),
      BinaryOperator::LessEq => self.emit((Opcode::LessEqual, (left, right)), expr.loc),
      BinaryOperator::Greater => self.emit((Opcode::Greater, (left, right)), expr.loc),
      BinaryOperator::GreaterEq => self.emit((Opcode::GreaterEqual, (left, right)), expr.loc),
      BinaryOperator::Add => self.emit((Opcode::Add, (left, right)), expr.loc),
      BinaryOperator::Sub => self.emit((Opcode::Sub, (left, right)), expr.loc),
      BinaryOperator::Mul => self.emit((Opcode::Mul, (left, right)), expr.loc),
      BinaryOperator::Div => self.emit((Opcode::Div, (left, right)), expr.loc),
      BinaryOperator::Mod => self.emit((Opcode::Rem, (left, right)), expr.loc),
    }
  }

  fn and_expr(&mut self, expr: AndExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_placeholder(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_jump_to_here(short_circuit, Opcode::And);
  }

  fn or_expr(&mut self, expr: OrExpression) {
    self.emit_expr(*expr.left);
    let short_circuit = self.emit_placeholder(expr.loc);
    self.emit_expr(*expr.right);
    self.patch_jump_to_here(short_circuit, Opcode::Or);
  }

  fn ident_expr(&mut self, expr: IdentExpression) {
    if let Some(lookup) = self.resolve_ident(&expr.ident, expr.loc) {
      let get = match lookup {
        Lookup::Local(index) => (Opcode::Load, (Storage::Local, LongAddr(index))),
        Lookup::Global => {
          if let Some(var) = self.declare_global(expr.ident) {
            (Opcode::Load, (Storage::Global, LongAddr(var)))
          } else {
            self.error(expr.loc, "could not declare global ident");
            return;
          }
        }
      };

      self.emit(get, expr.loc);
    }
  }

  fn assign_expr(&mut self, expr: AssignExpression) {
    match expr.lvalue {
      LValue::Ident(ident) => self.assign_ident(ident, expr.op, *expr.value, expr.loc),
      LValue::Index(index) => self.assign_index(index, expr.op, *expr.value, expr.loc),
      LValue::Member(member) => self.assign_member(member, expr.op, *expr.value, expr.loc),
    }
  }

  fn assign_ident(&mut self, ident: Ident, op: AssignOperator, value: Expression, loc: SourceLocation) {
    match op {
      AssignOperator::Assign => {
        if let Some(lookup) = self.resolve_ident(&ident, loc) {
          let set = match lookup {
            Lookup::Local(index) => (Opcode::Store, (Storage::Local, LongAddr(index))),
            Lookup::Global => {
              if let Some(var) = self.declare_global(ident) {
                (Opcode::Store, (Storage::Global, LongAddr(var)))
              } else {
                self.error(loc, "failed to declare global");
                return;
              }
            }
          };

          self.emit_expr(value);

          self.emit(set, loc);
        }
      }
      AssignOperator::Add => self.ident_op_assign(ident, Opcode::Add, value, loc),
      AssignOperator::Sub => self.ident_op_assign(ident, Opcode::Sub, value, loc),
      AssignOperator::Mul => self.ident_op_assign(ident, Opcode::Mul, value, loc),
      AssignOperator::Div => self.ident_op_assign(ident, Opcode::Div, value, loc),
      AssignOperator::Rem => self.ident_op_assign(ident, Opcode::Rem, value, loc),
    }
  }

  fn ident_op_assign(&mut self, ident: Ident, op: Opcode, value: Expression, loc: SourceLocation) {
    if let Some(lookup) = self.resolve_ident(&ident, loc) {
      let (set, get) = match lookup {
        Lookup::Local(index) => (
          (Opcode::Store, (Storage::Local, LongAddr(index))),
          (Opcode::Load, (Storage::Local, LongAddr(index))),
        ),
        Lookup::Global => {
          if let Some(global_index) = self.declare_global(ident) {
            (
              (Opcode::Store, (Storage::Global, LongAddr(global_index))),
              (Opcode::Load, (Storage::Global, LongAddr(global_index))),
            )
          } else {
            self.error(loc, "failed to declare global");
            return;
          }
        }
      };

      self.emit(get, loc);

      self.emit_expr(value);

      self.emit(op, loc);

      self.emit(set, loc);
    }
  }

  fn assign_index(&mut self, expr: IndexExpression, op: AssignOperator, value: Expression, loc: SourceLocation) {
    match op {
      AssignOperator::Assign => {
        self.emit_expr(*expr.indexable);
        self.emit_expr(*expr.index);
        self.emit_expr(value);
        self.emit(Opcode::AssignIndex, expr.loc);
      }
      AssignOperator::Add => self.index_op_assign(expr, Opcode::Add, value, loc),
      AssignOperator::Sub => self.index_op_assign(expr, Opcode::Sub, value, loc),
      AssignOperator::Mul => self.index_op_assign(expr, Opcode::Mul, value, loc),
      AssignOperator::Div => self.index_op_assign(expr, Opcode::Div, value, loc),
      AssignOperator::Rem => self.index_op_assign(expr, Opcode::Rem, value, loc),
    }
  }

  fn index_op_assign(&mut self, expr: IndexExpression, op: Opcode, value: Expression, loc: SourceLocation) {
    self.emit_expr(*expr.indexable);
    // [indexable]
    self.emit((Opcode::Load, (Storage::Stack, LongAddr(0))), expr.loc);
    // [indexable]
    // [indexable]
    self.emit_expr(*expr.index);
    // [index]
    // [indexable]
    // [indexable]
    self.emit((Opcode::Load, (Storage::Stack, LongAddr(0))), expr.loc);
    // [index]
    // [index]
    // [indexable]
    // [indexable]
    self.emit((Opcode::Swap, (ShortAddr(1), ShortAddr(2))), expr.loc);
    // [index]
    // [indexable]
    // [index]
    // [indexable]
    self.emit(Opcode::Index, expr.loc);
    // [idxval]
    // [index]
    // [indexable]
    self.emit_expr(value);
    // [value]
    // [idxval]
    // [index]
    // [indexable]
    self.emit(op, loc);
    // [output]
    // [index]
    // [indexable]
    self.emit(Opcode::AssignIndex, expr.loc);
    // [output]
  }

  fn assign_member(&mut self, expr: MemberAccessExpression, op: AssignOperator, value: Expression, loc: SourceLocation) {
    self.emit_expr(*expr.obj);
    if let Some(ident) = self.add_const_ident(expr.ident) {
      match op {
        AssignOperator::Assign => {
          self.emit_expr(value);
          self.emit((Opcode::AssignMember, ident), expr.loc);
        }
        AssignOperator::Add => self.member_op_assign(ident, Opcode::Add, value, loc),
        AssignOperator::Sub => self.member_op_assign(ident, Opcode::Sub, value, loc),
        AssignOperator::Mul => self.member_op_assign(ident, Opcode::Mul, value, loc),
        AssignOperator::Div => self.member_op_assign(ident, Opcode::Div, value, loc),
        AssignOperator::Rem => self.member_op_assign(ident, Opcode::Rem, value, loc),
      }
    } else {
      self.error(loc, "failed to add ident const");
    }
  }

  fn member_op_assign(&mut self, ident: usize, op: Opcode, value: Expression, loc: SourceLocation) {
    self.emit((Opcode::PeekMember, ident), loc);
    self.emit_expr(value);
    self.emit(op, loc);
    self.emit((Opcode::AssignMember, ident), loc);
  }

  fn call_expr(&mut self, expr: CallExpression) {
    let nargs = expr.args.len();
    self.emit_expr(*expr.callable);

    for arg in expr.args {
      self.emit_expr(arg)
    }

    self.emit((Opcode::Invoke, nargs), expr.loc);
    self.emit(Opcode::SwapPop, expr.loc);
  }

  fn vec_expr(&mut self, expr: VecExpression) {
    let sz = expr.items.len();
    for item in expr.items {
      self.emit_expr(item);
    }
    self.emit((Opcode::CreateVec, sz), expr.loc);
  }

  fn sized_vec_expr(&mut self, expr: SizedVecExpression) {
    self.emit_expr(*expr.item);
    self.emit((Opcode::CreateSizedVec, expr.size as usize), expr.loc);
  }

  fn dynamic_vec_expr(&mut self, expr: DynVecExpression) {
    self.emit_expr(*expr.item);
    self.emit_expr(*expr.size);
    self.emit(Opcode::CreateDynamicVec, expr.loc);
  }

  fn index_expr(&mut self, expr: IndexExpression) {
    self.emit_expr(*expr.indexable);
    self.emit_expr(*expr.index);
    self.emit(Opcode::Index, expr.loc);
  }

  fn is_expr(&mut self, expr: IsExpression) {
    self.emit_expr(*expr.left);
    self.emit_expr(*expr.right);
    self.emit(Opcode::Is, expr.loc);
  }

  fn struct_expr(&mut self, expr: StructExpression) {
    let nmem = expr.members.len();
    for (member, value) in expr.members {
      if let Some(ident) = self.add_const_ident(member) {
        self.emit_expr(value);
        self.emit((Opcode::Const, ident), expr.loc);
      } else {
        self.error(expr.loc, "failed to add struct ident");
      }
    }

    self.emit((Opcode::CreateStruct, nmem), expr.loc);
  }

  fn class_expr(&mut self, expr: ClassExpression) {
    if let Some(ident) = self.add_const_ident(expr.name) {
      self.emit_expr(*expr.self_type);
      self.emit((Opcode::CreateClass, ident), expr.loc);

      if let Some(initializer) = expr.initializer {
        if let Some((function, is_static)) = self.create_class_fn(Ident::new("<constructor>"), *initializer) {
          if is_static {
            self.emit_const(ConstantValue::Fn(function), expr.loc);
            self.emit(Opcode::InitializeConstructor, expr.loc);
          } else {
            self.error(expr.loc, "method was used as initializer somehow (logic error)");
          }
        } else {
          self.error(expr.loc, "unable to create initializer function for class");
        }
      }

      for (method_name, method) in expr.methods {
        if let Some((function, is_static)) = self.create_class_fn(method_name.clone(), method) {
          if let Some(ident) = self.add_const_ident(method_name) {
            if is_static {
              self.emit_const(ConstantValue::Fn(function), expr.loc);
              self.emit((Opcode::InitializeMember, ident), expr.loc);
            } else {
              self.emit_const(ConstantValue::Fn(function), expr.loc);
              self.emit((Opcode::InitializeMethod, ident), expr.loc);
            }
          } else {
            self.error(expr.loc, "could not add method name");
          }
        } else {
          self.error(expr.loc, format!("unable to create method {}", method_name.name));
        }
      }
    } else {
      self.error(expr.loc, "could not create class name");
    }
  }

  fn mod_expr(&mut self, expr: ModExpression) {
    if let Some(ident) = self.add_const_ident(expr.name) {
      self.emit((Opcode::CreateModule, ident), expr.loc);
      for (member, assign) in expr.items {
        if let Some(ident) = self.add_const_ident(member) {
          self.emit_expr(assign);
          self.define_scope(ident, expr.loc);
          self.emit(Opcode::Pop, expr.loc);
        } else {
          self.error(expr.loc, "failed to add mod member name");
        }
      }
      self.emit(Opcode::PopScope, expr.loc);
    } else {
      self.error(expr.loc, "failed to add mod name");
    }
  }

  fn lambda_expr(&mut self, expr: LambdaExpression) {
    let nargs = expr.params.len();
    self.emit_fn(None, expr.params, nargs, *expr.body, expr.loc);
  }

  fn closure_expr(&mut self, mut expr: ClosureExpression) {
    let num_captures = expr.captures.len();

    if num_captures == 0 {
      self.lambda_expr(LambdaExpression::from(expr));
    } else {
      self.new_scope(|this| {
        let mut params = Vec::with_capacity(num_captures);
        for member in expr.captures {
          params.push(member.ident.clone());
          this.ident_expr(member);
        }

        this.emit((Opcode::CreateVec, params.len()), expr.loc);

        let nargs = expr.params.len();

        expr.params.extend(params);

        this.emit_fn(None, expr.params, nargs, *expr.body, expr.loc);

        this.emit(Opcode::CreateClosure, expr.loc);
      });
    }
  }

  fn method_expr(&mut self, expr: MethodExpression) {
    let nargs = expr.params.len();
    self.emit_fn(Some(expr.name), expr.params, nargs, *expr.body, expr.loc);
  }

  fn member_access_expr(&mut self, expr: MemberAccessExpression) {
    if let Some(ident) = self.add_const_ident(expr.ident) {
      self.emit_expr(*expr.obj);
      self.emit((Opcode::LookupMember, ident), expr.loc);
    } else {
      self.error(expr.loc, "could not add member name");
    }
  }

  fn req_expr(&mut self, expr: ReqExpression) {
    self.emit_expr(*expr.file);
    self.emit(Opcode::Req, expr.loc);
  }

  fn scope_resolution_expr(&mut self, expr: ScopeResolutionExpression) {
    if let Some(ident) = self.add_const_ident(expr.ident) {
      self.emit_expr(*expr.obj);
      self.emit((Opcode::Resolve, ident), expr.loc);
    } else {
      self.error(expr.loc, "could not add resolve ident");
    }
  }

  /* Utility Functions */

  fn emit(&mut self, inst: impl TryIntoInstruction + Debug, loc: SourceLocation) {
    match inst.try_into_inst() {
      Ok(inst) => {
        self.current_ctx().write(inst, loc.line, loc.column);
      }
      Err(opcode) => self.error(loc, format!("failed to create instruction: {opcode:?}")),
    }
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
      Statement::Export(stmt) => self.export_stmt(stmt),
      Statement::Fn(stmt) => self.fn_stmt(stmt),
      Statement::For(stmt) => self.for_stmt(stmt),
      Statement::If(stmt) => self.if_stmt(stmt),
      Statement::Let(stmt) => self.let_stmt(stmt),
      Statement::Loop(stmt) => self.loop_stmt(stmt),
      Statement::Match(stmt) => self.match_stmt(stmt),
      Statement::Mod(stmt) => self.mod_stmt(stmt),
      Statement::Println(stmt) => self.println_stmt(stmt),
      Statement::Quack(stmt) => self.quack_stmt(stmt),
      Statement::Req(stmt) => self.req_stmt(stmt),
      Statement::Ret(stmt) => self.ret_stmt(stmt),
      Statement::Use(stmt) => self.use_stmt(stmt),
      Statement::While(stmt) => self.while_stmt(stmt),
      Statement::Breakpoint(loc) => self.breakpoint_stmt(loc),
      Statement::Expression(stmt) => self.expr_stmt(stmt),
    }
  }

  fn emit_expr(&mut self, expr: Expression) {
    #[cfg(feature = "visit-ast")]
    {
      println!("expr {}", expr);
    }
    match expr {
      Expression::And(expr) => self.and_expr(expr),
      Expression::Assign(expr) => self.assign_expr(expr),
      Expression::Binary(expr) => self.binary_expr(expr),
      Expression::BinaryRegister(expr) => self.binary_register_expr(expr),
      Expression::Call(expr) => self.call_expr(expr),
      Expression::Class(expr) => self.class_expr(expr),
      Expression::Closure(expr) => self.closure_expr(expr),
      Expression::Ident(expr) => self.ident_expr(expr),
      Expression::Index(expr) => self.index_expr(expr),
      Expression::Is(expr) => self.is_expr(expr),
      Expression::Lambda(expr) => self.lambda_expr(expr),
      Expression::Literal(expr) => self.literal_expr(expr),
      Expression::MemberAccess(expr) => self.member_access_expr(expr),
      Expression::Method(expr) => self.method_expr(expr),
      Expression::Mod(expr) => self.mod_expr(expr),
      Expression::Or(expr) => self.or_expr(expr),
      Expression::Req(expr) => self.req_expr(expr),
      Expression::ScopeResolution(expr) => self.scope_resolution_expr(expr),
      Expression::Struct(expr) => self.struct_expr(expr),
      Expression::Unary(expr) => self.unary_expr(expr),
      Expression::Vec(expr) => self.vec_expr(expr),
      Expression::SizedVec(expr) => self.sized_vec_expr(expr),
      Expression::DynVec(expr) => self.dynamic_vec_expr(expr),
    }
  }

  /// F returns the location of where continue should jump to
  fn emit_loop_block<F>(&mut self, start: usize, loc: SourceLocation, f: F)
  where
    F: FnOnce(&mut Self) -> Option<usize>,
  {
    let mut breaks = Vec::default();
    std::mem::swap(&mut breaks, &mut self.breaks);

    let mut continues = Vec::default();
    std::mem::swap(&mut continues, &mut self.continues);

    let loop_depth = self.loop_depth;
    self.loop_depth = self.scope_depth;

    let manual_continue = self.new_scope(|this| f(this));
    let continue_pos = manual_continue.unwrap_or(start);

    self.emit_loop(start, loc);

    std::mem::swap(&mut breaks, &mut self.breaks);
    self.patch_jumps_to_here(breaks);

    std::mem::swap(&mut continues, &mut self.continues);
    self.patch_jumps_to(continue_pos, continues);

    self.reduce_locals_to_depth(self.scope_depth, loc);

    self.loop_depth = loop_depth;
  }

  fn emit_fn(&mut self, name: Option<Ident>, args: Vec<Ident>, airity: usize, body: Statement, loc: SourceLocation) {
    let function = self.create_fn(name, args, airity, body, loc);
    self.emit_const(ConstantValue::Fn(function), loc);
  }

  fn emit_loop(&mut self, start: usize, loc: SourceLocation) {
    let backjump = self.current_instruction_count() - start;
    self.emit((Opcode::Loop, backjump), loc)
  }

  fn emit_const(&mut self, c: ConstantValue, loc: SourceLocation) -> bool {
    if let Some(c) = self.add_const(c) {
      self.emit((Opcode::Const, c), loc);
      true
    } else {
      false
    }
  }

  pub(crate) fn add_const(&mut self, c: ConstantValue) -> Option<usize> {
    let string = if let ConstantValue::String(string) = &c {
      if let Some(index) = self.cache.strings.get_by_right(string.as_str()) {
        return Some(*index);
      }
      Some(string.clone())
    } else {
      None
    };

    let index = self.cache.consts.len();
    self.cache.consts.push(c);

    if let Some(string) = string {
      self.cache.strings.insert(index, string);
    }

    Some(index)
  }

  /**
   * Emits a no op instruction and returns its index, the "jump" is made later with a patch
   */
  fn emit_placeholder(&mut self, loc: SourceLocation) -> usize {
    let offset = self.current_ctx().num_instructions();
    self.emit(Opcode::Unknown, loc);
    offset
  }

  fn patch_inst<D>(&mut self, index: usize, opcode: Opcode, data: D) -> bool
  where
    D: InstructionData,
  {
    self.current_ctx().replace_instruction(index, opcode, data)
  }

  fn patch_jump_to_here(&mut self, index: usize, opcode: Opcode) -> bool {
    let offset = self.current_ctx().num_instructions() - index;
    self.patch_inst(index, opcode, offset)
  }

  fn patch_jumps_to(&mut self, location: usize, jumps: Vec<usize>) -> bool {
    for inst_index in jumps {
      let offset = location - inst_index;
      if !self.patch_inst(inst_index, Opcode::Jump, offset) {
        return false;
      }
    }

    true
  }

  fn patch_jumps_to_here(&mut self, jumps: Vec<usize>) {
    for inst_index in jumps {
      if !self.patch_jump_to_here(inst_index, Opcode::Jump) {
        break;
      }
    }
  }

  fn add_const_ident(&mut self, ident: Ident) -> Option<usize> {
    self.add_const(ConstantValue::String(ident.name))
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

  fn current_instruction_count(&mut self) -> usize {
    self.current_ctx().num_instructions()
  }

  fn new_scope<R, F>(&mut self, f: F) -> R
  where
    F: FnOnce(&mut Self) -> R,
  {
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
      self.declare_global(ident)
    } else if self.declare_local(ident, loc) {
      Some(0)
    } else {
      self.error(loc, "tried to declare existing variable");
      None
    }
  }

  /**
   * Returns the index of the identifier name
   */
  fn declare_global(&mut self, ident: Ident) -> Option<usize> {
    self.add_const_ident(ident)
  }

  /**
   * Checks if the given identifier is present in all locals in the current scope
   *
   * If found but the depth is less, this is an ok declaration. Adds to the list of locals and returns true
   * Otherwise returns false
   */
  fn declare_local(&mut self, ident: Ident, loc: SourceLocation) -> bool {
    for local in self.locals.iter().rev() {
      // declared already in parent scope, break to redeclare in current scope
      if local.initialized && local.depth < self.scope_depth {
        break;
      }

      if ident.name == local.name {
        self.error(loc, "variable with the same name already declared");
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
    self.emit((Opcode::DefineGlobal, var), loc);
  }

  fn define_scope(&mut self, var: usize, loc: SourceLocation) {
    self.emit((Opcode::DefineScope, var), loc);
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
      self.error(loc, "could not define variable");
      false
    }
  }

  fn resolve_ident(&mut self, ident: &Ident, loc: SourceLocation) -> Option<Lookup> {
    if !self.locals.is_empty() {
      let mut index = self.locals.len() - 1;

      'search: for local in self.locals.iter().rev() {
        if local.depth < self.fn_depth {
          break 'search;
        }

        if ident.name == local.name {
          if !local.initialized {
            self.error(loc, "tried to use an undeclared identifier");
            return None;
          } else {
            return Some(Lookup::Local(index));
          }
        }

        index = index.saturating_sub(1);
      }
    }

    Some(Lookup::Global)
  }

  fn reduce_locals_to_depth(&mut self, depth: usize, loc: SourceLocation) {
    let count = self.num_locals_in_exclusive_depth(depth);

    self.locals.truncate(self.locals.len().saturating_sub(count));

    if count > 0 {
      self.emit((Opcode::PopN, count), loc);
    }
  }

  fn num_locals_in_exclusive_depth(&self, depth: usize) -> usize {
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

  fn create_class_fn(&mut self, ident: Ident, expr: Expression) -> Option<(FunctionConstant, bool)> {
    match expr {
      Expression::Method(m) => {
        let param_count = m.params.len();
        Some((self.create_fn(Some(ident), m.params, param_count, *m.body, m.loc), false))
      }
      Expression::Lambda(l) => {
        let param_count = l.params.len();
        Some((self.create_fn(Some(ident), l.params, param_count, *l.body, l.loc), true))
      }
      _ => None,
    }
  }

  fn create_fn(
    &mut self,
    ident: Option<Ident>,
    args: Vec<Ident>,
    airity: usize,
    body: Statement,
    loc: SourceLocation,
  ) -> FunctionConstant {
    self.function_id += 1;

    let mut locals = Vec::default();
    std::mem::swap(&mut locals, &mut self.locals);

    let parent_ctx = self.current_ctx_ptr();
    let prev_fn = self.current_fn.take();

    let reflection = Reflection::new(ident, parent_ctx.meta.file_id, parent_ctx.meta.source.clone());

    self.current_fn = Some(SmartPtr::new(Context::new(self.function_id, reflection)));

    self.new_scope(|this| {
      let prev_fn_depth = this.fn_depth;
      this.fn_depth = this.scope_depth;

      this.new_scope(|this| {
        for arg in args {
          if arg.global {
            this.error(loc, "parameter cannot be a global variable");
            continue;
          }

          if !this.declare_local(arg, loc) {
            continue;
          }

          if !this.define_local() {
            this.error(loc, sanity_check!())
          }
        }

        this.emit_stmt(body);

        this.reduce_locals_to_depth(this.fn_depth, loc);

        this.emit(Opcode::Ret, loc);

        let ctx = this.current_fn.take().unwrap();

        // restore
        this.current_fn = prev_fn;
        this.locals = locals;
        this.fn_depth = prev_fn_depth;

        FunctionConstant::new(airity, ctx)
      })
    })
  }

  fn error(&mut self, loc: SourceLocation, msg: impl ToString) {
    let msg = msg.to_string();
    self.errors.add(CompiletimeError {
      msg,
      file_display: self.file_id.map(FileDisplay::Id),
      line: loc.line,
      column: loc.column,
    });
  }
}

#[cfg(test)]
mod test;
