use crate::util::UnwrapAnd;

use super::{
  ast::{
    BinaryExpression, BinaryOperator, BinaryRegisterExpression, Expression, LiteralExpression, LiteralValue, Statement,
    StorageLocation,
  },
  Ast,
};

pub fn optimize(ast: &mut Ast) {
  opt_stmts(&mut ast.statements);
}

fn opt_stmts(stmts: &mut [Statement]) {
  for stmt in stmts.iter_mut() {
    opt_stmt(stmt);
  }
}

fn opt_stmt(stmt: &mut Statement) {
  match stmt {
    Statement::Block(stmt) => opt_stmts(&mut stmt.statements),
    Statement::Break(_) => (),
    Statement::Cont(_) => (),
    Statement::Class(_) => (),
    Statement::Export(stmt) => opt_expr(&mut stmt.expr),
    Statement::Fn(stmt) => opt_stmt(&mut stmt.body),
    Statement::For(_) => (),
    Statement::If(_) => (),
    Statement::Let(stmt) => stmt.value.as_mut().unwrap_and(opt_expr),
    Statement::Loop(_) => (),
    Statement::Match(_) => (),
    Statement::Mod(_) => (),
    Statement::Println(stmt) => opt_expr(&mut stmt.expr),
    Statement::Quack(_) => (),
    Statement::Req(_) => (),
    Statement::Ret(stmt) => stmt.expr.as_mut().unwrap_and(opt_expr),
    Statement::Use(_) => (),
    Statement::While(_) => (),
    Statement::Expression(stmt) => opt_expr(&mut stmt.expr),
    Statement::Breakpoint(_) => (),
  }
}

fn opt_expr(expr: &mut Expression) {
  match expr {
    Expression::Empty => (),
    Expression::And(_) => (),
    Expression::Assign(expr) => opt_expr(&mut expr.value),
    Expression::Binary(_) => {
      let mut tmp = Expression::Empty;
      std::mem::swap(expr, &mut tmp);
      if let Expression::Binary(texpr) = tmp {
        *expr = opt_binary_expr(texpr);
      }
    }
    Expression::BinaryRegister(_) => (), // optimization
    Expression::Call(_) => (),
    Expression::Class(_) => (),
    Expression::Closure(expr) => {
      opt_stmt(&mut expr.body);
    }
    Expression::Ident(_) => (),
    Expression::Index(_) => (),
    Expression::Lambda(expr) => {
      opt_stmt(&mut expr.body);
    }
    Expression::Literal(_) => (),
    Expression::MemberAccess(_) => (),
    Expression::Method(_) => (),
    Expression::Mod(_) => (),
    Expression::Or(_) => (),
    Expression::Req(_) => (),
    Expression::ScopeResolution(_) => (),
    Expression::Struct(_) => (),
    Expression::Unary(_) => (),
    Expression::Vec(_) => (),
    Expression::VecWithSize(_) => (),
    Expression::VecWithDynamicSize(_) => (),
  };
}

fn opt_binary_expr(mut expr: BinaryExpression) -> Expression {
  opt_expr(&mut expr.left);
  opt_expr(&mut expr.right);

  match (&mut *expr.left, &mut *expr.right) {
    (Expression::Literal(left), Expression::Literal(right)) => {
      macro_rules! lit_expr {
        ($op:expr) => {
          Expression::from(LiteralExpression::new(LiteralValue::from($op), expr.loc))
        };
      }

      macro_rules! lit_expr_i32_f64 {
        ($l:ident $op:tt $r:ident) => {
          Expression::from(LiteralExpression::new(LiteralValue::from((*$l as f64) $op *$r), expr.loc))
        };
      }

      macro_rules! lit_expr_f64_i32 {
        ($l:ident $op:tt $r:ident) => {
          Expression::from(LiteralExpression::new(LiteralValue::from(*$l $op (*$r as f64)), expr.loc))
        };
      }

      match (&left.value, &right.value) {
        (LiteralValue::I32(l), LiteralValue::I32(r)) => match expr.op {
          BinaryOperator::Add => lit_expr!(l + r),
          BinaryOperator::Sub => lit_expr!(l - r),
          BinaryOperator::Mul => lit_expr!(l * r),
          BinaryOperator::Div => lit_expr!(l / r),
          BinaryOperator::Mod => lit_expr!(l % r),
          BinaryOperator::Equal => lit_expr!(l == r),
          BinaryOperator::NotEq => lit_expr!(l != r),
          BinaryOperator::Less => lit_expr!(l < r),
          BinaryOperator::LessEq => lit_expr!(l <= r),
          BinaryOperator::Greater => lit_expr!(l > r),
          BinaryOperator::GreaterEq => lit_expr!(l >= r),
        },
        (LiteralValue::I32(l), LiteralValue::F64(r)) => match expr.op {
          BinaryOperator::Add => lit_expr_i32_f64!(l + r),
          BinaryOperator::Sub => lit_expr_i32_f64!(l - r),
          BinaryOperator::Mul => lit_expr_i32_f64!(l * r),
          BinaryOperator::Div => lit_expr_i32_f64!(l / r),
          BinaryOperator::Mod => lit_expr_i32_f64!(l % r),
          BinaryOperator::Equal => lit_expr_i32_f64!(l == r),
          BinaryOperator::NotEq => lit_expr_i32_f64!(l != r),
          BinaryOperator::Less => lit_expr_i32_f64!(l < r),
          BinaryOperator::LessEq => lit_expr_i32_f64!(l <= r),
          BinaryOperator::Greater => lit_expr_i32_f64!(l > r),
          BinaryOperator::GreaterEq => lit_expr_i32_f64!(l >= r),
        },
        (LiteralValue::F64(l), LiteralValue::I32(r)) => match expr.op {
          BinaryOperator::Add => lit_expr_f64_i32!(l + r),
          BinaryOperator::Sub => lit_expr_f64_i32!(l - r),
          BinaryOperator::Mul => lit_expr_f64_i32!(l * r),
          BinaryOperator::Div => lit_expr_f64_i32!(l / r),
          BinaryOperator::Mod => lit_expr_f64_i32!(l % r),
          BinaryOperator::Equal => lit_expr_f64_i32!(l == r),
          BinaryOperator::NotEq => lit_expr_f64_i32!(l != r),
          BinaryOperator::Less => lit_expr_f64_i32!(l < r),
          BinaryOperator::LessEq => lit_expr_f64_i32!(l <= r),
          BinaryOperator::Greater => lit_expr_f64_i32!(l > r),
          BinaryOperator::GreaterEq => lit_expr_f64_i32!(l >= r),
        },
        (LiteralValue::F64(l), LiteralValue::F64(r)) => match expr.op {
          BinaryOperator::Add => lit_expr!(l + r),
          BinaryOperator::Sub => lit_expr!(l - r),
          BinaryOperator::Mul => lit_expr!(l * r),
          BinaryOperator::Div => lit_expr!(l / r),
          BinaryOperator::Mod => lit_expr!(l % r),
          BinaryOperator::Equal => lit_expr!(l == r),
          BinaryOperator::NotEq => lit_expr!(l != r),
          BinaryOperator::Less => lit_expr!(l < r),
          BinaryOperator::LessEq => lit_expr!(l <= r),
          BinaryOperator::Greater => lit_expr!(l > r),
          BinaryOperator::GreaterEq => lit_expr!(l >= r),
        },
        (LiteralValue::String(l), LiteralValue::String(r)) => match expr.op {
          BinaryOperator::Add => lit_expr!(format!("{l}{r}")),
          BinaryOperator::Equal => lit_expr!(l == r),
          BinaryOperator::NotEq => lit_expr!(l != r),
          BinaryOperator::Less => lit_expr!(l < r),
          BinaryOperator::LessEq => lit_expr!(l <= r),
          BinaryOperator::Greater => lit_expr!(l > r),
          BinaryOperator::GreaterEq => lit_expr!(l >= r),
          _ => Expression::from(expr),
        },
        (LiteralValue::String(s), literal) => match expr.op {
          BinaryOperator::Add => lit_expr!(format!("{s}{literal}")),
          BinaryOperator::Equal => lit_expr!(false),
          BinaryOperator::NotEq => lit_expr!(false),
          BinaryOperator::Less => lit_expr!(false),
          BinaryOperator::LessEq => lit_expr!(false),
          BinaryOperator::Greater => lit_expr!(false),
          BinaryOperator::GreaterEq => lit_expr!(false),
          _ => Expression::from(expr),
        },
        _ => Expression::from(expr),
      }
    }
    (Expression::Ident(l), Expression::Ident(r)) => Expression::from(BinaryRegisterExpression::new(
      StorageLocation::Ident(l.ident.clone()),
      expr.op,
      StorageLocation::Ident(r.ident.clone()),
      expr.loc,
    )),
    (Expression::Ident(l), Expression::Binary(_) | Expression::BinaryRegister(_)) => {
      Expression::from(BinaryRegisterExpression::new(
        StorageLocation::Ident(l.ident.clone()),
        expr.op,
        StorageLocation::Stack(expr.right),
        expr.loc,
      ))
    }
    (Expression::Binary(_) | Expression::BinaryRegister(_), Expression::Ident(r)) => {
      Expression::from(BinaryRegisterExpression::new(
        StorageLocation::Stack(expr.left),
        expr.op,
        StorageLocation::Ident(r.ident.clone()),
        expr.loc,
      ))
    }
    (_, _) => Expression::from(expr),
  }
}
