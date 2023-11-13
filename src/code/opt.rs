use crate::util::UnwrapAnd;

use super::{
  ast::{BinaryExpression, BinaryOperator, BinaryRegisterExpression, Expression, LiteralExpression, LiteralValue, Statement},
  Ast,
};

pub fn optimize(ast: &mut Ast) {
  for stmt in ast.statements.iter_mut() {
    opt_stmt(stmt);
  }
}

fn opt_stmt(stmt: &mut Statement) {
  match stmt {
    Statement::Block(_) => (),
    Statement::Break(_) => (),
    Statement::Cont(_) => (),
    Statement::Class(_) => (),
    Statement::Export(stmt) => opt_expr(&mut stmt.expr),
    Statement::Fn(_) => (),
    Statement::For(_) => (),
    Statement::If(_) => (),
    Statement::Let(_) => (),
    Statement::Loop(_) => (),
    Statement::Match(_) => (),
    Statement::Mod(_) => (),
    Statement::Println(stmt) => opt_expr(&mut stmt.expr),
    Statement::Quack(_) => (),
    Statement::Req(_) => (),
    Statement::Ret(_) => (),
    Statement::Use(_) => (),
    Statement::While(_) => (),
    Statement::Expression(stmt) => opt_expr(&mut stmt.expr),
    Statement::Breakpoint(_) => (),
  }
}

fn opt_expr(expr: &mut Expression) {
  let replacement = match expr {
    Expression::And(_) => None,
    Expression::Assign(_) => None,
    Expression::Binary(expr) => opt_binary_expr(expr),
    Expression::BinaryRegister(_) => None, // optimization
    Expression::Call(_) => None,
    Expression::Class(_) => None,
    Expression::Closure(_) => None,
    Expression::Ident(_) => None,
    Expression::Index(_) => None,
    Expression::Lambda(_) => None,
    Expression::Literal(_) => None,
    Expression::MemberAccess(_) => None,
    Expression::Method(_) => None,
    Expression::Mod(_) => None,
    Expression::Or(_) => None,
    Expression::Req(_) => None,
    Expression::ScopeResolution(_) => None,
    Expression::Struct(_) => None,
    Expression::Unary(_) => None,
    Expression::Vec(_) => None,
    Expression::VecWithSize(_) => None,
    Expression::VecWithDynamicSize(_) => None,
  };

  replacement.unwrap_and(|rep| {
    *expr = rep;
  });
}

fn opt_binary_expr(expr: &mut BinaryExpression) -> Option<Expression> {
  opt_expr(&mut expr.left);
  opt_expr(&mut expr.right);

  match (&mut *expr.left, &mut *expr.right) {
    (Expression::Literal(left), Expression::Literal(right)) => {
      macro_rules! lit_expr {
        ($op:expr) => {
          Some(Expression::from(LiteralExpression::new(
            LiteralValue::from($op),
            expr.loc,
          )))
        };
      }

      macro_rules! lit_expr_i32_f64 {
        ($l:ident $op:tt $r:ident) => {
          Some(Expression::from(LiteralExpression::new(LiteralValue::from((*$l as f64) $op *$r), expr.loc)))
        };
      }

      macro_rules! lit_expr_f64_i32 {
        ($l:ident $op:tt $r:ident) => {
          Some(Expression::from(LiteralExpression::new(LiteralValue::from(*$l $op (*$r as f64)), expr.loc)))
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
        _ => None,
      }
    }
    (Expression::Ident(l), Expression::Ident(r)) => {
      let l = &l.ident;
      let r = &r.ident;
      Some(Expression::from(BinaryRegisterExpression::new(
        l.clone(),
        expr.op,
        r.clone(),
        expr.loc,
      )))
    }
    (_, _) => None,
  }
}
