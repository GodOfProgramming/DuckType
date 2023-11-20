use super::{
  ast::{
    AndExpression, AssignExpression, BinaryExpression, BinaryOperator, BinaryRegisterExpression, BlockStatement,
    CallExpression, ClassExpression, ClassStatement, ClosureExpression, DynVecExpression, ExportStatement, Expression,
    ExpressionStatement, FnStatement, ForStatement, Ident, IfStatement, IndexExpression, IsExpression, LambdaExpression,
    LetStatement, LiteralExpression, LiteralValue, LoopStatement, MatchStatement, MemberAccessExpression, MethodExpression,
    ModExpression, ModStatement, OrExpression, PrintlnStatement, QuackStatement, ReqExpression, ReqStatement, RetStatement,
    ScopeResolutionExpression, SizedVecExpression, Statement, StructExpression, UnaryExpression, VarStorage, VecExpression,
    WhileStatement,
  },
  Ast,
};

pub fn optimize(ast: Ast) -> Ast {
  ast.optimize()
}

trait Optimize {
  type Output;
  fn optimize(self) -> Self::Output;
}

impl Optimize for Ast {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    Ast {
      statements: self.statements.optimize(),
    }
  }
}

/* Statements */

impl Optimize for Statement {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    match self {
      Statement::Block(stmt) => stmt.optimize(),
      Statement::Class(stmt) => stmt.optimize(),
      Statement::Export(stmt) => stmt.optimize(),
      Statement::Fn(stmt) => stmt.optimize(),
      Statement::For(stmt) => stmt.optimize(),
      Statement::If(stmt) => stmt.optimize(),
      Statement::Let(stmt) => stmt.optimize(),
      Statement::Loop(stmt) => stmt.optimize(),
      Statement::Match(stmt) => stmt.optimize(),
      Statement::Mod(stmt) => stmt.optimize(),
      Statement::Println(stmt) => stmt.optimize(),
      Statement::Quack(stmt) => stmt.optimize(),
      Statement::Req(stmt) => stmt.optimize(),
      Statement::Ret(stmt) => stmt.optimize(),
      Statement::While(stmt) => stmt.optimize(),
      Statement::Expression(stmt) => stmt.optimize(),
      stmt => stmt,
    }
  }
}

impl Optimize for Box<Statement> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    Box::new((*self).optimize())
  }
}

impl Optimize for Option<Box<Statement>> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.map(Optimize::optimize)
  }
}

impl Optimize for Vec<Statement> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.into_iter().map(Optimize::optimize).collect()
  }
}

impl Optimize for BlockStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.statements = self.statements.optimize();
    self.into()
  }
}

impl Optimize for ClassStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for ExportStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for FnStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for ForStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.initializer = self.initializer.optimize();
    self.comparison = self.comparison.optimize();
    self.increment = self.increment.optimize();
    self.block = self.block.optimize();
    self.into()
  }
}

impl Optimize for IfStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.comparison = self.comparison.optimize();
    self.block = self.block.optimize();
    self.else_block = self.else_block.optimize();
    self.into()
  }
}

impl Optimize for LetStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.value = self.value.optimize();
    self.into()
  }
}

impl Optimize for LoopStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.block = self.block.optimize();
    self.into()
  }
}

impl Optimize for MatchStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.branches = self.branches.optimize();
    self.default = self.default.optimize();
    self.into()
  }
}

impl Optimize for ModStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for PrintlnStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for QuackStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for ReqStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for RetStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for WhileStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.comparison = self.comparison.optimize();
    self.block = self.block.optimize();
    self.into()
  }
}

impl Optimize for ExpressionStatement {
  type Output = Statement;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

/* Expressions */

impl Optimize for Expression {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    match self {
      Expression::And(expr) => expr.optimize(),
      Expression::Assign(expr) => expr.optimize(),
      Expression::Binary(expr) => expr.optimize(),
      Expression::Call(expr) => expr.optimize(),
      Expression::Class(expr) => expr.optimize(),
      Expression::Closure(expr) => expr.optimize(),
      Expression::Index(expr) => expr.optimize(),
      Expression::Is(expr) => expr.optimize(),
      Expression::Lambda(expr) => expr.optimize(),
      Expression::MemberAccess(expr) => expr.optimize(),
      Expression::Method(expr) => expr.optimize(),
      Expression::Mod(expr) => expr.optimize(),
      Expression::Or(expr) => expr.optimize(),
      Expression::Req(expr) => expr.optimize(),
      Expression::ScopeResolution(expr) => expr.optimize(),
      Expression::Struct(expr) => expr.optimize(),
      Expression::Unary(expr) => expr.optimize(),
      Expression::Vec(expr) => expr.optimize(),
      Expression::DynVec(expr) => expr.optimize(),
      expr => expr,
    }
  }
}

impl Optimize for Box<Expression> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    Box::new((*self).optimize())
  }
}

impl Optimize for Option<Expression> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.map(Optimize::optimize)
  }
}

impl Optimize for Option<Box<Expression>> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.map(Optimize::optimize)
  }
}

impl Optimize for Vec<Expression> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.into_iter().map(Optimize::optimize).collect()
  }
}

impl Optimize for AndExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.left = self.left.optimize();
    self.right = self.right.optimize();
    self.into()
  }
}

impl Optimize for AssignExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.value = self.value.optimize();
    self.into()
  }
}

impl Optimize for BinaryExpression {
  type Output = Expression;
  fn optimize(self) -> Self::Output {
    let left = self.left.optimize();
    let right = self.right.optimize();

    match (*left, *right) {
      (Expression::Literal(left), Expression::Literal(right)) => {
        macro_rules! lit_expr {
          ($op:expr) => {
            Expression::from(LiteralExpression::new(LiteralValue::from($op), self.loc))
          };
        }

        macro_rules! lit_expr_i32_f64 {
        ($l:ident $op:tt $r:ident) => {
          Expression::from(LiteralExpression::new(LiteralValue::from((*$l as f64) $op *$r), self.loc))
        };
      }

        macro_rules! lit_expr_f64_i32 {
        ($l:ident $op:tt $r:ident) => {
          Expression::from(LiteralExpression::new(LiteralValue::from(*$l $op (*$r as f64)), self.loc))
        };
      }

        match (&left.value, &right.value) {
          (LiteralValue::I32(l), LiteralValue::I32(r)) => match self.op {
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
          (LiteralValue::I32(l), LiteralValue::F64(r)) => match self.op {
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
          (LiteralValue::F64(l), LiteralValue::I32(r)) => match self.op {
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
          (LiteralValue::F64(l), LiteralValue::F64(r)) => match self.op {
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
          (LiteralValue::String(l), LiteralValue::String(r)) => match self.op {
            BinaryOperator::Add => lit_expr!(format!("{l}{r}")),
            BinaryOperator::Equal => lit_expr!(l == r),
            BinaryOperator::NotEq => lit_expr!(l != r),
            BinaryOperator::Less => lit_expr!(l < r),
            BinaryOperator::LessEq => lit_expr!(l <= r),
            BinaryOperator::Greater => lit_expr!(l > r),
            BinaryOperator::GreaterEq => lit_expr!(l >= r),
            _ => Expression::from(BinaryExpression::new(left, self.op, right, self.loc)),
          },
          (LiteralValue::String(s), literal) => match self.op {
            BinaryOperator::Add => lit_expr!(format!("{s}{literal}")),
            BinaryOperator::Equal => lit_expr!(false),
            BinaryOperator::NotEq => lit_expr!(false),
            BinaryOperator::Less => lit_expr!(false),
            BinaryOperator::LessEq => lit_expr!(false),
            BinaryOperator::Greater => lit_expr!(false),
            BinaryOperator::GreaterEq => lit_expr!(false),
            _ => Expression::from(BinaryExpression::new(left, self.op, right, self.loc)),
          },
          _ => Expression::from(BinaryExpression::new(left, self.op, right, self.loc)),
        }
      }
      (Expression::Ident(l), Expression::Ident(r)) => Expression::from(BinaryRegisterExpression::new(
        VarStorage::ident(l),
        self.op,
        VarStorage::ident(r),
        self.loc,
      )),
      (Expression::Ident(l), Expression::Binary(r)) => Expression::from(BinaryRegisterExpression::new(
        VarStorage::ident(l),
        self.op,
        VarStorage::stack(r),
        self.loc,
      )),
      (Expression::Ident(l), Expression::BinaryRegister(r)) => Expression::from(BinaryRegisterExpression::new(
        VarStorage::ident(l),
        self.op,
        VarStorage::stack(r),
        self.loc,
      )),
      (Expression::Binary(l), Expression::Ident(r)) => Expression::from(BinaryRegisterExpression::new(
        VarStorage::stack(l),
        self.op,
        VarStorage::ident(r),
        self.loc,
      )),
      (Expression::BinaryRegister(l), Expression::Ident(r)) => Expression::from(BinaryRegisterExpression::new(
        VarStorage::stack(l),
        self.op,
        VarStorage::ident(r),
        self.loc,
      )),
      (left, right) => Expression::from(BinaryExpression::new(left, self.op, right, self.loc)),
    }
  }
}

impl Optimize for CallExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.callable = self.callable.optimize();
    self.args = self.args.optimize();
    self.into()
  }
}

impl Optimize for ClassExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.self_type = self.self_type.optimize();
    self.initializer = self.initializer.optimize();
    self.methods = self.methods.optimize();
    self.into()
  }
}

impl Optimize for ClosureExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for IndexExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.indexable = self.indexable.optimize();
    self.index = self.index.optimize();
    self.into()
  }
}

impl Optimize for IsExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.left = self.left.optimize();
    self.right = self.right.optimize();
    self.into()
  }
}

impl Optimize for LambdaExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for MemberAccessExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.obj = self.obj.optimize();
    self.into()
  }
}

impl Optimize for MethodExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.body = self.body.optimize();
    self.into()
  }
}

impl Optimize for ModExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.items = self.items.optimize();
    self.into()
  }
}

impl Optimize for OrExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.left = self.left.optimize();
    self.right = self.right.optimize();
    self.into()
  }
}

impl Optimize for ReqExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.file = self.file.optimize();
    self.into()
  }
}

impl Optimize for ScopeResolutionExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.obj = self.obj.optimize();
    self.into()
  }
}

impl Optimize for StructExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.members = self.members.optimize();
    self.into()
  }
}

impl Optimize for UnaryExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.expr = self.expr.optimize();
    self.into()
  }
}

impl Optimize for VecExpression {
  type Output = Expression;
  fn optimize(mut self) -> Self::Output {
    self.items = self.items.optimize();
    self.into()
  }
}

impl Optimize for DynVecExpression {
  type Output = Expression;
  fn optimize(self) -> Self::Output {
    match *self.size {
      Expression::Literal(LiteralExpression {
        value: LiteralValue::I32(size),
        loc,
      }) => SizedVecExpression::new(*self.item, size, loc).into(),
      _ => self.into(),
    }
  }
}

/* Weird */

impl Optimize for Vec<(Ident, Expression)> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.into_iter().map(Optimize::optimize).collect()
  }
}

impl Optimize for (Ident, Expression) {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    (self.0, self.1.optimize())
  }
}

impl Optimize for Vec<(Expression, Statement)> {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    self.into_iter().map(Optimize::optimize).collect()
  }
}

impl Optimize for (Expression, Statement) {
  type Output = Self;
  fn optimize(self) -> Self::Output {
    (self.0.optimize(), self.1.optimize())
  }
}
