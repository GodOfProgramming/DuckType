use enum_map::Enum;

use super::Expression;
use crate::code::{
  ast::{AstExpression, AstGenerator, Ident, Precedence, SELF_IDENT},
  lex::Token,
  SourceLocation,
};

#[derive(Debug)]
pub enum AssignOperator {
  Assign,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
}

#[derive(Debug)]
pub struct AndExpression {
  pub left: Box<Expression>,
  pub right: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl AndExpression {
  pub(super) fn new(left: Expression, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      right: Box::new(right),
      loc,
    }
  }
}

impl AstExpression for AndExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let rule = AstGenerator::rule_for(&Token::And);

    if let Some(next_precedence) = rule.precedence.next() {
      let op_meta = ast.meta_at::<1>()?;
      let expr = ast.parse_precedence(next_precedence)?;
      Some(Expression::from(Self::new(left, expr, op_meta)))
    } else {
      ast.error::<1>(String::from("unable to retrieve precedence for and expr")); // this may not be an error?
      None
    }
  }
}

#[derive(Debug)]
pub enum LValue {
  Ident(Ident),
  Index(IndexExpression),
  Member(MemberAccessExpression),
}

impl TryFrom<Expression> for LValue {
  type Error = &'static str;
  fn try_from(value: Expression) -> Result<Self, Self::Error> {
    match value {
      Expression::Ident(expr) => Ok(Self::Ident(expr.ident)),
      Expression::Index(expr) => Ok(Self::Index(expr)),
      Expression::MemberAccess(expr) => Ok(Self::Member(expr)),
      _ => Err("invalid lvalue for assignment"),
    }
  }
}

#[derive(Debug)]
pub struct AssignExpression {
  pub lvalue: LValue,
  pub op: AssignOperator,
  pub value: Box<Expression>,

  pub loc: SourceLocation, // location of the =
}

impl AssignExpression {
  pub(super) fn new(lvalue: LValue, op: AssignOperator, value: Expression, loc: SourceLocation) -> Self {
    Self {
      lvalue,
      op,
      value: Box::new(value),
      loc,
    }
  }
}

impl AstExpression for AssignExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    if let Expression::Ident(expr) = &left {
      if expr.ident.name == SELF_IDENT {
        ast.error::<0>(String::from("cannot change the value of self"));
      }
    }

    let op = ast.previous()?;
    let op_meta = ast.meta_at::<1>()?;
    let value = ast.expression()?;

    let op = match op {
      Token::Equal => AssignOperator::Assign,
      Token::PlusEqual => AssignOperator::Add,
      Token::MinusEqual => AssignOperator::Sub,
      Token::AsteriskEqual => AssignOperator::Mul,
      Token::SlashEqual => AssignOperator::Div,
      Token::PercentEqual => AssignOperator::Rem,
      t => {
        ast.error::<1>(format!("invalid token {}", t));
        return None;
      }
    };

    match LValue::try_from(left) {
      Ok(lvalue) => Some(Expression::from(AssignExpression::new(lvalue, op, value, op_meta))),
      Err(e) => {
        ast.error::<0>(e);
        None
      }
    }
  }
}

#[derive(Debug, Clone, Copy, Enum)]
pub enum BinaryOperator {
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

#[derive(Debug)]
pub struct BinaryExpression {
  pub left: Box<Expression>,
  pub op: BinaryOperator,
  pub right: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl BinaryExpression {
  pub(crate) fn new(
    left: impl Into<Expression>,
    op: BinaryOperator,
    right: impl Into<Expression>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      left: Box::new(left.into()),
      op,
      right: Box::new(right.into()),
      loc,
    }
  }
}

impl AstExpression for BinaryExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    if let Some(operator_token) = ast.previous() {
      let op_meta = ast.meta_at::<1>()?;
      let op = match operator_token {
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEq,
        Token::Greater => BinaryOperator::Greater,
        Token::GreaterEqual => BinaryOperator::GreaterEq,
        Token::Less => BinaryOperator::Less,
        Token::LessEqual => BinaryOperator::LessEq,
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Sub,
        Token::Asterisk => BinaryOperator::Mul,
        Token::Slash => BinaryOperator::Div,
        Token::Percent => BinaryOperator::Mod,
        _ => {
          ast.error::<1>(String::from("invalid binary operator"));
          return None;
        }
      };

      let rule = AstGenerator::rule_for(&operator_token);

      if let Some(next_precedence) = rule.precedence.next() {
        let expr = ast.parse_precedence(next_precedence)?;
        Some(Expression::from(Self::new(left, op, expr, op_meta)))
      } else {
        ast.error::<1>(String::from(""));
        None
      }
    } else {
      ast.error::<2>(String::from("unexpected end of token stream"));
      None
    }
  }
}

#[derive(Debug)]
pub enum VarStorage {
  Stack(Box<Expression>),
  Ident(Ident),
}

impl VarStorage {
  pub(crate) fn stack(expr: impl Into<Expression>) -> Self {
    Self::Stack(Box::new(expr.into()))
  }

  pub(crate) fn ident(ident: impl Into<Ident>) -> Self {
    Self::Ident(ident.into())
  }
}

#[derive(Debug)]
pub struct BinaryRegisterExpression {
  pub left: VarStorage,
  pub op: BinaryOperator,
  pub right: VarStorage,

  pub loc: SourceLocation,
}

impl BinaryRegisterExpression {
  pub(crate) fn new(left: VarStorage, op: BinaryOperator, right: VarStorage, loc: SourceLocation) -> Self {
    Self { left, op, right, loc }
  }
}

#[derive(Debug)]
pub struct CallExpression {
  pub callable: Box<Expression>,
  pub args: Vec<Expression>,

  pub loc: SourceLocation,
}

impl CallExpression {
  pub(super) fn new(callable: Expression, args: Vec<Expression>, loc: SourceLocation) -> Self {
    Self {
      callable: Box::new(callable),
      args,
      loc,
    }
  }
}

impl AstExpression for CallExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let meta = ast.meta_at::<1>()?;
    let mut args = Vec::default();

    if let Some(token) = ast.current() {
      loop {
        if token == Token::RightParen {
          break;
        }

        args.push(ast.expression()?);

        if !ast.advance_if_matches(Token::Comma) {
          break;
        }
      }
    }

    if ast.consume(Token::RightParen, "expect ')' after arguments") {
      Some(Expression::from(CallExpression::new(left, args, meta)))
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct IndexExpression {
  pub indexable: Box<Expression>,
  pub index: Box<Expression>,

  pub loc: SourceLocation,
}

impl IndexExpression {
  pub(super) fn new(indexable: Expression, index: Expression, loc: SourceLocation) -> Self {
    Self {
      indexable: Box::new(indexable),
      index: Box::new(index),
      loc,
    }
  }
}

impl AstExpression for IndexExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let bracket_meta = ast.meta_at::<1>()?;

    let index = ast.expression()?;

    if ast.consume(Token::RightBracket, "expected ']' after expression") {
      Some(Expression::from(IndexExpression::new(left, index, bracket_meta)))
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct IsExpression {
  pub left: Box<Expression>,
  pub right: Box<Expression>,

  pub loc: SourceLocation,
}

impl IsExpression {
  fn new(left: Expression, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      right: Box::new(right),
      loc,
    }
  }
}

impl AstExpression for IsExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let loc = ast.meta_at::<0>()?;
    let right = ast.expression()?;
    Some(Expression::from(IsExpression::new(left, right, loc)))
  }
}

#[derive(Debug)]
pub struct MemberAccessExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub loc: SourceLocation,
}

impl MemberAccessExpression {
  fn new(obj: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      loc,
    }
  }
}

impl AstExpression for MemberAccessExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    if let Some(token) = ast.current() {
      if let Token::Identifier(member) = token {
        let ident_meta = ast.meta_at::<0>()?;

        ast.advance();

        Some(Expression::from(Self::new(left, Ident::new(member), ident_meta)))
      } else {
        ast.error::<1>(String::from("expected identifier"));
        None
      }
    } else {
      ast.error::<1>(String::from("unexpected end of file"));
      None
    }
  }
}

#[derive(Debug)]
pub struct OrExpression {
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub loc: SourceLocation, // location of the operator
}

impl OrExpression {
  pub(super) fn new(left: Expression, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      right: Box::new(right),
      loc,
    }
  }
}

impl AstExpression for OrExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let rule = AstGenerator::rule_for(&Token::Or);

    if let Some(next_precedence) = rule.precedence.next() {
      let op_meta = ast.meta_at::<1>()?;
      let expr = ast.parse_precedence(next_precedence)?;
      Some(Expression::from(Self::new(left, expr, op_meta)))
    } else {
      ast.error::<1>(String::from("unable to retrieve precedence for or expr")); // this may not be an error?
      None
    }
  }
}

#[derive(Debug)]
pub struct ReqExpression {
  pub file: Box<Expression>,
  pub loc: SourceLocation,
}

impl ReqExpression {
  pub(super) fn new(file: Expression, loc: SourceLocation) -> Self {
    Self {
      file: Box::new(file),
      loc,
    }
  }
}

impl AstExpression for ReqExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let loc = ast.meta_at::<1>()?;
    let expr = ast.expression()?;
    Some(Expression::from(Self::new(expr, loc)))
  }
}

#[derive(Debug)]
pub struct ScopeResolutionExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub loc: SourceLocation,
}

impl ScopeResolutionExpression {
  fn new(obj: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      loc,
    }
  }
}

impl AstExpression for ScopeResolutionExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    let ident_meta = ast.meta_at::<0>()?;
    if let Token::Identifier(member) = ast.expect_current()? {
      ast.advance();
      Some(Expression::from(Self::new(left, Ident::new(member), ident_meta)))
    } else {
      ast.error::<1>(String::from("expected identifier after scope"));
      None
    }
  }
}

#[derive(Debug)]
pub enum UnaryOperator {
  Not,
  Negate,
}

#[derive(Debug)]
pub struct UnaryExpression {
  pub op: UnaryOperator,
  pub expr: Box<Expression>,

  pub loc: SourceLocation, // location of the operator
}

impl UnaryExpression {
  pub(super) fn new(op: UnaryOperator, expr: Expression, loc: SourceLocation) -> Self {
    Self {
      op,
      expr: Box::new(expr),
      loc,
    }
  }
}

impl AstExpression for UnaryExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    if let Some(operator_token) = ast.previous() {
      let op_meta = ast.meta_at::<1>()?;
      let op = match operator_token {
        Token::Bang => UnaryOperator::Not,
        Token::Minus => UnaryOperator::Negate,
        _ => {
          ast.error::<1>(String::from("invalid unary operator"));
          return None;
        }
      };

      let expr = ast.parse_precedence(Precedence::Unary)?;
      Some(Expression::from(Self::new(op, expr, op_meta)))
    } else {
      ast.error::<1>(String::from("tried to make unary expression without operator"));
      None
    }
  }
}
