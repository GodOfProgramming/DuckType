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
  Mod,
}

#[derive(Debug)]
pub struct Assignment {
  pub op: AssignOperator,
  pub value: Box<Expression>,
}

impl Assignment {
  fn new(op: AssignOperator, value: Expression) -> Self {
    Self {
      op,
      value: Box::new(value),
    }
  }
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
pub struct AssignExpression {
  pub ident: Ident,
  pub op: AssignOperator,
  pub value: Box<Expression>,

  pub loc: SourceLocation, // location of the =
}

impl AssignExpression {
  pub(super) fn new(ident: Ident, op: AssignOperator, value: Expression, loc: SourceLocation) -> Self {
    Self {
      ident,
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

    if let Expression::Ident(ident_expr) = left {
      let op = ast.previous()?;
      let op_meta = ast.meta_at::<1>()?;
      let value = ast.expression()?;

      let op = match op {
        Token::Equal => AssignOperator::Assign,
        Token::PlusEqual => AssignOperator::Add,
        Token::MinusEqual => AssignOperator::Sub,
        Token::AsteriskEqual => AssignOperator::Mul,
        Token::SlashEqual => AssignOperator::Div,
        Token::PercentEqual => AssignOperator::Mod,
        t => {
          ast.error::<1>(format!("invalid token {}", t));
          return None;
        }
      };

      Some(Expression::from(AssignExpression::new(ident_expr.ident, op, value, op_meta)))
    } else {
      ast.error::<1>(String::from("can only assign to variables"));
      None
    }
  }
}

#[derive(Debug)]
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
  pub(super) fn new(left: Expression, op: BinaryOperator, right: Expression, loc: SourceLocation) -> Self {
    Self {
      left: Box::new(left),
      op,
      right: Box::new(right),
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
pub struct MemberAccessExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub assignment: Option<Assignment>,
  pub loc: SourceLocation,
}

impl MemberAccessExpression {
  fn new(obj: Expression, ident: Ident, assignment: Option<Assignment>, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      assignment,
      loc,
    }
  }

  fn new_access(obj: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self::new(obj, ident, None, loc)
  }

  fn new_assignment(obj: Expression, ident: Ident, assignment: Assignment, loc: SourceLocation) -> Self {
    Self::new(obj, ident, Some(assignment), loc)
  }
}

impl AstExpression for MemberAccessExpression {
  fn infix(ast: &mut AstGenerator, left: Expression) -> Option<Expression> {
    if let Some(token) = ast.current() {
      if let Token::Identifier(member) = token {
        let ident_meta = ast.meta_at::<0>()?;

        ast.advance();

        if let Some(current) = ast.current() {
          match current {
            Token::Equal => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Assign, value),
                ident_meta,
              )))
            }
            Token::PlusEqual => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Add, value),
                ident_meta,
              )))
            }
            Token::MinusEqual => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Sub, value),
                ident_meta,
              )))
            }
            Token::AsteriskEqual => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Mul, value),
                ident_meta,
              )))
            }
            Token::SlashEqual => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Div, value),
                ident_meta,
              )))
            }
            Token::PercentEqual => {
              ast.advance();
              let value = ast.expression()?;
              Some(Expression::from(Self::new_assignment(
                left,
                Ident::new(member),
                Assignment::new(AssignOperator::Mod, value),
                ident_meta,
              )))
            }
            _ => Some(Expression::from(Self::new_access(left, Ident::new(member), ident_meta))),
          }
        } else {
          ast.error::<1>(String::from("expected token following member access"));
          None
        }
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
