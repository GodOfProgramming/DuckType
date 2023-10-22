use crate::code::{
  ast::{AstExpression, AstGenerator, Ident},
  lex::{NumberToken, Token},
  ConstantValue, SourceLocation,
};

use super::Expression;

#[derive(Debug)]
pub struct GroupExpression {
  pub expr: Box<Expression>,

  pub loc: SourceLocation, // location of the left paren
}

impl GroupExpression {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self {
      expr: Box::new(expr),
      loc,
    }
  }
}

impl AstExpression for GroupExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let paren_meta = ast.meta_at::<1>()?;
    let expr = ast.expression()?;
    if ast.consume(Token::RightParen, "expected ')' after expression") {
      Some(Expression::from(GroupExpression::new(expr, paren_meta)))
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct IdentExpression {
  pub ident: Ident,

  pub loc: SourceLocation, // location of the identifier
}

impl IdentExpression {
  pub(crate) fn new(ident: Ident, loc: SourceLocation) -> Self {
    Self { ident, loc }
  }
}

impl AstExpression for IdentExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    if let Some(ident_token) = ast.previous() {
      if let Token::Identifier(ident_name) = ident_token {
        Some(Expression::from(IdentExpression::new(
          Ident::new(ident_name),
          ast.meta_at::<1>()?,
        )))
      } else {
        ast.error::<2>(String::from("variable name is not an identifier"));
        None
      }
    } else {
      ast.error::<2>(String::from("unexpected end of token stream"));
      None
    }
  }
}

#[derive(Debug)]
pub struct ListExpression {
  pub items: Vec<Expression>,
  pub loc: SourceLocation,
}

impl ListExpression {
  pub(super) fn new(items: Vec<Expression>, loc: SourceLocation) -> Self {
    Self { items, loc }
  }
}

impl AstExpression for ListExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let bracket_meta = ast.meta_at::<1>()?;
    let mut items = Vec::default();

    if let Some(token) = ast.current() {
      if token != Token::RightBracket {
        loop {
          items.push(ast.expression()?);
          if !ast.advance_if_matches(Token::Comma) {
            break;
          }
        }
      }
    }

    if ast.consume(Token::RightBracket, "expect ']' after arguments") {
      let list = ListExpression::new(items, bracket_meta);
      if ast.advance_if_matches(Token::Pipe) {
        ast.closure_expr(Token::Pipe, list)
      } else {
        Some(Expression::from(list))
      }
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct LiteralExpression {
  pub value: ConstantValue,

  pub loc: SourceLocation, // location of the literal
}

impl LiteralExpression {
  pub(super) fn new(value: ConstantValue, loc: SourceLocation) -> Self {
    Self { value, loc }
  }
}

impl AstExpression for LiteralExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<super::Expression> {
    let mut expr = None;

    if let Some(prev) = ast.previous() {
      match prev {
        Token::Nil => {
          expr = Some(Expression::from(Self::new(ConstantValue::Nil, ast.meta_at::<1>()?)));
        }
        Token::True => {
          expr = Some(Expression::from(Self::new(ConstantValue::Bool(true), ast.meta_at::<1>()?)));
        }
        Token::False => {
          expr = Some(Expression::from(Self::new(ConstantValue::Bool(false), ast.meta_at::<1>()?)));
        }
        Token::String(s) => expr = Some(Expression::from(Self::new(ConstantValue::String(s), ast.meta_at::<1>()?))),
        Token::Number(n) => {
          expr = Some(Expression::from(Self::new(
            match n {
              NumberToken::I32(i) => ConstantValue::Integer(i),
              NumberToken::F64(f) => ConstantValue::Float(f),
            },
            ast.meta_at::<1>()?,
          )))
        }
        _ => {
          ast.error::<1>(String::from("sanity check, invalid literal, very bad logic error"));
        }
      }
    } else {
      ast.error::<1>(String::from("sanity check, no previous token, very bad logic error"));
    }

    expr
  }
}
