use crate::{AstExpression, AstGenerator, AstStatement, Expression, Ident, ReqExpression};
use common::{errors::AstGenerationErrorMsg, util::UnwrapAnd, SourceLocation, Token};

use super::Statement;

#[derive(Debug)]
pub struct ExportStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl ExportStatement {
  pub(crate) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for ExportStatement {
  fn stmt(ast: &mut AstGenerator) {
    if ast.export_found {
      ast.error::<1>(AstGenerationErrorMsg::MultipleExports);
    }
    ast.export_found = true;
    ast.token_location::<1>().unwrap_and(|loc| {
      if let Some(current) = ast.current() {
        if let Some(expr) = ast.expression() {
          if !matches!(current, Token::Mod | Token::Class | Token::Fn) && !ast.consume(Token::Semicolon) {
            return;
          }

          ast.statements.push(Statement::from(Self::new(expr, loc)));
        }
      }
    });
  }
}

#[derive(Debug)]
pub struct PrintlnStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl PrintlnStatement {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for PrintlnStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.token_location::<1>().unwrap_and(|loc| {
      if let Some(expr) = ast.expression() {
        if ast.consume(Token::Semicolon) {
          ast.statements.push(Statement::from(Self::new(expr, loc)));
        }
      }
    });
  }
}

#[derive(Debug)]
pub struct QuackStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl QuackStatement {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for QuackStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.token_location::<0>().unwrap_and(|loc| {
      ast.expression().unwrap_and(|expr| {
        if ast.consume(Token::Semicolon) {
          ast.statements.push(Statement::from(Self::new(expr, loc)));
        }
      });
    });
  }
}

#[derive(Debug)]
pub struct ReqStatement {
  pub expr: Expression,
  pub ident: Ident,
  pub loc: SourceLocation,
}

impl ReqStatement {
  pub(super) fn new(expr: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self { expr, ident, loc }
  }
}

impl AstStatement for ReqStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.token_location::<1>().unwrap_and(|loc| {
      ReqExpression::prefix(ast).unwrap_and(|expr| {
        if ast.consume(Token::As) {
          if let Some(Token::Identifier(ident)) = ast.current() {
            ast.advance();
            if ast.consume(Token::Semicolon) {
              ast.statements.push(Statement::from(Self::new(expr, Ident::new(ident), loc)));
            }
          }
        }
      });
    });
  }
}
