use crate::{
  code::{
    ast::{AstExpression, AstGenerator, AstStatement, Expression, Ident, ReqExpression},
    lex::Token,
    SourceLocation,
  },
  UnwrapAnd,
};

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
      ast.error::<1>("can only export once per file");
    }
    ast.export_found = true;
    ast.meta_at::<1>().unwrap_and(|loc| {
      if let Some(current) = ast.current() {
        if let Some(expr) = ast.expression() {
          if !matches!(current, Token::Mod | Token::Class | Token::Fn)
            && !ast.consume(Token::Semicolon, "expected ';' after expression")
          {
            return;
          }

          ast.statements.push(Statement::from(Self::new(expr, loc)));
        }
      }
    });
  }
}

#[derive(Debug)]
pub struct PrintStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl PrintStatement {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for PrintStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<1>().unwrap_and(|loc| {
      if let Some(expr) = ast.expression() {
        if ast.consume(Token::Semicolon, "expected ';' after value") {
          ast.statements.push(Statement::from(Self::new(expr, loc)));
        }
      }
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
    ast.meta_at::<1>().unwrap_and(|loc| {
      ReqExpression::prefix(ast).unwrap_and(|expr| {
        if ast.consume(Token::As, "expected as after req") {
          if let Some(Token::Identifier(ident)) = ast.current() {
            ast.advance();
            if ast.consume(Token::Semicolon, "expected ';' after ident") {
              ast
                .statements
                .push(Statement::from(ReqStatement::new(expr, Ident::new(ident), loc)));
            }
          }
        }
      });
    });
  }
}
