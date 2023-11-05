use crate::{
  code::{
    ast::{AstExpression, AstGenerator, AstStatement, ClassExpression, Expression, Ident, ModExpression},
    lex::Token,
    SourceLocation,
  },
  UnwrapAnd,
};

use super::Statement;

#[derive(Debug)]
pub struct ClassStatement {
  pub ident: Ident,
  pub body: Expression,
  pub loc: SourceLocation,
}

impl ClassStatement {
  pub(super) fn new(ident: Ident, body: Expression, loc: SourceLocation) -> Self {
    Self { ident, body, loc }
  }
}

impl AstStatement for ClassStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<0>().unwrap_and(|class_loc| {
      if let Some(Token::Identifier(class_name)) = ast.current() {
        let ident = Ident::new_global(class_name);
        ClassExpression::prefix(ast).unwrap_and(|expr| {
          ast
            .statements
            .push(Statement::from(ClassStatement::new(ident, expr, class_loc)));
        });
      } else {
        ast.error::<0>(String::from("expected an identifier"));
      }
    });
  }
}

#[derive(Debug)]
pub struct FnStatement {
  pub ident: Ident,
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl FnStatement {
  pub(crate) fn new(ident: Ident, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      ident,
      params,
      body: Box::new(body),
      loc,
    }
  }
}

impl AstStatement for FnStatement {
  fn stmt(ast: &mut AstGenerator) {
    if let Some(stmt) = ast.parse_fn(true) {
      ast.statements.push(stmt);
    }
  }
}

#[derive(Debug)]
pub struct LetStatement {
  pub ident: Ident,
  pub value: Option<Expression>,

  pub loc: SourceLocation, // location of the let
}

impl LetStatement {
  pub(crate) fn new(ident: Ident, value: Option<Expression>, loc: SourceLocation) -> Self {
    Self { ident, value, loc }
  }
}

impl AstStatement for LetStatement {
  fn stmt(ast: &mut AstGenerator) {
    if let Some(declaration) = ast.declaration() {
      if !ast.consume(Token::Semicolon, "expected ';' after expression") {
        return;
      }

      ast.statements.push(Statement::from(declaration));
    }
  }
}

#[derive(Debug)]
pub struct ModStatement {
  pub ident: Ident,
  pub body: Expression,
  pub loc: SourceLocation,
}

impl ModStatement {
  pub(super) fn new(ident: Ident, body: Expression, loc: SourceLocation) -> Self {
    Self { ident, body, loc }
  }
}

impl AstStatement for ModStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<0>().unwrap_and(|mod_loc| {
      if let Some(Token::Identifier(mod_name)) = ast.current() {
        let ident = Ident::new_global(mod_name);
        ModExpression::prefix(ast).unwrap_and(|expr| {
          ast.statements.push(Statement::from(Self::new(ident, expr, mod_loc)));
        });
      } else {
        ast.error::<1>("expected ident after mod");
      }
    });
  }
}

#[derive(Debug)]
pub struct UseStatement {
  pub path: Vec<Ident>,
  pub loc: SourceLocation,
}

impl UseStatement {
  pub(super) fn new(path: Vec<Ident>, loc: SourceLocation) -> Self {
    Self { path, loc }
  }
}

impl AstStatement for UseStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<0>().unwrap_and(|loc| {
      let path = ast.resolve();
      if path.is_empty() {
        ast.error::<1>(String::from("use must have a type following it"));
      } else if ast.consume(Token::Semicolon, "expected ';' after use") {
        ast.statements.push(Statement::from(UseStatement::new(path, loc)));
      }
    });
  }
}
