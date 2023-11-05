use super::{AstGenerator, AstStatement, Expression, ExpressionStatement, Statement};
use crate::{
  code::{lex::Token, SourceLocation},
  UnwrapAnd,
};

#[derive(Debug)]
pub struct BlockStatement {
  pub statements: Vec<Statement>,
  pub loc: SourceLocation,
}

impl BlockStatement {
  pub(crate) fn new(statements: Vec<Statement>, loc: SourceLocation) -> Self {
    Self { statements, loc }
  }
}

impl AstStatement for BlockStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<1>().unwrap_and(|block_loc| {
      ast.normal_block(block_loc).unwrap_and(|block| {
        ast.statements.push(Statement::from(block));
      });
    });
  }
}

#[derive(Debug)]
pub struct BreakStatement {
  pub loc: SourceLocation,
}

impl BreakStatement {
  pub(crate) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

impl AstStatement for BreakStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.in_loop {
      ast.error::<1>(String::from("break statements can only be used within loops"));
      return;
    }

    if !ast.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    ast.meta_at::<2>().unwrap_and(|loc| {
      ast.statements.push(Statement::from(Self::new(loc)));
    });
  }
}

#[derive(Debug)]
pub struct ContStatement {
  pub loc: SourceLocation,
}

impl ContStatement {
  pub(crate) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

impl AstStatement for ContStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.in_loop {
      ast.error::<1>(String::from("cont statements can only be used within loops"));
      return;
    }

    if !ast.consume(Token::Semicolon, "expect ';' after statement") {
      return;
    }

    ast.meta_at::<2>().unwrap_and(|loc| {
      ast.statements.push(Statement::from(Self::new(loc)));
    });
  }
}

#[derive(Debug)]
pub struct DefaultConstructorRet {
  pub loc: SourceLocation,
}

impl DefaultConstructorRet {
  pub(crate) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

#[derive(Debug)]
pub struct ForStatement {
  pub initializer: Box<Statement>,
  pub comparison: Expression,
  pub increment: Expression,
  pub block: Box<Statement>,

  pub loc: SourceLocation,
}

impl ForStatement {
  pub(super) fn new(
    initializer: Statement,
    comparison: Expression,
    increment: Expression,
    block: Statement,
    loc: SourceLocation,
  ) -> Self {
    Self {
      initializer: Box::new(initializer),
      comparison,
      increment,
      block: Box::new(block),
      loc,
    }
  }
}

impl AstStatement for ForStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.meta_at::<1>().unwrap_and(|for_loc| {
      ast.meta_at::<0>().unwrap_and(|initializer_loc| {
        let initializer = if ast.advance_if_matches(Token::Let) {
          if let Some(declaration) = ast.declaration() {
            Statement::from(declaration)
          } else {
            return;
          }
        } else if let Some(expr) = ast.expression() {
          Statement::from(ExpressionStatement::new(expr, initializer_loc))
        } else {
          return;
        };

        if !ast.consume(Token::Semicolon, "expected ';' after expression") {
          return;
        }

        if let Some(comparison) = ast.expression() {
          if !ast.consume(Token::Semicolon, "expected ';' after comparison") {
            return;
          }

          if let Some(increment) = ast.expression() {
            if ast.consume(Token::LeftBrace, "expected '{' after increment") {
              let prev = ast.in_loop;
              ast.in_loop = true;
              ast.meta_at::<1>().unwrap_and(|block_loc| {
                if let Some(block) = ast.normal_block(block_loc) {
                  ast.statements.push(Statement::from(Self::new(
                    initializer,
                    comparison,
                    increment,
                    Statement::from(block),
                    for_loc,
                  )));
                }
              });
              ast.in_loop = prev;
            }
          } else {
            ast.error::<0>(String::from("expected increment after comparison"));
          }
        } else {
          ast.error::<0>(String::from("expected comparison after initializer"));
        }
      });
    });
  }
}

#[derive(Debug)]
pub struct IfStatement {
  pub comparison: Expression,
  pub block: Box<Statement>,
  pub else_block: Option<Box<Statement>>,
  pub loc: SourceLocation,
}

impl IfStatement {
  pub(crate) fn new(comparison: Expression, block: Statement, else_block: Option<Statement>, loc: SourceLocation) -> Self {
    Self {
      comparison,
      block: Box::new(block),
      else_block: else_block.map(Box::new),
      loc,
    }
  }
}

impl AstStatement for IfStatement {
  fn stmt(ast: &mut AstGenerator) {
    if let Some(if_stmt) = ast.branch() {
      ast.statements.push(Statement::from(if_stmt));
    }
  }
}

#[derive(Debug)]
pub struct LoopStatement {
  pub block: Box<Statement>,
  pub loc: SourceLocation,
}

impl LoopStatement {
  pub(super) fn new(block: Statement, loc: SourceLocation) -> Self {
    Self {
      block: Box::new(block),
      loc,
    }
  }
}

impl AstStatement for LoopStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.consume(Token::LeftBrace, "expect '{' after loop") {
      return;
    }

    let prev = ast.in_loop;
    ast.in_loop = true;

    if let Some(loc) = ast.meta_at::<1>() {
      if let Some(block) = ast.normal_block(loc.clone()) {
        ast
          .statements
          .push(Statement::from(LoopStatement::new(Statement::from(block), loc)));
      }
    } else {
      // sanity check
      ast.error::<0>(String::from("could not find original token"));
    }

    ast.in_loop = prev;
  }
}

#[derive(Debug)]
pub struct MatchStatement {
  pub expr: Expression,
  pub branches: Vec<(Expression, Statement)>,
  pub default: Option<Box<Statement>>,
  pub loc: SourceLocation,
}

impl MatchStatement {
  pub(super) fn new(
    expr: Expression,
    branches: Vec<(Expression, Statement)>,
    default: Option<Statement>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      expr,
      branches,
      default: default.map(Box::new),
      loc,
    }
  }
}

impl AstStatement for MatchStatement {
  fn stmt(ast: &mut AstGenerator) {
    if let Some(loc) = ast.meta_at::<1>() {
      if let Some(expr) = ast.expression() {
        if !ast.consume(Token::LeftBrace, "expected '{' after expression") {
          return;
        }

        let mut branches = Vec::default();

        while let Some(token) = ast.current() {
          if token == Token::RightBrace {
            break;
          }

          if let Some(condition) = ast.expression() {
            if !ast.consume(Token::Arrow, "expected => after expression") {
              break;
            }

            if let Some(eval_loc) = ast.meta_at::<0>() {
              let stmt = if ast.advance_if_matches(Token::LeftBrace) {
                if let Some(block) = ast.normal_block(eval_loc) {
                  Statement::from(block)
                } else {
                  break;
                }
              } else if let Some(eval_loc) = ast.meta_at::<0>() {
                if let Some(eval) = ast.expression() {
                  if !ast.consume(Token::Comma, "expected ',' after expression") {
                    break;
                  }
                  Statement::from(ExpressionStatement::new(eval, eval_loc))
                } else {
                  break;
                }
              } else {
                break;
              };

              branches.push((condition, stmt));
            } else {
              break; // error but need to restore statements
            }
          } else {
            // sanity check
            ast.error::<0>(String::from("could not find original token"));
          }
        }

        if !ast.consume(Token::RightBrace, "expected '}' after match") {
          return;
        }

        let default = if ast.advance_if_matches(Token::Else) && ast.advance_if_matches(Token::LeftBrace) {
          if let Some(else_loc) = ast.meta_at::<2>() {
            ast.normal_block(else_loc).map(Statement::from)
          } else {
            // sanity check
            ast.error::<0>(String::from("could not find original token"));
            None
          }
        } else {
          None
        };

        ast.statements.push(Statement::from(Self::new(expr, branches, default, loc)))
      }
    }
  }
}

#[derive(Debug)]
pub struct RetStatement {
  pub expr: Option<Expression>,
  pub loc: SourceLocation,
}

impl RetStatement {
  pub(super) fn new(expr: Option<Expression>, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for RetStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.returnable {
      ast.error::<1>("cannot return from this scope");
    }
    ast.meta_at::<1>().unwrap_and(|loc| {
      if let Some(current) = ast.current() {
        let expr = if current == Token::Semicolon {
          None
        } else if let Some(expr) = ast.expression() {
          Some(expr)
        } else {
          return;
        };

        if !ast.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }

        ast.statements.push(Statement::from(RetStatement::new(expr, loc)));
      }
    });
  }
}

#[derive(Debug)]
pub struct WhileStatement {
  pub comparison: Expression,
  pub block: Box<Statement>,
  pub loc: SourceLocation,
}

impl WhileStatement {
  pub(super) fn new(comparison: Expression, block: Statement, loc: SourceLocation) -> Self {
    Self {
      comparison,
      block: Box::new(block),
      loc,
    }
  }
}

impl AstStatement for WhileStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.expression().unwrap_and(|expr| {
      if !ast.consume(Token::LeftBrace, "expected '{' after expression") {
        return;
      }

      let prev = ast.in_loop;
      ast.in_loop = true;

      ast.meta_at::<1>().unwrap_and(|loc| {
        if let Some(block) = ast.normal_block(loc.clone()) {
          ast
            .statements
            .push(Statement::from(WhileStatement::new(expr, Statement::from(block), loc)));
        }
      });

      ast.in_loop = prev;
    });
  }
}
