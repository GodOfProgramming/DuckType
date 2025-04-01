use crate::{code::ast::*, error::AstGenerationErrorMsg, util::UnwrapAnd};

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
    ast.token_location::<1>().unwrap_and(|block_loc| {
      ast.normal_block(block_loc).unwrap_and(|block| ast.add(block));
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
      ast.error::<1>(AstGenerationErrorMsg::InvalidBreakStatement);
      return;
    }

    if !ast.consume(Token::Semicolon) {
      return;
    }

    ast.token_location::<2>().unwrap_and(|loc| ast.add(Self::new(loc)));
  }
}

#[derive(Debug)]
pub struct NextStatement {
  pub loc: SourceLocation,
}

impl NextStatement {
  pub(crate) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

impl AstStatement for NextStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.in_loop {
      ast.error::<1>(AstGenerationErrorMsg::InvalidNextStatement);
      return;
    }

    if !ast.consume(Token::Semicolon) {
      return;
    }

    ast.token_location::<2>().unwrap_and(|loc| ast.add(Self::new(loc)));
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
    initializer: impl Into<Statement>,
    comparison: impl Into<Expression>,
    increment: impl Into<Expression>,
    block: impl Into<Statement>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      initializer: Box::new(initializer.into()),
      comparison: comparison.into(),
      increment: increment.into(),
      block: Box::new(block.into()),
      loc,
    }
  }
}

impl AstStatement for ForStatement {
  fn stmt(ast: &mut AstGenerator) {
    ast.token_location::<1>().unwrap_and(|for_loc| {
      ast.token_location::<0>().unwrap_and(|initializer_loc| {
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

        if !ast.consume(Token::Semicolon) {
          return;
        }

        if let Some(comparison) = ast.expression() {
          if !ast.consume(Token::Semicolon) {
            return;
          }

          if let Some(increment) = ast.expression() {
            if ast.consume(Token::LeftBrace) {
              let prev = ast.in_loop;
              ast.in_loop = true;
              ast.token_location::<1>().unwrap_and(|block_loc| {
                if let Some(block) = ast.normal_block(block_loc) {
                  ast.add(Self::new(initializer, comparison, increment, block, for_loc));
                }
              });
              ast.in_loop = prev;
            }
          } else {
            ast.error::<0>(AstGenerationErrorMsg::MissingExpression);
          }
        } else {
          ast.error::<0>(AstGenerationErrorMsg::MissingExpression);
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

  fn branch(ast: &mut AstGenerator) -> Option<Self> {
    let expr = ast.expression()?;
    if !ast.consume(Token::LeftBrace) {
      return None;
    }

    let block_loc = ast.token_location::<1>()?;
    let block = ast.normal_block(block_loc)?;
    let else_block = if ast.advance_if_matches(Token::Else) {
      let else_meta = ast.token_location::<1>()?;
      let token = ast.current()?;
      match token {
        Token::LeftBrace => {
          ast.advance();
          Some(Statement::from(ast.normal_block(else_meta)?))
        }
        Token::If => {
          ast.advance();
          Some(Statement::from(Self::branch(ast)?))
        }
        t => {
          ast.error::<0>(AstGenerationErrorMsg::UnexpectedToken(t));
          return None;
        }
      }
    } else {
      None
    };

    Some(IfStatement::new(expr, Statement::from(block), else_block, block_loc))
  }
}

impl AstStatement for IfStatement {
  fn stmt(ast: &mut AstGenerator) {
    Self::branch(ast).unwrap_and(|stmt| ast.add(stmt));
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
    if !ast.consume(Token::LeftBrace) {
      return;
    }

    let prev = ast.in_loop;
    ast.in_loop = true;

    ast.token_location::<1>().unwrap_and(|loc| {
      if let Some(block) = ast.normal_block(loc) {
        ast.add(LoopStatement::new(Statement::from(block), loc))
      }
    });

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
    if let Some(loc) = ast.token_location::<1>() {
      if let Some(expr) = ast.expression() {
        if !ast.consume(Token::LeftBrace) {
          return;
        }

        let mut branches = Vec::default();

        while let Some(token) = ast.current() {
          if token == Token::RightBrace {
            break;
          }

          if let Some(condition) = ast.expression() {
            if !ast.consume(Token::Arrow) {
              break;
            }

            if let Some(eval_loc) = ast.token_location::<0>() {
              let stmt = if ast.advance_if_matches(Token::LeftBrace) {
                if let Some(block) = ast.normal_block(eval_loc) {
                  Statement::from(block)
                } else {
                  break;
                }
              } else if let Some(eval_loc) = ast.token_location::<0>() {
                if let Some(eval) = ast.expression() {
                  if !ast.consume(Token::Comma) {
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
          }
        }

        if !ast.consume(Token::RightBrace) {
          return;
        }

        let default = if ast.advance_if_matches(Token::Else) && ast.advance_if_matches(Token::LeftBrace) {
          ast
            .token_location::<2>()
            .and_then(|else_loc| ast.normal_block(else_loc).map(Statement::from))
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
  pub(crate) fn new(expr: Option<Expression>, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for RetStatement {
  fn stmt(ast: &mut AstGenerator) {
    if !ast.returnable {
      ast.error::<1>(AstGenerationErrorMsg::InvalidRetStatement);
    }
    ast.token_location::<1>().unwrap_and(|loc| {
      if let Some(current) = ast.current() {
        let expr = if current == Token::Semicolon {
          None
        } else if let Some(expr) = ast.expression() {
          Some(expr)
        } else {
          return;
        };

        if !ast.consume(Token::Semicolon) {
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
      if !ast.consume(Token::LeftBrace) {
        return;
      }

      let prev = ast.in_loop;
      ast.in_loop = true;

      ast.token_location::<1>().unwrap_and(|loc| {
        if let Some(block) = ast.normal_block(loc) {
          ast
            .statements
            .push(Statement::from(WhileStatement::new(expr, Statement::from(block), loc)));
        }
      });

      ast.in_loop = prev;
    });
  }
}
