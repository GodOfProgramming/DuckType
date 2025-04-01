mod actions;
mod control;
mod decl;

use super::{AstGenerator, AstStatement, Expression};
use crate::code::{SourceLocation, lex::Token};
pub use actions::*;
pub use control::*;
pub use decl::*;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Statement {
  Block(BlockStatement),
  Break(BreakStatement),
  Class(ClassStatement),
  Export(ExportStatement),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Let(LetStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Mod(ModStatement),
  Next(NextStatement),
  Println(PrintlnStatement),
  Quack(QuackStatement),
  Req(ReqStatement),
  Ret(RetStatement),
  Use(UseStatement),
  While(WhileStatement),
  Expression(ExpressionStatement),
  Breakpoint(SourceLocation),
}

impl Statement {
  pub(super) fn dump(&self) {}

  fn dump_children(&self) {}
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Block(_) => write!(f, "block"),
      Self::Break(_) => write!(f, "break"),
      Self::Class(c) => write!(f, "class {}", c.ident.name),
      Self::Export(_) => write!(f, "export"),
      Self::Fn(function) => write!(f, "fn {}", function.ident.name),
      Self::For(_) => write!(f, "for"),
      Self::If(_) => write!(f, "if"),
      Self::Let(_) => write!(f, "let"),
      Self::Loop(_) => write!(f, "loop"),
      Self::Match(_) => write!(f, "match"),
      Self::Mod(_) => write!(f, "mod"),
      Self::Next(_) => write!(f, "next"),
      Self::Println(_) => write!(f, "println"),
      Self::Quack(_) => write!(f, "quack"),
      Self::Req(_) => write!(f, "req"),
      Self::Ret(_) => write!(f, "ret"),
      Self::While(_) => write!(f, "while"),
      Self::Use(_) => write!(f, "use"),
      Self::Breakpoint(_) => write!(f, "__breakpoint__"),
      Self::Expression(_) => write!(f, "expression"),
    }
  }
}

impl From<BlockStatement> for Statement {
  fn from(stmt: BlockStatement) -> Self {
    Self::Block(stmt)
  }
}

impl From<BreakStatement> for Statement {
  fn from(stmt: BreakStatement) -> Self {
    Self::Break(stmt)
  }
}

impl From<ClassStatement> for Statement {
  fn from(stmt: ClassStatement) -> Self {
    Self::Class(stmt)
  }
}

impl From<ExportStatement> for Statement {
  fn from(stmt: ExportStatement) -> Self {
    Self::Export(stmt)
  }
}

impl From<FnStatement> for Statement {
  fn from(stmt: FnStatement) -> Self {
    Self::Fn(stmt)
  }
}

impl From<ForStatement> for Statement {
  fn from(stmt: ForStatement) -> Self {
    Self::For(stmt)
  }
}

impl From<IfStatement> for Statement {
  fn from(stmt: IfStatement) -> Self {
    Self::If(stmt)
  }
}

impl From<LetStatement> for Statement {
  fn from(stmt: LetStatement) -> Self {
    Self::Let(stmt)
  }
}

impl From<LoopStatement> for Statement {
  fn from(stmt: LoopStatement) -> Self {
    Self::Loop(stmt)
  }
}

impl From<MatchStatement> for Statement {
  fn from(stmt: MatchStatement) -> Self {
    Self::Match(stmt)
  }
}

impl From<ModStatement> for Statement {
  fn from(stmt: ModStatement) -> Self {
    Self::Mod(stmt)
  }
}

impl From<NextStatement> for Statement {
  fn from(stmt: NextStatement) -> Self {
    Self::Next(stmt)
  }
}

impl From<PrintlnStatement> for Statement {
  fn from(stmt: PrintlnStatement) -> Self {
    Self::Println(stmt)
  }
}

impl From<QuackStatement> for Statement {
  fn from(stmt: QuackStatement) -> Self {
    Self::Quack(stmt)
  }
}

impl From<ReqStatement> for Statement {
  fn from(stmt: ReqStatement) -> Self {
    Self::Req(stmt)
  }
}

impl From<RetStatement> for Statement {
  fn from(stmt: RetStatement) -> Self {
    Self::Ret(stmt)
  }
}

impl From<UseStatement> for Statement {
  fn from(stmt: UseStatement) -> Self {
    Self::Use(stmt)
  }
}

impl From<WhileStatement> for Statement {
  fn from(stmt: WhileStatement) -> Self {
    Self::While(stmt)
  }
}

impl From<ExpressionStatement> for Statement {
  fn from(stmt: ExpressionStatement) -> Self {
    Self::Expression(stmt)
  }
}

#[derive(Debug)]
pub struct ExpressionStatement {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl ExpressionStatement {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
  }
}

impl AstStatement for ExpressionStatement {
  fn stmt(ast: &mut AstGenerator) {
    if let Some(loc) = ast.token_location::<0>() {
      if let Some(expr) = ast.expression() {
        if !ast.consume(Token::Semicolon) {
          return;
        }
        ast.statements.push(Statement::from(ExpressionStatement::new(expr, loc)));
      }
    }
  }
}
