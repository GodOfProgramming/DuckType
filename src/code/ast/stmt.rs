use crate::code::SourceLocation;
#[cfg(feature = "visit-ast")]
use horrorshow::{html, prelude::*};
use std::fmt::{self, Display, Formatter};

use super::{Expression, Ident};

#[derive(Debug)]
pub enum Statement {
  Block(BlockStatement),
  Break(BreakStatement),
  Cont(ContStatement),
  Class(ClassStatement),
  DefaultConstructorRet(DefaultConstructorRet),
  Export(ExportStmt),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Let(LetStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Mod(ModStatement),
  Print(PrintStatement),
  Req(ReqStatement),
  Ret(RetStatement),
  Use(UseStatement),
  While(WhileStatement),
  Yield(YieldStatement),
  Expression(ExpressionStatement),
  Breakpoint(SourceLocation),
}

impl Statement {
  #[cfg(feature = "visit-ast")]
  pub(super) fn dump(&self, tmpl: &mut TemplateBuffer) {
    html! {
      div(class="node vertically-centered") {
        span(class="bubble", onclick="click_node(this)") : self.to_string();
        div(id="child", class="hidden") {
          |tmpl| self.dump_children(tmpl);
        }
      }
    }
    .render(tmpl);
  }

  #[cfg(feature = "visit-ast")]
  fn dump_children(&self, tmpl: &mut TemplateBuffer) {
    match self {
      Statement::Block(blk) => {
        for statement in &blk.statements {
          statement.dump(tmpl)
        }
      }
      Statement::Break(_) => (),
      Statement::Cont(_) => (),
      Statement::Class(c) => c.body.dump(tmpl),
      Statement::DefaultConstructorRet(_) => (),
      Statement::Export(_) => (),
      Statement::Fn(_) => (),
      Statement::For(_) => (),
      Statement::If(_) => (),
      Statement::Let(l) => {
        if let Some(expr) = &l.value {
          html! {
            div(class="children") {
              div(class="bubble child-node") : l.ident.to_string();
              div(class="bubble child-node") { |tmpl| expr.dump(tmpl); }
            }
          }
          .render(tmpl);
        } else {
          html! {
            div(class="bubble unary") : l.ident.to_string();
          }
          .render(tmpl);
        }
      }
      Statement::Loop(_) => (),
      Statement::Match(_) => (),
      Statement::Mod(m) => m.body.dump(tmpl),
      Statement::Print(_) => (),
      Statement::Req(_) => (),
      Statement::Ret(_) => (),
      Statement::Use(u) => {
        html! {
          span(class="bubble") : itertools::join(u.path.iter(), ".");
        }
        .render(tmpl);
      }
      Statement::While(_) => (),
      Statement::Yield(_) => (),
      Statement::Breakpoint(_) => (),
      Statement::Expression(e) => e.expr.dump(tmpl),
    }
  }
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Block(_) => write!(f, "block"),
      Self::Break(_) => write!(f, "break"),
      Self::Cont(_) => write!(f, "cont"),
      Self::Class(c) => write!(f, "class {}", c.ident.name),
      Self::DefaultConstructorRet(_) => write!(f, "default constructor ret"),
      Self::Export(_) => write!(f, "export"),
      Self::Fn(function) => write!(f, "fn {}", function.ident.name),
      Self::For(_) => write!(f, "for"),
      Self::If(_) => write!(f, "if"),
      Self::Let(_) => write!(f, "let"),
      Self::Loop(_) => write!(f, "loop"),
      Self::Match(_) => write!(f, "match"),
      Self::Mod(_) => write!(f, "mod"),
      Self::Print(_) => write!(f, "print"),
      Self::Req(_) => write!(f, "req"),
      Self::Ret(_) => write!(f, "ret"),
      Self::While(_) => write!(f, "while"),
      Self::Use(_) => write!(f, "use"),
      Self::Yield(_) => write!(f, "yield"),
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

impl From<ContStatement> for Statement {
  fn from(stmt: ContStatement) -> Self {
    Self::Cont(stmt)
  }
}

impl From<ClassStatement> for Statement {
  fn from(stmt: ClassStatement) -> Self {
    Self::Class(stmt)
  }
}

impl From<DefaultConstructorRet> for Statement {
  fn from(stmt: DefaultConstructorRet) -> Self {
    Self::DefaultConstructorRet(stmt)
  }
}

impl From<ExportStmt> for Statement {
  fn from(stmt: ExportStmt) -> Self {
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

impl From<PrintStatement> for Statement {
  fn from(stmt: PrintStatement) -> Self {
    Self::Print(stmt)
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

impl From<YieldStatement> for Statement {
  fn from(stmt: YieldStatement) -> Self {
    Self::Yield(stmt)
  }
}

impl From<ExpressionStatement> for Statement {
  fn from(stmt: ExpressionStatement) -> Self {
    Self::Expression(stmt)
  }
}

#[derive(Debug)]
pub struct BlockStatement {
  pub statements: Vec<Statement>,
  pub loc: SourceLocation,
}

impl BlockStatement {
  pub(super) fn new(statements: Vec<Statement>, loc: SourceLocation) -> Self {
    Self { statements, loc }
  }
}

#[derive(Debug)]
pub struct BreakStatement {
  pub loc: SourceLocation,
}

impl BreakStatement {
  pub(super) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

#[derive(Debug)]
pub struct ContStatement {
  pub loc: SourceLocation,
}

impl ContStatement {
  pub(super) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

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

#[derive(Debug)]
pub struct DefaultConstructorRet {
  pub loc: SourceLocation,
}

impl DefaultConstructorRet {
  pub(super) fn new(loc: SourceLocation) -> Self {
    Self { loc }
  }
}

#[derive(Debug)]
pub struct ExportStmt {
  pub expr: Expression,
  pub loc: SourceLocation,
}

impl ExportStmt {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self { expr, loc }
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
  pub(super) fn new(ident: Ident, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      ident,
      params,
      body: Box::new(body),
      loc,
    }
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

#[derive(Debug)]
pub struct IfStatement {
  pub comparison: Expression,
  pub block: Box<Statement>,
  pub else_block: Option<Box<Statement>>,
  pub loc: SourceLocation,
}

impl IfStatement {
  pub(super) fn new(comparison: Expression, block: Statement, else_block: Option<Statement>, loc: SourceLocation) -> Self {
    Self {
      comparison,
      block: Box::new(block),
      else_block: else_block.map(Box::new),
      loc,
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
  pub(super) fn new(ident: Ident, value: Option<Expression>, loc: SourceLocation) -> Self {
    Self { ident, value, loc }
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

#[derive(Debug)]
pub struct YieldStatement {
  pub loc: SourceLocation,
}

impl YieldStatement {
  pub(super) fn new(loc: SourceLocation) -> Self {
    Self { loc }
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
