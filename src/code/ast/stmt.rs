mod actions;
mod control;
mod decl;

use super::{AstGenerator, AstStatement, Expression};
use crate::code::{lex::Token, SourceLocation};
pub use actions::*;
pub use control::*;
pub use decl::*;
#[cfg(feature = "visit-ast")]
use horrorshow::{html, prelude::*};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Statement {
  Block(BlockStatement),
  Break(BreakStatement),
  Cont(ContStatement),
  Class(ClassStatement),
  DefaultConstructorRet(DefaultConstructorRet),
  Export(ExportStatement),
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
          span(class="bubble") : itertools::join(u.path.iter(), "::");
        }
        .render(tmpl);
      }
      Statement::While(_) => (),
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
    if let Some(loc) = ast.meta_at::<0>() {
      if let Some(expr) = ast.expression() {
        if !ast.consume(Token::Semicolon, "expected ';' after value") {
          return;
        }
        ast.statements.push(Statement::from(ExpressionStatement::new(expr, loc)));
      }
    }
  }
}
