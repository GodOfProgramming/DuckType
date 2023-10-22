mod literals;
mod ops;

#[cfg(feature = "visit-ast")]
use horrorshow::{html, prelude::*};
use std::fmt::{Display, Formatter};

use super::{Ident, Statement};
use crate::code::SourceLocation;
pub use literals::*;
pub use ops::*;

#[derive(Debug)]
pub enum Expression {
  Literal(LiteralExpression),
  Unary(UnaryExpression),
  Binary(BinaryExpression),
  And(AndExpression),
  Or(OrExpression),
  Group(GroupExpression),
  Ident(IdentExpression),
  Assign(AssignExpression),
  Call(CallExpression),
  List(ListExpression),
  Index(IndexExpression),
  Struct(StructExpression),
  Class(ClassExpression),
  Mod(ModExpression),
  MemberAccess(MemberAccessExpression),
  MemberAssign(MemberAssignExpression),
  Lambda(LambdaExpression),
  Closure(ClosureExpression),
  Method(MethodExpression),
  Req(ReqExpression),
}

impl Expression {
  #[cfg(feature = "visit-ast")]
  pub(super) fn dump(&self, tmpl: &mut TemplateBuffer) {
    match self {
      Expression::Literal(l) => {
        html! {
          : l.value.to_string();
        }
        .render(tmpl);
      }
      Expression::Unary(_) => (),
      Expression::Binary(_) => (),
      Expression::And(_) => (),
      Expression::Or(_) => (),
      Expression::Group(_) => (),
      Expression::Ident(_) => (),
      Expression::Assign(_) => (),
      Expression::Call(_) => (),
      Expression::List(_) => (),
      Expression::Index(_) => (),
      Expression::Struct(_) => (),
      Expression::Class(c) => {
        if let Some(init) = &c.initializer {
          html! {
            div(class="children") {
              div(class="vertically-centered") {
                div(class="bubble") : "new";
                |tmpl| init.dump(tmpl);
              }
              @ for (ident, method) in c.methods.iter() {
                div(class="vertically-centered") {
                  div(class="bubble") : format_args!("fn {}", ident.to_string());
                  |tmpl| method.dump(tmpl);
                }
              }
            }
          }
          .render(tmpl);
        } else {
          html! {
            div(class="children") {
              @ for (ident, method) in c.methods.iter() {
                div(class="vertically-centered") {
                  div(class="bubble") : format_args!("fn {}", ident.to_string());
                  |tmpl| method.dump(tmpl);
                }
              }
            }
          }
          .render(tmpl);
        }
      }
      Expression::Mod(_) => (),
      Expression::MemberAccess(_) => (),
      Expression::MemberAssign(_) => (),
      Expression::Lambda(l) => l.body.dump(tmpl),
      Expression::Closure(c) => c.body.dump(tmpl),
      Expression::Method(m) => m.body.dump(tmpl),
      Expression::Req(_) => (),
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Self::Literal(l) => write!(f, "literal {}", l.value),
      Self::Unary(u) => write!(f, "unary {:?}", u.op),
      Self::Binary(_) => write!(f, "binary"),
      Self::And(_) => write!(f, "and"),
      Self::Or(_) => write!(f, "or"),
      Self::Group(_) => write!(f, "group"),
      Self::Ident(i) => write!(f, "ident {}", i.ident.name),
      Self::Assign(_) => write!(f, "assign"),
      Self::MemberAccess(_) => write!(f, "member access"),
      Self::MemberAssign(_) => write!(f, "member assign"),
      Self::Call(_) => write!(f, "call"),
      Self::List(_) => write!(f, "list"),
      Self::Index(_) => write!(f, "index"),
      Self::Struct(_) => write!(f, "struct"),
      Self::Class(_) => write!(f, "class"),
      Self::Mod(_) => write!(f, "mod"),
      Self::Lambda(_) => write!(f, "lambda"),
      Self::Closure(_) => write!(f, "closure"),
      Self::Method(_) => write!(f, "method"),
      Self::Req(_) => write!(f, "req"),
    }
  }
}

impl From<LiteralExpression> for Expression {
  fn from(expr: LiteralExpression) -> Self {
    Self::Literal(expr)
  }
}

impl From<UnaryExpression> for Expression {
  fn from(expr: UnaryExpression) -> Self {
    Self::Unary(expr)
  }
}

impl From<BinaryExpression> for Expression {
  fn from(expr: BinaryExpression) -> Self {
    Self::Binary(expr)
  }
}

impl From<AndExpression> for Expression {
  fn from(expr: AndExpression) -> Self {
    Self::And(expr)
  }
}

impl From<OrExpression> for Expression {
  fn from(expr: OrExpression) -> Self {
    Self::Or(expr)
  }
}

impl From<GroupExpression> for Expression {
  fn from(expr: GroupExpression) -> Self {
    Self::Group(expr)
  }
}

impl From<IdentExpression> for Expression {
  fn from(expr: IdentExpression) -> Self {
    Self::Ident(expr)
  }
}

impl From<AssignExpression> for Expression {
  fn from(expr: AssignExpression) -> Self {
    Self::Assign(expr)
  }
}

impl From<CallExpression> for Expression {
  fn from(expr: CallExpression) -> Self {
    Self::Call(expr)
  }
}

impl From<ListExpression> for Expression {
  fn from(expr: ListExpression) -> Self {
    Self::List(expr)
  }
}

impl From<IndexExpression> for Expression {
  fn from(expr: IndexExpression) -> Self {
    Self::Index(expr)
  }
}

impl From<StructExpression> for Expression {
  fn from(expr: StructExpression) -> Self {
    Self::Struct(expr)
  }
}

impl From<ClassExpression> for Expression {
  fn from(expr: ClassExpression) -> Self {
    Self::Class(expr)
  }
}

impl From<ModExpression> for Expression {
  fn from(expr: ModExpression) -> Self {
    Self::Mod(expr)
  }
}

impl From<MemberAccessExpression> for Expression {
  fn from(expr: MemberAccessExpression) -> Self {
    Self::MemberAccess(expr)
  }
}

impl From<MemberAssignExpression> for Expression {
  fn from(expr: MemberAssignExpression) -> Self {
    Self::MemberAssign(expr)
  }
}

impl From<LambdaExpression> for Expression {
  fn from(expr: LambdaExpression) -> Self {
    Self::Lambda(expr)
  }
}

impl From<ClosureExpression> for Expression {
  fn from(expr: ClosureExpression) -> Self {
    Self::Closure(expr)
  }
}

impl From<MethodExpression> for Expression {
  fn from(expr: MethodExpression) -> Self {
    Self::Method(expr)
  }
}

impl From<ReqExpression> for Expression {
  fn from(expr: ReqExpression) -> Self {
    Self::Req(expr)
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

#[derive(Debug)]
pub struct MemberAccessExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub loc: SourceLocation,
}

impl MemberAccessExpression {
  pub(super) fn new(obj: Expression, ident: Ident, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      loc,
    }
  }
}

#[derive(Debug)]
pub struct MemberAssignExpression {
  pub obj: Box<Expression>,
  pub ident: Ident,
  pub op: AssignOperator,
  pub value: Box<Expression>,
  pub loc: SourceLocation,
}

impl MemberAssignExpression {
  pub(super) fn new(obj: Expression, ident: Ident, op: AssignOperator, value: Expression, loc: SourceLocation) -> Self {
    Self {
      obj: Box::new(obj),
      ident,
      op,
      value: Box::new(value),
      loc,
    }
  }
}

#[derive(Debug)]
pub struct StructExpression {
  pub members: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl StructExpression {
  pub(super) fn new(members: Vec<(Ident, Expression)>, loc: SourceLocation) -> Self {
    Self { members, loc }
  }
}

#[derive(Debug)]
pub struct ClassExpression {
  pub name: Option<Ident>,
  pub initializer: Option<Box<Expression>>,
  pub methods: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl ClassExpression {
  pub(super) fn new(
    name: Option<Ident>,
    initializer: Option<Expression>,
    methods: Vec<(Ident, Expression)>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      name,
      initializer: initializer.map(Box::new),
      methods,
      loc,
    }
  }
}

#[derive(Debug)]
pub struct ModExpression {
  pub name: Option<Ident>,
  pub items: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl ModExpression {
  pub(super) fn new(name: Option<Ident>, items: Vec<(Ident, Expression)>, loc: SourceLocation) -> Self {
    Self { name, items, loc }
  }
}

#[derive(Debug)]
pub struct LambdaExpression {
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl LambdaExpression {
  pub(super) fn new(params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      params,
      body: Box::new(body),
      loc,
    }
  }
}

impl From<ClosureExpression> for LambdaExpression {
  fn from(expr: ClosureExpression) -> Self {
    Self {
      params: expr.params,
      body: expr.body,
      loc: expr.loc,
    }
  }
}

#[derive(Debug)]
pub struct ClosureExpression {
  pub captures: Vec<IdentExpression>,
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl ClosureExpression {
  pub(super) fn new(captures: Vec<IdentExpression>, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      captures,
      params,
      body: Box::new(body),
      loc,
    }
  }
}

#[derive(Debug)]
pub struct MethodExpression {
  pub name: Ident,
  pub params: Vec<Ident>,
  pub body: Box<Statement>,
  pub loc: SourceLocation,
}

impl MethodExpression {
  pub(super) fn new(name: Ident, params: Vec<Ident>, body: Statement, loc: SourceLocation) -> Self {
    Self {
      name,
      params,
      body: Box::new(body),
      loc,
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
