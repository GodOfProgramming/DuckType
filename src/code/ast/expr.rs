mod literals;
mod ops;

#[cfg(feature = "visit-ast")]
use horrorshow::{html, prelude::*};
use std::fmt::{Display, Formatter};

pub use literals::*;
pub use ops::*;

#[derive(Debug)]
pub enum Expression {
  And(AndExpression),
  Assign(AssignExpression),
  Binary(BinaryExpression),
  Call(CallExpression),
  Class(ClassExpression),
  Closure(ClosureExpression),
  Group(GroupExpression),
  Ident(IdentExpression),
  Index(IndexExpression),
  Lambda(LambdaExpression),
  List(ListExpression),
  Literal(LiteralExpression),
  MemberAccess(MemberAccessExpression),
  Method(MethodExpression),
  Mod(ModExpression),
  Or(OrExpression),
  Req(ReqExpression),
  ScopeResolution(ScopeResolutionExpression),
  Struct(StructExpression),
  Unary(UnaryExpression),
}

impl Expression {
  #[cfg(feature = "visit-ast")]
  pub(super) fn dump(&self, tmpl: &mut TemplateBuffer) {
    match self {
      Expression::And(_) => (),
      Expression::Assign(_) => (),
      Expression::Binary(_) => (),
      Expression::Call(_) => (),
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
      Expression::Closure(c) => c.body.dump(tmpl),
      Expression::Group(_) => (),
      Expression::Ident(_) => (),
      Expression::Index(_) => (),
      Expression::Lambda(l) => l.body.dump(tmpl),
      Expression::List(_) => (),
      Expression::Literal(l) => {
        html! {
          : l.value.to_string();
        }
        .render(tmpl);
      }
      Expression::MemberAccess(_) => (),
      Expression::Method(m) => m.body.dump(tmpl),
      Expression::Mod(_) => (),
      Expression::Or(_) => (),
      Expression::Req(_) => (),
      Expression::ScopeResolution(_) => (),
      Expression::Struct(_) => (),
      Expression::Unary(_) => (),
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Self::And(_) => write!(f, "and"),
      Self::Assign(_) => write!(f, "assign"),
      Self::Binary(_) => write!(f, "binary"),
      Self::Call(_) => write!(f, "call"),
      Self::Class(_) => write!(f, "class"),
      Self::Closure(_) => write!(f, "closure"),
      Self::Group(_) => write!(f, "group"),
      Self::Ident(i) => write!(f, "ident {}", i.ident.name),
      Self::Index(_) => write!(f, "index"),
      Self::Lambda(_) => write!(f, "lambda"),
      Self::List(_) => write!(f, "list"),
      Self::Literal(l) => write!(f, "literal {}", l.value),
      Self::MemberAccess(_) => write!(f, "member access"),
      Self::Method(_) => write!(f, "method"),
      Self::Mod(_) => write!(f, "mod"),
      Self::Or(_) => write!(f, "or"),
      Self::Req(_) => write!(f, "req"),
      Self::ScopeResolution(_) => write!(f, "scope resolution"),
      Self::Struct(_) => write!(f, "struct"),
      Self::Unary(u) => write!(f, "unary {:?}", u.op),
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

impl From<ScopeResolutionExpression> for Expression {
  fn from(expr: ScopeResolutionExpression) -> Self {
    Self::ScopeResolution(expr)
  }
}
