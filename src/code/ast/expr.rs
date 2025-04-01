mod literals;
mod ops;

use std::fmt::{Display, Formatter};

pub use literals::*;
pub use ops::*;

#[derive(Debug)]
pub enum Expression {
  And(AndExpression),
  Assign(AssignExpression),
  Binary(BinaryExpression),
  BinaryRegister(BinaryRegisterExpression),
  Call(CallExpression),
  Class(ClassExpression),
  Closure(ClosureExpression),
  Ident(IdentExpression),
  Index(IndexExpression),
  Is(IsExpression),
  Lambda(LambdaExpression),
  Literal(LiteralExpression),
  MemberAccess(MemberAccessExpression),
  Method(MethodExpression),
  Mod(ModExpression),
  Or(OrExpression),
  Req(ReqExpression),
  ScopeResolution(ScopeResolutionExpression),
  Struct(StructExpression),
  Unary(UnaryExpression),
  Vec(VecExpression),
  SizedVec(SizedVecExpression),
  DynVec(DynVecExpression),
}

impl Expression {
  pub(super) fn dump(&self) {}
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Self::And(_) => write!(f, "and"),
      Self::Assign(_) => write!(f, "assign"),
      Self::Binary(_) => write!(f, "binary"),
      Self::BinaryRegister(_) => write!(f, "binary_reg"),
      Self::Call(_) => write!(f, "call"),
      Self::Class(_) => write!(f, "class"),
      Self::Closure(_) => write!(f, "closure"),
      Self::Ident(i) => write!(f, "ident {}", i.ident.name),
      Self::Index(_) => write!(f, "index"),
      Self::Is(_) => write!(f, "is"),
      Self::Lambda(_) => write!(f, "lambda"),
      Self::Literal(l) => write!(f, "literal {}", l.value),
      Self::MemberAccess(_) => write!(f, "member access"),
      Self::Method(_) => write!(f, "method"),
      Self::Mod(_) => write!(f, "mod"),
      Self::Or(_) => write!(f, "or"),
      Self::Req(_) => write!(f, "req"),
      Self::ScopeResolution(_) => write!(f, "scope resolution"),
      Self::Struct(_) => write!(f, "struct"),
      Self::Unary(u) => write!(f, "unary {:?}", u.op),
      Self::Vec(_) => write!(f, "vec"),
      Self::SizedVec(_) => write!(f, "sized vec"),
      Self::DynVec(_) => write!(f, "dynamic vec"),
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

impl From<BinaryRegisterExpression> for Expression {
  fn from(expr: BinaryRegisterExpression) -> Self {
    Self::BinaryRegister(expr)
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

impl From<IdentExpression> for Expression {
  fn from(expr: IdentExpression) -> Self {
    Self::Ident(expr)
  }
}

impl From<IsExpression> for Expression {
  fn from(expr: IsExpression) -> Self {
    Self::Is(expr)
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

impl From<VecExpression> for Expression {
  fn from(expr: VecExpression) -> Self {
    Self::Vec(expr)
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

impl From<SizedVecExpression> for Expression {
  fn from(expr: SizedVecExpression) -> Self {
    Self::SizedVec(expr)
  }
}

impl From<DynVecExpression> for Expression {
  fn from(expr: DynVecExpression) -> Self {
    Self::DynVec(expr)
  }
}
