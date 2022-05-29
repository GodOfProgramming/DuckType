use crate::types::Value;

pub struct Ast {
  pub statements: Vec<Statement>,
}

pub struct Ident {
  pub name: String,
}

pub enum Statement {
  Break(BreakStatement),
  Cont(ContStatement),
  End(EndStatement),
  Fn(FnStatement),
  For(ForStatement),
  If(IfStatement),
  Block(BlockStatement),
  Let(LetStatement),
  Load(LoadStatement),
  Loop(LoopStatement),
  Match(MatchStatement),
  Print(PrintStatement),
  Ret(RetStatement),
  While(WhileStatement),
}

pub struct BreakStatement {
  pub jump_index: usize,
}

pub struct ContStatement {
  pub loop_index: usize,
}

pub struct EndStatement {
  pub expr: Option<Expression>,
}

pub struct FnStatement {
  pub ident: Ident,
  pub params: Vec<Ident>,
  pub body: Box<BlockStatement>,
}

pub struct ForStatement {
  pub initializer: Option<Expression>,
  pub comparison: Option<Expression>,
  pub increment: Option<Expression>,
  pub block: Box<BlockStatement>,
}

pub struct IfStatement {
  pub comparison: Expression,
  pub block: Box<BlockStatement>,
}

pub struct BlockStatement {
  pub statements: Vec<Statement>,
}

pub struct LetStatement {
  pub ident: Ident,
  pub value: Option<Expression>,
}

pub struct LoadStatement {
  pub file: String,
}

pub struct LoopStatement {
  pub block: Box<BlockStatement>,
}

pub struct MatchStatement {
  pub expr: Expression,
  pub matchers: Vec<IfStatement>,
}

pub struct PrintStatement {
  pub expr: Expression,
}

pub struct RetStatement {
  pub expr: Option<Expression>,
}

pub struct WhileStatement {
  pub comparison: Expression,
  pub block: Box<BlockStatement>,
}

pub enum Expression {
  Literal(LiteralExpression),
  Unary(UnaryExpression),
  Binary(BinaryExpression),
  Group(GroupExpression),
  Call(CallExpression),
}

pub struct LiteralExpression {
  pub value: Value,
}

pub enum UnaryOperator {
  Not,
  Negate,
}

pub struct UnaryExpression {
  pub op: UnaryOperator,
  pub expr: Box<Expression>,
}

pub enum BinaryOperator {
  Or,
  And,
  Equal,
  NotEq,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Add,
  Sub,
  Mul,
  Div,
}

pub struct BinaryExpression {
  pub l: Box<Expression>,
  pub op: BinaryOperator,
  pub r: Box<Expression>,
}

pub struct GroupExpression {
  internal: Box<Expression>,
}

pub struct CallExpression {
  args: Vec<Expression>,
}
