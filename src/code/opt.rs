use crate::code::ast::Ast;

pub struct Optimizer<const O: usize> {
  ast: Ast,
}

impl<const O: usize> Optimizer<O> {
  pub fn new(ast: Ast) -> Self {
    Self { ast }
  }

  pub fn optimize(self) -> Ast {
    self.ast
  }
}
