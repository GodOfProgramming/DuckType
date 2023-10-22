use std::collections::BTreeSet;

use crate::code::{
  ast::{AstExpression, AstGenerator, BlockStatement, DefaultConstructorRet, Ident, Params, SelfRules, Statement},
  lex::{NumberToken, Token},
  ConstantValue, SourceLocation,
};

use super::Expression;

#[derive(Debug)]
pub struct GroupExpression {
  pub expr: Box<Expression>,

  pub loc: SourceLocation, // location of the left paren
}

impl GroupExpression {
  pub(super) fn new(expr: Expression, loc: SourceLocation) -> Self {
    Self {
      expr: Box::new(expr),
      loc,
    }
  }
}

impl AstExpression for GroupExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let paren_meta = ast.meta_at::<1>()?;
    let expr = ast.expression()?;
    if ast.consume(Token::RightParen, "expected ')' after expression") {
      Some(Expression::from(GroupExpression::new(expr, paren_meta)))
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct IdentExpression {
  pub ident: Ident,

  pub loc: SourceLocation, // location of the identifier
}

impl IdentExpression {
  pub(crate) fn new(ident: Ident, loc: SourceLocation) -> Self {
    Self { ident, loc }
  }
}

impl AstExpression for IdentExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    if let Some(ident_token) = ast.previous() {
      if let Token::Identifier(ident_name) = ident_token {
        Some(Expression::from(IdentExpression::new(
          Ident::new(ident_name),
          ast.meta_at::<1>()?,
        )))
      } else {
        ast.error::<2>(String::from("variable name is not an identifier"));
        None
      }
    } else {
      ast.error::<2>(String::from("unexpected end of token stream"));
      None
    }
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

impl ClassExpression {
  pub(crate) fn expr(ast: &mut AstGenerator, name: Option<Ident>) -> Option<Expression> {
    let class_loc = ast.meta_at::<0>()?;
    // needed here because infix advances to '{' after seeing "class"
    if !ast.consume(Token::LeftBrace, "expected '{' to begin class body") {
      return None;
    }

    let mut initializer = None;
    let mut class_members = Vec::default();
    let mut declared_functions = BTreeSet::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.meta_at::<0>()?;
      match token {
        Token::New => {
          ast.advance();
          if initializer.is_none() {
            if ast.consume(Token::LeftParen, "expected '(' after 'new'") {
              initializer = LambdaExpression::expr(ast, SelfRules::Require, Token::RightParen, |_this, params, mut body| {
                body
                  .statements
                  .push(Statement::from(DefaultConstructorRet::new(member_loc.clone())));
                Some(Expression::from(LambdaExpression::new(
                  params.list,
                  Statement::from(body),
                  member_loc,
                )))
              });
            }
          } else {
            ast.error::<0>(String::from("duplicate initializer found"));
          }
        }
        Token::Fn => {
          ast.advance();
          if let Some(fn_name) = ast.current() {
            ast.advance();
            if ast.consume(Token::LeftParen, "expected '(' after identifier") {
              if let Some(params) = ast.parse_parameters(Token::RightParen) {
                if let Some(ident) = ast.fn_ident(fn_name, &params) {
                  if !declared_functions.contains(&ident.name) {
                    if let Some(function) = ast.parse_lambda(params, |_this, params, body| {
                      declared_functions.insert(ident.name.clone());
                      if params.found_self {
                        Some(Expression::from(MethodExpression::new(
                          ident.clone(),
                          params.list,
                          Statement::from(body),
                          member_loc,
                        )))
                      } else {
                        Some(Expression::from(LambdaExpression::new(
                          params.list,
                          Statement::from(body),
                          member_loc,
                        )))
                      }
                    }) {
                      class_members.push((ident, function));
                    }
                  } else {
                    ast.error::<0>("duplicate method definition");
                  }
                }
              }
            }
          }
        }
        Token::RightBrace => break,
        t => ast.error::<0>(format!("unexpected token in class {t}")),
      }
    }

    if !ast.consume(Token::RightBrace, "expected '}' after class body") {
      return None;
    }

    Some(Expression::from(ClassExpression::new(
      name,
      initializer,
      class_members,
      class_loc,
    )))
  }
}

impl AstExpression for ClassExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    Self::expr(ast, None)
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

impl ClosureExpression {
  fn expr(ast: &mut AstGenerator, param_term: Token, captures: ListExpression) -> Option<Expression> {
    let loc = ast.meta_at::<0>()?;

    let mut vetted_captures = Vec::new();
    for capture in captures.items {
      match capture {
        Expression::Ident(expr) => vetted_captures.push(expr),
        _ => {
          ast.error::<0>("invalid lambda capture");
          return None;
        }
      }
    }

    LambdaExpression::expr(ast, SelfRules::Disallow, param_term, |_this, params, body| {
      Some(Expression::from(ClosureExpression::new(
        vetted_captures,
        params.list,
        Statement::from(body),
        loc,
      )))
    })
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

  fn expr<F>(ast: &mut AstGenerator, self_rules: SelfRules, param_term: Token, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut AstGenerator, Params, BlockStatement) -> Option<Expression>,
  {
    let params = ast.parse_parameters(param_term)?;

    if self_rules == SelfRules::Disallow && params.found_self {
      ast.error::<0>(String::from("found 'self' in invalid context"));
      return None;
    }

    if self_rules == SelfRules::Require && !params.found_self {
      ast.error::<0>(String::from("missing 'self' in function"));
      return None;
    }

    ast.parse_lambda(params, f)
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

impl AstExpression for LambdaExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let loc = ast.meta_at::<0>()?;
    LambdaExpression::expr(ast, SelfRules::Disallow, Token::Pipe, |_this, params, body| {
      Some(Expression::from(LambdaExpression::new(
        params.list,
        Statement::from(body),
        loc,
      )))
    })
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

impl AstExpression for ListExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let bracket_meta = ast.meta_at::<1>()?;
    let mut items = Vec::default();

    if let Some(token) = ast.current() {
      if token != Token::RightBracket {
        loop {
          items.push(ast.expression()?);
          if !ast.advance_if_matches(Token::Comma) {
            break;
          }
        }
      }
    }

    if ast.consume(Token::RightBracket, "expect ']' after arguments") {
      let list = ListExpression::new(items, bracket_meta);
      if ast.advance_if_matches(Token::Pipe) {
        ClosureExpression::expr(ast, Token::Pipe, list)
      } else {
        Some(Expression::from(list))
      }
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct LiteralExpression {
  pub value: ConstantValue,

  pub loc: SourceLocation, // location of the literal
}

impl LiteralExpression {
  pub(super) fn new(value: ConstantValue, loc: SourceLocation) -> Self {
    Self { value, loc }
  }
}

impl AstExpression for LiteralExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<super::Expression> {
    let mut expr = None;

    if let Some(prev) = ast.previous() {
      match prev {
        Token::Nil => {
          expr = Some(Expression::from(Self::new(ConstantValue::Nil, ast.meta_at::<1>()?)));
        }
        Token::True => {
          expr = Some(Expression::from(Self::new(ConstantValue::Bool(true), ast.meta_at::<1>()?)));
        }
        Token::False => {
          expr = Some(Expression::from(Self::new(ConstantValue::Bool(false), ast.meta_at::<1>()?)));
        }
        Token::String(s) => expr = Some(Expression::from(Self::new(ConstantValue::String(s), ast.meta_at::<1>()?))),
        Token::Number(n) => {
          expr = Some(Expression::from(Self::new(
            match n {
              NumberToken::I32(i) => ConstantValue::Integer(i),
              NumberToken::F64(f) => ConstantValue::Float(f),
            },
            ast.meta_at::<1>()?,
          )))
        }
        _ => {
          ast.error::<1>(String::from("sanity check, invalid literal, very bad logic error"));
        }
      }
    } else {
      ast.error::<1>(String::from("sanity check, no previous token, very bad logic error"));
    }

    expr
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

impl ModExpression {
  pub(crate) fn expr(ast: &mut AstGenerator, name: Option<Ident>) -> Option<Expression> {
    let mod_loc = ast.meta_at::<0>()?;

    if !ast.consume(Token::LeftBrace, "expected '{' after mod name") {
      return None;
    }

    let mut module_items = Vec::default();
    let mut declared_items = BTreeSet::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.meta_at::<0>()?;
      match token {
        Token::Identifier(ident) => {
          declared_items.insert(ident.clone());
          ast.advance();

          if ast.advance_if_matches(Token::Colon) {
            let ident = Ident::new(ident);
            if let Some(expr) = ast.expression() {
              module_items.push((ident, expr));
            }
          } else {
            module_items.push((
              Ident::new(ident.clone()),
              Expression::from(IdentExpression::new(Ident::new(ident), member_loc)),
            ))
          }
          if !matches!(ast.current(), Some(Token::RightBrace)) && !ast.consume(Token::Comma, "expected ',' after expression") {
            return None;
          }
        }
        Token::Mod => {
          ast.advance();
          if let Some(Token::Identifier(ident)) = ast.current() {
            declared_items.insert(ident.clone());
            ast.advance();
            let ident = Ident::new(ident);
            if let Some(module) = Self::expr(ast, Some(ident.clone())) {
              module_items.push((ident, module));
            }
          } else {
            ast.error::<0>("mod name is invalid");
          }
        }
        Token::Class => {
          ast.advance();
          if let Some(Token::Identifier(ident)) = ast.current() {
            declared_items.insert(ident.clone());
            ast.advance();
            let ident = Ident::new(ident);
            if let Some(class) = ClassExpression::expr(ast, Some(ident.clone())) {
              module_items.push((ident, class));
            }
          } else {
            ast.error::<0>("class name is invalid");
          }
        }
        Token::Fn => {
          ast.advance();
          if let Some(fn_name) = ast.current() {
            ast.advance();
            if ast.consume(Token::LeftParen, "expected '(' after identifier") {
              if let Some(params) = ast.parse_parameters(Token::RightParen) {
                if let Some(ident) = ast.fn_ident(fn_name, &params) {
                  if !declared_items.contains(&ident.name) {
                    if let Some(function) = ast.parse_lambda(params, |this, params, body| {
                      declared_items.insert(ident.name.clone());
                      if params.found_self {
                        this.error::<0>("cannot use ast in module function");
                        None
                      } else {
                        Some(Expression::from(LambdaExpression::new(
                          params.list,
                          Statement::from(body),
                          member_loc,
                        )))
                      }
                    }) {
                      module_items.push((ident, function));
                    }
                  } else {
                    ast.error::<0>("duplicate method definition");
                  }
                }
              }
            }
          }
        }
        Token::RightBrace => break,
        t => ast.error::<0>(format!("unexpected token in class {t}")),
      }
    }

    if !ast.consume(Token::RightBrace, "expected '}' after class body") {
      return None;
    }

    Some(Expression::from(ModExpression::new(name, module_items, mod_loc)))
  }
}

impl AstExpression for ModExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    Self::expr(ast, None)
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

impl AstExpression for StructExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let struct_meta = ast.meta_at::<1>()?;
    // needed here because infix advances to '{' after seeing "struct"
    if !ast.consume(Token::LeftBrace, "expected '{' to begin struct body") {
      return None;
    }

    let mut members = Vec::default();

    while let Some(token) = ast.current() {
      let struct_meta = struct_meta.clone();
      if token == Token::RightBrace {
        break;
      }

      if let Token::Identifier(ident) = token {
        ast.advance();
        if ast.advance_if_matches(Token::Comma) || ast.check_peek_after::<0>(Token::RightBrace) {
          members.push((
            Ident::new(ident.clone()),
            Expression::from(IdentExpression::new(Ident::new(ident), struct_meta)),
          ))
        } else if ast.consume(Token::Colon, "expected ':' after identifier") {
          let value = ast.expression()?;
          members.push((Ident::new(ident), value));
          ast.advance_if_matches(Token::Comma);
        } else {
          return None;
        }
      } else {
        ast.error::<0>("expected identifier");
        return None;
      }
    }

    if !ast.consume(Token::RightBrace, "expected '}' after struct") {
      return None;
    }

    Some(Expression::from(StructExpression::new(members, struct_meta)))
  }
}
