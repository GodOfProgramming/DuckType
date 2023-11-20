use std::{collections::BTreeSet, fmt::Display};

use crate::code::{
  ast::{AstExpression, AstGenerator, BlockStatement, Ident, Params, RetStatement, SelfRules, Statement, SELF_IDENT},
  lex::{NumberToken, Token},
  SourceLocation,
};

use super::Expression;

#[derive(Debug)]
pub struct GroupExpression;

impl AstExpression for GroupExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let expr = ast.expression()?;
    if ast.consume(Token::RightParen, "expected ')' after expression") {
      Some(expr)
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

impl From<IdentExpression> for Ident {
  fn from(value: IdentExpression) -> Self {
    value.ident
  }
}

#[derive(Debug)]
pub struct ClassExpression {
  pub name: Ident,
  pub self_type: Box<Expression>,
  pub initializer: Option<Box<Expression>>,
  pub methods: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl ClassExpression {
  pub(super) fn new(
    name: Ident,
    creator: Expression,
    initializer: Option<Expression>,
    methods: Vec<(Ident, Expression)>,
    loc: SourceLocation,
  ) -> Self {
    Self {
      name,
      self_type: Box::new(creator),
      initializer: initializer.map(Box::new),
      methods,
      loc,
    }
  }
}

impl AstExpression for ClassExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let class_loc = ast.meta_at::<0>()?;

    let name = if let Some(Token::Identifier(name)) = ast.current() {
      ast.advance();
      Ident::new(name)
    } else {
      ast.error::<0>("expected ident after class");
      return None;
    };

    if !ast.consume(Token::LeftBrace, "expected '{' to begin class body") {
      return None;
    }

    let mut creator = None;
    let mut initializer = None;
    let mut class_members = Vec::default();
    let mut declared_functions = BTreeSet::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.meta_at::<0>()?;
      match token {
        Token::Identifier(ident) if ident == SELF_IDENT => {
          ast.advance();
          ast.consume(Token::As, "expected 'as' keyword after self");
          let initializer = ast.expression()?;
          if creator.is_none() {
            creator = Some(Expression::from(LambdaExpression::new(
              Vec::default(),
              Statement::Ret(RetStatement::new(Some(initializer), member_loc)),
              member_loc,
            )));
          } else {
            ast.error::<0>("self is already defined");
          }
        }
        Token::New => {
          ast.advance();
          if initializer.is_none() {
            if ast.consume(Token::LeftParen, "expected '(' after 'new'") {
              initializer = LambdaExpression::expr(
                ast,
                false,
                SelfRules::Require,
                Token::RightParen,
                |_this, params, mut body| {
                  body.statements.push(Statement::from(RetStatement::new(
                    Some(IdentExpression::new(Ident::new(SELF_IDENT), member_loc).into()),
                    member_loc,
                  )));
                  Some(Expression::from(LambdaExpression::new(
                    params.list,
                    Statement::from(body),
                    member_loc,
                  )))
                },
              );
            }
          } else {
            ast.error::<0>(String::from("duplicate initializer found"));
          }
        }
        Token::Fn => {
          ast.advance();
          if let Some(validator) = ast.fn_ident_validator() {
            if ast.consume(Token::LeftParen, "expected '(' after identifier") {
              if let Some(params) = ast.parse_parameters(Token::RightParen) {
                if let Some(ident) = validator(ast, &params) {
                  if !declared_functions.contains(&ident.name) {
                    if let Some(function) = ast.parse_lambda(true, params, |_this, params, body| {
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

    if let Some(creator) = creator.take() {
      Some(Expression::from(ClassExpression::new(
        name,
        creator,
        initializer,
        class_members,
        class_loc,
      )))
    } else {
      ast.error::<0>("self must be defined in classes");
      None
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

impl ClosureExpression {
  fn expr(ast: &mut AstGenerator, param_term: Token, captures: VecExpression) -> Option<Expression> {
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

    LambdaExpression::expr(ast, true, SelfRules::Disallow, param_term, |_this, params, body| {
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

  fn expr<F>(ast: &mut AstGenerator, can_return: bool, self_rules: SelfRules, param_term: Token, f: F) -> Option<Expression>
  where
    F: FnOnce(&mut AstGenerator, Params, BlockStatement) -> Option<Expression>,
  {
    let params = ast.parse_parameters(param_term)?;

    match (self_rules, params.found_self) {
      (SelfRules::Disallow, true) => {
        ast.error::<0>(String::from("found 'self' in invalid context"));
        None
      }
      (SelfRules::Require, false) => {
        ast.error::<0>(String::from("missing 'self' in function"));
        None
      }
      _ => ast.parse_lambda(can_return, params, f),
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

impl AstExpression for LambdaExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let loc = ast.meta_at::<0>()?;
    LambdaExpression::expr(ast, true, SelfRules::Disallow, Token::Pipe, |_this, params, body| {
      Some(Expression::from(LambdaExpression::new(
        params.list,
        Statement::from(body),
        loc,
      )))
    })
  }
}

#[derive(Debug)]
pub enum LiteralValue {
  Nil,
  Bool(bool),
  I32(i32),
  F64(f64),
  String(String),
}

impl Display for LiteralValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LiteralValue::Nil => write!(f, "nil"),
      LiteralValue::Bool(b) => write!(f, "{b}"),
      LiteralValue::I32(n) => write!(f, "{n}"),
      LiteralValue::F64(n) => write!(f, "{n}"),
      LiteralValue::String(s) => write!(f, "{s}"),
    }
  }
}

impl From<()> for LiteralValue {
  fn from(_: ()) -> Self {
    LiteralValue::Nil
  }
}

impl From<bool> for LiteralValue {
  fn from(value: bool) -> Self {
    LiteralValue::Bool(value)
  }
}

impl From<i32> for LiteralValue {
  fn from(value: i32) -> Self {
    LiteralValue::I32(value)
  }
}

impl From<f64> for LiteralValue {
  fn from(value: f64) -> Self {
    LiteralValue::F64(value)
  }
}

impl From<String> for LiteralValue {
  fn from(value: String) -> Self {
    LiteralValue::String(value)
  }
}

#[derive(Debug)]
pub struct LiteralExpression {
  pub value: LiteralValue,

  pub loc: SourceLocation, // location of the literal
}

impl LiteralExpression {
  pub(crate) fn new(value: LiteralValue, loc: SourceLocation) -> Self {
    Self { value, loc }
  }
}

impl AstExpression for LiteralExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<super::Expression> {
    let mut expr = None;

    if let Some(prev) = ast.previous() {
      match prev {
        Token::Nil => {
          expr = Some(Expression::from(Self::new(LiteralValue::Nil, ast.meta_at::<1>()?)));
        }
        Token::True => {
          expr = Some(Expression::from(Self::new(LiteralValue::Bool(true), ast.meta_at::<1>()?)));
        }
        Token::False => {
          expr = Some(Expression::from(Self::new(LiteralValue::Bool(false), ast.meta_at::<1>()?)));
        }
        Token::String(s) => expr = Some(Expression::from(Self::new(LiteralValue::String(s), ast.meta_at::<1>()?))),
        Token::Number(n) => {
          expr = Some(Expression::from(Self::new(
            match n {
              NumberToken::I32(i) => LiteralValue::I32(i),
              NumberToken::F64(f) => LiteralValue::F64(f),
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
  pub name: Ident,
  pub items: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl ModExpression {
  pub(super) fn new(name: Ident, items: Vec<(Ident, Expression)>, loc: SourceLocation) -> Self {
    Self { name, items, loc }
  }
}

impl AstExpression for ModExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let mod_loc = ast.meta_at::<0>()?;

    let name = if let Some(Token::Identifier(name)) = ast.current() {
      ast.advance();
      Ident::new(name)
    } else {
      ast.error::<0>("expected ident after mod");
      return None;
    };

    if !ast.consume(Token::LeftBrace, "expected '{' after mod name") {
      return None;
    }

    let mut module_items = Vec::default();
    let mut declared_items = BTreeSet::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.meta_at::<0>()?;
      match token {
        Token::Identifier(ident) => {
          if !declared_items.contains(&ident) {
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
          } else {
            ast.error::<0>("duplicate identifier found");
          }
          if !matches!(ast.current(), Some(Token::RightBrace)) && !ast.consume(Token::Comma, "expected ',' after expression") {
            return None;
          }
        }
        Token::Mod => {
          ast.advance();
          if let Some(Token::Identifier(ident)) = ast.current() {
            if !declared_items.contains(&ident) {
              declared_items.insert(ident.clone());
              if let Some(module) = Self::prefix(ast) {
                module_items.push((Ident::new(ident), module));
              }
            } else {
              ast.error::<0>("duplicate identifier found");
            }
          } else {
            ast.error::<0>("mod name is invalid");
          }
        }
        Token::Class => {
          ast.advance();
          if let Some(Token::Identifier(ident)) = ast.current() {
            if !declared_items.contains(&ident) {
              declared_items.insert(ident.clone());
              if let Some(class) = ClassExpression::prefix(ast) {
                module_items.push((Ident::new(ident), class));
              }
            } else {
              ast.error::<0>("duplicate identifier found");
            }
          } else {
            ast.error::<0>("class name is invalid");
          }
        }
        Token::Fn => {
          ast.advance();
          if let Some(validator) = ast.fn_ident_validator() {
            if ast.consume(Token::LeftParen, "expected '(' after identifier") {
              if let Some(params) = ast.parse_parameters(Token::RightParen) {
                if let Some(ident) = validator(ast, &params) {
                  if !declared_items.contains(&ident.name) {
                    if let Some(function) = ast.parse_lambda(true, params, |this, params, body| {
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
        t => ast.error::<0>(format!("unexpected token in module {t}")),
      }
    }

    if !ast.consume(Token::RightBrace, "expected '}' after module body") {
      return None;
    }

    Some(Expression::from(ModExpression::new(name, module_items, mod_loc)))
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

#[derive(Debug)]
pub struct VecExpression {
  pub items: Vec<Expression>,
  pub loc: SourceLocation,
}

impl VecExpression {
  pub(super) fn new(items: Vec<Expression>, loc: SourceLocation) -> Self {
    Self { items, loc }
  }
}

impl AstExpression for VecExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let bracket_meta = ast.meta_at::<1>()?;
    let mut items = Vec::default();

    'items: while let Some(token) = ast.current() {
      if token == Token::RightBracket {
        break 'items;
      }

      items.push(ast.expression()?);

      if !ast.advance_if_matches(Token::Comma) {
        break 'items;
      }
    }

    let mut size = None;

    if items.len() == 1 {
      // test if ';' is found
      if ast.advance_if_matches(Token::Semicolon) {
        // then check for a size
        size = Some(ast.expression()?);
      }
    }

    if ast.consume(Token::RightBracket, "expect ']' after arguments") {
      if let Some(size) = size {
        Some(DynVecExpression::new(items.swap_remove(0), size, bracket_meta).into())
      } else {
        // normal vec
        let vec = Self::new(items, bracket_meta);

        // closure
        if ast.advance_if_matches(Token::Pipe) {
          ClosureExpression::expr(ast, Token::Pipe, vec)
        } else {
          Some(vec.into())
        }
      }
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct DynVecExpression {
  pub item: Box<Expression>,
  pub size: Box<Expression>,
  pub loc: SourceLocation,
}

impl DynVecExpression {
  fn new(item: Expression, size: Expression, loc: SourceLocation) -> Self {
    Self {
      item: Box::new(item),
      size: Box::new(size),
      loc,
    }
  }
}

#[derive(Debug)]
pub struct SizedVecExpression {
  pub item: Box<Expression>,
  pub size: i32,
  pub loc: SourceLocation,
}

impl SizedVecExpression {
  pub fn new(item: Expression, size: i32, loc: SourceLocation) -> Self {
    Self {
      item: Box::new(item),
      size,
      loc,
    }
  }
}
