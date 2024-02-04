use crate::{
  code::{
    ast::*,
    lex::{NumberToken, Token},
    SourceLocation,
  },
  error::AstGenerationErrorMsg,
};
use std::{collections::BTreeSet, fmt::Display};

#[derive(Debug)]
pub struct GroupExpression;

impl AstExpression for GroupExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let expr = ast.expression()?;
    if ast.consume(Token::RightParen) {
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
    match ast.previous() {
      Some(Token::Identifier(ident_name)) => Some(Expression::from(IdentExpression::new(
        Ident::new(ident_name),
        ast.token_location::<1>()?,
      ))),
      Some(t) => {
        ast.error::<2>(AstGenerationErrorMsg::InvalidIdentifier(t));
        None
      }
      None => {
        ast.error::<2>(AstGenerationErrorMsg::UnexpectedEof);
        None
      }
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
  pub initializer: Option<Box<Expression>>,
  pub methods: Vec<(Ident, Expression)>,
  pub loc: SourceLocation,
}

impl ClassExpression {
  pub(super) fn new(
    name: Ident,
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

impl AstExpression for ClassExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let class_loc = ast.token_location::<0>()?;

    let name = if let Some(Token::Identifier(name)) = ast.current() {
      ast.advance();
      Ident::new(name)
    } else {
      ast.error::<0>(AstGenerationErrorMsg::MissingIdentifier);
      return None;
    };

    if !ast.consume(Token::LeftBrace) {
      return None;
    }

    let mut initializer = None;
    let mut class_members = Vec::default();
    let mut declared_functions = BTreeSet::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.token_location::<0>()?;
      match token {
        Token::New => {
          ast.advance();
          if initializer.is_none() {
            if ast.consume(Token::LeftParen) {
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
            ast.error::<0>(AstGenerationErrorMsg::DuplicateClassFunction);
          }
        }
        Token::Fn => {
          ast.advance();
          if let Some(validator) = ast.fn_ident_validator() {
            if ast.consume(Token::LeftParen) {
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
                    ast.error::<0>(AstGenerationErrorMsg::DuplicateClassFunction);
                  }
                }
              }
            }
          }
        }
        Token::RightBrace => break,
        t => ast.error::<0>(AstGenerationErrorMsg::InvalidToken(t)),
      }
    }

    if !ast.consume(Token::RightBrace) {
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
    let loc = ast.token_location::<0>()?;

    let mut vetted_captures = Vec::new();
    for capture in captures.items {
      match capture {
        Expression::Ident(expr) => vetted_captures.push(expr),
        _ => {
          ast.error::<0>(AstGenerationErrorMsg::InvalidCapture);
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
        ast.error::<0>(AstGenerationErrorMsg::SelfNotAllowed);
        None
      }
      (SelfRules::Require, false) => {
        ast.error::<0>(AstGenerationErrorMsg::SelfParameterUndefined);
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
    let loc = ast.token_location::<0>()?;
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
    let prev = ast.previous()?;
    match prev {
      Token::Nil => Some(Expression::from(Self::new(LiteralValue::Nil, ast.token_location::<1>()?))),
      Token::True => Some(Expression::from(Self::new(
        LiteralValue::Bool(true),
        ast.token_location::<1>()?,
      ))),
      Token::False => Some(Expression::from(Self::new(
        LiteralValue::Bool(false),
        ast.token_location::<1>()?,
      ))),
      Token::String(s) => Some(Expression::from(Self::new(
        LiteralValue::String(s),
        ast.token_location::<1>()?,
      ))),
      Token::Number(n) => Some(Expression::from(Self::new(
        match n {
          NumberToken::I32(i) => LiteralValue::I32(i),
          NumberToken::F64(f) => LiteralValue::F64(f),
        },
        ast.token_location::<1>()?,
      ))),
      _ => {
        // would need to screw up parse rules to hit this
        unreachable!();
      }
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
pub struct ModExpression {
  pub name: Ident,
  pub statements: Vec<Statement>,
  pub loc: SourceLocation,
}

impl ModExpression {
  pub(super) fn new(name: Ident, items: Vec<Statement>, loc: SourceLocation) -> Self {
    Self {
      name,
      statements: items,
      loc,
    }
  }

  fn named_expr(
    ast: &mut AstGenerator,
    ident: String,
    declared_items: &mut BTreeSet<String>,
    module_items: &mut Vec<Statement>,
    loc: SourceLocation,
  ) -> Option<()> {
    if !declared_items.contains(&ident) {
      declared_items.insert(ident.clone());
      ast.advance();

      let member = Ident::new_module(ident.clone());

      let value = if ast.advance_if_matches(Token::Colon) {
        ast.expression()?
      } else {
        let ident = Ident::new(ident);
        Expression::from(IdentExpression::new(ident, loc))
      };

      module_items.push(Statement::from(LetStatement::new(member, Some(value), loc)));
    } else {
      ast.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
      return None;
    }

    if !matches!(ast.current(), Some(Token::RightBrace)) && !ast.consume(Token::Comma) {
      return None;
    }

    Some(())
  }

  fn inner_module(
    ast: &mut AstGenerator,
    declared_items: &mut BTreeSet<String>,
    module_items: &mut Vec<Statement>,
    loc: SourceLocation,
  ) -> Option<()> {
    ast.advance();
    match ast.current()? {
      Token::Identifier(ident) => {
        if !declared_items.contains(&ident) {
          declared_items.insert(ident);
          let module = Self::prefix(ast)?;
          module_items.push(Statement::from(ExpressionStatement::new(module, loc)));
          Some(())
        } else {
          ast.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
          None
        }
      }
      t => {
        ast.error::<0>(AstGenerationErrorMsg::InvalidIdentifier(t));
        None
      }
    }
  }

  fn class(
    ast: &mut AstGenerator,
    declared_items: &mut BTreeSet<String>,
    module_items: &mut Vec<Statement>,
    loc: SourceLocation,
  ) -> Option<()> {
    ast.advance();
    match ast.current()? {
      Token::Identifier(ident) => {
        if !declared_items.contains(&ident) {
          declared_items.insert(ident);
          let class = ClassExpression::prefix(ast)?;
          module_items.push(Statement::from(ExpressionStatement::new(class, loc)));
          Some(())
        } else {
          ast.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
          None
        }
      }
      t => {
        ast.error::<0>(AstGenerationErrorMsg::InvalidIdentifier(t));
        None
      }
    }
  }

  fn function(ast: &mut AstGenerator, declared_items: &mut BTreeSet<String>, module_items: &mut Vec<Statement>) -> Option<()> {
    ast.advance();
    match ast.current()? {
      Token::Identifier(ident) => {
        if !declared_items.contains(&ident) {
          declared_items.insert(ident);
          let function = ast.parse_fn()?;
          module_items.push(function);
          Some(())
        } else {
          ast.error::<0>(AstGenerationErrorMsg::DuplicateIdentifier);
          None
        }
      }
      t => {
        ast.error::<0>(AstGenerationErrorMsg::InvalidIdentifier(t));
        None
      }
    }
  }
}

impl AstExpression for ModExpression {
  fn prefix(ast: &mut AstGenerator) -> Option<Expression> {
    let mod_loc = ast.token_location::<0>()?;

    let name = if let Some(Token::Identifier(name)) = ast.current() {
      ast.advance();
      Ident::new(name)
    } else {
      ast.error::<0>(AstGenerationErrorMsg::MissingIdentifier);
      return None;
    };

    if !ast.consume(Token::LeftBrace) {
      return None;
    }

    let mut declared_items = BTreeSet::default();
    let mut module_items = Vec::default();

    while let Some(token) = ast.current() {
      let member_loc = ast.token_location::<0>()?;
      match token {
        Token::Identifier(ident) => Self::named_expr(ast, ident, &mut declared_items, &mut module_items, member_loc)?,
        Token::Mod => Self::inner_module(ast, &mut declared_items, &mut module_items, member_loc)?,
        Token::Class => Self::class(ast, &mut declared_items, &mut module_items, member_loc)?,
        Token::Fn => Self::function(ast, &mut declared_items, &mut module_items)?,
        Token::RightBrace => break,
        t => ast.error::<0>(AstGenerationErrorMsg::InvalidToken(t)),
      }
    }

    if !ast.consume(Token::RightBrace) {
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
    let struct_meta = ast.token_location::<1>()?;
    // needed here because infix advances to left brace after seeing "struct"
    if !ast.consume(Token::LeftBrace) {
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
        } else if ast.consume(Token::Colon) {
          let value = ast.expression()?;
          members.push((Ident::new(ident), value));
          ast.advance_if_matches(Token::Comma);
        } else {
          return None;
        }
      } else {
        ast.error::<0>(AstGenerationErrorMsg::MissingIdentifier);
        return None;
      }
    }

    if !ast.consume(Token::RightBrace) {
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
    let bracket_meta = ast.token_location::<1>()?;
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

    if ast.consume(Token::RightBracket) {
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
