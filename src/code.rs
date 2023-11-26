use crate::prelude::*;
use common::util::FileIdType;
use common::{errors::Error, Instruction};
use gen::BytecodeGenerator;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  path::PathBuf,
  rc::Rc,
  str,
};

pub(crate) struct CompileOpts {
  pub(crate) optimize: bool,
}

pub(crate) fn compile_file(
  cache: &mut Cache,
  file_id: FileIdType,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  compile(cache, Some(file_id), source, opts)
}

pub(crate) fn compile_string(
  cache: &mut Cache,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  compile(cache, None, source, opts)
}

pub(crate) fn compile(
  cache: &mut Cache,
  file_id: Option<FileIdType>,
  source: impl AsRef<str>,
  opts: CompileOpts,
) -> Result<SmartPtr<Context>, Error> {
  let (tokens, token_locations) = lex::tokenize(file_id, source.as_ref())?;

  let mut ast = ast::generate(file_id, tokens, token_locations)?;

  if opts.optimize {
    ast = ast::optimize(ast);
  }

  let source = Rc::new(source.as_ref().to_string());
  let reflection = Reflection::new(Some("<main>"), file_id, Rc::clone(&source));
  let ctx = SmartPtr::new(Context::new(0, reflection));

  gen::generate(cache, file_id, ctx, ast)
}

#[derive(Default)]
pub struct FileMap {
  map: BTreeMap<FileIdType, PathBuf>,
}

impl FileMap {
  pub(crate) fn add(&mut self, id: FileIdType, path: impl Into<PathBuf>) {
    self.map.insert(id, path.into());
  }

  pub(crate) fn get(&self, id: Option<FileIdType>) -> PathBuf {
    id.and_then(|id| self.map.get(&id).cloned())
      .unwrap_or_else(|| PathBuf::from("<anonymous>"))
  }
}
