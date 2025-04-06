pub mod strproc;

#[cfg(windows)]
pub mod windows;
#[cfg(windows)]
pub use windows::*;
#[cfg(windows)]
pub type PlatformMetadata = WindowsMetadata;

#[cfg(unix)]
pub mod unix;
#[cfg(unix)]
pub use unix::*;
#[cfg(unix)]
pub type PlatformMetadata = UnixMetadata;

use anyhow::Result;
use lazy_regex::regex;
use std::path::Path;

pub fn terminal_width() -> Result<usize> {
  #[cfg(unix)]
  return unix::terminal_width();
  #[cfg(windows)]
  return windows::terminal_width();
}

pub trait FileMetadata {
  fn id_of(path: &Path) -> Result<FileIdType>;
}

pub(crate) trait UnwrapAnd<T> {
  fn unwrap_and(self, f: impl FnOnce(T));
}

impl<T> UnwrapAnd<T> for Option<T> {
  fn unwrap_and(self, f: impl FnOnce(T)) {
    if let Some(inner) = self {
      f(inner);
    }
  }
}

pub struct VarPath {
  parts: Vec<String>,
}

impl VarPath {
  pub fn from_parts<'a>(value: impl Iterator<Item = &'a str>) -> Self {
    Self {
      parts: value.into_iter().map(String::from).collect(),
    }
  }

  pub fn from_str(value: impl AsRef<str>) -> Self {
    let split_re = regex!("::|\\.");
    let parts = split_re.split(value.as_ref());
    Self::from_parts(parts)
  }

  pub fn root(&self) -> Option<&str> {
    self.parts.first().map(<String as AsRef<str>>::as_ref)
  }

  pub fn children(&self) -> &[String] {
    (!self.parts.is_empty()).then(|| &self.parts[1..]).unwrap_or(&[])
  }
}

#[cfg(not(feature = "serde"))]
pub trait MaybeSerde {}

#[cfg(not(feature = "serde"))]
impl<T> MaybeSerde for T {}

#[cfg(feature = "serde")]
pub trait MaybeSerde: serde::Serialize + for<'de> serde::Deserialize<'de> {}

#[cfg(feature = "serde")]
impl<T> MaybeSerde for T where T: serde::Serialize + for<'de> serde::Deserialize<'de> {}

#[cfg(test)]
mod tests {
  use crate::util::VarPath;

  #[test]
  fn var_path_split_by_string() {
    const PATH: &str = "foo::bar.baz";
    let vp = VarPath::from_str(PATH);
    assert_eq!(vp.root(), Some("foo"));
    assert_eq!(vp.children(), &["bar", "baz"]);
  }
}
