use std::{error::Error, fs, path::Path};

const SOURCES: &'static [&'static str] = &["src/cpp/lib.cpp"];
const HEADERS: &'static [&'static str] = &["src/cpp/lib.hpp"];

fn main() -> Result<(), Box<dyn Error>> {
  for file in SOURCES.iter().chain(HEADERS) {
    println!("cargo:rerun-if-changed={}", file);
  }

  let mut builder = bindgen::builder().blocklist_item("\\bstd::.*");

  for header in HEADERS {
    builder = builder.header(*header);
  }

  let bindings = builder.generate()?;

  let generated = Path::new("src").join("generated");

  fs::create_dir_all(&generated)?;

  bindings.write_to_file(generated.join("ffi.rs"))?;

  cc::Build::new().cpp(true).files(SOURCES).compile("ffi");

  Ok(())
}
