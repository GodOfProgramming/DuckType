use std::error::Error;

const SOURCES: &'static [&'static str] = &["src/cpp/lib.cpp"];
const HEADERS: &'static [&'static str] = &["src/cpp/lib.hpp"];

fn main() -> Result<(), Box<dyn Error>> {
  for file in SOURCES.iter().chain(HEADERS) {
    println!("cargo:rerun-if-changed={}", file);
  }

  let mut builder = bindgen::builder().allowlist_item("OPCODE_\\w+").blocklist_item("\\bstd::.*");

  for header in HEADERS {
    builder = builder.header(*header);
  }

  let bindings = builder.generate()?;

  bindings.write_to_file("src/generated/ffi.rs")?;

  cc::Build::new().cpp(true).files(SOURCES).compile("ffi");

  Ok(())
}
