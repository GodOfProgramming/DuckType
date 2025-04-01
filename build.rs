use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
  #[cfg(target_os = "linux")]
  linux_main()?;

  Ok(())
}

#[cfg(target_os = "linux")]
fn linux_main() -> Result<(), Box<dyn Error>> {
  #[cfg(feature = "jtbl")]
  {
    const SOURCES: &[&str] = &["src/cpp/lib.cpp"];
    const HEADERS: &[&str] = &["src/cpp/lib.hpp"];

    for file in SOURCES.iter().chain(HEADERS) {
      println!("cargo:rerun-if-changed={}", file);
    }
    use std::{fs, path::Path};

    let mut builder = bindgen::builder().blocklist_item("\\bstd::.*");

    for header in HEADERS {
      builder = builder.header(*header);
    }

    let bindings = builder.generate()?;

    let generated = Path::new("src").join("generated");

    fs::create_dir_all(&generated)?;

    bindings.write_to_file(generated.join("ffi.rs"))?;

    cc::Build::new()
      .cpp(true)
      .define(
        "DISASM_ENABLED",
        if cfg!(feature = "runtime-disassembly") { "1" } else { "0" },
      )
      .files(SOURCES)
      .compile("ffi");
  }

  Ok(())
}
