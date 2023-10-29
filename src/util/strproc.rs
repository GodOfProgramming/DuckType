use lazy_regex::regex;
use regex::Replacer;

pub fn escape(input: &str) -> anyhow::Result<String> {
  let output = input.replace("\\n", "\n").replace("\\t", "\t");

  let hex_re = regex!(r#"\\x([0-9a-fA-F]{2})"#);
  let output = hex_re.replace_all(&output, HexReplacer);

  Ok(output.into())
}

struct HexReplacer;

impl Replacer for HexReplacer {
  fn replace_append(&mut self, caps: &regex::Captures<'_>, dst: &mut String) {
    let h = u32::from_str_radix(&caps[1], 16).unwrap();
    let c = unsafe { char::from_u32_unchecked(h) };
    dst.push(c);
  }
}
