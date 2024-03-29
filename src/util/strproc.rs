use lazy_regex::regex;
use regex::Replacer;

pub fn escape(input: &str) -> String {
  // bfnrt
  let output = input
    .replace("\\\"", "\"")
    .replace("\\b", "\x08")
    .replace("\\f", "\x0c")
    .replace("\\n", "\n")
    .replace("\\r", "\r")
    .replace("\\t", "\t");

  let hex_re = regex!(r#"\\x([0-9a-fA-F]{2})"#);
  let output = hex_re.replace_all(&output, HexReplacer);

  let uni_re = regex!(r#"\\u([0-9a-fA-F]{4})"#);
  let output = uni_re.replace_all(&output, HexReplacer);

  output.into()
}

struct HexReplacer;

impl Replacer for HexReplacer {
  fn replace_append(&mut self, caps: &regex::Captures<'_>, dst: &mut String) {
    let h = u32::from_str_radix(&caps[1], 16).unwrap();
    let c = unsafe { char::from_u32_unchecked(h) };
    dst.push(c);
  }
}
