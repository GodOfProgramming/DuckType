#[test]
fn user_tests() {
  let t = trybuild::TestCases::new();
  t.pass("tests/pass/*.rs")
}
