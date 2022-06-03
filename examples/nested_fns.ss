# issue with nested functions and locals
fn first(arg) {
  fn second(arg) {
    fn third(arg) {
      fn fourth() {
        first(false);
      }

      if arg {
        fourth(true);
      } else {
        print "here";
      }
    }

    if arg {
      third(true);
    } else {

    }
  }

  if arg {
    second(true);
  } else {

  }
}