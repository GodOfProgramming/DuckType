fn main() {
  let c = 0;
  let fb = new_buff();
  loop {
    let bb = new_buff();

    if no_change(fb, bb) {
      break;
    }

    fb = bb;
    c += 1;

    if c == 10 {
      __breakpoint__;
    }
  }
}

fn new_buff() { ret nil; }

fn no_change(_1, _2) { ret false; }

export main();
