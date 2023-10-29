req "common" as common;

use std::str;

export mod {
  BLACK: 0,
  RED: 1,
  GREEN: 2,
  YELLOW: 3,
  BLUE: 4,
  MAGENTA: 5,
  CYAN: 6,
  WHITE: 7,
  DEFAULT: 9,

  fn fg(color) {
    common::exec(str::concat("[", 30 + color, "m"));
  }

  fn bg(color) {
    common::exec(str::concat("[", 40 + color, "m"));
  }
}