req "common" as common;

use std::str;

export mod color {
  BLACK: 0,
  RED: 1,
  GREEN: 2,
  YELLOW: 3,
  BLUE: 4,
  MAGENTA: 5,
  CYAN: 6,
  WHITE: 7,
  DEFAULT: 9,

  FOREGROUND: 30,
  BACKGROUND: 40,

  ID: |id| { ret str::concat(";5;", id); },
  RGB: |r, g, b| { ret str::concat(";2;", r, ";", g, ";", b); },

  fn paint(mode, color) {
    common::exec(str::concat("[", mode + 8, color, "m"));
  }

  fn fg(color) {
    common::exec(str::concat("[", FOREGROUND + color, "m"));
  }

  fn bg(color) {
    common::exec(str::concat("[", BACKGROUND + color, "m"));
  }
}