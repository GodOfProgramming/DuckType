req "common" as common;

use std::console;
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
    console::write(fmt::paint(mode, color));
  }

  fn fg(color) {
    console::write(fmt::fg(color));
  }

  fn bg(color) {
    console::write(fmt::bg(color));
  }

  mod fmt {
    fn paint(mode, color) {
      ret common::fmt(str::concat("[", mode + 8, color, "m"));
    }

    fn fg(color) {
      ret common::fmt(str::concat("[", FOREGROUND + color, "m"));
    }

    fn bg(color) {
      ret common::fmt(str::concat("[", BACKGROUND + color, "m"));
    }
  }
}