let $ESC = "\x1b";

use std::console;
use std::str;

fn exec(code) {
  console::write($ESC + code);
}

export mod {
  mod cursor {
    fn save() {
      exec(" 7");
    }

    fn restore() {
      exec("[u");
    }
  }

  mod display {
    fn clear() {
      exec("[J");
    }
  }

  mod text {
    BOLD: "[22m",

    fn enable(code) {
      exec(code);
    }

    fn reset() {
      exec("[0m");
    }

    fn scope(f) {
      f();
      reset();
    }

    mod color {
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
        exec(str::concat("[", 30 + color, "m"));
      }

      fn bg(color) {
        exec(str::concat("[", 40 + color, "m"));
      }
    }
  }
}