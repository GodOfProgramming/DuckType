req "examples/lib/ansi.ss" as ansi;

use ansi::display;
use ansi::cursor;
use ansi::text;
use text::color;
use std::console;
use std::math;
use std::str;
use std::ps::thread;

let $NUM_ROWS = 25;
let $NUM_COLUMNS = 30;

fn main() {
  let SLEEP_TIME = 32.0 / 1000.0;

  let fb = new_buff();

  seed_buffer(fb);

  # run
  loop {
    display::clear();
    cursor::home();

    display_buffer(fb);

    if !is_population_alive(fb) {
      break;
    }

    let bb = new_buff();

    update(fb, bb);

    # bb disappears after this
    if no_change(fb, bb) {
      break;
    }

    fb = bb;

    thread::sleep(SLEEP_TIME);
  }
}

fn new_buff() {
  let b = [nil; $NUM_ROWS];

  for let i = 0; i < $NUM_ROWS; i += 1 {
    b[i] = [nil; $NUM_COLUMNS];
  }

  ret b;
}

fn seed_buffer(buff) {
  for let r = 0; r < $NUM_ROWS; r += 1 {
    for let c = 0; c < $NUM_COLUMNS; c += 1 {
      buff[r][c] = math::abs(math::rand_i32()) % 2;
    }
  }
}

fn display_buffer(buff) {
  row_line();
  for let r = 0; r < $NUM_ROWS; r += 1 {
    let row_parts = [];
    row_parts.push(":");
    for let c = 0; c < $NUM_COLUMNS; c += 1 {
      let v = buff[r][c];

      let cell_color;
      match v {
        0 => cell_color = color::fmt::paint(color::FOREGROUND, color::RGB(56, 56, 56)),
        1 => cell_color = color::fmt::fg(color::CYAN),
      }

      row_parts.push(str::concat("  ", cell_color, v, text::fmt::reset(), "  :"));
    }
    console::write(row_parts.join(nil));
    row_line();
  }
}

fn row_line() {
  console::writeln();
  for let c = 0; c < $NUM_COLUMNS; c += 1 {
    console::write(" -----");
  }
  console::writeln();
}

fn update(fb, bb) {
  for let r = 0; r < $NUM_ROWS; r += 1 {
    for let c = 0; c < $NUM_COLUMNS; c += 1 {
      let neighbor_live_cell = count_live_neighbor_cells(fb, r, c);
      if fb[r][c] == 1 and (neighbor_live_cell == 2 or neighbor_live_cell == 3) {
        bb[r][c] = 1;
      } else if fb[r][c] == 0 and neighbor_live_cell == 3 {
        bb[r][c] = 1;
      } else {
        bb[r][c] = 0;
      }
    }
  }
}

fn count_live_neighbor_cells(buff, row, col) {
  let count = 0;
  for let r = row - 1; r <= row + 1; r += 1 {
    if r < 0 or r >= $NUM_ROWS {
      cont;
    }

    for let c = col - 1; c <= col + 1; c += 1 {
      if r == row and c == col {
        cont;
      }

      if c < 0 or c >= $NUM_COLUMNS {
        cont;
      }

      if (buff[r][c] == 1) {
        count += 1;
      }
    }
  }
  ret count;
}

fn is_population_alive(buff) {
  for let r = 0; r < $NUM_ROWS; r += 1 {
    for let c = 0; c < $NUM_COLUMNS; c += 1 {
      if buff[r][c] == 1 {
        ret true;
      }
    }
  }
  ret false;
}

fn no_change(a, b) {
  for let r = 0; r < $NUM_ROWS; r += 1 {
    for let c = 0; c < $NUM_COLUMNS; c += 1 {
      if a[r][c] != b[r][c] {
        ret false;
      }
    }
  }
  ret true;
}

export main();
