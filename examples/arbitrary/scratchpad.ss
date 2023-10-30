req "examples/lib/ansi.ss" as ansi;

use ansi::cursor;
use ansi::display;
use ansi::text;
use std::console;
use std::math;
use text::color;
use color::fmt;

fn random_rgb() {
  ret [math::rand_i32() % 255, math::rand_i32() % 255, math::rand_i32() % 255];
}

fn seizurize_text(input) {
  let parts = [];
  for let i = 0; i < input.len(); i += 1 {
    let fg = random_rgb();
    let bg = random_rgb();
    parts.push(fmt::paint(color::FOREGROUND, color::RGB(fg[0], fg[1], fg[2])));
    parts.push(fmt::paint(color::BACKGROUND, color::RGB(bg[0], bg[1], bg[2])));
    parts.push(input[i]);
  }
  ret parts.join('');
}

text::scope(|| {
  loop {
    let input = console::readln();

    if input == "quit" {
      break;
    }

    input = seizurize_text(input);

    console::flushln(input);
  }
});
