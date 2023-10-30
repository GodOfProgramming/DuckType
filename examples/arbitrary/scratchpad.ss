req "examples/lib/ansi.ss" as ansi;

use ansi::cursor;
use ansi::display;
use ansi::text;
use text::color;

text::scope(|| {
  color::paint(color::FOREGROUND, color::RGB(50, 100, 150));
  color::paint(color::BACKGROUND, color::RGB(150, 100, 50));
  println("colored");
});

println("basic");
