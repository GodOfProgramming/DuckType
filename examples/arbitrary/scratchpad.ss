req "examples/lib/ansi.ss" as ansi;

use ansi::cursor;
use ansi::display;
use ansi::text;
use text::color;

text::scope(|| {
  color::fg(color::RED);
  color::bg(color::BLUE);
  println("colored");
});

println("basic");
