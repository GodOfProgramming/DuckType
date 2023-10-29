req "examples/lib/ansi" as ansi;

use ansi::cursor;
use ansi::display;
use ansi::text;
use text::color;

text::scope(|| {
  color::fg(color::RED);
  color::bg(color::BLUE);
  println("HI");
});
