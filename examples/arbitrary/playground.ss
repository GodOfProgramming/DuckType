req "examples/lib/ansi.ss" as ansi;

use ansi::display;

println("before");

display::clear();

println("after");
