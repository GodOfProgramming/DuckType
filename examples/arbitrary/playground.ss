#!/usr/bin/env -S -- ss run

fn require(file) {
  ret req file;
}

let x = require("syntaxerr");

println x;
