# DuckType

## Introduction

It's syntax is _mostly_ similar to Rust in many ways. Currently 4x - 6x slower than Ruby for simple tests/benchmarks

### TODO list

- Need to group logical operators together to short circuit properly

### Notes about this godforsaken idea

function calls should not execute by creating a real stack frame

instead they should tell the vm "my context is the next to execute" and create a new stack frame, etc...

when the vm returns in the context loop, if there are more stack frames continue looping

or in other words, loop while stack frames pop == some
