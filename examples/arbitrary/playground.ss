fn add(x: i32 or f64, y: i32 or f64) => struct { x: number } {
  ret struct { x: x + y };
}

let sub = |x, y| => number {
  ret x - y;
};

println add(1, 2);

println sub(1, 2);
