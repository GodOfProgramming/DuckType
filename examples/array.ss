req "std" => std;

let array = std.Array(1, 2, 3);

print array;
print array[0];
print array[1];
print array[2];

array.push("hello", "world");

print array;

fn add_to_array(arr) {
  arr.push(1);
}

add_to_array(array);

print array;
print array.len();
