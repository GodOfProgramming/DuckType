
req "env" => env;
req "string" => str;
req "console" => console;

let value = str.parse_number(env.ARGV[0]);
console.write(value);
