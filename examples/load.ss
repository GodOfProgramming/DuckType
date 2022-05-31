req "examples/lib.ss" => func;
req "examples/dylib.ss" => dylib;
req dylib => dyfunc;

func("hello world");
dyfunc("hello", "world");
