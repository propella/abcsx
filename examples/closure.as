function hello() {
    var free = "Hello ";
    return function (arg) {
	print(free + arg);
    }
}

var func = hello();
func("world");

