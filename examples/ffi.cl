extern "c" const c_abs = fn(int) -> int from "libc" as "abs";
extern "c" const c_strlen = fn(str) -> int from "libc" as "strlen";

const main = fn() => {
	let value = c_abs(-7);
	print(value);
	let value = c_strlen("hello");
	print(value);	
};
