extern "c" const c_abs = fn(x: int) -> int from "libc" as "abs";

const main = fn() => {
	let value = c_abs(-7);
	print(value);
};
