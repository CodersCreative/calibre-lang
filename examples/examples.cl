const test_str = fn(splits : list:<str>, inputs : list:<str>, extra : bool, extra2 : int) => {
	print(move splits);
	print(move inputs);
	print("extra : " & extra & " and " & extra2);
};

const main = fn () => {
	let name = "Ty";
	let age = "10";
	test_str"Hello, my name is {name} and I'm {age} years old!" <(true, 900003);

	print(bmi(52.5, 1.65));
	print(factorial(5));
	print(lock(3, 4,6));
	a_fn();
	a_two_fn();

	let tri = is_a_triangle(10f, 20f);
	print(tri);
	let tri = tri(10f);
	print(tri);
};

const a_fn = fn() => {
	let mut a = 1;
	print(a + => {
		a += 1;
		a;
	});
}

const a_two_fn = fn() => {
	let mut a = 1;

	let f = fn() => {
		a += 1;
		a;	
	};

	print(a + f());
}

const bmi = fn (mass height : float) -> float => mass / height ** 2;

const is_a_triangle = fn (a b c : float) -> bool => a + b > c && b + c > a && c + a > b;

const heron = fn (a b c : float) -> float => {
	let p = (a + b + c) / 2;
	(p * (p - a) * (p - b) * (p - c)) ** 0.5;
};

const area_of_triangle = fn (a b c : float) -> float? => if is_a_triangle(a, b, c) => return heron(a, b, c) else => none;

const factorial = fn (x : int) -> int => if x <= 1 => return 1 else => x * factorial(x - 1);

const lock = fn(c d e:  int) -> bool => if c*d> e => true else => false ;
