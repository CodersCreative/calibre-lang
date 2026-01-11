type SmthType = struct {txt : str}

const main = fn () => {
	let smth = SmthType{
		txt : "tyui",
	};
	let mut d = list[list[2, 0, 4], list[1, 9, 3], list[4, 7, 9], list[9, 0, 1]];
	let lst = list<int>[2, 0, 4];

	let mtchfn = match str -> int{
		"hello" => 0,
		_ => 10,
	};

	print(mtchfn("he"));

	let mut correct = -10;
	let mut other = -10;

	(false) ? (8 == 9) ? correct : other : other = 0;

	print(correct);
	print(other);
	// for i in 0..5 => print(i);
	print("Ho");
	// debug {d[0][0]};
};
