type SmthType = struct {txt : str}

const main = fn () => {
	let smth = SmthType{
		txt : "tyui",
	};
	let mut d = list<list<int>>[list<int>[2, 0, 4], list<int>[1, 9, 3], list<int>[4, 7, 9], list<int>[9, 0, 1]];
	let lst = list<int>[2, 0, 4];

	let mtchfn = match str -> int{
		"hello" => 0,
		_ => 10,
	};

	print(mtchfn("he"));
	// for i in 0..5 => print(i);
	print("Ho");
	// debug {d[0][0]};
};
