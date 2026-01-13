type SmthType = struct {txt : str}

let => <mult_scope>[$ident = adder_int, $t = type : int] {
	const $ident = fn (first, second : $t) -> $t => {
		print("multiplyer");
		return first * second;
	}
}

const adder_int = => <mult_scope>[];
const adder_float = => <mult_scope>[$ident = adder_float,$t = type : float];

const main = fn () => {
	print(adder_float(90, 10.6));
	let smth = SmthType{
		txt : "tyui",
	};
	let mut d = comp, 0 => list[list[2, 0, 4], list[1, 9, 3], list[4, 7, 9], list[9, 0, 1]];
	let lst = list<int>[2, 0, 4];

	let mtchfn = match str -> int{
		"hello" => 0,
		_ => 10,
	};

	print(mtchfn("he"));

	let => <add_scope>[$first = 9.0, $second = 11.0, $t = type : float] {
		let sum : $t = comp => $first + $second;
		print("add_scope: ");
		print(sum);	
	};

	=> <add_scope>[];

	=> <add_scope>[$first = 5.0];	

	=> <add_scope>[$first = 30, $second = 70, $t = type : int];

	let mut correct = -10;
	let mut other = -10;

	let hello = try ok("Yesssss") : hello => {
		print("Nooooo");
	};

	print(hello);

	(false) ? (8 == 9) ? correct : other : other = 0;

	print(correct);
	print(other);
	// for i in 0..5 => print(i);
	print("Ho");
	// debug {d[0][0]};
};
