type SmthType = struct {
	txt : type Smth = struct {
		txt : int
	}
}

type NumType = struct {
	num : int,
} @overload {
	fn "+" (self: NumType, value : int) -> int => {
		return self.num + value;
	}

	fn "/" (self: NumType, value : int) -> int => {
		return self.num / value;
	}	
}

let => @mult_scope [$ident = adder_int, $t = type : int] const $ident = fn (first : dyn, second : $t) -> $t => {
	print("multiplyer");
	return first * second;
};

let @float_mult_scope => @mult_scope [$ident = adder_float, $t = type : float];

=> @mult_scope [];
=> @float_mult_scope [];
=> @float_mult_scope [$ident = adder_flt];

const main = fn () => {
	print(adder_float(90, 10.6));

	print("overload");
	let mut num = NumType{num : 10};
	let res : int = num + 10;
	print(res);
	let res : int = (num + 90) / 2;
	print(res);
	print("overload done");
	
	
	let mut d = comp, 0 => list[list[2, 0, 4], list[1, 9, 3], list[4, 7, 9], list[9, 0, 1]];
	let lst = list<int>[2, 0, 4];
	
	let mtchfn = fn match str -> int {
		"hello" => 0,
		_ => 10
	};
	
	print(mtchfn("he"));

	let match_res : int = match "hello" {
		"hello" => 0,
		_ => 10,
	};
	print(match_res);
	
	let => @add_scope [$first = 9.0, $second = 11.0, $t = type : float] {
		let sum : $t = comp, 0 => $first + $second;
		print("add_scope: ");
		print(sum);
	};
	
	=> @add_scope [];
	
	=> @add_scope [$first = (5.0 / 99.0)];
	
	// Now it doesn't create a new scope overriding the default implementation
	=> @add_scope [$first = 30, $second = 70, $t = type : int] {{}};
	print(sum + 10);
	
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
};
