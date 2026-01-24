/* Future generic type syntax pseudocode
type SmthType<T> = struct {
	value : T
} @overload {
	fn "+" (self: SmthType<T>, value : T) -> T => {
		return self.value + value;
	}

	// Type specific override
	fn "/" (self: SmthType<int>, value : int) -> int => {
		return self.value / value;
	}
}

const generic = fn<T>(smth : SmthType<T>, other : T) -> T => {
	let res : T = smth + other;
	res;
};

const main = fn() => {
	// Atleast at this stage of the language's development we'll just force people to type generics with minimal to no inference for compiler simplicity.
	// However I'll try to make it infer at var declarations and operator overrides...
	let res = generic:<int>(SmthType<int>{value : 10}, 10);
	print(res);	
};

trait Person {
	// This doesn't have a default implementation it only specifies the type of the function that must be implemented.
	const name : fn(&Self) -> str;

	// Default implementation
	const planet = fn() -> str => {
		"Earth";	
	};

	// This can use other functions that need to be implied by the trait
	const name_and_planet = fn(self: &Self) -> <str, str> => {
		tuple(self.name(), Self.planet());
	};
}

trait Student : Person {
	const university : fn(&Self) -> str;

	// You can use functions defined in implied traits
	const greeting = fn(self : &Self) -> str => {
		"Hello, I'm " & self.name() & " and I learn at " & self.university() & " which is on planet " & self.planet() & "."; 
	};
}

trait Programmer {
	const fav_language : fn(&Self) -> str;
}

// Anytime CompSciStudent is used it will automatically assume that Programmer and Student are implemented instead of constantly having to mention Programmer + Student
trait CompSciStudent : Programmer + Student {
	const git_username : fn(&Self) -> str;
}
*/

// All string functions need to take in 2 lists with the first one needing to be a list<str>, however they can have any return type
// This just showcases how to have a basic format function like in rust
const fmt = fn(splits : list:<str>, inputs : list:<str>) -> str => {
	let mut txt : str = "";

	for i in len(splits) => {
		txt &= splits[i];
		if i < len(inputs) => txt &= inputs[i];
	};
	
	txt;
};


// Other potential configs
// For intaking a variety of different types
// const fmt = fn(splits : list<str>, ..inputs : list<dyn>) -> str => ...
// Or for taking advantage of generics
// const fmt = fn<T>(splits : list<str>, ..inputs : list<T>) -> str => ...
// It will also allow for all the inputs to be individually wriiten like this:
// const fmt = fn(splits : list<str>, num : int, txt : str) -> str => ...// 


type NumType = struct {
	num : int,
} @overload {
	// TODO Add support for Self type
	const "+" = fn(self: NumType, value : int) -> int => {
		return self.num + value;
	}

	const "/" = fn(self: NumType, value : int) -> int => {
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

const print_text = fn (values : list:<str>) => {
	print(values);
};

const main = fn () => {
	let name = "Ty";
	let age = "10";
	// In these string function blocks "{{}}" can be used to mean "{}"
	let txt : str = fmt"Hello, my name is {name} and I'm {age} years old!";
	print(txt);
	
	print(adder_float(90, 10.6));

	print("piping");
	40 |: forty> adder_float(forty) |> $(80) |> print;

	print("overload");
	let mut num = NumType{num : 10};
	let res = num + 10;
	print(res);
	let res = (num + 90) / 2;
	print(res);
	print("overload done");
	
	// The inputs get combined into a list
	print_text("1", "2", "6");
	
	let mut d = comp, 0 => list[list[2, 0, 4], list[1, 9, 3], list[4, 7, 9], list[9, 0, 1]];
	let lst = list:<int>[2, 0, 4];
	
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

	// until allows you to end test the loop with the condition only breaking at the end of each iteration
	for i in 0..10 => print("hello") until true;

	// until also works with list comprehensions
	let lst = list[10 for i in 10 until i > 4];
	print(lst);	
	
	print(correct);
	print(other);
	
	// for i in 0..5 => print(i);
	print("Ho");
}
