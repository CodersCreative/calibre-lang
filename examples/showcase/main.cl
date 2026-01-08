import data;
import * from data;

// The const keyword creates an immutable variable that cannot be shadowed.
const language_forced : Language = Language.ENGLISH(6);

// The main function is the entry point to every calibre lange program
const main = fn () => {
	// By default the type of a variable will be infered by what is being assigned to it.
	let language = Language.FRENCH{data : 10, code : 5};
	let mut recursive_language = Language.ARABIC(language, Language.SPANISH);
	
	/* A simple match statement for enums with values.. If a specific ennum meember isnt required then it can be left out of the match.
		Mutability needs to be specified at the beginning for all branches
		match statements need to be called, either using a pipe or brackets as they're considered functions
	*/
	recursive_language |> match &mut Language {
		data:Language.ARABIC(Language.FRENCH{_}, Language.ARABIC(Language.FRENCH{code})) => print("Code: " & code),
		data:Language.ARABIC(Language.FRENCH{data}, Language.SPANISH) => {
			print("Enum: " & data);
			data = 5;
			print("Enum Changed: " & data);
		}
	};
	
	/* In order to do an if let statement this syntax is used with extra conditions being added after the let
		if let Language.FRENCH{data} if true if ((try 9 as ulong) == 9.0 == 9) <- &language => {
		    print("Enum 2: " & data)
		  }
		By not putting any data by the match it will only check for the enum member.
		Ifs can be added for further selection.
	*/
	language_forced |> match dyn {
		Language.ENGLISH if false => print("Enum: ENGLISH?"),
		Language.ENGLISH => print("Enum: ENGLISH")
	};
	
	// The mut keyword can be added for mutable variable that can be shadowed.
	let mut num : int = 0;
	
	// If a type is specified the data will automatically be converted to the type.
	let country = Country(Language.SPANISH);
	
	// print function to output to stdout.
	print(language_forced);
	
	let mut x = 100;
	
	// statements will automatically return the last value they use.
	let mut b : <int, int> = tuple(10, if x == 100 => 100 else => 10);
	print("b -> " & b);
	
	/* input function to get an input from the user.
		A value can be inputted to this function and will outputted to the user.
	*/
	if trim(unwrap(std.console.input("Hello enter 1: "))) == "Yes" => print("Yes");
	
	// Arrays are dynamic if some values are of a different type when it is created, otherwise a type will be infered.
	let mut list_dyn = [0, 10, 30, "Hello"];
	
	// The language has support for list comprehension to iter and filter an iterable value.
	let mut lst : list<int> = [x ** 2 for x in 0..100 if x % 2 == 0 if x % 8 != 0];
	
	print("lst");
	print(*lst);
	
	// A static array
	for l in &mut lst => {
		*l *= 2;
	};
	print("lst 2");
	print(*lst);
	
	// A static array
	for index in 1..=100 => num += index;
	
	// A while loop.
	for x == num => print(x + num);
	
	// Loops also allow for function calls including the built-in range() function.
	for i in range2(100, step = 25) => {
		print("val: " & i);
	};
	
	let smth_fn = fn (y : dyn) -> str!int => {
		if y > 10 => {
			return y as str!int;
		};
		
		err("needs to be larger than 10");
	};
	
	18 |> smth_fn |> match dyn {
		// This is equivalent to Ok(x) if x == 18
		Ok(18) => print("Equated to 18"),
		// This is equivalent to Ok(x) if x in 0..20
		Ok(0..20) => print("Within range"),
		// This is equivalent to Ok(x) if x in [20, 30, 40, 50, 60, 70]
		Ok([20, 30, 40, 50, 60, 70]) => print("Within list"),
		Ok(x) => print(x),
		Err(x) => print(x)
	};
	
	// The previous function was shadowed
	let smth_fn = fn (y : int) -> int? => {
		if y > 10 => return y as int?;
		
		some(9);
	};
	
	// Closures can be used to easily represent the previous results from pipes.
	200 |> smth_fn |> fn ($ : dyn) => print("This + That = " & $);
	
	18 |> smth_fn |> match dyn {
		Some(x) if x == 18 => print("Equated to 18"),
		Some(x) => print(x),
		None => print("none")
	};
	
	16 |> match int {
		1 => print("one"),
		2 => print("two"),
		3 => print("three"),
		4 => print("four"),
		5 => print("five"),
		// A similar syntax for ranges and lists and tuples can be used without the need for an enum.
		[6, 7, 8, 9, 10] => print("6 to 10"),
		// A similar syntax for ranges and lists and tuples can be used without the need for an enum.
		11..=20 => print("11 to 20"),
		// The Let keyword can be used to reference all other options whilst passing off the data
		Let(data) if data > 200 => print("value is > 200 "),
		// if the name is not in scope it can be used without the Let keyword
		data => print("value is : " & data)
	};
	
	secondary(num);
	tertiary();
};

// Return types and parameters of functions are recommended to have a specified type but it's not required
const range2 = fn (stop : int, start : int = 0, step : int = 10) -> list<int> => range(start, stop * 2, step * 2);

/* TODO Implement generics with dyn<Add, Sub, Mul...>
TODO Allow traits of generics to be easily specified and add to new type system. Like : "type Output = dyn<...traits...>;"
TODO Also allow for <T : dyn<...traits...>> to show common types.
Using this syntax for results will assume a dynamic value for the error;
*/
const secondary = fn (num : int) -> dyn!int => {
	// Values can be manually coercced, but using this method will return a result type.
	let x : dyn!int = "50" as int;
	
	// The result type will need to be unwrapped using the unwrap or unwrap_err methods. The unwrap method also works for options.
	let u : int = unwrap(*x) + 70;
	print("Unsafe : " & u);
	
	// The try keyword can be used to automatically defer errors when inside a function
	let mut x : int = unwrap(*x) + try {{
		"58" as int;
	}};
	
	x += 4;
	
	// same as x = x + 4;
	print(x);
	
	let mut x = true;
	
	let z = 'a';
	if z == 'a' => {
		let w = 'b';
		let d = z & w;
		
		// Should give "ab"
		print(d & " ; " & x & " ; " & num);
	};
	
	print(x);
	print(num);
	
	number;
};

const tertiary = fn () => {
	// Currying works by first seeing if all the arguments have been met and if not creating a new function that only requires the remaining arguments.
	let currying = fn (a : int, b : int, c : int) -> int => a * b * c;
	
	// This is an example of currying using pipes
	let currying_2 = 18 |> 20 |> currying;
	let currying_2 = 15 |> currying_2;
	print("Curryed " & currying_2);
	
	// Currying also works using the normal call syntax
	print("Curryed " & currying(18)(20, 15));
	
	// The is keyword can be used to check types.
	if 4 is number => print("Yay!! 4 is a number");
	if !(4 is list<str>) => print("Yay!! 4 is not a list of strings");
	
	// the in keyword can be used to see if a certain value is contained by a string or list.
	print("__name__ = " & __name__);
	
	if (4 in [4, 16, 32]) && __name__ == "__main__" => print("Success, Random Num = " & (std.random.generate(10..=1000) as int));
};