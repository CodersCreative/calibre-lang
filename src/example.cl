/*import "env/whatever.cl"  will import the file at this location
by default when code is imported it will run all the code not in functions in order
the pub keyword is used to expose objects to other files */

struct CountryBase {
	language : Language,
}

struct Country {}

impl CountryBase {
	fn get_language(&self) : Language {
		self.language
	}
}


enum Language {
	FRENCH { data : int},
	ENGLISH,
	SPANISH,
}

// language cannot change || it is a constant
let language = Language.FRENCH;
let language_forced : Language = Language.FRENCH;

let mut x = 100;
let mut y : number = 0;
let mut list_dyn = [0, 10, 30, 50]; // Arrays are dynamic by default
let mut lst : list(number) = [0, 40, 20]; // A static array 

for l in lst {
  x += l;
}

for i in 0..100 {
  t += i;
}

for x == y {
  print(x + y);
}

fn main() -> number {
	x = 50;
	x++; // should increment by 1
	x += 4 // same as x = x + 4
 
	var x = true;

	let z = "a";
	let w = "b";
	let d = z + w; // Should give "ab"
	
	print(x);
	print(y);
	print("Hello World") // Semicolons have no effect so they can be left out

	return y;
}

main())
