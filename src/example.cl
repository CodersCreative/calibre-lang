/*import "env/whatever.cl"  will import the file at this location
by default when code is imported it will run all the code not in functions in order
the pub keyword is used to expose objects to other files */

enum Language {
	FRENCH { data : int},
	ENGLISH,
	SPANISH,
}

struct CountryBase {
	language : Language,
}

struct Country {}

impl CountryBase {
	fn get_language(&self) -> Language {
		self.language
	}
}

// language cannot change || it is a constant
let language = Language.FRENCH{data : 1};
const language_forced : Language = Language.ENGLISH;

let mut x = 100;
let mut b : (int, int) = (10, 10);


let mut y : int = 0;
let mut list_dyn = [0, 10, 30, 50]; // Arrays are dynamic by default
let mut lst : list(int) = [0, 40, 20]; // A static array 

for l in lst {
  x += l;
}

for i in 0..=100 {
  y += i;
}

for x == y {
  print(x + y);
}

fn main(b : (int, int)) -> int {
print(b);
	x = 50;
	x++; // should increment by 1
	x += 4 // same as x = x + 4
	print(x);
 
	let mut x = true;

	let z = "a";
	let w = "b";
	let d = z + w ; // Should give "ab"
	
	print(d + " ; " + x + " ; " + y);
	print(x);
	print(y);
	print("Hello World") // Semicolons have no effect so they can be left out

	return y;
}

main(b)
