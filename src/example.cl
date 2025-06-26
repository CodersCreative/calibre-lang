/*import "env/whatever.cl"  will import the file at this location
by default when code is imported it will run all the code not in functions in order
the pub keyword is used to expose objects to other files */

enum Language {
	FRENCH { data : int},
	ENGLISH { int},
	SPANISH,
}

struct CountryBase {
	language : Language,
}

struct Country {Language}

impl CountryBase {
	fn get_language(&self) -> Language {
		self.language
	}
}

// language cannot change || it is a constant
let language = Language.FRENCH{data : 1};
const language_forced : Language = Language.ENGLISH {6};

let country : Country= {Language.SPANISH}

print(language_forced);

let mut x = 100;
let mut b : (int, int) = (10, 10);


let mut y : int = 0;
let mut list_dyn = [0, 10, 30, 50]; // Arrays are dynamic by default
let mut lst : list(int) = [x^2 for x in 0..100 if x % 2 == 0];

print(lst);// A static array 

for l in lst {
  x += l;
}

for i in 0..=100 {
  y += i;
}

for x == y {
  print(x + y);
}

fn range2(stop : int, start : int = 0, step : int = 10) -> list(int) {
  range(start, stop * 2, step * 2)
}

for i in range2(100, step = 10) {
  print("val: " + i);
}

fn main() -> int {
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

main()
