/*import "env/whatever.cl"  will import the file at this locatioon
by default when code is imported it will run all the code not in functions in order*/

// Enums declaration
enum Language {
  // Enums can have hasmap type data structuress.
	FRENCH { data : int, code : int},
  // Enums can have tuple type data structuress.
	ENGLISH {int},
	SPANISH,
}

// Struct declaration
struct CountryBase {
  // Fields in a struct must be explicitly typed.
	language : Language,
}

// Structs can also be declaratied using a tuole structures.
struct Country {Language}

// To add static functions to an object (struct or enum) the impl keyword is used.
impl CountryBase {
  // In impl blocks the self keyword can be used to referece the object variable.
  // Methods in the impl block can also be invoked by Object.function()
	fn get_language(&self) -> Language {
		self.language
	}
}

// By default the type of a variable will be infered by what is being assigned to it.
// The let keyword creates an immutable variable that can be shadowed.
let language = Language.FRENCH{data : 1, code : 5};

// A simple match statement for enums with values.. If a specific ennum meember isnt required then it can be left out of the match.
match language {
  Language.FRENCH{data} -> print("Enum: " + data),
}
// The const keyword creates an immutable variable that cannot be shadowed.
const language_forced : Language = Language.ENGLISH {6};

// By not putting any data by the match it will only check for the enum member.
// Ifs can be added for further selection.
match language_forced {
  Language.ENGLISH if false -> print("Enum: ENGLISH?"), 
  Language.ENGLISH -> print("Enum: ENGLISH"),
}

// The mut keyword can be added for mutable variable that can be shadowed.
let mut y : int = 0;

// If a type is specified the data will automatically be converted to the type.
let country : Country= {Language.SPANISH}

// print function to output to stdout.
print(language_forced);

let mut x = 100;
let mut b : (int, int) = (10, 10);

/* input function to get an input from the user. 
A value can be inputted to this function and will outputted to the user.*/
let bg = input("Hello enter 1: ");

if trim(bg) == "Yes" {
  print("Yes");
}

// Arrays are dynamic if some values are of a different type when it is created, otherwise a type will be infered.
let mut list_dyn = [0, 10, 30, "Hello"]; 

// The language has support for list comprehension to iter and filter an iterable value.
// Typed arrays are created using the following syntax:
let mut lst : list(int) = [x^2 for x in 0..100 if x % 2 == 0 if x % 8 != 0];

print(lst);// A static array 

// A foreach loop.
for l in lst {
  x += l;
}

// A for loop using the recommended range syntax.
for i in 0..=100 {
  y += i;
}

// A while loop.
for x == y {
  print(x + y);
}

/* Functions require all variables to have a specified type including those with default values.
Return types of functions are recommended to specify their return type if they have any*/
fn range2(stop : int, start : int = 0, step : int = 10) -> list(int) {
  range(start, stop * 2, step * 2)
}

// Loops also allow for function calls including the built-in range() function.
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
  if z == "a" {
    let w = "b";
    let d = z + w ; // Should give "ab"
    
    print(d + " ; " + x + " ; " + y);
  }

	print(x);
	print(y);
	print("Hello World") // Semicolons have no effect so they can be left out

	return y;
}

// the in keyword can be used to see if a certain value is contained by a string or list.
if 4 in lst {
  main()
  print("Success")
}
