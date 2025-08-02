import data
import * from data

// By default the type of a variable will be infered by what is being assigned to it.
// The let keyword creates an immutable variable that can be shadowed.
let language = Language.FRENCH{data : 10, code : 5};
let mut recursive_language = Language.ARABIC(language, Language.SPANISH);
print("Firsy");

// A simple match statement for enums with values.. If a specific ennum meember isnt required then it can be left out of the match.
// Mutability needs to be specified at the beginning for all branches
match &mut {
  data.Language.ARABIC(Language.FRENCH{data}, Language.ARABIC(Language.FRENCH{code : cod})) => {
    print("Code: " + cod);
    print("Enum: " + data)
    data = 10;
    print("Enum Changed: " + data)
  },
} (recursive_language)

// In order to do an if let statement this syntax is used with extra conditions being added after the let
if let data.Language.FRENCH{data} if true if ((try 9 as ulong) == 9.0 == 9) -> &language => {
  print("Enum: " + data)
}

// The const keyword creates an immutable variable that cannot be shadowed.
const language_forced : Language = Language.ENGLISH (6);

// By not putting any data by the match it will only check for the enum member.
// Ifs can be added for further selection.
match {
  Language.ENGLISH if false => print("Enum: ENGLISH?"),
  Language.ENGLISH => print("Enum: ENGLISH"),
}(language_forced)

// The mut keyword can be added for mutable variable that can be shadowed.
let mut y : int = 0;
print("Noo");
// If a type is specified the data will automatically be converted to the type.
let country : Country = Country(Language.SPANISH)
print("No");

// print function to output to stdout.
print(language_forced);

let mut x = 100;

// statements will automatically return the last value they use.
let mut b : <int, int> = (10, if x == 100 => {100} else => {10});
print("b -> " + b);

/* input function to get an input from the user.
A value can be inputted to this function and will outputted to the user.*/
let bg = std.console.input("Hello enter 1: ");

if trim(bg) == "Yes" => {
  print("Yes");
}

// Arrays are dynamic if some values are of a different type when it is created, otherwise a type will be infered.
let mut list_dyn = [0, 10, 30, "Hello"];

// The language has support for list comprehension to iter and filter an iterable value.
// Typed arrays are created using the following syntax:
let mut lst : list<int> = [x^2 for x in 0..100 if x % 2 == 0 if x % 8 != 0];

print("lst")
print(lst);// A static array

// A foreach loop.
for l in &mut lst => {
  l *= 2;
}
print("lst 2")
print(lst);// A static array
// A for loop using the recommended range syntax.
for i in 0..=100 => {
  y += i;
}

// A while loop.
for x == y => {
  print(x + y);
}

/* Functions require all variables to have a specified type including those with default values.
Return types of functions are recommended to specify their return type if they have any but not required*/
const range2 = fn (stop : int, start : int = 0, step : int = 10) -> list<int> => {
  range(start, stop * 2, step * 2)
}

// Loops also allow for function calls including the built-in range() function.
for i in range2(100, step = 25) => {
  print("val: " + i);
}

let smth_fn = fn (y : int) -> string!int => {
  if y > 10 => {
    return ok(y);
  }

  err("needs to be larger than 10")
}

match {
  // This is equivalent to Ok(x) if x == 18
  Ok(18) => print("Equated to 18"),
  // This is equivalent to Ok(x) if x in 0..20
  Ok(0..20) => print("Within range"),
  // This is equivalent to Ok(x) if x in [20, 30, 40, 50, 60, 70]
  Ok([20, 30, 40, 50, 60, 70]) => print("Within list"),
  Ok(x) => print(x),
  Err(x) => print(x),
} (smth_fn(20))

// The previous function was shadowed
let smth_fn = fn (y : int) -> int? => {
  if y > 10 => return some(y);

  some(9)
}

match {
  Some(x) if x == 18 => print("Equated to 18"),
  Some(x) => print(x),
  None => print("none"),
} (smth_fn(18))

match int {
  1 => print("one"),
  2 => print("two"),
  3 => print("three"),
  4 => print("four"),
  5 => print("five"),
  // A similar syntax for ranges and lists and tuples can be used without the need for an enum.
  [6 ,7 ,8 ,9 ,10] => print("6 to 10"),
  // A similar syntax for ranges and lists and tuples can be used without the need for an enum.
  11..=20 => print("11 to 20"),
  // The Let keyword can be used to reference all other options whilst passing off the data
  Let(data) if data > 200 => print("value is > 200 "),
  // if the name is not in scope it can be used without the Let keyword
  data => print("value is : " + data),
}(16)

// TODO Implement generics with dyn<Add, Sub, Mul...>
// TODO Allow traits of generics to be easily specified and add to new type system. Like : "type Output = dyn<...traits...>;"
// TODO Also allow for <T : dyn<...traits...>> to show common types.

// Using this syntax for results will assume a dynamic value for the error;
const main = fn () -> !int => {
  // Values can be manually coercced, but using this method will return a result type.
  let x : !int = "50" as int;

  // The result type will be unwrapped if an operation forces it to unwrap or if its type is forced.
  let u = x + 70;
  print("Unsafe : " + u);

  // The try keyword can be used to automatically defer errors when inside a function
	let mut x : long = try x + try "58" as uint;

	x++; // should increment by 1
	x += 4 // same as x = x + 4
	print(x);

	let mut x = true;

	let z = "a";
  if z == "a" => {
    let w = "b";
    let d = z + w ; // Should give "ab"

    print(d + " ; " + x + " ; " + y);
  }

	print(x);
	print(y);
  let sentence = "Hello, World!";

  match sentence{
    // The Prefix and Suffix keywords can be used to pattern match on a string easily.
    Prefix("Hello,", line) if false => print(trim(line)),
    Suffix(", World!", line) => print(trim(line))
  }

	y
}

// the in keyword can be used to see if a certain value is contained by a string or list.
if 4 in [4, 16, 32] => {
  print("main result: " + main());
  print("Success, Random Num = " + (std.random.generate(10..=1000) as uint))
}
