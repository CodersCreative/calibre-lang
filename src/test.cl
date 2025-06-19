let mut foo : float = 50;

print(foo)

struct Language {
	code : int,
  function : fn(int) -> string,
}

impl Language {
  fn print_lang(&self) {
    print(self);
  }
}

struct Country {
  people : int,
  name : string,
	language : Language,
}

struct Test {
  x : int,
  y : float,
}

let map : Test = {
  x : 100,
  y : 32 * 2,
  foo,
  z : {
    bar : false,
  },
};

fn test(test : Test) {
  print(test)
}

fn main(number : int) -> string{
  fn add (x : int, y : int) -> int {
    x + y
  }

  add(number, 100)
}

let zimbabwe : Country = {name : "Zimbabwe", people : 10, language : {code : 19, function :main}}

let english : Language = {code : 20, function : main}
// english.print_lang()

let lst : list(list(int)) = [[10, 10, 100], [0,10]];

fn hello_int(function : fn(int) -> string, amt : string) -> int {
  let first = function(amt);
  print(first);
  first
}

let mut t = "bye";
fn test_again(txt : &mut string) {
  txt = "hello8"
  //print(t);;
}

t = "hello4";
test_again(t);
print(t);

fn hello(function : fn(int) -> string, amt : string) {
  let first = function(amt);
  print(first);
}

// print(hello == hello);

if hello != hello{
  print("hello");
}else if false{
  print("No")
}else {
  print("Yes")
}

//let b = true && true;

// print(b);

// var hello : fn(int) -> string = main;

// hello(main, "50");

print(lst)
print(lst(0)(0))
print(map)

let mut fb = "foo_bar";
/*foo(20)

foo++; 
let foo_bar = foo + 100; let bar_foo = 1 / foo_bar;

print(foo_bar)
print(bar_foo)
print(fb)

print(foo)

print(fb);
var fb : list(char) = fb;
print(fb);
print(zimbabwe)
print(lst)*/

// test(map);
