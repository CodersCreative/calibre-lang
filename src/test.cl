var foo : float = 50;

print(foo)

struct Language {
	code : int,
  function : fn(int) -> string,
}

struct Country {
  people : int,
  name : string,
	language : Language,
}

let map = {
  x : 100,
  y : 32 * 2,
  foo,
  z : {
    bar : false,
  },
};

fn main(number : int) -> string{
  fn add (x : int, y : int) -> int {
    x + y
  }

  add(number, 100)
}

let zimbabwe : Country = {name : "Zimbabwe", people : 10, language : {code : 19, function :main}}
let lst : list(list(int)) = [[10, 10, 100], [0,10]];

fn hello_int(function : fn(int) -> string, amt : string) -> int {
  let first = function(amt);
  print(first);
  first
}

fn hello(function : fn(int) -> string, amt : string) {
  let first = function(amt);
  print(first);
}

print(hello);

// var hello : fn(int) -> string = main;

hello(main, "50");

var fb = "foo_bar";
/*foo(20)



foo++; 
let foo_bar = foo + 100; let bar_foo = 1 / foo_bar;

print(foo_bar)
print(bar_foo)
print(fb)

print(foo)*/

print(fb);
var fb : list(char) = fb;
print(fb);
print(zimbabwe)
print(lst)
