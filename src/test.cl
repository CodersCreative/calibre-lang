let mut foo : float = 50;

print(foo)

struct Language {
	code : int,
}

impl Language {
  fn print_lang(&mut self) {
    print("test");
    if self.code == 50 {
      print(self);
      return null;
    }

    print(self.code);
  }

  fn new() -> Language {
    {code : 90}
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

let mut zimbabwe : Country = {name : "Zimbabwe", people : 10, language : {code : 19}}

let english : Language = {code : 25, function : main}
// zimbabwe.language.code = 50;


print(zimbabwe.language.print_lang());
Language.new().print_lang();

print(zimbabwe.language.code)
let lst : list(list(int)) = [[10, 10, 100], [0,10]];

fn hello_int(function : fn(int) -> string, amt : string) -> int {
  let first = function(amt);
  print(first);
  first
}

let mut t = "bye";
fn test_again(txt : string) {
  //txt = "hello8"
  print(txt);
}

test_again("JI")

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
