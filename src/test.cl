var foo : float = 50;

struct Language {
	code : int,
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

let zimbabwe : Country = {name : "Zimbabwe", people : 10, language : {code : 19}}

fn main(number : int) : string{
  fn add (x : int, y : int) : int {
    x + y
  }

  add(number, 100)
}

print(main(100));

/*foo(20)

let fb = "foo_bar";

foo++; 
let foo_bar = foo + 100; let bar_foo = 1 / foo_bar;

print(foo_bar)
print(bar_foo)
print(fb)
print(zimbabwe)
print(foo)*/
