var foo : float = 50;

struct Language {
	code : int,
}

struct Country {
  people : int,
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

let zimbabwe : Country = {people : 10, language : {code : 19}}

foo(20)
foo++;
print(foo)
print(zimbabwe)
