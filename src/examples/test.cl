type Test = struct(list);
let mut lst = [0 for x in 0..90 if x % 2 == 0];

const change = fn(l : &mut list) => l[std.random.generate(0..=10) as int] = 80;
print(lst);
change(lst)
print(lst);
let mut test = Test(lst);
change(test.0)
print(test);
