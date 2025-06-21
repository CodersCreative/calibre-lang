let mut second = 0;
let mut first = 0;

for true{
  if first > 9 {
    second++;
    if second > 30{
      print("Done")
      print(second);
      print(first);
      break;
    }
  }else{
    first++;
  }
}

print("Done")


struct Time {
	day : int,
}

struct Language {
	code : int,
  time : Time
}

let mut obj : Language = {code : 20, time : {day : 49}};

impl Language {
  fn new() -> Language {
    {code : 90, time : {day : 50}}
  }

  fn print_code(&self) {
    print(self.code)
  }
}

obj.print_code();

print(Language.new())

if obj.code == 20{
  print(obj.code);
}

let mut lst : list(number) = [0, 40, 20]; // A static array 
lst[1] = 80;
print(lst[1]);
obj.time.day = 30;

print("Starting")
for false {
  for l in &mut lst {
    l = l + 50;
  }
  print(lst)
}


for l in lst {
  print(l);
}
print(obj.time.day);
obj.time.day = -30;
print(obj.time.day);
