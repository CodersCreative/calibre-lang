
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

print(obj.time.day);
obj.time.day = 30;
print(obj.time.day);
