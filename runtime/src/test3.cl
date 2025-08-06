struct Language {
	code : int,
}

impl Language {
  fn new(code : int) -> Language {
    return {code : code}
  }

  fn print_code(&mut self) -> int {
    print(self.code);
    self.code = 80;
    print(self.code);
    self.code;
  }
}

let mut lang = Language.new(20);
lang.code = 40;
lang.print_code();
print(lang.code);

fn hello(time : int , day : int = 22, hour : int = lang.code){
    print(day, " at ", time, hour);
}
hello(50);

let mut num = 0
for i in 0..1000000{
  num += i;
}
print(num);
