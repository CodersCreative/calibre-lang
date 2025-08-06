class Language:
    def __init__(self, code) -> None:
        self.code = code;

    def print_code(self) -> None:
        print(self.code);
        self.code = 80;
        print(self.code);
        self.code;


lang = Language(20);
lang.code = 40;
lang.print_code();
print(lang.code);


def hello(time, day = 22, hour = lang.code):
    print(day, " at ", time, hour);
hello(50);

for i in range(1000000):
  lang.code += i;

print(lang.code);
