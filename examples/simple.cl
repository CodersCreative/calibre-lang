const main = fn () => {
    print(factorial(5));
};

const factorial = fn (x : int) -> int => if x <= 1 => return 1 else => x * factorial(x - 1);
