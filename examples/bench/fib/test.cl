const fib = fn(n : int) -> int => {
    if n < 2 => return n;
    fib(n - 1) + fib(n - 2);
};

const main = fn() => {
    let n = 35;
    let result = fib(n);
    print(result);
};
