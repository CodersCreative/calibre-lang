const fib = fn(n : int) -> int => (n < 2) ? n : fib(n - 1) + fib(n - 2);

const main = fn() => {
    let n = 15;
    let result = fib(n);
    print(result);
};
