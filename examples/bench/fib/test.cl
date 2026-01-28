const fib = fn(n : int) -> int => (n < 2) ? return n : return fib(n - 1) + fib(n - 2);

const main = fn() => {
    let n = 15;
    let result = fib(n);
    print(result);
};
