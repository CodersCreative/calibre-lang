fn fib(n) {
    let a = 0;
    let b = 1;
    for i in 0..n {
        let temp = a + b;
        a = b;
        b = temp;
    }
    a
}

let n = 28;
let result = fib(n);
print(result);
