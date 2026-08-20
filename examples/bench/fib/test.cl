fn fib (n : int) -> int {
  if n < 2 {
    return n;
  }
  
  return fib(n - 1) + fib(n - 2);
}

let n = 28;
let result = fib(n);
print(result);
