fn fib(n){
    if n < 2 {
        n
    } else {
      fib(n-1) + fib(n-2)  
    }
}

for i in 0..31 {
    print(fib(i));
}

