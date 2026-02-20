def fib(n)    
    return n if n < 2
    return fib(n - 1) + fib(n - 2); 
end

n = 30;
result = fib n;
puts result
