def fib(n)    
    return n if n < 2
    return fib(n - 1) + fib(n - 2); 
end

for i in 0..30
    puts(fib i)
end
