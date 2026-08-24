fib : I64 -> I64
fib = |n| {
    if n < 2 {
        return n
    }

    return fib(n - 1) + fib(n - 2)
}

main! = |_args| {
    n = 28
    result = fib(n)
    echo!(result.to_str())
    Ok({})
}
