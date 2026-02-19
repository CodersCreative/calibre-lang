def fib_gen(n):
    i = 0
    a, b = 0, 1
    while i < n:
        yield a
        nxt = a + b
        a = b
        b = nxt
        i += 1

if __name__ == "__main__":
    n = 30
    for v in fib_gen(n + 1):
        print(v)
