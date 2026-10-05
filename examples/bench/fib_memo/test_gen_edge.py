def fib():
    a = 0
    b = 1
    while True:
        yield a
        a, b = b, a + b

if __name__ == "__main__":
    values = fib()
    
    for i in range(31):
        print(values())
