from itertools import islice

def fib():
    a = 0
    b = 1
    while True:
        yield a
        a, b = b, a + b

if __name__ == "__main__":
    values = islice(fib(), 31)
    
    for i in values:
        print(i)
