import sys

def fib(n: int) -> int:
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

sys.set_int_max_str_digits(100000000)

if __name__ == "__main__":
    n = 1000000
    result = fib(n)
    print(result)
