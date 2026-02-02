from dataclasses import dataclass


@dataclass
class Pair:
    a: int
    b: int


def sum_pairs(n: int) -> int:
    pairs = [Pair(a=i, b=i + 1) for i in range(n)]
    acc = 0
    for i in range(n):
        p = pairs[i]
        acc += p.a + p.b
    return acc


def main() -> None:
    n = 2000
    print(sum_pairs(n))


if __name__ == "__main__":
    main()
