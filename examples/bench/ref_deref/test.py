def sum_refs(n: int) -> int:
    xs = list(range(n))
    acc = 0
    for i in range(n):
        x = xs[i]
        acc += x
    return acc


def main() -> None:
    n = 2000
    print(sum_refs(n))


if __name__ == "__main__":
    main()
