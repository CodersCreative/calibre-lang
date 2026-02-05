def sum_index(n: int) -> int:
    xs = list(range(n))
    acc = 0
    for i in range(n):
        acc += xs[i]
    return acc


def main() -> None:
    n = 2000
    print(sum_index(n))


if __name__ == "__main__":
    main()
