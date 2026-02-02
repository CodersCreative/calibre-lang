def step(x: int) -> int:
    return x + 1


def run(n: int) -> int:
    acc = 0
    for _ in range(n):
        acc = step(acc)
    return acc


def main() -> None:
    n = 200000
    print(run(n))


if __name__ == "__main__":
    main()
