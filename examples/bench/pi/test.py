def calculate_pi(n_terms):
    pi = 0.0
    denominator = 1.0
    operation = 1.0

    for i in range(n_terms):
        pi += operation * (4.0 / denominator)
        denominator += 2.0
        operation *= -1.0

    return pi

if __name__ == "__main__":
    n = 100
    result = calculate_pi(n)
    print(result)
