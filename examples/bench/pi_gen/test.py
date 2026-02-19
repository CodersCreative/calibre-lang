def pi_terms_gen(n_terms):
    i = 0
    denominator = 1.0
    operation = 1.0

    while i < n_terms:
        yield operation * (4.0 / denominator)
        denominator += 2.0
        operation *= -1.0
        i += 1


def calculate_pi_gen(n_terms):
    pi = 0.0
    for term in pi_terms_gen(n_terms):
        pi += term
    return pi


if __name__ == "__main__":
    n = 100_000
    result = calculate_pi_gen(n)
    print(result)
