const calculate_pi = fn (n_terms: int) -> float => {
    const terms : float = try (n_terms * 2 + 1) as float;
    let mut pi : float = 0.0;
    let mut denominator = 1.0;
    let mut operation = 1.0;

    for => {
        if denominator > terms => {
            break;
        }
        pi += operation * (4.0 / denominator);
        denominator += 2.0;
        operation *= -1.0;
    }

    return pi;
};

const main = fn () => {
    let n = 10000;
    let result = calculate_pi(n);
    print(result);
};
