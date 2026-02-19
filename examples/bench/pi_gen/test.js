function* piTermsGen(nTerms) {
  let i = 0;
  let denominator = 1.0;
  let operation = 1.0;

  while (i < nTerms) {
    yield operation * (4.0 / denominator);
    denominator += 2.0;
    operation *= -1.0;
    i += 1;
  }
}

function calculatePiGen(nTerms) {
  let pi = 0.0;
  for (const term of piTermsGen(nTerms)) {
    pi += term;
  }
  return pi;
}

const n = 100_000;
const result = calculatePiGen(n);
console.log(result);
