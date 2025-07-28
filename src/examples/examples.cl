const bmi = fn(mass : float, height : float) -> float => mass / height ^ 2

print(bmi(52.5, 1.65))

const is_a_trinagle= fn(a : float, b : float, c : float) -> bool => a + b > c && b + c > a && c + a > b;

const heron = fn(a : float, b : float, c : float) -> float => {
  let p = (a + b + c) / 2;
  (p * (p - a) * (p - b) * (p - c)) ^ 0.5
}

const area_of_triangle = fn(a : float, b : float, c : float) -> float? {
  if is_a_trinagle(a, b, c) => return heron(a, b, c) else => none
}

const factorial = fn(x : uint) -> ulong => {
  if x <= 1 => return 1;
  x * factorial(x - 1)
}
