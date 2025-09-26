let matmul = fn (n : uint, a b : list<list<float>>) -> list<list<float>> => {
  let m : uint = len(b[0]);
  let p : uint = len(a[0]);
  let mut c : list<list<float>> = [];

  for i in 0..n => {
    let mut ci : list<float> = [0.0 for _ in 0..m];
    for k in 0..p => {
      let aik : float = a[i][k];
      let bk : list<float> = b[k];
      for j in 0..m => ci[j] += aik * bk[j]
    }
    c <<= ci
  }
  c
} 

let main = fn() => {
  let n : uint = 100;
  let tmp : float = 1.0 / n / n
	let a : list<list<float>> = [[tmp * (i - j) * (i + j) for j in 0..n] for i in 0..n]
	let b : list<list<float>> = [[tmp * (i - j) * (i + j) for j in 0..n] for i in 0..n]
	let d = matmul(n, a, b)
  
  print(d[n / 2 as int][n / 2 as int])
  print("Success");
}

if __name__ == "__main__" => main();
