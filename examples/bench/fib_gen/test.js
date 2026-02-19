function* fibGen(n) {
  let i = 0;
  let a = 0;
  let b = 1;
  while (i < n) {
    yield a;
    const nxt = a + b;
    a = b;
    b = nxt;
    i += 1;
  }
}

const n = 30;
for (const v of fibGen(n + 1)) {
  console.log(v);
}
