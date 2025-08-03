let currying = fn (a : int, b : int, c : int) -> int => a * b * c ;
let val = 18 |> currying |> 20 |> 15 |> print ("S " + $)

let smth_fn = fn (y : int) -> string!int => {
  if y > 10 => {
    return ok(y);
  }

  err("needs to be larger than 10")
}

18 |> smth_fn |> match {
  // This is equivalent to Ok(x) if x == 18
  Ok(18) => print("Equated to 18"),
  // This is equivalent to Ok(x) if x in 0..20
  Ok(0..20) => print("Within range"),
  // This is equivalent to Ok(x) if x in [20, 30, 40, 50, 60, 70]
  Ok([20, 30, 40, 50, 60, 70]) => print("Within list"),
  Ok(x) => print(x),
  Err(x) => print(x),
  Let(x) => print(x),
}
