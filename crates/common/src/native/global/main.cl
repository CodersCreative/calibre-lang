const unwrap = fn(val : dyn) -> dyn => {
  *val |> match {
    Ok(x) => return x,
    Some(x) => return x,
    _ => panic()
  }
}

const unwrap_err = fn(val : dyn) -> dyn => {
  *val |> match {
    Err(x) => return x,
    _ => panic()
  }
}

const range = fn(start end : mut int = INT_MIN, step : int = 1, inclusive : bool = false) -> list<int> => {
  if start != INT_MIN && end == INT_MIN => {
    end = start;
    start = 0;
  } else if start == INT_MIN && end == INT_MIN => return [];

  if inclusive => end += step;

  let mut lst : list<int> = [];
  let mut index : int = start;

  for (index < end && step > 0) || (index > end && step < 0) => {
    lst <<= index;
    index += step;
  }

  lst
}
