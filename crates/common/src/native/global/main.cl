const unwrap = fn(val : dyn) -> dyn => {
  match {
    Ok(x) => return x,
    Some(x) => return x,
    _ => panic()
  }(*val)
}

const unwrap_err = fn(val : dyn) -> dyn => {
  match {
    Err(x) => return x,
    _ => panic()
  }(*val)
}
