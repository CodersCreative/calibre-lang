type Int = struct ();

impl Int {
  const new = fn (value : dyn) -> int {
    value as int
  }
}
