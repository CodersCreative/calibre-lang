type Int = struct ();

impl Int {
  const new = fn (value : dyn) -> int => unwrap(value as int);
  
}
