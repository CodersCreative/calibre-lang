let mut abs = 1;
print("main result: " & main());
// Currying works by first seeing if all the arguments have been met and if not creating a new function that only requires the remaining arguments.
// In short it is implied currying.
let currying = fn (a : int, b : int, c : int = 5) -> int => a * b * c;

// This is an example of currying using pipes
let currying = 18 |> 20 |> currying
let currying = 15 |> currying
print("Curryed " & currying)
