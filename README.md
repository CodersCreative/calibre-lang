# Calibre Language

Calibre is a statically typed language implemented in Rust with a working stackless bytecode VM, formatter, and LSP.
The project is actively evolving, but it already supports a broad set of language features and a usable workflow.

---

## Current Status

- Parser and AST pipeline are implemented (`crates/parser`)
- MIR lowering and type inference are implemented (`crates/mir`)
- LIR/bytecode lowering is implemented (`crates/lir`)
- VM runtime/interpreter is implemented (`crates/vm`)
- Formatter is implemented (`fmt`)
- LSP server is implemented and actively improving (`lsp`)
- Cranelift backend exists but is still in-progress (`crates/cranelift`)

---

## Language Tour

### Variables and Mutability

- `let` creates immutable bindings
- `let mut` creates mutable bindings
- tuple and struct destructuring declarations are supported

```cal
let x = 10;
let mut y = 20;
let mut a, mut b = 1, 2;
```

### Functions

- Functions in calibre are first-class values and can even have param types and return types inferred if enough information is provided

```cal
const add = fn (a, b) => a + b;

const main = fn => {
  let result = add(2, 3);
  print(result);
};
```

### Data Types

Calibre currently supports primitives and structured types including:

- `int`, `uint`, `float`, `bool`, `char`, `str`, `null`
- `list:<T>`
- `T?` / `Err!Ok`
- generators (`gen:<T>`)
- references/pointers in relevant contexts (`&T`, `&mut T`, `ptr:<T>`)

### Structs, Enums, and Match

```cal
type Point = struct { x : int, y : int };

type MaybeInt = enum {
  Some : int,
  None,
};

const inspect = fn (v : MaybeInt) => {
  match v {
    .Some : x => print("value=" & x),
    .None => print("none")
  };
};
```

### Traits and Impl

```cal
trait Person {
  const name : fn (&Self) -> str;
  const greeting = fn (self : &Self) -> str => "Hello " & self.name();
};

type User = struct { name : str };

impl Person for User {
  const name = fn (self : &User) -> str => self.name;
};
```

### Control Flow

Calibre currently supports:

- `if` / `else`
- `match` with value and condition styles
- loops and list comprehensions `for`
- `break`, `continue`, `return`, `defer`

```cal
for i in 0..10 => {
  if i % 2 == 0 => continue;
  print(i);
};
```

### Generators and Collection-style Pipelines

```cal
const evens = fn(x for x in 0..20 if x % 2 == 0);
print(evens.collect());
```

### Async/Concurrency Primitives

The standard library includes runtime-backed primitives such as:

- `Channel`
- `WaitGroup`
- `Mutex`
- `spawn`
- `select`

Examples are available in `examples/async.cal`.

### FFI (`extern`)

Calibre supports C/Zig-style extern declarations with explicit signatures.

```cal
extern "c" const c_strlen = fn(str) -> @usize from "libc" as "strlen";
```

---

## CLI Usage

### Build

```sh
cargo build -p calibre
```

### REPL

```sh
cargo run -p calibre
```

### Run a file

```sh
cargo run -p calibre -- run examples/examples.cal
```

### Run project example by name (inside a `calibre.toml` project)

```sh
cargo run -p calibre -- run --example my_example
```

### Run tests / benchmarks

```sh
cargo run -p calibre -- test
cargo run -p calibre -- bench
```

### Formatter

```sh
cargo run -p calibre-fmt -- --max-width 100 --path examples/examples.cal
```

### LSP

```sh
cargo run -p calibre-lsp
```

---

## Examples

The `examples/` directory contains practical programs for current language/runtime features, including:

- `examples/showcase/main.cal` (general language surface)
- `examples/traits.cal` (traits/impl)
- `examples/generators.cal` (generators/pipelines)
- `examples/async.cal` (channels, mutexes, spawn/select)
- `examples/hashmap.cal` (collections)
- `examples/stdlib_showcase.cal` (stdlib breadth)

---

## Repository Structure

- `calibre/`: CLI frontend and REPL (package `calibre`)
- `lsp/`: language server
- `fmt/`: source formatter
- `crates/parser/`: parser + AST + parser diagnostics
- `crates/mir/`: semantic analysis, type resolution, MIR
- `crates/lir/`: lower-level IR / bytecode prep
- `crates/vm/`: runtime VM + native stdlib bindings
- `crates/std/`: the standard library written in calibre
- `crates/diagnostics/`: diagnostics emission helpers
- `crates/cranelift/`: JIT/AOT compilation backend work-in-progress

---

## Roadmap

- [x] Interpreter backend
- [ ] Cranelift backend
- [x] Formatter
- [x] LSP support
- [ ] Package manager

---

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

---

## License

[MIT](https://choosealicense.com/licenses/mit/)
