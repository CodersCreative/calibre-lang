# Calibre Language

Calibre is a modern, statically-typed language built in Rust with a fast interpreter, a growing toolchain, and a pragmatic syntax for systems scripting. It emphasizes clear data modeling, expressive pattern matching, and straightforward FFI via `extern` with explicit C/Zig types.

---

## Features

- **Statically Typed**: Type inference and explicit typing for safety and expressiveness.
- **Pattern Matching**: Powerful `match` for enums, tuples, and structs.
- **Enums & Structs**: Algebraic data types with tuple and map-like variants.
- **Immutability by Default**: `let` for immutable, `let mut` for mutable variables.
- **First-Class Functions**: Functions as values, with concise syntax.
- **FFI**: `extern "c"` / `extern "zig"` with `@`-typed signatures and `ptr:<T>` pointers.
- **Interpreted Execution**: Fast iteration with a bytecode VM.
- **Tooling**: Tree-sitter grammar, formatter (`fmt`) with max-width option, and LSP (in progress).

---

## Example

```cl
type Point = struct {
  x: int,
  y: int,
};

const dot = fn(p: Point) -> int => p.x * p.y;

const classify = fn match &int {
  .Ok : value => "ok: " & value,
  .Err : msg => "err: " & msg,
};

const main = fn() => {
  let p = Point { x: 6, y: 7 };
  print(dot(p));

  let mut a, mut b = 10, 20;
  print(a + b);
};
```

### FFI (C/Zig)

```cl
extern "c" const c_strlen = fn(str) -> @usize from "..." as "strlen";
extern "zig" const zig_add = fn(@i32, @i32) -> @i32 from "..." as "zig_add";

const main = fn() => {
  print(c_strlen("hello"));
  print(zig_add(40, 2));
};
```

---

## Getting Started

1. **Clone the repository:**
   ```sh
   git clone https://github.com/CodersCreative/calibre-lang.git
   cd calibre-lang/cal
   ```

2. **Run a REPL:**
   ```sh
   cargo run -p cal
   ```

3. **Run an example:**
   ```sh
   cargo run -p cal -- ../examples/examples.cl
   ```

4. **Format code (optional):**
   ```sh
   cargo run -p cal-fmt -- --max-width 100 --path ../examples/examples.cl
   ```

5. **Install cal**
   ```sh
   cargo install -p cal
   ```

6. **Install cal-fmt**
   ```sh
   cargo install -p cal-fmt
   ```

---

## Roadmap

- [x] Interpreter backend
- [ ] Cranelift backend (`crates/cranelift`)
- [x] Tree-sitter grammar (https://github.com/CodersCreative/tree-sitter-calibre)
- [x] Formatter ('fmt')
- [ ] Language Server Protocol (LSP) (`lsp`)
- [ ] Package manager

---

## Contributing

Contributions are welcome! Please see CONTRIBUTING.md for guidelines.

---

## License

MIT License. See LICENSE for details.

---

## Repository Structure

- `cal/`: Main interpreter frontend
- `fmt/`: Formatter implementation
- `crates/`: Core language crates (parser, interpreter, VM, etc.)
- `examples/`: Example Calibre programs (including FFI and Zig)
- `lsp/`: Language Server Protocol implementation (in progress)
