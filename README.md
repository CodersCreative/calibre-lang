# Calibre Language

Calibre is a modern, statically-typed programming language designed for clarity, expressiveness, and extensibility. Built in Rust, Calibre aims to provide a robust interpreted experience, with future plans for JIT compilation via Cranelift and static compilation via LLVM (Inkwell). The language is ideal for both scripting and systems programming, and is designed with tooling in mind: a Tree-sitter grammar, formatter, and LSP support are all part of the roadmap.

---

## Features

- **Statically Typed**: Type inference and explicit typing for safety and expressiveness.
- **Pattern Matching**: Powerful match statements for enums and data structures.
- **Enums & Structs**: Algebraic data types with tuple and hashmap-like variants.
- **Immutability by Default**: `let` for immutable, `let mut` for mutable variables.
- **First-Class Functions**: Functions as values, with concise syntax.
- **Modules & Imports**: Simple module system for code organization.
- **Interpreted Execution**: Fast iteration with an interpreter backend.
- **Planned AOT Compilation**: Cranelift static compilation in development.
- **Tooling**: Tree-sitter grammar, formatter, and LSP (Language Server Protocol) support planned.

---

## Example

```cl
type Language = enum {
  FRENCH { data : int, code : int },
  ENGLISH (int),
  SPANISH,
  ARABIC (Language, Language),
}

type Country = struct (Language)

// Immutable variable with inferred type
let language = Language.FRENCH{data : 10, code : 5};

// Mutable variable
let mut recursive_language = Language.ARABIC(language, Language.SPANISH);

// Pattern matching with mutation
recursive_language |> match &mut {
  data.Language.ARABIC(Language.FRENCH{_}, Language.ARABIC(Language.FRENCH{code})) => print("Code: " & code),
  data.Language.ARABIC(Language.FRENCH{data}, Language.SPANISH) => {
    print("Enum: " & data)
    data = 5;
    print("Enum Changed: " & data)
  },
}
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
   cargo run
   ```

3. **Run an example:**
   ```sh
   cargo run -- --path ../examples/showcase/main.cl
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
- `crates/`: Core language crates (parser, interpreter, JIT, etc.)
- `examples/`: Example Calibre programs
- `lsp/`: Language Server Protocol implementation (in progress)
