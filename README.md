# Miracula (Miranda Interpreter in Chez Scheme)

Miracula is a lightweight interpreter and interactive REPL for a lazy functional programming language inspired by Miranda. It is written in Chez Scheme and features lazy evaluation, pattern-matching desugaring, list primitives, and an interactive environment.

## Features

- **Lazy Evaluation (Call-by-Need):** Expressions are evaluated only when required using memoized thunks to avoid redundant computation. Includes infinite loop / self-referential cycle detection.
- **Equation & Pattern Matching Desugaring:** Allows defining functions through multiple equations (pattern matching on integers and variables). The interpreter automatically compiles and desugars these equations into nested conditional and lambda expressions.
- **Native List Support:** Lists can be defined using standard bracket notation (e.g., `[1, 2, 3]`, `[]`).
- **Primitive List Operations:** Native list primitives `hd` and `tl` are built into the runtime environment.
- **Interactive REPL:** Provides a prompt (`miranda> `) to define variables/functions and evaluate expressions interactively.
  - `/e` command: Open and edit `script.m` in the terminal using `vi`, reloading all definitions on exit.
  - `/q` command: Exit the REPL.

## Syntax & Examples

### Basic Expressions
```miranda
miranda> 3 + 4
Result: 7 (Evaluated in 0 ms)
```

### Lambdas
Lambdas are defined using backslashes:
```miranda
miranda> (\x. x + 2) 5
Result: 7 (Evaluated in 0 ms)
```

### Conditionals
Use `ifzero` to inspect numeric values:
```miranda
miranda> ifzero 0 then 42 else 0
Result: 42 (Evaluated in 0 ms)
```

### Lists and Primitives
```miranda
miranda> hd [1, 2, 3]
Result: 1 (Evaluated in 0 ms)

miranda> tl [1, 2, 3]
Result: [2, 3] (Evaluated in 0 ms)
```

### Defining Functions (script.m)
You can define variables and functions directly in the REPL or load them from `script.m`. 
For example, in `script.m`:
```miranda
add1 x = x+1

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

x = fib (3+1)
```

## How to Build and Run

### Prerequisites
Make sure you have [Chez Scheme](https://cisco.github.io/ChezScheme/) installed on your system (accessible via the `chez` command).

### Build
Compile the interpreter to a native standalone executable program using Chez Scheme:
```bash
chez --script compile.ss
```

### Run
Launch the REPL by running the compiled executable directly:
```bash
./miracula
```
If a `script.m` file is present in the working directory, its bindings will be automatically loaded upon startup.
