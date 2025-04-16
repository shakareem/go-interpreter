# Go Interpreter

[![OCaml Version](https://img.shields.io/badge/OCaml-4.14%2B-blue)](https://ocaml.org/)
[![License](https://img.shields.io/github/license/shakareem/go-interpreter)](LICENSE)

An interpreter for a subset of the Go programming language, written in Ocaml. This project implements parsing, type checking and inference, evaluation of Go-like code.

## Supported Language Features

### Core Features
- **Basic data types**: `int`, `bool`, `string`
- **Loops**: `for`
- **Conditional statements**: `if-else`
- **Arrays**: accesing elements by indicies

### Functions
- Regular functions
- Recursive functions
- Closures

### Concurrency
- **Unbuffered channels**:
  - `receive` (extract from channel)
  - `send` (put into channel)
  - `close` channel
- **Goroutines**:
  - Lightweight threads
  - Context switching on channel operations

### Key words
- `break` `func` `defer` `go` `chan` `if` `else` `continue` `for` `return` `var`

### Predecclared Identifiers
- `true` `false` `nil` `make` `close` `len` `panic` `print` `println` `recover`

## Interpreter Features

- Parser: Builds Abstract Syntax Tree (AST) from Go source code
- Typechecker : Checks all expressions' types and inferences variable types
- Evaluator: Executes the parsed AST
- REPL: Read-Eval-Print Loop for interactive interpretation

## Usage

```bash
dune exec bin/interpret.exe <options> <filepath>
```

### Options

- `--ast`  Dump abstract syntax tree of a program
- `--typecheck`  Typecheck the program and print result

### REPL

If filepath isn't specified, REPL will start running and the program will be read from standard input. In REPL mode type:
- `quit` - to quit REPL mode
- `help` - to display help message

### Requirements
- OCaml 4.14+
- Dune 3.8+
