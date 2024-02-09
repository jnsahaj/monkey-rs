# An Interpreter for monkey, written in Rust
Inspired by Thorsten Ball's [interpreterbook.com](https://interpreterbook.com) "Writing an Interpreter in Go" where he writes a tree-walking Interpreter from scratch using Go, without any third-party libraries, for a modern programming language "monkey".

This was my first exposure to Go, and one of my first major undertakings in Rust. While the primary goal was to get good at Rust, learning about how compilers and interpreters work has left me with profound knowledge applicable immediately. 

This was also greatly helped by [NathanYee's implementation of monkey in Rust](https://github.com/NathanYee/monkey-rs/tree/master), which served as my primary reference to translate Go patterns into Rust. Although, I have tried to improve upon it in modest ways.

## List of features
- Integers
- Booleans
- Strings
- Arrays
- HashMaps
- Prefix-, Infix- and Index operators
- Conditionals
- Global and local bindings
- First-class functions
- Return statements
- Closures

## Usage
Start the REPL
```bash
~ cargo run
```

Run all tests
```bash
~ cargo test
```
