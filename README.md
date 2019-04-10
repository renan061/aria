# Aria

A programming language with built-in monitors as the only way to share mutable
data between multiple threads.

Lex (Flex), Yacc (Bison), LLVM (LLVM C API) & POSIX Threads.

*Disclaimer: this is an academic work in progress.*

## Extending Monitors

The acquire-release pair of functions' rules:

    1. Must be defined together, with the same name, and using the keywords.
    2. Must be defined inside a monitor.
    3. The acquire function must always return a monitor instance.
    4. An acquire function can't be called without the acquire-value statement.
    6. A release function can't be explicitly called.
