# Aria

A programming language with built-in monitors as the only way to share mutable
data between multiple threads.

Lex (Flex), Yacc (Bison), LLVM (LLVM C API) & POSIX Threads.

*Disclaimer: this is an academic work in progress.*

## Extending Monitors

The acquire-release pair of functions' rules:

    1. Must be defined together, with the same name, and using the keywords.
        - Ensured by the semantic analysis.
    2. Must be defined inside a monitor.
        - Ensured by the semantic analysis.
    3. The acquire function must always return a monitor instance.
    4. An acquire function can't be called without the acquire-value statement.
    6. A release function can't be explicitly called.

Notes:
    
    1. An acquire or release function can't be private.
        - Why?
        - I would say both have to be private or both have to be non-private.
            - Ensured by the semantic analysis.
