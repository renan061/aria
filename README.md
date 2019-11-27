# Aria

A programming language with built-in monitors as the only way to share mutable
data between threads.

Lex (Flex), Yacc (Bison), LLVM (LLVM C API) & POSIX Threads.

*Disclaimer: this is an academic work in progress.*

## Extending Monitors

    1. Must be defined together, with the same name, and using the keywords.
        - Ensured by the semantic analysis.

    2. Must be defined inside a monitor.
        - Ensured by the semantic analysis.

    3. The acquire function must always return a monitor instance.
        - Ensured by the semantic analysis.

    4. An acquire function can't be called without the acquire-value statement.
        - Ensured by the semantic analysis.

    6. A release function can't be explicitly called.
        - Ensured by the semantic analysis.
        - TODO: findmethod to receive special flag parameter?

    7. A syntax block delimited by an acquire-value statement must not return
       from its enclosing function.
        - Ensured by the semantic analysis.
    
    8. An acquire-value statement can only call `acquire` functions.
        - Ensured by the semantic analysis.

    9. The scoped value is of type `A!` (Unlocked A).
        - Ensured by the semantic analysis.

    10. Monitors in Aria natively provide an acquire-release pair of functions
       called unlocked.
        - Ensured by the semantic analysis.

Notes:
    
    1. An acquire or release function can't be private.
        - Why?
        - I would say both have to be private or both have to be non-private.
            - Ensured by the semantic analysis.
