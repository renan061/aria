#if !defined(macros_h)
#define macros_h

// forces statement behavior
#define STMT(x) do { x } while(0)

// marks unreachable code
#define UNREACHABLE STMT(assert(0);)

// expression (a if and only if b)
#define IFF(a, b) (((a) && (b)) || (!(a) && !(b)))

// iterator for lists that are defined with <next>
#define FOREACH(Type, e, e0) for (Type* e = e0; e; e = e->next)

// prepends an element in a list defined with <next>
#define PREPEND(t, head, e) STMT(t* tmp = head; head = e; head->next = tmp;)

#endif
