#if !defined(macros_h)
#define macros_h

// forces statement behavior
#define STMT(x) do { x } while(0)

// marks unreachable code
#define UNREACHABLE STMT(assert(0);)

// expression (a if and only if b)
#define IFF(a, b) (((a) && (b)) || (!(a) && !(b)))

// -----------------------------------------------------------------------------

// macros for lists that are defined with <next>

#define COUNT(t, list, n) FOREACH(t, e, list) { n++; }

#define FOREACH(t, e, list) for (t* e = list; e; e = e->next)

#define PREPEND(t, head, e) STMT(t* tmp = head; head = e; head->next = tmp;)

#endif
