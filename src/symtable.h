#if !defined(symtable_h)
#define symtable_h

// TODO: docs
// Ex.: After new is top_scope null? (yes)

#include <stdbool.h>

#include "ast.h"

typedef struct SymbolTable SymbolTable;
typedef struct Scope Scope;

extern SymbolTable* symtable_new(void);
extern void symtable_free(SymbolTable*);

extern Scope* symtable_enter_scope(SymbolTable*);
extern Scope* symtable_leave_scope(SymbolTable*);

// TODO: a better name
// only works for declarations
extern Definition* symtable_find_in_scope(Scope*, Id*);

// returns NULL if id could not be found
extern Definition* symtable_find(SymbolTable*, Id*);

// returns false if definition is already inside and true otherwise
extern bool symtable_insert(SymbolTable*, Definition*);

#endif
