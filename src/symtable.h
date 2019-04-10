#if !defined(symtable_h)
#define symtable_h

// TODO: Docs
// Ex.: After new is top_scope null? (yes)

#include <stdbool.h>

#include "ast.h"

typedef struct SymbolTable SymbolTable;
typedef struct Scope Scope;

extern SymbolTable* symtable_new(void);
extern void symtable_free(SymbolTable*);

extern Scope* symtable_enter_scope(SymbolTable*);
extern Scope* symtable_leave_scope(SymbolTable*);

// TODO: A better name
// Only works for declarations
Definition* symtable_find_in_scope(Scope*, Id*);

/*
 * Returns NULL if id could not be found.
 */
extern Definition* symtable_find(SymbolTable*, Id*);

/*
 * Returns false if definition is already inside and true otherwise.
 */
extern bool symtable_insert(SymbolTable*, Definition*);

#endif