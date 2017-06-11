#if !defined(symtable_h)
#define symtable_h

// TODO: Docs
// Ex.: After new is top_scope null? (yes)

#include <stdbool.h>

#include "ast.h"

typedef struct SymbolTable SymbolTable;

extern SymbolTable* symtable_new();
extern void symtable_free(SymbolTable*);

extern void symtable_enter_scope(SymbolTable*);
extern void symtable_leave_scope(SymbolTable*);

// TODO: A better name
// Only works for declarations
bool symtable_contains_in_current_scope(SymbolTable*, Id*);

/*
 * Returns NULL if id could not be found.
 */
extern Definition* symtable_find(SymbolTable*, Id*, int*);

/*
 * Returns false if definition is already inside and true otherwise.
 */
extern bool symtable_insert(SymbolTable*, Definition*);

#endif