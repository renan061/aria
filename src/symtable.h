#if !defined(symtable_h)
#define symtable_h

// TODO: Docs

#include <stdbool.h>

#include "ast.h"

typedef struct SymbolTable SymbolTable;

extern SymbolTable* symtable_new();
extern void symtable_free(SymbolTable*);

extern void symtable_enter_scope(SymbolTable*);
extern void symtable_leave_scope(SymbolTable*);

/*
 * Returns NULL if id could not be found.
 */
extern Declaration* symtable_find(SymbolTable*, Id*);

/*
 * Returns "false" if declaration->id is already inside and "true" otherwise.
 */
extern bool symtable_insert(SymbolTable*, Declaration*);

#endif