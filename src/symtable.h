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
extern Declaration* symtable_find_declaration(SymbolTable*, Id*);
extern Type* symtable_find_type(SymbolTable*, Id*);

/*
 * Returns false if declaration/type id is already inside and true otherwise.
 */
extern bool symtable_insert_declaration(SymbolTable*, Declaration*);
extern bool symtable_insert_type(SymbolTable*, Type*);

#endif