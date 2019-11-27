#include <assert.h>
#include <stdio.h> // TODO: remove

#include "alloc.h"
#include "errs.h"
#include "macros.h"
#include "symtable.h"

#define ERR_SCOPE "symbol table: did not leave all scopes"

/*
 * TODO:
 *  - Docs
 *  - More asserts
 */

// ==================================================
//
//  Auxiliary
//
// ==================================================

typedef struct Symbol Symbol;

struct SymbolTable {
    Scope* top_scope;
};

struct Scope {
    Symbol* first_symbol;
    Scope* next;
    Scope* previous;
};

struct Symbol {
    Symbol* next;
    Definition* definition;
};

static const char* definitionstring(Definition* definition) {
    switch (definition->tag) {
    case DEFINITION_CAPSA:
        assert(definition->capsa.capsa->tag == CAPSA_ID);
        return definition->capsa.capsa->id->name;
    case DECLARATION_FUNCTION: // fallthrough
    case DEFINITION_FUNCTION:  // fallthrough
    case DEFINITION_METHOD:    // fallthrough
    case DEFINITION_CONSTRUCTOR:
        assert(definition->function.id);
        return definition->function.id->name;
    case DEFINITION_TYPE:
        switch (definition->type->tag) {
        case TYPE_VOID:      UNREACHABLE;
        case TYPE_ID:        return definition->type->id->name;
        case TYPE_UNLOCKED:  UNREACHABLE;
        case TYPE_ARRAY:     UNREACHABLE;
        case TYPE_INTERFACE: // fallthrough
        case TYPE_STRUCTURE: // fallthrough
        case TYPE_MONITOR:   return definition->type->structure.id->name;
        default:             UNREACHABLE;
        }
    default:
        UNREACHABLE;
    }
}

// Finds a definition inside a given scope
static Definition* finddefinition(Scope* scope, const char* idstring) {
    for (Symbol* s = scope->first_symbol; s; s = s->next) {
        if (s->definition && definitionstring(s->definition) == idstring) {
            return s->definition;
        }
    }
    return NULL;
}

// ==================================================
//
//  Functions
//
// ==================================================

SymbolTable* symtable_new(void) {
    SymbolTable* table;
    MALLOC(table, SymbolTable);
    table->top_scope = NULL;
    return table;
}

void symtable_free(SymbolTable* table) {
    if (table->top_scope) {
        internal_error(ERR_SCOPE);
    }
    free(table);
}

Scope* symtable_enter_scope(SymbolTable* table) {
    Scope* scope;
    MALLOC(scope, Scope);

    scope->first_symbol = NULL;
    scope->next = table->top_scope;
    scope->previous = NULL;

    if (scope->next) {
        scope->next->previous = scope;
    }
    table->top_scope = scope;

    return table->top_scope;
}

Scope* symtable_leave_scope(SymbolTable* table) {
    assert(!table->top_scope->previous);

    Scope* scope = table->top_scope;
    table->top_scope = scope->next;
    if (table->top_scope) {
        table->top_scope->previous = NULL;
    }

    for (Symbol* symbol = scope->first_symbol; symbol;) {
        scope->first_symbol = symbol;
        symbol = symbol->next;
        free(scope->first_symbol);
    }
    free(scope);

    return table->top_scope;
}

Definition* symtable_find_in_scope(Scope* scope, Id* id) {
    Definition* found = NULL;
    for (Scope* s = scope; s && !found; s = s->previous) {
        found = finddefinition(s, id->name);
    }
    return found;
}

Definition* symtable_find(SymbolTable* table, Id* id) { 
    Definition* found = NULL;
    for (Scope* s = table->top_scope; s && !found; s = s->next) {
        found = finddefinition(s, id->name);
    }
    return found;
}

bool symtable_insert(SymbolTable* table, Definition* definition) {
    Definition* found = finddefinition(
        table->top_scope, definitionstring(definition)
    );

    if (found) { // repetition
        // TODO: gambiarra
        if (definition->tag == DECLARATION_FUNCTION ||
            definition->tag == DEFINITION_FUNCTION  ||
            definition->tag == DEFINITION_METHOD)   {
            Bitmap bm1 = found->function.qualifiers & ~FQ_PRIVATE;
            Bitmap bm2 = definition->function.qualifiers & ~FQ_PRIVATE;
            return bm1 != bm2;
        }
        return false;
    }

    Symbol* symbol;
    MALLOC(symbol, Symbol);
    symbol->next = table->top_scope->first_symbol;
    symbol->definition = definition;
    table->top_scope->first_symbol = symbol;

    return true;
}
