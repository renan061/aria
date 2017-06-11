#include <assert.h>
#include <stdio.h> // TODO: Remove

#include "alloc.h"
#include "errs.h"
#include "symtable.h"

#define ERR_SCOPE "symbol table: did not leave all scopes"

// TODO: Move this somewhere else and look for assert(NULL) in the code
#define UNREACHABLE assert(NULL)

// ==================================================
//
//	Auxiliary
//
// ==================================================

// TODO: Docs

typedef struct Scope Scope;
typedef struct Symbol Symbol;

struct SymbolTable {
	Scope* top_scope;
};

struct Scope {
	Scope* next;
	Symbol* first_symbol;
};

struct Symbol {
	Symbol* next;
	Definition* definition;
};

static const char* definitionstring(Definition* definition) {
	switch (definition->tag) {
	case DEFINITION_VARIABLE:
		assert(definition->variable.variable->tag == VARIABLE_ID);
		return definition->variable.variable->id->name;
	case DEFINITION_FUNCTION:
		/* fallthrough */
	case DEFINITION_METHOD:
		/* fallthrough */
	case DEFINITION_CONSTRUCTOR:
		assert(definition->function.id);
		return definition->function.id->name;
	case DEFINITION_TYPE:
		switch (definition->type->tag) {
		case TYPE_VOID:
			UNREACHABLE;
		case TYPE_ID:
			return definition->type->id->name;
		case TYPE_ARRAY:
			UNREACHABLE;
		case TYPE_MONITOR:
			return definition->type->monitor.id->name;
		default:
			UNREACHABLE;
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
//	Functions
//
// ==================================================

SymbolTable* symtable_new() {
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

void symtable_enter_scope(SymbolTable* table) {
	Scope* scope;
	MALLOC(scope, Scope);

	scope->next = table->top_scope;
	table->top_scope = scope;

	scope->first_symbol = NULL;
}

void symtable_leave_scope(SymbolTable* table) {
	Scope* scope = table->top_scope;
	table->top_scope = scope->next;

	for (Symbol* symbol = scope->first_symbol; symbol;) {
		scope->first_symbol = symbol;
		symbol = symbol->next;
		free(scope->first_symbol);
	}
	free(scope);
}

bool symtable_contains_in_current_scope(SymbolTable* tb, Id* id) {
	return (bool) finddefinition(tb->top_scope, id->name);
}

Definition* symtable_find(SymbolTable* table, Id* id, int* counter) {
	Definition* found = NULL;
	int n = 0;
	for (Scope* s = table->top_scope; s && !found; s = s->next, n++) {
		found = finddefinition(s, id->name);
	}
	if (counter) {
		*counter = n;		
	}
	return found;
}

bool symtable_insert(SymbolTable* table, Definition* definition) {
	if (finddefinition(table->top_scope, definitionstring(definition))) {
		return false; // repetition
	}
	
	Symbol* symbol;
	MALLOC(symbol, Symbol);
	symbol->next = table->top_scope->first_symbol;
	symbol->definition = definition;
	table->top_scope->first_symbol = symbol;

	return true;
}
