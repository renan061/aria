#include "alloc.h"
#include "errs.h"
#include "symtable.h"

#define ERR_SCOPE "symbol table: did not leave all scopes"

// ==================================================
//
//	Auxiliary
//
// ==================================================

typedef struct Scope Scope;
typedef struct Symbol Symbol;

struct SymbolTable {
	Scope* first_scope;
};

struct Scope {
	Scope* next;
	Symbol* first_symbol;
};

struct Symbol {
	Symbol* next;
	Id* id;
};

static Id* findinside(Scope* scope, Id* id) {
	for (Symbol* symbol = scope->first_symbol; symbol; symbol = symbol->next) {
		if (id == symbol->id) {
			return id;
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
	table->first_scope = NULL;
	return table;
}

void symtable_free(SymbolTable* table) {
	if (table->first_scope) {
		internal_error(ERR_SCOPE);
	}
	free(table);
}

void symtable_enter_scope(SymbolTable* table) {
	Scope* scope;
	MALLOC(scope, Scope);

	scope->next = table->first_scope;
	table->first_scope = scope;

	scope->first_symbol = NULL;
}

void symtable_leave_scope(SymbolTable* table) {
	Scope* scope = table->first_scope;
	table->first_scope = scope->next;

	for (Symbol* symbol = scope->first_symbol; symbol;) {
		scope->first_symbol = symbol;
		symbol = symbol->next;
		free(scope->first_symbol);
	}
	free(scope);
}

Id* symtable_find(SymbolTable* table, Id* id) {
	Id* found = NULL;
	for (Scope* scope = table->first_scope; scope; scope = scope->next) {
		if ((found = findinside(scope, id))) {
			break;
		}
	}
	return found;
}

bool symtable_insert(SymbolTable* table, Id* id) {
	// Checks for repetition inside same scope
	if (findinside(table->first_scope, id)) {
		return false;
	}

	Symbol* symbol;
	MALLOC(symbol, Symbol);
	symbol->id = id;
	symbol->next = table->first_scope->first_symbol;
	table->first_scope->first_symbol = symbol;
	return true;
}
