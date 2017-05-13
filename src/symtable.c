#include <assert.h>
#include <stdio.h> // TODO: Remove

#include "alloc.h"
#include "errs.h"
#include "symtable.h"

#define ERR_SCOPE "symbol table: did not leave all scopes"

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
	Declaration* declaration;
};

static Id* declarationid(Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		assert(declaration->variable->tag == VARIABLE_ID);
		return declaration->variable->id;
	case DECLARATION_FUNCTION:
		return declaration->function.id;
	}
	return assert(NULL), NULL; // unreachable
}

static Declaration* findinside(Scope* scope, Id* id) {
	for (Symbol* symbol = scope->first_symbol; symbol; symbol = symbol->next) {
		if (declarationid(symbol->declaration)->name == id->name) {
			return symbol->declaration;
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

Declaration* symtable_find(SymbolTable* table, Id* id) {
	Declaration* found = NULL;
	for (Scope* s = table->top_scope; s && !found; s = s->next) {
		found = findinside(s, id);
	}
	return found;
}

bool symtable_insert(SymbolTable* table, Declaration* declaration) {
	// Checks for repetition inside same scope
	if (findinside(table->top_scope, declarationid(declaration))) {
		return false;
	}

	Symbol* symbol;
	MALLOC(symbol, Symbol);
	symbol->declaration = declaration;
	symbol->next = table->top_scope->first_symbol;
	table->top_scope->first_symbol = symbol;
	return true;
}
