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
	Type* type;
};

static const char* declarationstring(Declaration* declaration) {
	switch (declaration->tag) {
	case DECLARATION_VARIABLE:
		assert(declaration->variable->tag == VARIABLE_ID);
		return declaration->variable->id->name;
	case DECLARATION_FUNCTION:
		assert(declaration->function.id); // can't be constructor
		return declaration->function.id->name;
	}
	return assert(NULL), NULL; // unreachable
}

static const char* typestring(Type* type) {
	switch (type->tag) {
	case TYPE_ID:
		return type->id->name;
	case TYPE_MONITOR:
		return type->monitor.id->name;
	default:
		return assert(NULL), NULL; // unreachable
	}
}

// Finds a declaration inside a given scope
static Declaration* finddeclaration(Scope* scope, const char* idstring) {
	for (Symbol* s = scope->first_symbol; s; s = s->next) {
		if (s->declaration && declarationstring(s->declaration) == idstring) {
			return s->declaration;
		}
	}
	return NULL;
}

// Finds a type inside a given scope
static Type* findtype(Scope* scope, const char* idstring) {
	for (Symbol* s = scope->first_symbol; s; s = s->next) {
		if (s->type && typestring(s->type) == idstring) {
			return s->type;
		}
	}
	return NULL;
}

// Used for inserting new symbols
static Symbol* newsymbol(SymbolTable* t, Declaration* declaration, Type* type) {
	Symbol* symbol;
	MALLOC(symbol, Symbol);
	symbol->declaration = declaration;
	symbol->type = type;
	symbol->next = t->top_scope->first_symbol;
	t->top_scope->first_symbol = symbol;
	return symbol;
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
	return finddeclaration(tb->top_scope, id->name) != NULL;
}

Declaration* symtable_find_declaration(SymbolTable* table, Id* id) {
	Declaration* found = NULL;
	for (Scope* s = table->top_scope; s && !found; s = s->next) {
		found = finddeclaration(s, id->name);
	}
	return found;
}

Type* symtable_find_type(SymbolTable* table, Id* id) {
	Type* found = NULL;
	for (Scope* s = table->top_scope; s && !found; s = s->next) {
		found = findtype(s, id->name);
	}
	return found;
}

bool symtable_insert_declaration(SymbolTable* table, Declaration* declaration) {
	if (finddeclaration(table->top_scope, declarationstring(declaration))) {
		return false; // repetition
	}
	newsymbol(table, declaration, NULL);
	return true;
}

bool symtable_insert_type(SymbolTable* table, Type* type) {
	if (findtype(table->top_scope, typestring(type))) {
		return false; // repetition
	}
	newsymbol(table, NULL, type);
	return true;
}
