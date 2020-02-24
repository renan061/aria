/*
 * TODO
 *
 *  - Immutable Monitor1 is wrong (TODO: being filtered by the parser)
 *  - Return statements with no expressions should return the 'nil' expression
 *
 */
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h> // TODO: remove
#include <string.h>

#include "macros.h"
#include "alloc.h"
#include "ast.h"
#include "errs.h"
#include "list.h"
#include "parser.h" // for the tokens
#include "scanner.h" // because of scanner_native
#include "symtable.h"

// TODO: move this somewhere else
#define structuretype(t) (t->tag == TYPE_STRUCTURE || t->tag == TYPE_MONITOR)
#define insidestructure(s) (s->structure && structuretype(s->structure))
#define insidemonitor(s) (s->structure && s->structure->tag == TYPE_MONITOR)

// TODO: also check if TODOERR errors have matching tests
#define TODOERR(line, err) \
    printf("line %d:\n\tsemantic error: %s\n", line, err); exit(1); \

// stores important information about the current state of the semantic analysis
typedef struct SemanticState {
    // symbol table for definitions
    SymbolTable* table;

    // <type> if currently analysing a structure's body, NULL otherwise
    Type* structure;

    // <type> if currently analysing a function's body, NULL otherwise
    Type* return_;

    // <true> if currently analysing a monitor's initializer, <false> otherwise
    bool initializer;

    // <true> if can call an acquire function, <false> otherwise
    bool can_acquire;

    // <true> if currently analysing an acquire-value statement, <false>
    // otherwise (switches to <false> when jumps to a function call)
    bool acquire_value;

    // the spawn scope if currently analysing a spawn block, NULL otherwise
    struct {
        Scope* scope;
        FunctionCall* function_call;
    } spawn;
} SS;

// primitive types
static Type* __void;
static Type* __boolean;
static Type* __integer;
static Type* __float;
static Type* __string;
static Type* __condition_queue;

// functions that analyse the abstract syntax tree recursively
static void sem_definition(SS*, Definition*);
static void sem_block(SS*, Block*);
static void sem_statement(SS*, Statement*);
static void sem_capsa(SS*, Capsa**);
static void sem_expression(SS*, Expression*);
static void sem_function_call(SS*, FunctionCall*);

// auxiliary functions that deal with type analysis
static void linktype(SymbolTable*, Type**);
static void assignment(Capsa*, Expression**);
static bool typeequals(Type*, Type*);
static void typecheck1(Type*, Expression**);
static Type* typecheck2(Expression**, Expression**);
static bool indextype(Type*);
static bool numerictype(Type*);
static bool conditiontype(Type*);
static bool equatabletype(Type*);
static bool safetype(Type*);

// TODO
static void freetypeid(Type*);

// auxiliary functions that deal with errors
static void err_redeclaration(Id*);
static void err_unknown_type(Id*);
static void err_invalid_condition_type(Expression*);
static void err_type(Line, Type*, Type*);
static void err_assignment_value(Statement*);
static void err_return_inside_spawn(Line);
static void err_return_initializer(Line);
static void err_return_void(Line, Type*);
static void err_capsa_unknown(Id*);
static void err_capsa_misuse(Id*);
static void err_capsa_array_type(Capsa*);
static void err_capsa_array_index_type(Capsa*);
static void err_function_call_unknown(Id*);
static void err_function_call_misuse(Id*);
static void err_function_call_array_constructor(Line, unsigned int);
static void err_function_call_no_constructor(Line, Id*);
static void err_function_call_few_args(Line);
static void err_function_call_excess_args(Line);
static void err_function_call_no_monitor(Line);
static void err_function_call_private(Line, Id*, Type*);
static void err_function_call_no_method(Line, Type*, Id*, bool);
static void err_monitor_statements(Line, const char*);
static void err_monitor_statements_constructor(Line, const char*);
static void err_monitor_function_type(Line);
static void err_spawn_capsa(Line);
static void err_spawn_unsafe(Line);

// TODO: experimental
typedef enum ErrorType {
    ERR_EXP_MINUS = 0,
    ERR_EXP_NOT,
    ERR_EXP_LEFT,
    ERR_EXP_RIGHT,
    ERR_EXP_LEFT_EQUAL,
    ERR_EXP_RIGHT_EQUAL,
    ERR_EXP_TYPECHECK_EQUAL
} ErrorType;
static void err_expression(ErrorType, Expression*);

// ==================================================
//
//  Exported
//
// ==================================================

// TODO
void sem_analyse(AST* ast) {
    // setup
    __void = ast_type_void();
    __boolean = ast_type_boolean();
    __integer = ast_type_integer();
    __float = ast_type_float();
    __string = ast_type_string();
    __condition_queue = ast_type_condition_queue();

    Definition* def_boolean = ast_definition_type(__boolean);
    Definition* def_integer = ast_definition_type(__integer);
    Definition* def_float = ast_definition_type(__float);
    Definition* def_string = ast_definition_type(__string);
    Definition* def_condition_queue = ast_definition_type(__condition_queue);

    SS ss = {
        /* symbol table         */ symtable_new(),
        /* current monitor      */ NULL,
        /* return type          */ NULL,
        /* inside initializer   */ false,
        /* inside acquire-value */ false,
    };
    ss.spawn.scope = NULL;
    ss.spawn.function_call = NULL;

    // analysis
    symtable_enter_scope(ss.table);
    symtable_insert(ss.table, def_boolean);
    symtable_insert(ss.table, def_integer);
    symtable_insert(ss.table, def_float);
    symtable_insert(ss.table, def_string);
    symtable_insert(ss.table, def_condition_queue);
    FOREACH(Definition, d, ast->definitions) {
        sem_definition(&ss, d);
    }
    symtable_leave_scope(ss.table);

    // teardown
    symtable_free(ss.table);
    free(def_boolean);
    free(def_integer);
    free(def_float);
    free(def_string);
    free(def_condition_queue);

    assert(!ss.structure);
    assert(!ss.return_);
    assert(!ss.initializer);
    assert(!ss.can_acquire);
    assert(!ss.acquire_value);
    assert(!ss.spawn.scope);
    assert(!ss.spawn.function_call);
}

// TODO: move this somewhere
static Capsa* astself(Line line) {
    const char* keyword = scanner_native[SCANNER_NATIVE_SELF];
    Capsa* capsa = ast_capsa_id(ast_id(line, keyword));
    capsa->global = false;
    capsa->value = true;
    return capsa;
}

// ==================================================
//
//  TODO
//
// ==================================================

// TODO: use symtable? do the same for functions?
// auxiliary - returns null if the structure does not contain the attribute
Capsa* findattribute(Capsa* capsa) {
    Type* type = capsa->attribute.structure->type;
    Id* id = capsa->attribute.field;

    assert(type->tag == TYPE_STRUCTURE);
    FOREACH(Definition, d, type->structure.definitions) {
        if (d->tag != DEFINITION_CAPSA) {
            continue;
        }
        if (id->name == d->capsa.capsa->id->name) {
            return d->capsa.capsa;
        }
    }
    return NULL;
}

// ==================================================
//
//  Definition
//
// ==================================================

static void sem_definition_capsa(SS*, Definition*);
static void sem_declaration_function(SS*, Definition*);
static void sem_definition_function(SS*, Definition*);
static void sem_definition_method(SS*, Definition*);
static void sem_definition_constructor(SS*, Definition*);
static void sem_definition_interface(SS*, Definition*);
static void sem_definition_structure(SS*, Definition*);
static void sem_definition_monitor(SS*, Definition*);

static void semfunction(SS*, Definition*);
static void semstructure(SS*, Definition*);
static bool functionequals(Definition*, Definition*);
static void interfacecheck(SS*, Type*, Type*);
static void prependunlocked(Definition*, FunctionQualifier);
static void prependself(SS*, Definition*);
static void acquirecheck(Definition*);
static ListValue armatch(ListValue, ListValue);

static void sem_definition(SS* ss, Definition* def) {
    switch (def->tag) {
    case DEFINITION_CAPSA:       sem_definition_capsa(ss, def);       break;
    case DECLARATION_FUNCTION:   sem_declaration_function(ss, def);   break;
    case DEFINITION_FUNCTION:    sem_definition_function(ss, def);    break;
    case DEFINITION_METHOD:      sem_definition_method(ss, def);      break;
    case DEFINITION_CONSTRUCTOR: sem_definition_constructor(ss, def); break;
    case DEFINITION_TYPE:
        switch (def->type->tag) {
        case TYPE_INTERFACE: sem_definition_interface(ss, def); break;
        case TYPE_STRUCTURE: sem_definition_structure(ss, def); break;
        case TYPE_MONITOR:   sem_definition_monitor(ss, def);   break;
        default:
            UNREACHABLE;
        }
        break;
    default:
        UNREACHABLE;
    }
}

static void sem_definition_capsa(SS* ss, Definition* def) {
    Capsa* capsa = def->capsa.capsa;

    // checkes if it's a definition (not a declaration)
    if (def->capsa.expression) {
        sem_expression(ss, def->capsa.expression);
    }

    // checks if the variable is being redeclared
    if (!symtable_insert(ss->table, def)) {
        err_redeclaration(capsa->id);
    }

    // gets the variable's type from the symbol table (for non-inferred types)
    if (capsa->type) {
        linktype(ss->table, &capsa->type);
    }

    // deals with the variable's definition and it's type inference (if any)
    if (def->capsa.expression) {
        assignment(capsa, &def->capsa.expression);
        if (capsa->global && !safetype(capsa->type)) {
            TODOERR(capsa->line, "global values must have safe types");
        }
    }
}

static void sem_declaration_function(SS* ss, Definition* def) {
    prependself(ss, def);
    sem_definition_function(ss, def);
    def->tag = DEFINITION_METHOD;
}

static void sem_definition_function(SS* ss, Definition* def) {
    assert(!insidemonitor(ss));

    if (def->function.qualifiers) { // not a normal function without qualifiers
        TODOERR(def->function.id->line,
            "private and acquire-release functions must "
            "be defined inside a monitor"
        );
    }

    // checks if the function is being redeclared
    if (!symtable_insert(ss->table, def)) {
        err_redeclaration(def->function.id);
    }

    // gets the function's return type from the symbol table
    linktype(ss->table, &def->function.type);

    symtable_enter_scope(ss->table);
    semfunction(ss, def);
    symtable_leave_scope(ss->table);
}

static void sem_definition_method(SS* ss, Definition* def) {
    assert(insidemonitor(ss));

    prependself(ss, def);

    // checks if the method is being redeclared
    if (!symtable_insert(ss->table, def)) {
        err_redeclaration(def->function.id);
    }

    // gets the function's return type from the symbol table
    linktype(ss->table, &def->function.type);

    symtable_enter_scope(ss->table);
    semfunction(ss, def);
    symtable_leave_scope(ss->table);
}

static void sem_definition_constructor(SS* ss, Definition* def) {
    assert(insidestructure(ss));
    ss->initializer = true;

    def->function.id = ss->structure->structure.id;
    def->function.type = ss->structure;

    // creates the <self> value to be returned by the constructor
    Capsa* self = astself(def->function.id->line);
    self->type = ss->structure;

    symtable_enter_scope(ss->table);
    symtable_insert(ss->table, ast_definition_capsa(self, NULL));
    semfunction(ss, def);
    symtable_leave_scope(ss->table);

    ss->initializer = false;
}

static void sem_definition_interface(SS* ss, Definition* def) {
    semstructure(ss, def);
}

static void sem_definition_structure(SS* ss, Definition* def) {
    semstructure(ss, def);
}

static void sem_definition_monitor(SS* ss, Definition* def) {
    // prepending the <unlocked> acquire-release
    // pair of functions to the monitor
    prependunlocked(def, FQ_RELEASE);
    prependunlocked(def, FQ_ACQUIRE);

    // generic interface/record/monitor analysis
    semstructure(ss, def);

    // checks interface implementation
    if (def->type->structure.interface) {
        linktype(ss->table, &def->type->structure.interface);
        interfacecheck(ss, def->type->structure.interface, def->type);
    }
}

// -----------------------------------------------------------------------------

// auxiliary - parameters and block
static void semfunction(SS* ss, Definition* def) {
    FOREACH(Definition, p, def->function.parameters) {
        sem_definition(ss, p);
        if (insidemonitor(ss) && !safetype(p->capsa.capsa->type)) {
            TODOERR(
                p->capsa.capsa->line,
                "parameters of monitor functions need to have safe types"
            );
        }
    }
    ss->return_ = def->function.type;
    if (!def->function.block) { // interface functions don't have blocks
        assert(def->tag == DECLARATION_FUNCTION);
    } else {
        sem_block(ss, def->function.block);
    }
    ss->return_ = NULL;
}

// auxiliary - interface, structures and monitors
static void semstructure(SS* ss, Definition* def) {
    def->type->structure.attributes_size = 0;
    def->type->structure.methods_size = 0;

    if (!symtable_insert(ss->table, def)) {
        err_redeclaration(def->type->structure.id);
    }
    ss->structure = def->type;
    symtable_enter_scope(ss->table);

    // capsas
    FOREACH(Definition, d, def->type->structure.definitions) {
        if (d->tag == DEFINITION_CAPSA) {
            sem_definition(ss, d);
            def->type->structure.attributes_size++;
        }
    }

    // FIXME: still possible to call a structure function from inside structure
    //        functions without adding "self." or "dog."
    // functions
    FOREACH(Definition, d, def->type->structure.definitions) {
        switch (d->tag) {
            case DECLARATION_FUNCTION:
                // fallthrough
            case DEFINITION_FUNCTION:
                // fallthrough
            case DEFINITION_METHOD:
                sem_definition(ss, d);
                def->type->structure.methods_size++;
                if (d->function.qualifiers & FQ_ACQUIRE) {
                    acquirecheck(d);
                }
                break;
            case DEFINITION_CONSTRUCTOR:
                def->type->structure.constructor = d;
                break;
            default:
                break;
        }
    }
    if (def->type->tag == TYPE_MONITOR) {
        if (!def->type->structure.constructor) {
            TODOERR(def->type->structure.id->line,
                "structure must define an initializer"
            );
        }
        sem_definition(ss, def->type->structure.constructor);
    }
    symtable_leave_scope(ss->table);
    ss->structure = NULL;

    MALLOC_ARRAY(
        def->type->structure.attributes,
        Definition*,
        def->type->structure.attributes_size
    );
    MALLOC_ARRAY(
        def->type->structure.methods,
        Definition*,
        def->type->structure.methods_size
    );
    unsigned int i_attributes = 0;
    unsigned int i_methods = 0;
    FOREACH(Definition, d, def->type->structure.definitions) {
        switch (d->tag) {
        case DEFINITION_CAPSA:
            def->type->structure.attributes[i_attributes++] = d;
            break;
        case DEFINITION_METHOD:
            // checks if all methods return types are safe (can't be placed
            // inside `sem_definition_method` in case one of the methods returns
            // the current monitor type)
            if (!safetype(d->function.type)) {
                err_monitor_function_type(d->function.id->line);
            }
            // fallthrough
        case DECLARATION_FUNCTION:
            // fallthrough
        case DEFINITION_FUNCTION:
            def->type->structure.methods[i_methods] = d;
            d->function.vmti = i_methods++; // TODO: backend.c
            break;
        case DEFINITION_CONSTRUCTOR:
            break;
        default:
            UNREACHABLE;
        }
    }
    assert(i_attributes == def->type->structure.attributes_size);
    assert(i_methods == def->type->structure.methods_size);

    { // checks for acquire-release pairs of functions
        List* ars = list_new(); // list with acquire-release functions

        // filling the list
        for (int i = 0; i < i_methods; i++) {
            Definition* d = def->type->structure.methods[i];
            Bitmap bm = d->function.qualifiers;
            if (bm & FQ_ACQUIRE || bm & FQ_RELEASE) {
                list_append(ars, d);
            }
        }

        // removing pairs from the list
        for (int i = 0; i < i_methods; i++) {
            Definition* d = def->type->structure.methods[i];
            if (list_remove(ars, &armatch, d)) { // removes match
                list_remove(ars, NULL, d); // removes self if found a match
            }
        }

        // checking if the list is empty
        if (!list_empty(ars)) {
            Definition* d = (Definition*) ars->first->value;
            TODOERR(d->function.id->line,
                "function needs its acquire-release pair"
            );
        }

        list_destroy(ars);
    }
}

// auxiliary - compares method declaration with interface function declaration
static bool functionequals(Definition* ifunction, Definition* method) {
    // same function name
    if (ifunction->function.id->name != method->function.id->name) {
        return false;
    }
    // same funtion returning type
    if (!typeequals(ifunction->function.type, method->function.type)) {
        return false;
    }

    // same parameters
    Definition* p2 = method->function.parameters;
    assert(p2);
    p2 = p2->next; // skipping <self> (also skips <self> for ifunction)

    FOREACH(Definition, p1, ifunction->function.parameters->next) {
        if (!p2) {
            return false; // parameters count
        }
        if (p1->capsa.capsa->id->name != p2->capsa.capsa->id->name) {
            return false; // parameter name
        }
        if (!typeequals(p1->capsa.capsa->type, p2->capsa.capsa->type)) {
            return false; // parameter type
        }
        p2 = p2->next;
    }
    if (p2) {
        return false; // parameters count
    }
    return true;
}

// auxiliary - checks if the interface is implemented by a type
static void interfacecheck(SS* ss, Type* interface, Type* type) {
    FOREACH(Definition, f1, interface->structure.definitions) {
        bool found = false;
        FOREACH(Definition, f2, type->structure.definitions) {
            if (f2->tag == DEFINITION_METHOD && functionequals(f1, f2)) {
                found = true;
                break;
            }
        }
        if (!found) {
            // TODO: "<name> does not implement interface <name>"
            TODOERR(type->structure.id->line, "interface not implemented");
        }
    }
}

// auxiliary - adds <unlocked> as functions of the monitor
static void prependunlocked(Definition* m, FunctionQualifier fq) {
    Line ln = m->type->structure.id->line;
    Type* return_type = (fq == FQ_ACQUIRE)
        ? ast_type_id(ast_id(ln, m->type->structure.id->name))
        : __void
        ;
    Definition* unlocked = ast_declaration_function(
        ast_id(ln, scanner_native[SCANNER_NATIVE_UNLOCKED]), NULL, return_type
    );
    unlocked = ast_definition_function(unlocked, ast_block(ln, NULL));
    unlocked->tag = DEFINITION_METHOD;
    unlocked->function.qualifiers |= fq;
    PREPEND(Definition, m->type->structure.definitions, unlocked);
}

// auxiliary - adds <self> as the first parameter to the function
static void prependself(SS* ss, Definition* def) {
    Line line = def->function.id->line;
    Capsa* self = astself(line);
    self->type = ast_type_id(ast_id(line, ss->structure->structure.id->name));
    Definition* self_definition = ast_definition_capsa(self, NULL);
    PREPEND(Definition, def->function.parameters, self_definition);
}

// checks if the acquire function returns a monitor type
static void acquirecheck(Definition* method) {
    if (method->function.type->tag != TYPE_MONITOR) {
        TODOERR(method->function.id->line,
            "an acquire function must always return a monitor type"
        );
    }
}

// auxiliary - called inside function `semstructure`
// compares function definitions and checks if they are an acquire-release pair
static ListValue armatch(ListValue x, ListValue y) {
    Definition* a = (Definition*) x;
    Definition* b = (Definition*) y;
    if (a->function.id->name != b->function.id->name) { // different names
        return NULL;
    }

    Bitmap am = a->function.qualifiers;
    Bitmap bm = b->function.qualifiers;
    if ((am & FQ_PRIVATE) != (bm & FQ_PRIVATE) || // incompatible `private` tag
        (am & FQ_ACQUIRE) == (bm & FQ_ACQUIRE) || // bot are `acquire`
        (am & FQ_RELEASE) == (bm & FQ_RELEASE)) { // both are `release`
        return NULL;
    }

    a->function.pair = b;
    b->function.pair = a;

    return x;
}

// ==================================================
//
//  Capsa
//
// ==================================================

static void sem_capsa_id(SS*, Capsa**);
static void sem_capsa_attribute(SS*, Capsa**);
static void sem_capsa_indexed(SS*, Capsa**);

static void sem_capsa(SS* ss, Capsa** capsa_pointer) {
    switch ((*capsa_pointer)->tag) {
    case CAPSA_ID:        sem_capsa_id(ss, capsa_pointer);        break;
    case CAPSA_ATTRIBUTE: sem_capsa_attribute(ss, capsa_pointer); break;
    case CAPSA_INDEXED:   sem_capsa_indexed(ss, capsa_pointer);   break;
    default:
        UNREACHABLE;
    }
}

static void sem_capsa_id(SS* state, Capsa** capsa_pointer) {
    Capsa* capsa = *capsa_pointer;
    assert(!capsa->type);

    Definition* definition = symtable_find(state->table, capsa->id);
    if (!definition) {
        err_capsa_unknown(capsa->id);
    }
    if (definition->tag != DEFINITION_CAPSA) {
        err_capsa_misuse(capsa->id);
    }

    assert(definition->capsa.capsa->tag == CAPSA_ID);

    bool from_outside_spawn_scope = false;
    if (state->spawn.scope) {
        if (!symtable_find_in_scope(state->spawn.scope, capsa->id)) {
            if (!definition->capsa.capsa->value) {
                err_spawn_capsa(capsa->line);
            }
            if (!safetype(definition->capsa.capsa->type)) {
                err_spawn_unsafe(capsa->line);
            }
            from_outside_spawn_scope = true;
        }
    }

    // TODO: this is messy [and wrong] (define lambdas someday)
    if (from_outside_spawn_scope && !capsa->global) {
        FunctionCall* call = state->spawn.function_call;
        Definition* function = call->fn;
        Type* type = definition->capsa.capsa->type;

        Capsa* parameter_capsa = ast_capsa_id(
            ast_id(capsa->line, capsa->id->name)
        );
        parameter_capsa->type = type;
        Definition* parameter = ast_definition_capsa(
            parameter_capsa, NULL
        );
        parameter->capsa.capsa->value = true;
        parameter->capsa.capsa->type = type;
        symtable_insert(state->table, parameter);

        Expression* argument = ast_expression_capsa(
            definition->capsa.capsa
        );
        argument->type = type;

        if (call->argc == -1) {
            call->arguments = argument;
            call->argc = 1;
            function->function.parameters = parameter;
        } else if (call->argc > 0) {
            argument->next = call->arguments;
            call->arguments = argument;
            call->argc++;
            parameter->next = function->function.parameters;
            function->function.parameters = parameter;
        } else {
            UNREACHABLE;
        }

        free(capsa->id);
        free(capsa);
        *capsa_pointer = parameter->capsa.capsa;
    } else {
        free(capsa->id);
        free(capsa);
        *capsa_pointer = definition->capsa.capsa;
    }
}

static void sem_capsa_attribute(SS* ss, Capsa** capsa_pointer) {
    Capsa* capsa = *capsa_pointer;
    sem_expression(ss, capsa->attribute.structure);
    Capsa* attribute = findattribute(capsa);
    if (!attribute) {
        TODOERR(capsa->line, "structure does not contain attribute");
    }
    capsa->type = attribute->type;
}

static void sem_capsa_indexed(SS* ss, Capsa** capsa_pointer) {
    Capsa* capsa = *capsa_pointer;
    assert(!capsa->type);

    sem_expression(ss, capsa->indexed.array);
    sem_expression(ss, capsa->indexed.index);
    if (capsa->indexed.array->type->tag != TYPE_ARRAY) {
        err_capsa_array_type(capsa);
    }
    if (!indextype(capsa->indexed.index->type)) {
        err_capsa_array_index_type(capsa);
    }
    capsa->type = capsa->indexed.array->type->array;
}

// ==================================================
//
//  Block
//
// ==================================================

static void sem_block(SS* ss, Block* block) {
    assert(block->tag == BLOCK);
    FOREACH(Block, b, block->next) {
        switch (b->tag) {
        case BLOCK_DEFINITION:
            FOREACH(Definition, d, b->definition) {
                sem_definition(ss, d);
            }
            break;
        case BLOCK_STATEMENT:
            sem_statement(ss, b->statement);
            break;
        default:
            UNREACHABLE;
        }
    }
}

// ==================================================
//
//  Statement
//
// ==================================================

static void sem_statement_assignment(SS*, Statement*);
static void sem_statement_function_call(SS*, Statement*);
static void sem_statement_wait_for_in(SS*, Statement*);
static void sem_statement_signal(SS*, Statement*);
static void sem_statement_broadcast(SS*, Statement*);
static void sem_statement_return(SS*, Statement*);
static void sem_statement_if(SS*, Statement*);
static void sem_statement_if_else(SS*, Statement*);
static void sem_statement_while(SS*, Statement*);
static void sem_statement_numeric_for(SS*, Statement*);
static void sem_statement_for(SS*, Statement*);
static void sem_statement_spawn(SS*, Statement*);
static void sem_statement_acquire_value(SS*, Statement*);
static void sem_statement_block(SS*, Statement*);

static void sem_statement(SS* ss, Statement* stmt) {
    switch (stmt->tag) {
    case STATEMENT_ASSIGNMENT:    sem_statement_assignment(ss, stmt);    break;
    case STATEMENT_FUNCTION_CALL: sem_statement_function_call(ss, stmt); break;
    case STATEMENT_WAIT_FOR_IN:   sem_statement_wait_for_in(ss, stmt);   break;
    case STATEMENT_SIGNAL:        sem_statement_signal(ss, stmt);        break;
    case STATEMENT_BROADCAST:     sem_statement_broadcast(ss, stmt);     break;
    case STATEMENT_RETURN:        sem_statement_return(ss, stmt);        break;
    case STATEMENT_IF:            sem_statement_if(ss, stmt);            break;
    case STATEMENT_IF_ELSE:       sem_statement_if_else(ss, stmt);       break;
    case STATEMENT_WHILE:         sem_statement_while(ss, stmt);         break;
    case STATEMENT_NUMERIC_FOR:   sem_statement_numeric_for(ss, stmt);   break;
    case STATEMENT_FOR:           sem_statement_for(ss, stmt);           break;
    case STATEMENT_SPAWN:         sem_statement_spawn(ss, stmt);         break;
    case STATEMENT_ACQUIRE_VALUE: sem_statement_acquire_value(ss, stmt); break;
    case STATEMENT_BLOCK:         sem_statement_block(ss, stmt);         break;
    default:
        UNREACHABLE;
    }
}

static void sem_statement_assignment(SS* ss, Statement* stmt) {
    sem_capsa(ss, &stmt->assignment.capsa);
    if (stmt->assignment.capsa->value) { // can't be reassigned
        err_assignment_value(stmt);
    }
    if (stmt->assignment.capsa->tag == CAPSA_INDEXED &&
        stmt->assignment.capsa->indexed.array->type->immutable) {
        TODOERR(stmt->line, "can't assign to immutable arrays");
    }
    sem_expression(ss, stmt->assignment.expression);
    assignment(stmt->assignment.capsa, &stmt->assignment.expression);
}

static void sem_statement_function_call(SS* ss, Statement* stmt) {
    sem_function_call(ss, stmt->function_call);
}

static void sem_statement_wait_for_in(SS* ss, Statement* stmt) {
    if (!insidemonitor(ss)) {
        err_monitor_statements(stmt->line, "wait-for-in");
    }
    if (ss->initializer) {
        err_monitor_statements_constructor(stmt->line, "wait-for-in");
    }
    sem_expression(ss, stmt->wait_for_in.condition);
    if (!conditiontype(stmt->wait_for_in.condition->type)) {
        err_invalid_condition_type(stmt->wait_for_in.condition);
    }
    sem_expression(ss, stmt->wait_for_in.queue);
    if (stmt->wait_for_in.queue->type != __condition_queue) {
        TODOERR(stmt->line,
            "'wait-for-in' statement's second expression must be "
            "of type ConditionQueue"
        );
    }
}

static void sem_statement_signal(SS* ss, Statement* stmt) {
    if (!insidemonitor(ss)) {
        err_monitor_statements(stmt->line, "signal");
    }
    if (ss->initializer) {
        err_monitor_statements_constructor(stmt->line, "signal");
    }
    sem_expression(ss, stmt->signal);
    if (stmt->signal->type != __condition_queue) {
        TODOERR(stmt->line,
            "'signal' statement must receive an expression"
            " of type ConditionQueue"
        );
    }
}

static void sem_statement_broadcast(SS* ss, Statement* stmt) {
    if (!insidemonitor(ss)) {
        err_monitor_statements(stmt->line, "broadcast");
    }
    if (ss->initializer) {
        err_monitor_statements_constructor(stmt->line, "broadcast");
    }
    sem_expression(ss, stmt->broadcast);
    if (stmt->broadcast->type != __condition_queue) {
        TODOERR(stmt->line,
            "'broadcast' statement must receive an expression"
            " of type ConditionQueue"
        );
    }
}

static void sem_statement_return(SS* ss, Statement* stmt) {
    // can't return inside a spawn block
    if (ss->spawn.scope) {
        err_return_inside_spawn(stmt->line);
    }
    // can't return inside an acquire-value block
    if (ss->acquire_value) {
        TODOERR(stmt->line, "can't return inside an acquire-value block");
    }
    // can't return an expression inside an initializer
    if (ss->initializer) {
        if (stmt->return_) {
            err_return_initializer(stmt->line);
        } else {
            return;
        }
    }
    // can't return empty when the function expects a return type
    if (ss->return_ != __void && !stmt->return_) {
        err_return_void(stmt->line, ss->return_);
    }
    if (stmt->return_) {
        sem_expression(ss, stmt->return_);
        typecheck1(ss->return_, &stmt->return_);
    }
}

static void sem_statement_if(SS* ss, Statement* stmt) {
    sem_expression(ss, stmt->if_.expression);
    if (!conditiontype(stmt->if_.expression->type)) {
        err_invalid_condition_type(stmt->if_.expression);
    }
    symtable_enter_scope(ss->table);
    sem_block(ss, stmt->if_.block);
    symtable_leave_scope(ss->table);
}

static void sem_statement_if_else(SS* ss, Statement* stmt) {
    sem_expression(ss, stmt->if_else.expression);
    if (!conditiontype(stmt->if_else.expression->type)) {
        err_invalid_condition_type(stmt->if_else.expression);
    }
    symtable_enter_scope(ss->table);
    sem_block(ss, stmt->if_else.if_block);
    symtable_leave_scope(ss->table);
    symtable_enter_scope(ss->table);
    sem_block(ss, stmt->if_else.else_block);
    symtable_leave_scope(ss->table);
}

static void sem_statement_while(SS* ss, Statement* stmt) {
    sem_expression(ss, stmt->while_.expression);
    if (!conditiontype(stmt->while_.expression->type)) {
        err_invalid_condition_type(stmt->while_.expression);
    }
    symtable_enter_scope(ss->table);
    sem_block(ss, stmt->while_.block);
    symtable_leave_scope(ss->table);
}

static void sem_statement_numeric_for(SS* ss, Statement* stmt) {
    stmt->numeric_for.v->capsa.capsa->value = true;
    symtable_enter_scope(ss->table);
    sem_expression(ss, stmt->numeric_for.range);
    sem_definition(ss, stmt->numeric_for.v);
    Capsa* v = stmt->numeric_for.v->capsa.capsa;
    v->type = stmt->numeric_for.range->range.first->type;
    sem_block(ss, stmt->numeric_for.block);
    symtable_leave_scope(ss->table);
    stmt->numeric_for.v->capsa.capsa->value = false;
}

static void sem_statement_for(SS* ss, Statement* stmt) {
    symtable_enter_scope(ss->table);
    sem_definition(ss, stmt->for_.initialization);
    sem_expression(ss, stmt->for_.condition);
    if (!conditiontype(stmt->for_.condition->type)) {
        err_invalid_condition_type(stmt->for_.condition);
    }
    sem_statement(ss, stmt->for_.increment);
    sem_block(ss, stmt->for_.block);
    symtable_leave_scope(ss->table);
}

static void sem_statement_spawn(SS* ss, Statement* stmt) {
    if (insidemonitor(ss)) {
        TODOERR(stmt->line, "can't spawn inside monitors");
    }

    Scope* previous_spawn_scope = ss->spawn.scope;
    FunctionCall* previous_spawn_function_call = ss->spawn.function_call;

    ss->spawn.scope = symtable_enter_scope(ss->table);
    ss->spawn.function_call = stmt->spawn;
    sem_block(ss, stmt->spawn->fn->function.block);
    symtable_leave_scope(ss->table);

    ss->spawn.scope = previous_spawn_scope;
    ss->spawn.function_call = previous_spawn_function_call;
}

static void sem_statement_acquire_value(SS* ss, Statement* stmt) {
    bool previous;

    symtable_enter_scope(ss->table);

    previous = ss->can_acquire;
    ss->can_acquire = true;
    sem_definition(ss, stmt->acquire_value.value);
    ss->can_acquire = previous;

    Type* unlocked = stmt->acquire_value.value->capsa.capsa->type;
    unlocked = ast_type_unlocked(unlocked);
    stmt->acquire_value.value->capsa.capsa->type = unlocked;

    previous = ss->acquire_value;
    ss->acquire_value = true;
    sem_block(ss, stmt->acquire_value.block);
    ss->acquire_value = previous;

    symtable_leave_scope(ss->table);
}

static void sem_statement_block(SS* ss, Statement* stmt) {
    symtable_enter_scope(ss->table);
    sem_block(ss, stmt->block);
    symtable_leave_scope(ss->table);
}

// ==================================================
//
//  TODO
//
// ==================================================

static void sem_expression_literal_array(SS*, Expression*);
static void sem_expression_unary(SS*, Expression*);
static void sem_expression_binary(SS*, Expression*);

static void sem_expression_unary_minus(SS*, Expression*);
static void sem_expression_unary_not(SS*, Expression*);

static void sem_expression_binary_logic(SS*, Expression*);
static void sem_expression_binary_equality(SS*, Expression*);
static void sem_expression_binary_inequality(SS*, Expression*);
static void sem_expression_binary_arithmetic(SS*, Expression*);

static void semliteralarray(Expression*);

static void sem_expression(SS* ss, Expression* exp) {
    switch (exp->tag) {
    case EXP_LITERAL_BOOLEAN:
        exp->type = __boolean;
        break;
    case EXP_LITERAL_INTEGER:
        exp->type = __integer;
        break;
    case EXP_LITERAL_FLOAT:
        exp->type = __float;
        break;
    case EXP_LITERAL_STRING:
        exp->type = __string;
        break;
    case EXP_LITERAL_ARRAY:
        sem_expression_literal_array(ss, exp);
        break;
    case EXP_CAPSA:
        sem_capsa(ss, &exp->capsa);
        exp->type = exp->capsa->type;
        break;
    case EXP_FUNCTION_CALL:
        sem_function_call(ss, exp->function_call);
        exp->type = exp->function_call->type;
        break;
    case EXP_COMPREHENSION: {
        symtable_enter_scope(ss->table);
        sem_expression(ss, exp->comprehension.iterable);
        sem_definition(ss, exp->comprehension.v);
        Capsa* v = exp->comprehension.v->capsa.capsa;
        v->type = exp->comprehension.iterable->range.first->type;
        sem_expression(ss, exp->comprehension.e);
        symtable_leave_scope(ss->table);
        if (exp->comprehension.e->type == __void) {
            TODOERR(exp->line, "invalid Void element in list comprehension");
        }
        exp->type = ast_type_array(exp->comprehension.e->type);
        exp->type->immutable = exp->comprehension.immutable;
        if (exp->type->immutable && !exp->type->array->immutable) {
            TODOERR(exp->line,
                "immutable list comprehension must have immutable elements"
            );
        }
        break;
    }
    case EXP_RANGE: {
        sem_expression(ss, exp->range.first);
        if (!numerictype(exp->range.first->type)) {
            goto ERROR_RANGE_NOT_NUMERIC;
        }
        bool isfloat = exp->range.first->type == __float;
        if (exp->range.second) {
            sem_expression(ss, exp->range.second);
            if (!numerictype(exp->range.second->type)) {
                goto ERROR_RANGE_NOT_NUMERIC;
            }
            isfloat = isfloat || (exp->range.second->type == __float);
        }
        sem_expression(ss, exp->range.last);
        if (!numerictype(exp->range.last->type)) {
            goto ERROR_RANGE_NOT_NUMERIC;
        }
        isfloat = isfloat || (exp->range.last->type == __float);
        
        if (isfloat) {
            typecheck1(__float, &exp->range.first);
            if (exp->range.second) {
                typecheck1(__float, &exp->range.second);
            }
            typecheck1(__float, &exp->range.last);
        }
        
        exp->type = NULL; // TODO
        break;
        ERROR_RANGE_NOT_NUMERIC: {
            TODOERR(exp->line,
                "elements of a range expression must have numeric types"
            );
        }
    }
    case EXP_UNARY:
        sem_expression_unary(ss, exp);
        break;
    case EXP_BINARY:
        sem_expression_binary(ss, exp);
        break;
    case EXP_CAST:
        sem_expression(ss, exp->cast);
        linktype(ss->table, &exp->type);
        break;
    default:
        UNREACHABLE;
    }
}

static void sem_expression_literal_array(SS* ss, Expression* exp) {
    FOREACH(Expression, e, exp->literal.array) {
        sem_expression(ss, e);
        typecheck2(&exp->literal.array, &e);
    }
    FOREACH(Expression, e, exp->literal.array->next) { // not redundant
        if (!typecheck2(&exp->literal.array, &e)) {
            TODOERR(exp->line,
                "elements of an array literal must have equivalent types"
            );
        }
    }
    exp->type = ast_type_array(exp->literal.array->type);
    if ((exp->type->immutable = exp->literal.immutable)) {
        FOREACH(Expression, e, exp->literal.array) {
            semliteralarray(e);
        }
    }
}

static void sem_expression_unary(SS* ss, Expression* exp) {
    sem_expression(ss, exp->unary.expression);
    switch (exp->unary.token) {
    case '-':    sem_expression_unary_minus(ss, exp); break;
    case TK_NOT: sem_expression_unary_not(ss, exp);   break;
    default:
        UNREACHABLE;
    }
}

static void sem_expression_binary(SS* ss, Expression* exp) {
    sem_expression(ss, exp->binary.left_expression);
    sem_expression(ss, exp->binary.right_expression);
    switch (exp->binary.token) {
    case TK_OR: case TK_AND:
        sem_expression_binary_logic(ss, exp);
        break;
    case TK_EQUAL: case TK_NEQUAL:
        sem_expression_binary_equality(ss, exp);
        break;
    case TK_LEQUAL: case TK_GEQUAL: case '<': case '>':
        sem_expression_binary_inequality(ss, exp);
        break;
    case '+': case '-': case '*': case '/':
        sem_expression_binary_arithmetic(ss, exp);
        break;
    default:
        UNREACHABLE;
    }
}

static void sem_expression_unary_minus(SS* ss, Expression* exp) {
    if (!numerictype(exp->unary.expression->type)) {
        err_expression(ERR_EXP_MINUS, exp);
    }
    exp->type = exp->unary.expression->type;
}

static void sem_expression_unary_not(SS* ss, Expression* exp) {
    if (!conditiontype(exp->unary.expression->type)) {
        err_expression(ERR_EXP_NOT, exp);
    }
    exp->type = __boolean;
}

static void sem_expression_binary_logic(SS* ss, Expression* exp) {
    if (!conditiontype(exp->binary.left_expression->type)) {
        err_expression(ERR_EXP_LEFT, exp);
    }
    if (!conditiontype(exp->binary.right_expression->type)) {
        err_expression(ERR_EXP_RIGHT, exp);
    }
    exp->type = __boolean;
}

static void sem_expression_binary_equality(SS* ss, Expression* exp) {
    if (!equatabletype(exp->binary.left_expression->type)) {
        err_expression(ERR_EXP_LEFT_EQUAL, exp);
    }
    if (!equatabletype(exp->binary.right_expression->type)) {
        err_expression(ERR_EXP_RIGHT_EQUAL, exp);
    }
    Expression** l = &exp->binary.left_expression;
    Expression** r = &exp->binary.right_expression;
    if (!typecheck2(l, r)) {
        err_expression(ERR_EXP_TYPECHECK_EQUAL, exp);
    }
    exp->type = __boolean;
}

static void sem_expression_binary_inequality(SS* ss, Expression* exp) {
    sem_expression_binary_arithmetic(ss, exp);
    exp->type = __boolean;
}

static void sem_expression_binary_arithmetic(SS* ss, Expression* exp) {
    if (!numerictype(exp->binary.left_expression->type)) {
        err_expression(ERR_EXP_LEFT, exp);
    }
    if (!numerictype(exp->binary.right_expression->type)) {
        err_expression(ERR_EXP_RIGHT, exp);
    }
    Expression** l = &exp->binary.left_expression;
    Expression** r = &exp->binary.right_expression;
    exp->type = typecheck2(l, r); // for casting, if necessary
    assert(exp->type);
}

// -----------------------------------------------------------------------------

static void semliteralarray(Expression* e) {
    if (e->type->tag != TYPE_ARRAY) {
        return;
    }
    if (e->tag == EXP_LITERAL_ARRAY) {
        if (!e->literal.immutable) {
            e->literal.immutable = true;
            e->type->immutable = true;
        }
        return semliteralarray(e->literal.array);
    }
    if (!e->type->immutable) {
        TODOERR(e->line, "an immutable array can not contain a mutable array");
    }
}

// ==================================================
//
//  FunctionCall
//
// ==================================================

static void sem_function_call_basic(SS*, FunctionCall*);
static void sem_function_call_method(SS*, FunctionCall*);
static void sem_function_call_constructor(SS*, FunctionCall*);

static void sem_function_call_constructor_native(SS*, FunctionCall*);
static void sem_function_call_constructor_unlocked(SS*, FunctionCall*);
static void sem_function_call_constructor_array(SS*, FunctionCall*);
static void sem_function_call_constructor_monitor(SS*, FunctionCall*);

static bool nativefunction(SS*, FunctionCall*);
static void checkarguments(SS*, FunctionCall*);
static Definition* findmethod(FunctionCall*, bool);

#define countarguments(fc) do { \
    fc->argc = 0; \
    FOREACH(Expression, e, fc->arguments) { \
        fc->argc++; \
    } \
} while (0) \

static void sem_function_call(SS* ss, FunctionCall* call) {
    switch (call->tag) {
    case FUNCTION_CALL_BASIC:
        sem_function_call_basic(ss, call);
        break;
    case FUNCTION_CALL_METHOD:
        sem_function_call_method(ss, call);
        break;
    case FUNCTION_CALL_CONSTRUCTOR:
        sem_function_call_constructor(ss, call);
        break;
    default:
        UNREACHABLE;
    }
}

static void sem_function_call_basic(SS* ss, FunctionCall* call) {
    if (nativefunction(ss, call)) {
        return;
    }

    // looks for the function definition in the table of symbols
    call->fn = symtable_find(ss->table, call->id);
    if (!call->fn) {
        err_function_call_unknown(call->id);
    }
    call->type = call->fn->function.type;

    // checks for calls over types that are not functions
    DefinitionTag tag = call->fn->tag;
    if (tag != DEFINITION_FUNCTION && tag != DEFINITION_METHOD) {
        err_function_call_misuse(call->id);
    }

    // prepends <self> for method calls inside monitors
    if (call->fn->tag == DEFINITION_METHOD) {
        assert(insidemonitor(ss));
        PREPEND(Expression,
            call->arguments,
            ast_expression_capsa(astself(call->line))
        );
    }

    // frees <call->id>
    // <call->type> already contains the id with the function's name
    free(call->id);
    call->id = NULL;

    // checks for non-safe function calls inside a monitor
    if (insidemonitor(ss) && !safetype(call->type)) {
        TODOERR(call->line,
            "can't call a function that returns a "
            "non-safe type from inside a monitor"
        );
    }

    checkarguments(ss, call);
}

static void sem_function_call_method(SS* ss, FunctionCall* call) {
    sem_expression(ss, call->obj);
    assert(call->obj->type); // instance type should be linked already

    // can only call methods over interfaces, records, and monitors
    if (call->obj->type->tag == TYPE_VOID      ||
        call->obj->type->tag == TYPE_ID        ||
        call->obj->type->tag == TYPE_ARRAY     ||
        call->obj->type->tag == TYPE_STRUCTURE) {
        err_function_call_no_monitor(call->line);
    }

    // finds the function's definition in the monitor and sets the call's type
    call->fn = findmethod(call, ss->can_acquire);
    call->type = call->fn->function.type;

    // prepends <self> to arguments
    PREPEND(Expression, call->arguments, call->obj);
    if (call->arguments->next) {
        call->arguments->next->previous = call->obj;
    }

    // frees <call->id>
    // <call->type> already contains the id with the function's name
    free(call->id);
    call->id = NULL;

    checkarguments(ss, call);
}

static void sem_function_call_constructor(SS* ss, FunctionCall* call) {
    linktype(ss->table, &call->type);

    switch (call->type->tag) {
    case TYPE_ID:
        sem_function_call_constructor_native(ss, call);
        break;
    case TYPE_UNLOCKED:
        sem_function_call_constructor_unlocked(ss, call);
        break;
    case TYPE_ARRAY:
        sem_function_call_constructor_array(ss, call);
        break;
    case TYPE_MONITOR:
        sem_function_call_constructor_monitor(ss, call);
        break;
    default:
        UNREACHABLE;
    }
}

static void sem_function_call_constructor_native(SS* ss, FunctionCall* call) {
    if (call->type == __condition_queue) {
        countarguments(call);
        if (call->argc != 0) {
            TODOERR(call->line, "ConditionQueue constructor has no parameters");
        }
    } else if (call->type == __integer) {
        // TODO: test
        countarguments(call);
        if (call->argc != 1) {
            TODOERR(call->line, "Integer() receives one parameter");
        }
        sem_expression(ss, call->arguments);
        if (!numerictype(call->arguments->type)) {
            TODOERR(call->line, "Integer() receives a numeric parameter");
        }
    } else {
        UNREACHABLE;
    }
}

static void sem_function_call_constructor_unlocked(SS* ss, FunctionCall* call) {
    TODOERR(call->line, "unlocked types don't have constructors");
}

static void sem_function_call_constructor_array(SS* ss, FunctionCall* call) {
    countarguments(call);
    switch (call->argc) {
    case 0:
        // defaults to 8 (TODO: remove)
        call->argc = 1;
        call->arguments = ast_expression_literal_integer(call->line, 8);
        call->arguments->type = __integer;
        break;
    case 1:
        sem_expression(ss, call->arguments);
        typecheck1(__integer, &call->arguments);
        break;
    default:
        err_function_call_array_constructor(call->line, call->argc);
    }
}

static void sem_function_call_constructor_monitor(SS* ss, FunctionCall* call) {
    // finds the constructor inside the monitor (no overloading)
    FOREACH(Definition, d, call->type->structure.definitions) {
        if (d->tag == DEFINITION_CONSTRUCTOR) {
            call->fn = d;
            break;
        }
    }

    if (!call->fn) {
        err_function_call_no_constructor(call->line, call->type->structure.id);
    }

    checkarguments(ss, call);
}

// -----------------------------------------------------------------------------

// auxiliary - deals with native function calls
static bool nativefunction(SS* ss, FunctionCall* fc) {
    countarguments(fc);

    // FIXME: compare pointer from the definition of print (from symtable)
    if (!strcmp(fc->id->name, "print")) {
        fc->type = __integer;
        FOREACH(Expression, e, fc->arguments) {
            sem_expression(ss, e);
        }
        return true;
    }
    if (!strcmp(fc->id->name, "rand")) {
        if (fc->argc != 0) {
            TODOERR(fc->line, "function <rand> receives no arguments");
        }
        fc->type = __integer;
        return true;
    }
    if (!strcmp(fc->id->name, "srand")) {
        if (fc->argc != 1) {
            TODOERR(fc->line, "function <srand> receives one argument");
        }
        sem_expression(ss, fc->arguments);
        typecheck1(__integer, &fc->arguments);
        fc->type = __void;
        return true;
    }
    if (!strcmp(fc->id->name, "getTime")) {
        if (fc->argc != 0) {
            TODOERR(fc->line, "function <getTime> receives no arguments");
        }
        fc->type = __float;
        return true;
    }
    if (!strcmp(fc->id->name, "assert")) {
        if (fc->argc != 2) {
            TODOERR(fc->line, "function <assert> receives two arguments");
        }
        sem_expression(ss, fc->arguments);
        typecheck1(__boolean, &fc->arguments);
        sem_expression(ss, fc->arguments->next);
        typecheck1(__string, &fc->arguments->next);
        fc->type = __void;
        return true;
    }
    return false;
}

// auxiliary - compares parameters from the function's definition against
// arguments from the function's call
static void checkarguments(SS* ss, FunctionCall* call) {
    assert(call->fn);

    countarguments(call);

    Definition* parameter = call->fn->function.parameters;
    Expression* argument = call->arguments;
    Expression** pointer = &call->arguments;

    // skips comparing between the first parameter and argument of a method
    if (call->tag == FUNCTION_CALL_METHOD) {
        assert(parameter && argument);
        parameter = parameter->next;
        argument = (*pointer)->next;
        pointer = &argument;
    }

    // compares arguments with parameters
    while (parameter || argument) {
        if (parameter && !argument) {
            err_function_call_few_args(call->line);
        }
        if (!parameter && argument) {
            err_function_call_excess_args(call->line);
        }

        sem_expression(ss, argument);
        typecheck1(parameter->capsa.capsa->type, pointer);

        parameter = parameter->next;
        argument = (*pointer)->next;
        pointer = &argument;
    }
}

// auxiliary - finds a function's definition inside a monitor
static Definition* findmethod(FunctionCall* call, bool acquire) {
    // TODO: currently checking only the first matching method & no overloading

    Definition* method_definition = NULL;
    Type* structure = call->obj->type;
    if (structure->tag == TYPE_UNLOCKED) {
        structure = structure->unlocked;
    }

    FOREACH(Definition, d, structure->structure.definitions) {
        if ((d->tag == DEFINITION_METHOD || d->tag == DECLARATION_FUNCTION) &&
             d->function.id->name == call->id->name &&
             !(d->function.qualifiers & FQ_RELEASE) &&
             IFF(acquire, d->function.qualifiers & FQ_ACQUIRE)) {

            if (d->function.qualifiers & FQ_PRIVATE) {
                err_function_call_private(call->line, call->id, structure);
            }
            method_definition = d;
            break;
        }
    }

    if (!method_definition) {
        err_function_call_no_method(call->line, structure, call->id, acquire);
    }

    return method_definition;
}

// ==================================================
//
//  Auxiliary TODO
//
// ==================================================

static void freetypeid(Type* type) {
    assert(type->tag == TYPE_ID);
    if (type->primitive) {
        return;
    }
    free(type->id);
    free(type);
}

/*
 * Replaces an id-type for its declaration equivalent using the symbol table.
 * Deals with errors internally.
 */
static void linktype(SymbolTable* table, Type** pointer) {
    assert(pointer && *pointer);

    switch ((*pointer)->tag) {
    case TYPE_VOID:
        // It's not necessary to look for in the symbol table because
        // TypeVoid instance is not provided by the user as an id
        break;
    case TYPE_ID: {
        Definition* definition = symtable_find(table, (*pointer)->id);
        if (!definition) {
            err_unknown_type((*pointer)->id);
        }
        assert(definition->tag == DEFINITION_TYPE);
        freetypeid(*pointer);
        *pointer = definition->type;
        break;
    }
    case TYPE_UNLOCKED:
        linktype(table, &(*pointer)->unlocked);
        if (!((*pointer)->unlocked->tag == TYPE_MONITOR ||
            (*pointer)->unlocked->tag == TYPE_INTERFACE )) {
            TODOERR(-1,
                "a unlocked type must be an interface or a monitor"
            );
        }
        break;
    case TYPE_ARRAY:
        for (; (*pointer)->tag == TYPE_ARRAY; pointer = &(*pointer)->array);
        linktype(table, pointer);
        break;
    default:
        UNREACHABLE;
    }
}

/*
 * Checks for type equivalence between capsa and expression.
 * Works for definitions and simple assignments.
 * Deals with errors internally.
 */
static void assignment(Capsa* capsa, Expression** expression) {
    if ((*expression)->type == __void) {
        TODOERR(capsa->line, "can't assign to expression of type Void");
    }
    if (capsa->type) {
        typecheck1(capsa->type, expression);
    } else { // when defining with implicit type
        capsa->type = (*expression)->type;
    }
}

/*
 * Checks for type equality between two types.
 */
static bool typeequals(Type* type1, Type* type2) {
    if (type1->tag == TYPE_UNLOCKED && type2->tag == TYPE_UNLOCKED) {
        return typeequals(type1->unlocked, type2->unlocked);
    } else if (type1->tag == TYPE_UNLOCKED || type2->tag == TYPE_UNLOCKED) {
        return false;
    }
    if (type1->immutable != type2->immutable) {
        return false;
    }
    if (type1 == type2) {
        return true;
    }
    if (type1->tag != TYPE_ARRAY || type2->tag != TYPE_ARRAY) {
        return false;
    }
    return typeequals(type1->array, type2->array);
}

/*
 * Checks for type equivalence of one expression.
 * Performes casts if necessary.
 * Deals with errors internally.
 */
static void typecheck1(Type* type, Expression** e) {
    // checks equality
    if (typeequals(type, (*e)->type)) {
        return;
    }

    // implicitly casts from Integer to Float
    if (!(type == __float && (*e)->type == __integer)) {
        err_type((*e)->line, type, (*e)->type);
    }

    // performs the cast
    *e = ast_expression_cast((*e)->line, *e, type);
}

/*
 * Checks for type equivalence of two expressions.
 * Performes casts if necessary.
 * Returns NULL if types are incompatible.
 */
static Type* typecheck2(Expression** l, Expression** r) {
    Type* t1 = (*l)->type;
    Type* t2 = (*r)->type;

    // checks equality
    if (typeequals(t1, t2)) {
        return t1;
    }

    // needs both to be numeric
    if (!(numerictype(t1) && numerictype(t2))) {
        return NULL;
    }

    // performs casts
    if (t1 == __float) {
        return (*r = ast_expression_cast((*r)->line, *r, __float))->type;
    }
    if (t2 == __float) {
        return (*l = ast_expression_cast((*l)->line, *l, __float))->type;
    }

    UNREACHABLE;
}

static bool indextype(Type* type) {
    return type == __integer;
}

static bool numerictype(Type* type) {
    return type == __integer || type == __float;
}

static bool conditiontype(Type* type) {
    return type == __boolean;
}

static bool equatabletype(Type* type) { // TODO: Strings?
    return type == __boolean || type == __integer || type == __float;
}

static bool safetype(Type *type) {
    switch (type->tag) {
        case TYPE_VOID:
            // fallthrough
        case TYPE_ID:
            return type->immutable;
        case TYPE_UNLOCKED:
            return false;
        case TYPE_ARRAY:
            return type->immutable && safetype(type->array);
        case TYPE_INTERFACE:
            return true; // NOTE: for now
        case TYPE_STRUCTURE:
            return false; // TODO
        case TYPE_MONITOR:
            return true;
        default:
            UNREACHABLE;
    }
    UNREACHABLE;
}

// ==================================================
//
//  Errors
//
// ==================================================

// returns the string corresponding to the type
static const char* typestring(Type* type) {
    assert(type);
    char* str;

    switch (type->tag) {
    case TYPE_VOID:
        str = "Void";
        break;
    case TYPE_ID:
        str = (char*) type->id->name;
        break;
    case TYPE_UNLOCKED: {
        const char* aux = typestring(type->unlocked);
        MALLOC_ARRAY(str, char, strlen(aux) + 2);
        strcpy(str, aux);
        strcat(str, "!");
        break;
    }
    case TYPE_ARRAY: {
        unsigned int counter = 0;
        Type* t = type;
        for (; t->tag == TYPE_ARRAY; t = t->array, counter++);
        const char* id = typestring(t);
        size_t len = 2 * counter + strlen(id);
        MALLOC_ARRAY(str, char, len + 1);
        strcpy(str + counter, id);
        for (int i = 0; i < counter; i++) {
            str[i] = '[';
            str[len - i - 1] = ']';
        }
        str[len] = '\0';
        break;
    }
    case TYPE_INTERFACE:
        str = (char*) type->structure.id->name;
        break;
    case TYPE_STRUCTURE:
        UNREACHABLE;
    case TYPE_MONITOR:
        str = (char*) type->structure.id->name;
        break;
    default:
        UNREACHABLE;
    }

    if (!type->primitive && type->immutable) {
        const char* immutable = "Immutable ";
        size_t lenImmut = strlen(immutable), lenStr = strlen(str);
        size_t len = lenImmut + lenStr;
        char *str2;
        MALLOC_ARRAY(str2, char, len + 1);
        str2[len] = '\0';
        strcpy(str2, immutable);
        strcpy(&str2[lenImmut], str);
        str = str2;
    }

    return str;
}

static const char* tokenstring(Token token) {
    switch (token) {
    case TK_OR:     return "or";
    case TK_AND:    return "and";
    case TK_EQUAL:  return "==";
    case TK_NEQUAL: return "!=";
    case TK_LEQUAL: return "<=";
    case TK_GEQUAL: return ">=";
    case '<':       return "<";
    case '>':       return ">";
    case '+':       return "+";
    case '-':       return "-";
    case '*':       return "*";
    case '/':       return "/";
    default:
        UNREACHABLE;
    }
}

// TODO: these errs with variadic functions

// Creates a formatted error with one dynamic string
static const char* err1(const char* format, const char* name) {
    char* err;
    size_t len = strlen(format) - 2 + strlen(name); // -2 for '%s'
    MALLOC_ARRAY(err, char, len + 1);
    sprintf(err, format, name);
    err[len] = '\0';
    return err;
}

// Creates a formatted error with two dynamic strings
static const char* err2(const char* format, const char* s1, const char* s2) {
    char* err;
    size_t len = strlen(format) - 4 + strlen(s1) + strlen(s2); // -4 for '%s'
    MALLOC_ARRAY(err, char, len + 1);
    sprintf(err, format, s1, s2);
    err[len] = '\0';
    return err;
}

// Creates a formatted error with three dynamic strings
static const char* err3(const char* format, const char* s1, const char* s2,
    const char* s3) {

    char* err;
    // -6 for '%s'
    size_t len = strlen(format) - 6 + strlen(s1) + strlen(s2) + strlen(s3);
    MALLOC_ARRAY(err, char, len + 1);
    sprintf(err, format, s1, s2, s3);
    err[len] = '\0';
    return err;
}

static void err_redeclaration(Id* id) {
    sem_error(id->line, err1("redeclaration of name '%s'", id->name));
}

static void err_unknown_type(Id* id) {
    sem_error(id->line, err1("unknown type '%s'", id->name));
}

static void err_invalid_condition_type(Expression* expression) {
    const char* err = typestring(expression->type);
    err = err1("invalid type '%s' for condition (expecting Boolean)", err);
    sem_error(expression->line, err);
}

static void err_type(Line line, Type* type1, Type* type2) {
    const char* t1 = typestring(type1);
    const char* t2 = typestring(type2);
    const char* err = err2("type error (expected '%s', got '%s')", t1, t2);
    sem_error(line, err);
}

static void err_assignment_value(Statement* statement) {
    const char* err =
        err1("can't assign to '%s' since it was declared as a value",
            statement->assignment.capsa->id->name);
    sem_error(statement->line, err);
}

static void err_return_inside_spawn(Line line) {
    sem_error(line, "can't return inside a spawn block");
}

static void err_return_initializer(Line line) {
    sem_error(line, "can't return expression inside initializer");
}

static void err_return_void(Line line, Type* type) {
    const char* err = typestring(type);
    err = err1("return can't be empty, "
        "must return an expression of type '%s'", err);
    sem_error(line, err);
}

static void err_capsa_unknown(Id* id) {
    const char* err = err1("unknown variable '%s' being used", id->name);
    sem_error(id->line, err);
}

static void err_capsa_misuse(Id* id) {
    const char* err = err1("'%s' is not a variable", id->name);
    sem_error(id->line, err);
}

static void err_capsa_array_type(Capsa* capsa) {
    const char* err = typestring(capsa->indexed.array->type);
    err = err1("type '%s' can't be used as an array", err);
    sem_error(capsa->line, err);
}

static void err_capsa_array_index_type(Capsa* capsa) {
    const char* err = typestring(capsa->indexed.index->type);
    err = err1("type '%s' can't be used as an array index", err);
    sem_error(capsa->line, err);
}

static void err_function_call_unknown(Id* id) {
    const char* err = err1("unknown function '%s' being called", id->name);
    sem_error(id->line, err);
}

static void err_function_call_misuse(Id* id) {
    const char* err = err1("'%s' is not a function", id->name);
    sem_error(id->line, err);
}

static void err_function_call_array_constructor(Line line, unsigned int n) {
    char num[15];
    sprintf(num, "%d", n);
    const char* err = err1("an array constructor must have "
        "zero or one arguments, not %s", num);
    sem_error(line, err);
}

static void err_function_call_no_constructor(Line line, Id* id) {
    const char* err = err1(
        "monitor '%s' has no defined initializer", id->name
    );
    sem_error(line, err);
}

static void err_function_call_few_args(Line line) {
    sem_error(line, "function call has too few arguments");
}

static void err_function_call_excess_args(Line line) {
    sem_error(line, "function call has too many arguments");
}

static void err_function_call_no_monitor(Line line) {
    sem_error(line, "trying to call a method on a type that is not a monitor");
}

static void err_function_call_private(Line line, Id* id, Type* type) {
    const char* err = err2("method '%s' from monitor '%s' is private",
        id->name, typestring(type));
    sem_error(line, err);
}

static void err_function_call_no_method(Line line, Type* type, Id* id, bool a) {
    char *str =  a // acquire
        ? "monitor '%s' has no defined method 'acquire %s'"
        : "monitor '%s' has no defined method '%s'"
        ;
    sem_error(line, err2(str, typestring(type), id->name));
}

static void err_monitor_statements(Line line, const char* statement) {
    const char* err = err1("invalid use of '%s' statement "
        "outside a monitor's body", statement);
    sem_error(line, err);
}

static void err_monitor_statements_constructor(Line line, const char* stmt) {
    const char* err = err1("invalid use of '%s' statement "
        "inside a monitor's initializer", stmt);
    sem_error(line, err);
}

static void err_monitor_function_type(Line line) {
    sem_error(line, "a monitor's function can only return "
        "immutable or monitor types");
}

static void err_spawn_capsa(Line line) {
    sem_error(line, "can't access a variable inside a spawn block "
        "(only values)");
}

static void err_spawn_unsafe(Line line) {
    sem_error(line, "can't access a value of unsafe type "
        "inside a spawn block (only immutables and monitors)");
}

// ==================================================
//
//  TODO: Experimental
//
// ==================================================

static const char* errors[] = {
    "invalid type '%s' for unary minus (expecting numeric type)",
    "invalid type '%s' for unary not (expecting Boolean)",

    "invalid type '%s' for the left side of the '%s' expression",
    "invalid type '%s' for the right side of the '%s' expression",

    "invalid type for left side of the '%s' ('%s' is not an equatable type)",
    "invalid type for right side of the '%s' ('%s' is not an equatable type)",
    "incompatible types '%s' and '%s' for '%s' expression"
};

static void err_expression(ErrorType type, Expression* e) {
    Line line = e->line;
    const char* err = NULL;

    switch (type) {
    case ERR_EXP_MINUS:
    case ERR_EXP_NOT:
        err = err1(errors[type], typestring(e->unary.expression->type));
        break;
    case ERR_EXP_LEFT:
        err = err2(errors[type],
            typestring(e->binary.left_expression->type),
            tokenstring(e->binary.token)
        );
        break;
    case ERR_EXP_RIGHT:
        err = err2(errors[type],
            typestring(e->binary.right_expression->type),
            tokenstring(e->binary.token)
        );
        break;
    case ERR_EXP_LEFT_EQUAL:
        err = err2(errors[type],
            tokenstring(e->binary.token),
            typestring(e->binary.left_expression->type)
        );
        break;
    case ERR_EXP_RIGHT_EQUAL:
        err = err2(errors[type],
            tokenstring(e->binary.token),
            typestring(e->binary.right_expression->type)
        );
        break;
    case ERR_EXP_TYPECHECK_EQUAL:
        err = err3(errors[type], typestring(e->binary.left_expression->type),
            typestring(e->binary.right_expression->type),
            tokenstring(e->binary.token));
        break;
    default:
        UNREACHABLE;
    }

    sem_error(line, err);
}
