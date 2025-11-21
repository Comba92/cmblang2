#pragma once
#include "common.h"

typedef enum {
  ValueTypeBool = 0,
  ValueTypeFloat = 1,
} ValueType;

typedef struct {
  ValueType kind;
} Type;
VEC_DEF(Type);

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double num;
  };
} Value;

Value val_err() {
  return (Value) { .type = -1 };
}

typedef struct {
  char* name;
  Value val;
} Symbol;

VEC_DEF_NAMED(Scope, Symbol);
VEC_DEF(Scope);

typedef struct {
  ScopeVec scopes;
  TypeVec types;
} SymbolTable;

// TODO: should be pointer?
// Type symtable_get_type(SymbolTable* table, int idx) {
//   return table->types.data[idx];
// }

void symtable_insert(SymbolTable* table, char* str, size_t len, Value val) {
  // while (table->scopes.len <= depth) VEC_PUSH(table->scopes, (Scope) {0});

  Scope* scope = &table->scopes.data[table->scopes.len-1];

  int present = -1;
  VEC_FOR(*scope) {
    Symbol* sym = &scope->data[i];
    if (strncmp(sym->name, str, len) == 0) {
      present = i;
      break;
    }
  }

  if (present == -1) {
    // we own the identifier name string
    char* name = str_clone(str, len);
    Symbol e = { name, val };
    VEC_PUSH(*scope, e);
    printf("Value pushed to scope: %lf\n", val.num);
  } else {
    scope->data[present].val = val;
    printf("Value updated to scope: %lf\n",val.num);
  }
}

Value* symtable_find(SymbolTable* table, char* str, size_t len) {
  // while (table->scopes.len <= depth) VEC_PUSH(table->scopes, (Scope) {0});

  Symbol* present = NULL;

  for(int i=table->scopes.len-1; i >= 0; i--) {
    Scope* scope = &table->scopes.data[i];

    VEC_FOREACH(Symbol, *scope) {
      if (strncmp(it->name, str, len) == 0) {
        present = it;
        break;
      }
    }
  }

  if (present == NULL) {
    return NULL;
  } else {
    return &present->val;
  }
}

void symtable_push_scope(SymbolTable* table) {
  VEC_PUSH(table->scopes, (Scope) {0});
}

void symtable_pop_scope(SymbolTable* table) {
  Scope* scope = &table->scopes.data[table->scopes.len-1];
  free(scope->data);
  (void) VEC_POP(table->scopes);
}

SymbolTable symtable_init() {
  SymbolTable t = {0};
  symtable_push_scope(&t);

  VEC_PUSH(t.types, (Type) {ValueTypeBool});
  VEC_PUSH(t.types, (Type) {ValueTypeFloat});

  return t;
}