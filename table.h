#pragma once

#include <string.h>
#include "common.h"

typedef enum {
  ValueTypeErr,
  ValueTypeFloat,
} ValueType;

typedef struct {
  ValueType type;
  double num;
} Value;

typedef struct {
  char* name;
  Value val;
} Symbol;

VEC_DEF_NAMED(Scope, Symbol);
VEC_DEF(Scope);

typedef struct {
  ScopeVec scopes;
} SymbolTable;

void symtable_insert(SymbolTable* table, int depth, char* str, size_t len, Value val) {
  while (table->scopes.len <= depth) VEC_PUSH(table->scopes, (Scope) {0});

  Scope* scope = &table->scopes.data[depth];

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
    printf("Value pushed to scope %d: %lf\n", depth, val.num);
  } else {
    scope->data[present].val = val;
    printf("Value updated to scope %d: %lf\n", depth, val.num);
  }
}

Value* symtable_find(SymbolTable* table, int depth, char* str, size_t len) {
  while (table->scopes.len <= depth) VEC_PUSH(table->scopes, (Scope) {0});

  Symbol* present = NULL;

  for(int i=depth; i >= 0; i--) {
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
  (void) VEC_POP(*scope);
}

void table_dbg(SymbolTable* table) {
  
}