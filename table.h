#pragma once
#include "common.h"

typedef enum {
  ValueTypeBool = 0,
  ValueTypeFloat = 1,
  ValueTypeArray,
  ValueTypeStruct,
  ValueTypeFunc,
} ValueType;

typedef struct {
  IntVec fields_ids;
} TypeStruct;

typedef struct {

} TypeFunc;

typedef struct {
  ValueType type;
  char* name;
  union {
    int subtype_idx;
  };
} Type;
VEC_DEF(Type);

typedef struct {
  int len;
  void* data;
} ValueArray;

typedef struct {
  int type_idx;
  union {
    double num;
    bool boolean;
    ValueArray arr;
  };
} Value;

Value val_err() {
  return (Value) { .type_idx = -1 };
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

Type* symtable_get_val_type(SymbolTable* table, Value* val) {
  return &table->types.data[val->type_idx];
}

int symtable_insert_type(SymbolTable* table, Type t) {
  int present = -1;
  VEC_FOR(table->types) {
    Type* type = &table->types.data[i];
    if (strcmp(type->name, t.name) == 0) {
      present = i;
      break;
    }
  }

  if (present == -1) {
    VEC_PUSH(table->types, t);
    return table->types.len-1;
  } else {
    free(t.name);
    return present;
  }
}

bool symtable_type_eq(SymbolTable* table, Value* a, Value* b) {
  // TODO: for now this will do...
  Type* at = symtable_get_val_type(table, a);
  Type* bt = symtable_get_val_type(table, b);
  return strcmp(at->name, bt->name) == 0;
}

Value make_float_value(double val) {
  return (Value) { ValueTypeFloat, .num = val };
}

Value make_bool_value(bool val) {
  return (Value) { ValueTypeBool, .boolean = val };
}

Value make_arr_value(SymbolTable* table, int subtype, void* data, int len) {
  char* subtype_name = table->types.data[subtype].name;

  String name = {0};
  string_append(&name, "arr ");
  string_append(&name, subtype_name);

  Type t = { ValueTypeArray, name.data, .subtype_idx = subtype };
  int type_idx = symtable_insert_type(table, t);
  return (Value) { type_idx, .arr = { len, data }};
}

void symtable_insert(SymbolTable* table, char* str, size_t len, Value val) {
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
  } else {
    scope->data[present].val = val;
  }
}

Value* symtable_find(SymbolTable* table, char* str, size_t len) {
  Symbol* present = NULL;

  for(int i=table->scopes.len-1; i >= 0; i--) {
    Scope* scope = &table->scopes.data[i];

    VEC_FOREACH(Symbol, *scope) {
      if (strncmp(it->name, str, len) == 0) {
        present = it;
        break;
      }
    }

    if (present != NULL) break;
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
  // scope->len = 0;
  (void) VEC_POP(table->scopes);
}

const Type TYPE_BOOL  = {ValueTypeBool,  "bool", 0};
const Type TYPE_FLOAT = {ValueTypeFloat, "float", 0};

SymbolTable symtable_init() {
  SymbolTable t = {0};
  // global scope
  symtable_push_scope(&t);

  VEC_PUSH(t.types, TYPE_BOOL);
  VEC_PUSH(t.types, TYPE_FLOAT);

  return t;
}

// TODO: free symtable