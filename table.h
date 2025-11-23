#pragma once
#include "common.h"

typedef enum {
  ValueKindBool = 0,
  ValueKindInt = 1,
  ValueKindFloat = 2,
  ValueKindArray,
  ValueKindStruct,
  ValueKindFunc,
} ValueKind;

typedef struct {
  IntVec fields_ids;
} TypeStruct;

typedef struct {

} TypeFunc;

typedef struct {
  ValueKind kind;
  char* name;
  union {
    int subtype_idx;
  };
} Type;
VEC_DEF(Type);

typedef struct Value Value;
VEC_DEF(Value);

typedef struct {
  int len;
  Value* data;
} ValueArray;

struct Value {
  int type_idx;
  union {
    int integer;
    double floating;
    bool boolean;
    ValueArray arr;
  };
};

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
} SymTbl;

// int symtbl_push_val(SymTbl* tbl, Value val) {
//   VEC_PUSH(tbl->values, val);
//   return tbl->values.len-1;
// }

// Value* symtbl_get_val(SymTbl* tbl, int idx) {
//   return &tbl->values.data[idx];
// }

Type* symtbl_get_val_type(SymTbl* tbl, Value* val) {
  return &tbl->types.data[val->type_idx];
}

// Type* symtbl_get_val_id_type(SymTbl* tbl, int val_idx) {
//   return symtbl_get_val_type(tbl, symtbl_get_val(tbl, val_idx)); 
// }

int symtbl_insert_type(SymTbl* tbl, Type t) {
  int present = -1;
  VEC_FOR(tbl->types) {
    Type* type = &tbl->types.data[i];
    if (strcmp(type->name, t.name) == 0) {
      present = i;
      break;
    }
  }

  if (present == -1) {
    VEC_PUSH(tbl->types, t);
    return tbl->types.len-1;
  } else {
    free(t.name);
    return present;
  }
}

bool symtbl_type_eq(SymTbl* tbl, Value* a, Value* b) {
  // TODO: for now this will do...
  Type* at = symtbl_get_val_type(tbl, a);
  Type* bt = symtbl_get_val_type(tbl, b);
  return strcmp(at->name, bt->name) == 0;
}

Value make_int_value(double val) {
  return (Value) { ValueKindInt, .integer = val };
}

Value make_float_value(double val) {
  return (Value) { ValueKindFloat, .floating = val };
}

Value make_bool_value(bool val) {
  return (Value) { ValueKindBool, .boolean = val };
}

Value make_arr_value(SymTbl* tbl, int subtype, void* data, int len) {
  char* subtype_name = tbl->types.data[subtype].name;

  String name = {0};
  string_append(&name, "arr ");
  string_append(&name, subtype_name);

  Type t = { ValueKindArray, name.data, .subtype_idx = subtype };
  int type_idx = symtbl_insert_type(tbl, t);
  return (Value) { type_idx, .arr = { len, data }};
}

void symtbl_insert(SymTbl* tbl, char* str, size_t len, Value val) {
  Scope* scope = &tbl->scopes.data[tbl->scopes.len-1];

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

// TODO: consider making this return index
Value* symtbl_find(SymTbl* tbl, char* str, size_t len) {
  Symbol* present = NULL;

  for(int i=tbl->scopes.len-1; i >= 0; i--) {
    Scope* scope = &tbl->scopes.data[i];

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

void symtbl_push_scope(SymTbl* tbl) {
  VEC_PUSH(tbl->scopes, (Scope) {0});
}

void symtbl_pop_scope(SymTbl* tbl) {
  Scope* scope = &tbl->scopes.data[tbl->scopes.len-1];
  free(scope->data);
  // scope->len = 0;
  (void) VEC_POP(tbl->scopes);
}

const Type TYPE_BOOL  = {ValueKindBool,  "bool", 0};
const Type TYPE_INT   = {ValueKindInt, "int", 0};
const Type TYPE_FLOAT = {ValueKindFloat, "float", 0};

SymTbl symtbl_init() {
  SymTbl t = {0};
  // global scope
  symtbl_push_scope(&t);

  VEC_PUSH(t.types, TYPE_BOOL);
  VEC_PUSH(t.types, TYPE_INT);
  VEC_PUSH(t.types, TYPE_FLOAT);

  return t;
}

// TODO: free symtbl