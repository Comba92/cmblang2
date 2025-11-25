#pragma once

typedef enum {
  ValueKindBool = 0,
  ValueKindInt = 1,
  ValueKindFloat = 2,
  ValueKindArray,
  ValueKindFunc,
  ValueKindStruct,
} ValueKind;

typedef struct {
  int size;
  int subtype_id;
} TypeArray;

typedef struct {
  IntVec fields_ids;
} TypeStruct;

typedef struct {
  IntVec params_types;
  int return_type_idx;
} TypeFunc;

typedef struct {
  ValueKind kind;
  char* name;
  int size;
  union {
    int subtype_idx;
    TypeFunc func;
    TypeStruct strct;
  };
} Type;
VEC_DEF(Type);

typedef struct Value Value;
VEC_DEF(Value);

typedef struct {
  // should len be on the type or on the value?
  int len;
  Value* data;
} ValueArray;

typedef struct {
  Value* args;
  int block_id;
} ValueFunc;

struct Value {
  int type_idx;
  union {
    int integer;
    double floating;
    bool boolean;
    ValueArray arr;
    ValueFunc func;
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

int symtbl_parse_type(SymTbl* tbl, char* str) {

}

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

Value make_arr_value(SymTbl* tbl, int subtype_idx, Value* data, int len) {
  Type* subtype = &tbl->types.data[subtype_idx];
  String name = {0};
  string_append(&name, "[");
  string_append(&name, subtype->name);
  string_append(&name, "]");

  Type t = { ValueKindArray, name.data, .size = len * subtype->size, .subtype_idx = subtype_idx };
  int type_idx = symtbl_insert_type(tbl, t);
  return (Value) { type_idx, .arr = { len, data }};
}

// Value make_func_const(SymTbl* tbl, StmtFnDecl* decl, char* src) {
//   IntVec params_types = {0};

//   // build the params
//   String name = {0};
//   string_append(&name, "fn(");
//   VEC_FOREACH(FuncParam, decl.params) {
//     Token* type = it.type;
//     char* start = src + type->offset;
//     int len = type->len;

//     string_append_n(&name, start, len);
//     VEC_PUSH(params_types, )
//   }
//   string_append(&name, ")");
  
// }

void symtbl_insert(SymTbl* tbl, char* str, int len, Value val) {
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
Value* symtbl_find(SymTbl* tbl, char* str, int len) {
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

const Type TYPE_BOOL  = {ValueKindBool,  "bool", sizeof(bool), 0};
const Type TYPE_INT   = {ValueKindInt, "int", sizeof(int), 0};
const Type TYPE_FLOAT = {ValueKindFloat, "float", sizeof(double), 0};

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