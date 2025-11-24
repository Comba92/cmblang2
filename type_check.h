typedef struct {
  char* name;
  int type_idx;
} Sym;
VEC_DEF(Sym);
VEC_DEF_NAMED(ScopeVec, SymVec);

typedef struct {
  ScopeVec scopes;
  Parser* p;
} Symtbl;

SymVec* symtbl_top(Symtbl* tbl) {
  return &tbl->scopes.data[tbl->scopes.len-1];
}

void symtbl_insert(Symtbl* tbl, char* str, size_t len, int type_idx) {
  SymVec* scope = symtbl_top(tbl);

  Sym* present = NULL;
  VEC_FOREACH(Sym, *scope) {
    if (strncmp(it->name, str, len) == 0) {
      present = it;
      break;
    }
  }

  if (present == NULL) {
    // we own the identifier name string
    char* name = str_clone(str, len);
    Sym e = { name, type_idx };
    VEC_PUSH(*scope, e);
  } else {
    present->type_idx = type_idx;
  }
}

int symtbl_find(Symtbl* tbl, char* str, size_t len) {
  Sym* present = NULL;

  for(int i=tbl->scopes.len-1; i >= 0; i--) {
    SymVec* scope = &tbl->scopes.data[i];

    VEC_FOREACH(Sym, *scope) {
      if (strncmp(it->name, str, len) == 0) {
        present = it;
        break;
      }
    }

    if (present != NULL) break;
  }

  if (present == NULL) {
    return -1;
  } else {
    return present->type_idx;
  }
}

void symtbl_push_scope(Symtbl* tbl) {
  VEC_PUSH(tbl->scopes, (SymVec) {0});
}

void symtbl_pop_scope(Symtbl* tbl) {
  SymVec* scope = &tbl->scopes.data[tbl->scopes.len-1];
  // free(scope->data);
  scope->len = 0;
  (void) VEC_POP(tbl->scopes);
}