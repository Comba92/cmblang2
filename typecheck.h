#pragma once

typedef struct {
  char* name;
  int type_idx;
} Sym;
VEC_DEF(Sym);
VEC_DEF_NAMED(ScopeVec, SymVec);

typedef struct {
  Parser* parser;
  ScopeVec scopes;
  const char* err;
} Symtbl;

void typecheck_err(Symtbl* tbl, const char* err) {
  tbl->err = err;
  fprintf(stderr, "[TYPE ERR] %s\n", err); 
}

SymVec* symtbl_top(Symtbl* tbl) {
  return &tbl->scopes.data[tbl->scopes.len-1];
}

void symtbl_insert(Symtbl* tbl, int type_idx, Token* tok) {
  char* start = tbl->parser->src + tok->offset;
  int len = tok->len;

  SymVec* scope = symtbl_top(tbl);

  int present_idx = -1;
  VEC_FOR(*scope) {
    Sym it = scope->data[i];
    if (strncmp(it.name, start, len) == 0) {
      present_idx = i;
      break;
    }
  }

  if (present_idx == -1) {
    // we own the identifier name string
    char* name = str_clone(start, len);
    Sym e = { name, .type_idx = type_idx };
    VEC_PUSH(*scope, e);
  } else {
    scope->data[present_idx].type_idx = type_idx;
  }
}

int symtbl_find(Symtbl* tbl, Token* tok) {
  char* start = tbl->parser->src + tok->offset;
  int len = tok->len;

  Sym* present = NULL;
  for(int i=tbl->scopes.len-1; i >= 0; i--) {
    SymVec scope = tbl->scopes.data[i];

    VEC_FOREACH(Sym, scope) {
      if (strncmp(it->name, start, len) == 0) {
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
  SymVec* scope = symtbl_top(tbl);
  // free(scope->data);
  scope->len = 0;
  (void) VEC_POP(tbl->scopes);
}

Symtbl symtbl_init(Parser* p) {
  Symtbl tbl = {0};
  tbl.parser = p;
  // global scope
  symtbl_push_scope(&tbl);
  return tbl;
}

bool typecheck_eq(Symtbl* tbl, int ta, int tb) {
  TypeAnn* type_a = parser_get_type(tbl->parser, ta);
  TypeAnn* type_b = parser_get_type(tbl->parser, tb);

  if (type_a->kind != type_b->kind) return false;

  switch(type_a->kind) {
    case TypeAnnKindInt:
    case TypeAnnKindFloat:
    case TypeAnnKindBool:
      return true;

    case TypeAnnKindFunc: {
      TypeAnnFunc func_a = type_a->func;
      TypeAnnFunc func_b = type_b->func;

      if (func_a.params.len != func_b.params.len) return false;
      for(int i=0; i<func_a.params.len; i++) {
        if (!typecheck_eq(tbl, func_a.params.data[i].type_idx, func_b.params.data[i].type_idx))
          return false;
      }

      return func_a.ret_idx == func_b.ret_idx;
    } break;

    case TypeAnnKindArray: {
      return typecheck_eq(tbl, type_a->inner_idx, type_b->inner_idx);
    } break;
  }

  return false;
}

int typecheck_expr(Symtbl* tbl, int expr_id) {
  Expr* e = parser_get_expr(tbl->parser, expr_id);

  switch (e->kind) {
    case ExprKindLiteral: {
      ExprLiteral lit = e->lit;

      switch(lit.kind) {
        case LiteralKindInt:    return TypeAnnKindInt;
        case LiteralKindFloat:  return TypeAnnKindFloat;
        case LiteralKindTrue:
        case LiteralKindFalse:
          return TypeAnnKindBool;

        case LiteralKindArray: {
          IntVec ids = lit.arr.expr_ids;

          int expr_id = ids.data[0];
          int prev_type = typecheck_expr(tbl, expr_id);

          bool same_type = true;
          for(int i=1; i<ids.len; i++) {
            expr_id = ids.data[i];
            int curr_type = typecheck_expr(tbl, expr_id);
            if (!typecheck_eq(tbl, prev_type, curr_type)) {
              same_type = false;
              break;
            }
            prev_type = curr_type;
          }

          if (!same_type) {
            typecheck_err(tbl, "array values must be of the same type");
            return -1;
          }

          return prev_type;
        } break;
      }
    } break;

    case ExprKindVariable: {
      int type = symtbl_find(tbl, e->ident);

      if (type == -1) {
        typecheck_err(tbl, "undefined variable");
        return -1;
      } else {
        return type;
      }
    } break;

    case ExprKindUnary: {
      ExprUnary un = e->un;
      int rhs_type = typecheck_expr(tbl, un.expr);
      TypeAnn* type = parser_get_type(tbl->parser, rhs_type);

      switch (type->kind) {
        case TypeAnnKindInt:
        case TypeAnnKindFloat:
          if (un.op->kind != TokNot) {
            typecheck_err(tbl, "invalid numeric unary operation");
            return -1;
          }
          else return rhs_type;

        case TypeAnnKindBool:
          if (un.op->kind != TokNot) {
            typecheck_err(tbl, "invalid boolean unary operation");
            return -1;
          }
          else return rhs_type;
      }
    } break;

    case ExprKindBinary: {
      ExprBinary bin = e->bin;
      int lhs_type = typecheck_expr(tbl, bin.lhs_idx);
      int rhs_type = typecheck_expr(tbl, bin.rhs_idx);
      TypeAnn* lt = parser_get_type(tbl->parser, lhs_type);
      TypeAnn* rt = parser_get_type(tbl->parser, rhs_type);

      if (lt->kind == TypeAnnKindArray && rt->kind == TypeAnnKindInt) {
        // array access
        return 1;
      }

      if (!typecheck_eq(tbl, lhs_type, rhs_type)) {
        typecheck_err(tbl, "invalid numeric unary operation");
        return -1;
      } else {
        return lhs_type;
      }
    } break;

    case ExprKindCall: {
      ExprCall call = e->call;
      int callee_type = typecheck_expr(tbl, call.callee_idx);
      TypeAnn* type = parser_get_type(tbl->parser, callee_type);

      if (type->kind != TypeAnnKindFunc) {
        typecheck_err(tbl, "expect function for call operation");
        return -1;
      } else {
        return type->func.ret_idx;
      }
    } break;
  }

  return -1;
}

void typecheck_block(Symtbl* tbl, IntVec stmts) {
  VEC_FOREACH(int, stmts) {
    Stmt* s = parser_get_stmt(tbl->parser, *it);
    
    switch(s->kind) {
      case StmtKindDecl: {
        StmtDecl decl = s->decl;
        int type_idx = typecheck_expr(tbl, decl.rhs_idx);
        if (!typecheck_eq(tbl, type_idx, decl.type_idx)) {
          typecheck_err(tbl, "assignment of different type");
          return;
        }
        symtbl_insert(tbl, type_idx, decl.name);
      } break;

      case StmtKindFnDecl: {
        StmtFnDecl decl = s->fn_decl;
        symtbl_insert(tbl, decl.type_idx, decl.name);
        TypeAnn* type = parser_get_type(tbl->parser, decl.type_idx);
        TypeAnnFunc func = type->func;

        symtbl_push_scope(tbl);
        // push args to scope
        VEC_FOREACH(FnParam, func.params) symtbl_insert(tbl, it->type_idx, it->name);
        Stmt* block = parser_get_stmt(tbl->parser, func.block_idx);
        typecheck_block(tbl, block->block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindAssign: {
        StmtAssign assign = s->assign;
        int var_type = symtbl_find(tbl, assign.var);
        if (var_type == -1) {
          typecheck_err(tbl, "undeclared variable");
        } else {
          int expr_type = typecheck_expr(tbl, assign.rhs_idx);
          if (!typecheck_eq(tbl, var_type, expr_type)) {
            typecheck_err(tbl, "assignment of different type");
            return;
          }
        }
      } break;

      case StmtKindBlock: {
        symtbl_push_scope(tbl);
        typecheck_block(tbl, s->block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindIfElse: {
        StmtIfElse if_else = s->if_else;
        int cond_idx = typecheck_expr(tbl, if_else.cond_idx);
        if (parser_get_type(tbl->parser, cond_idx)->kind != TypeAnnKindBool) {
          typecheck_err(tbl, "if condition isn't bool");
          return;
        }

        symtbl_push_scope(tbl);
        Stmt* block = parser_get_stmt(tbl->parser, if_else.if_idx);
        typecheck_block(tbl, block->block.stmt_ids);
        symtbl_pop_scope(tbl);

        symtbl_push_scope(tbl);
        block = parser_get_stmt(tbl->parser, if_else.else_idx);
        typecheck_block(tbl, block->block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindWhile: {
        StmtWhile wloop = s->wloop;
        int cond_type = typecheck_expr(tbl, wloop.cond_idx);
        if (parser_get_type(tbl->parser, cond_type)->kind != TypeAnnKindBool) {
          typecheck_err(tbl, "while condition isn't bool");
          return;
        }

        symtbl_push_scope(tbl);
        Stmt* block = parser_get_stmt(tbl->parser, wloop.block_idx);
        typecheck_block(tbl, block->block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindExpr: {
        int expr_type = typecheck_expr(tbl, s->expr_idx);
      } break;

      case StmtKindReturn: {
        int expr_type = typecheck_expr(tbl, s->expr_idx);
      } break;
    }
  }
}

// TODO: should return bool?
bool typecheck(Symtbl* tbl) {
  typecheck_block(tbl, tbl->parser->top_lvl_stmts);
  return tbl->err == NULL;
}