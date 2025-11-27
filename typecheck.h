#pragma once

typedef struct {
  char* name;
  int type_id;
} Sym;
VEC_DEF(Sym);
VEC_DEF_NAMED(ScopeVec, SymVec);

typedef struct {
  Parser* parser;
  ScopeVec scopes;
  bool had_errors;
} Symtbl;

void typecheck_err(Symtbl* tbl, const char* err) {
  fprintf(stderr, "[TYPE ERR] %s\n", err);
  tbl->had_errors = true;
}

SymVec* symtbl_top(Symtbl* tbl) {
  return &tbl->scopes.data[tbl->scopes.len-1];
}

void symtbl_insert(Symtbl* tbl, int type_id, Token* tok) {
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
    Sym e = { name, .type_id = type_id };
    VEC_PUSH(*scope, e);
  } else {
    scope->data[present_idx].type_id = type_id;
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
    return present->type_id;
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

void symtbl_dbg(Symtbl* tbl) {
  VEC_FOREACH(Sym, *symtbl_top(tbl)) {
    printf("Name: %s\t", it->name);
    type_dbg(parser_get_type(tbl->parser, it->type_id));
  }
}

bool typecheck_eq(Symtbl* tbl, int ta, int tb) {
  TypeAnn type_a = parser_get_type(tbl->parser, ta);
  TypeAnn type_b = parser_get_type(tbl->parser, tb);
  return type_eq(tbl->parser, type_a, type_b);
}

// TODO: should return TypeAnn?
TypeId typecheck_expr(Symtbl* tbl, int expr_id) {
  Expr e = parser_get_expr(tbl->parser, expr_id);

  switch (e.kind) {
    case ExprKindLiteral: {
      ExprLiteral lit = e.lit;

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
            typecheck_err(tbl, "array literal values must be of the same type");
            return -1;
          }

          return prev_type;
        } break;
      }
    } break;

    case ExprKindVariable: {
      TypeId type = symtbl_find(tbl, e.ident);

      if (type == -1) {
        typecheck_err(tbl, fmt("undefined variable '"str_fmt"'", e.ident->len, tbl->parser->src + e.ident->offset));
        return -1;
      } else {
        return type;
      }
    } break;

    case ExprKindUnary: {
      ExprUnary un = e.un;
      int rhs_type = typecheck_expr(tbl, un.expr);
      TypeAnn type = parser_get_type(tbl->parser, rhs_type);

      switch (type.kind) {
        case TypeAnnKindInt:
        case TypeAnnKindFloat:
          if (un.op->kind != TokSub) {
            typecheck_err(tbl, fmt("invalid numeric unary operation %c", un.op->kind));
            return -1;
          }
          else return rhs_type;

        case TypeAnnKindBool:
          if (un.op->kind != TokNot) {
            typecheck_err(tbl, fmt("invalid boolean unary operation %c", un.op->kind));
            return -1;
          }
          else return rhs_type;

        default:
          typecheck_err(tbl, fmt("unary operator %c on invalid types %d %d", un.op->kind, type.kind, TypeAnnKindBool));
          return -1;
      }
    } break;

    case ExprKindBinary: {
      ExprBinary bin = e.bin;
      int lhs_type = typecheck_expr(tbl, bin.lhs_id);
      int rhs_type = typecheck_expr(tbl, bin.rhs_id);
      TypeAnn lt = parser_get_type(tbl->parser, lhs_type);
      TypeAnn rt = parser_get_type(tbl->parser, rhs_type);

      if (lt.kind == TypeAnnKindArray && rt.kind == TypeAnnKindInt) {
        // array access
        return 1;
      }

      if (!typecheck_eq(tbl, lhs_type, rhs_type)) {
        typecheck_err(tbl, "invalid binary operation on different types");
        return -1;
      } else {
        return lhs_type;
      }
    } break;

    case ExprKindCall: {
      ExprCall call = e.call;
      int callee_type = typecheck_expr(tbl, call.callee_id);
      TypeAnn type = parser_get_type(tbl->parser, callee_type);

      if (type.kind != TypeAnnKindFunc) {
        typecheck_err(tbl, fmt("expect function for call operation, got %d", type.kind));
        return -1;
      } else {
        return type.func.ret_id;
      }
    } break;
  }

  return -1;
}

bool typecheck_returns(Symtbl* tbl, IntVec stmts, TypeId target_type);
bool block_has_valid_return(Symtbl* tbl, IntVec stmts, TypeId target_type) {
  // get last stmt and check it is a return
  StmtId last_id = VEC_LAST(stmts);
  Stmt last = parser_get_stmt(tbl->parser, last_id);
  if (last.kind != StmtKindReturn) return false;

  TypeId expr_type = typecheck_expr(tbl, last.ret.expr_id);
  typecheck_eq(tbl, expr_type, target_type);

  return typecheck_returns(tbl, stmts, target_type);
}

bool typecheck_returns(Symtbl* tbl, IntVec stmts, TypeId target_type) {
  if (stmts.len == 0) return false;

  VEC_FOR(stmts) {
    // get current stmt
    int id = stmts.data[i];
    Stmt s = parser_get_stmt(tbl->parser, id);

    // check if it is a block
    switch (s.kind) {
      case StmtKindIfElse: {
        StmtIfElse if_else = s.if_else;
        Stmt if_block = parser_get_stmt(tbl->parser, if_else.if_id);
        Stmt else_block = parser_get_stmt(tbl->parser, if_else.else_id);

        return block_has_valid_return(tbl, if_block.block.stmt_ids, target_type)
          || block_has_valid_return(tbl, else_block.block.stmt_ids, target_type);
      } break;

      case StmtKindWhile: {
        StmtId block_id = s.wloop.block_id;
        Stmt block = parser_get_stmt(tbl->parser, block_id);

        return block_has_valid_return(tbl, block.block.stmt_ids, target_type);
      } break;
      case StmtKindBlock: {
        return block_has_valid_return(tbl, s.block.stmt_ids, target_type);
      } break;

      default: return false;
    }
  }

  return true;
}

void typecheck_block(Symtbl* tbl, IntVec stmts) {
  VEC_FOREACH(int, stmts) {
    Stmt s = parser_get_stmt(tbl->parser, *it);
    
    switch(s.kind) {
      case StmtKindDecl: {
        StmtDecl decl = s.decl;
        TypeId expr_type = typecheck_expr(tbl, decl.rhs_id);

        // if it is unknown, we infer it from the right expr
        if (s.decl.type_id != TypeAnnKindUnknown && !typecheck_eq(tbl, expr_type, decl.type_id)) {
          typecheck_err(tbl, "assignment of different type");
        }
        symtbl_insert(tbl, expr_type, decl.name);
      } break;

      case StmtKindFnDecl: {
        StmtFnDecl decl = s.func_decl;

        StmtBlock block = parser_get_stmt(tbl->parser, decl.block_id).block;
        TypeAnn type = parser_get_type(tbl->parser, decl.signature_id);
        TypeAnnFunc signature = type.func;

        if (!typecheck_returns(tbl, block.stmt_ids, signature.ret_id)) {
          typecheck_err(tbl, "not all function blocks ends with a return statment, or not all return statements are of the same type");
        }

        symtbl_insert(tbl, decl.signature_id, decl.name);

        symtbl_push_scope(tbl);
        // push args to scope
        VEC_FOR(decl.params_names) {
          Token* name = decl.params_names.data[i];
          TypeId type_id = signature.params_ids.data[i];
          symtbl_insert(tbl, type_id, name);
        }

        typecheck_block(tbl, block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindAssign: {
        StmtAssign assign = s.assign;
        int var_type = symtbl_find(tbl, assign.var);
        if (var_type == -1) {
          typecheck_err(tbl, fmt("undeclared variable '"str_fmt"'",  assign.var->len, tbl->parser->src + assign.var->offset));
        } else {
          int expr_type = typecheck_expr(tbl, assign.rhs_id);
          if (!typecheck_eq(tbl, var_type, expr_type)) {
            typecheck_err(tbl, "assignment of different type");
          }
        }
      } break;

      case StmtKindBlock: {
        symtbl_push_scope(tbl);
        typecheck_block(tbl, s.block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindIfElse: {
        StmtIfElse if_else = s.if_else;
        int cond_id = typecheck_expr(tbl, if_else.cond_id);
        if (parser_get_type(tbl->parser, cond_id).kind != TypeAnnKindBool) {
          typecheck_err(tbl, "if condition isn't bool");
        }

        symtbl_push_scope(tbl);
        Stmt block = parser_get_stmt(tbl->parser, if_else.if_id);
        typecheck_block(tbl, block.block.stmt_ids);
        symtbl_pop_scope(tbl);

        symtbl_push_scope(tbl);
        block = parser_get_stmt(tbl->parser, if_else.else_id);
        typecheck_block(tbl, block.block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindWhile: {
        StmtWhile wloop = s.wloop;
        int cond_type = typecheck_expr(tbl, wloop.cond_id);
        if (parser_get_type(tbl->parser, cond_type).kind != TypeAnnKindBool) {
          typecheck_err(tbl, "while condition isn't bool");
        }

        symtbl_push_scope(tbl);
        Stmt block = parser_get_stmt(tbl->parser, wloop.block_id);
        typecheck_block(tbl, block.block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindExpr: {
        // int expr_type = typecheck_expr(tbl, s.expr_id);
      } break;

      case StmtKindReturn: {
        // returns are checked in functions declarations
        // int expr_type = typecheck_expr(tbl, s.expr_id);
      } break;
    }
  }
}

bool typecheck(Symtbl* tbl) {
  typecheck_block(tbl, tbl->parser->top_lvl_stmts);
  return !tbl->had_errors;
}