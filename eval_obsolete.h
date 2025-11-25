#pragma once

// TODO: returning Value might be costly

typedef struct {
  Parser* p;
  Symtbl* tbl;

  const char* err;
} Context;

// int ctx_push_val(Context* c, Value val) {
//   VEC_PUSH(c->vals, val);
//   return c->vals.len-1;
// }

// Value* ctx_get_val(Context* c, int idx) {
//   return &c->vals.data[idx];
// }

void eval_log_err(Context* c, const char* err) {
  c->err = err;
  fprintf(stderr, "[EVAL ERR] %s\n", err); 
}

Value eval_expr(Context* ctx, int expr_id) {
  SymTbl* tbl = ctx->tbl;
  Parser* p = ctx->p;

  Expr* e = parser_get_expr(p, expr_id);

  switch (e->kind) {
    case ExprKindLiteral: {
      ExprLiteral lit = e->lit;

      Value v;
      switch (lit.kind) {
        case LiteralKindInt: {
          char* start = p->src + lit.tok->offset;
          int len = lit.tok->len;
          // TODO: consider using a buffer instead of allocating
          char* str = str_clone(start, len);
          int val = atoi(str);
          free(str);
          
          v = make_int_value(val);
        } break;

        case LiteralKindFloat: {
          char* start = p->src + lit.tok->offset;
          int len = lit.tok->len;
          // TODO: consider using a buffer instead of allocating
          char* str = str_clone(start, len);
          double val = atof(str);
          free(str);
          
          v = make_float_value(val);
        } break;

        case LiteralKindTrue: v = make_bool_value(true); break;
        case LiteralKindFalse: v = make_bool_value(false); break;
        case LiteralKindArray: {
          IntVec ids = lit.arr.expr_ids;
          
          // shouldn't be possible
          // if (ids.len == 0) {
          //   v = make_arr_value(ctx, ValueKindBool, NULL, 0);
          //   break;
          // }

          Value* buf = malloc(sizeof(Value) * ids.len);

          int expr_id = ids.data[0];
          Value prev = eval_expr(ctx, expr_id);
          buf[0] = prev;

          bool same_type = true;
          for(int i=1; i<ids.len; i++) {
            expr_id = ids.data[i];
            Value curr = eval_expr(ctx, expr_id);
            if (!symtbl_type_eq(tbl, &prev, &curr)) {
              same_type = false;
              break;
            }
            prev = curr;
            buf[i] = curr;
          }

          if (!same_type) {
            free(buf);
            eval_log_err(ctx, "array values must be of the same type");
            return val_err();
          }

          v = make_arr_value(tbl, prev.type_idx, buf, ids.len);
        } break;
      }

      // return ctx_push_val(ctx, v);
      return v;
    } break;

    case ExprKindVariable: {
      char* start = p->src + e->ident->offset;
      int len = e->ident->len;

      Value* res = symtbl_find(tbl, start, len);
      if (res == NULL) {
        eval_log_err(ctx, "undefined variable");
        return val_err();
      } else {
        return *res;
      }
    } break;

    case ExprKindUnary: {
      ExprUnary un = e->un;
      Value rhs = eval_expr(ctx, un.expr);
      Type* type = symtbl_get_val_type(tbl, &rhs);

      Value v = {0};
      v.type_idx = rhs.type_idx;
      switch (type->kind) {
        case ValueKindInt: {
          switch(un.op) {
            case TokSub: v.integer = -rhs.integer; break;
            default:
              eval_log_err(ctx, "invalid integer unary operator");
              return val_err();
          }
        } break;

        case ValueKindFloat: {
          switch(un.op) {
            case TokSub: v.floating = -rhs.floating; break;
            default:
              eval_log_err(ctx, "invalid floating unary operator");
              return val_err();
          }
        } break;

        case ValueKindBool: {
          switch(un.op) {
            case TokNot: v.boolean = !rhs.boolean; break;
            default:
              eval_log_err(ctx, "invalid bool unary operator");
              return val_err();
          } 
        } break;

        default:
          eval_log_err(ctx, "type doesn't support unary operation");
          return val_err();
      }

      return v;
    }

    case ExprKindBinary: {
      ExprBinary bin = e->bin;
      Value lhs = eval_expr(ctx, bin.lhs_idx);
      Value rhs = eval_expr(ctx, bin.rhs_idx);
      Type* lt = symtbl_get_val_type(tbl, &lhs);
      Type* rt = symtbl_get_val_type(tbl, &rhs);
      
      if (lt->kind == ValueKindArray && rt->kind == ValueKindInt) {
        // array access
        Value* buf = lhs.arr.data;
        Value res = buf[rhs.integer];
        return res;
      }

      if (!symtbl_type_eq(tbl, &lhs, &rhs)) {
        eval_log_err(ctx, "binary expression on different type");
        return val_err();
      }
      
      Value v = {0};
      switch (lt->kind) {
        case ValueKindInt: {
          switch(bin.op) {
            case TokAdd:
            case TokSub:
            case TokMul:
            case TokDiv:
            case TokRem:
            case TokExp:
              // TODO: not sure about this
              v.type_idx = ValueKindInt;
              break;

            case TokEq:     
            case TokNotEq:
            case TokGreat:
            case TokLess:
            case TokGreatEq:
            case TokLessEq:
              // TODO: not sure about this
              v.type_idx = ValueKindBool;
              break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return val_err();
          }

          switch(bin.op) {
            case TokAdd: v.integer = lhs.integer + rhs.integer; break;
            case TokSub: v.integer = lhs.integer - rhs.integer; break;
            case TokMul: v.integer = lhs.integer * rhs.integer; break;
            case TokDiv: v.integer = lhs.integer / rhs.integer; break;
            case TokRem: v.integer = lhs.integer % rhs.integer; break;
            case TokExp: v.integer = pow(lhs.integer, rhs.integer); break;

            case TokEq:       v.boolean = lhs.integer == rhs.integer; break;
            case TokNotEq:    v.boolean = lhs.integer != rhs.integer; break;
            case TokGreat:    v.boolean = lhs.integer >  rhs.integer; break;
            case TokLess:     v.boolean = lhs.integer <  rhs.integer; break;
            case TokGreatEq:  v.boolean = lhs.integer >= rhs.integer; break;
            case TokLessEq:   v.boolean = lhs.integer <= rhs.integer; break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return val_err();
          }

          return v;
        }

        case ValueKindFloat: {
          switch(bin.op) {
            case TokAdd:
            case TokSub:
            case TokMul:
            case TokDiv:
            case TokRem:
            case TokExp:
              // TODO: not sure about this
              v.type_idx = ValueKindFloat;
              break;

            case TokEq:     
            case TokNotEq:
            case TokGreat:
            case TokLess:
            case TokGreatEq:
            case TokLessEq:
              // TODO: not sure about this
              v.type_idx = ValueKindBool;
              break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return val_err();
          }

          switch(bin.op) {
            case TokAdd: v.floating = lhs.floating + rhs.floating; break;
            case TokSub: v.floating = lhs.floating - rhs.floating; break;
            case TokMul: v.floating = lhs.floating * rhs.floating; break;
            case TokDiv: v.floating = lhs.floating / rhs.floating; break;
            case TokRem: v.floating = fmod(lhs.floating, rhs.floating); break;
            case TokExp: v.floating = pow(lhs.floating, rhs.floating); break;

            case TokEq:       v.boolean = lhs.floating == rhs.floating; break;
            case TokNotEq:    v.boolean = lhs.floating != rhs.floating; break;
            case TokGreat:    v.boolean = lhs.floating >  rhs.floating; break;
            case TokLess:     v.boolean = lhs.floating <  rhs.floating; break;
            case TokGreatEq:  v.boolean = lhs.floating >= rhs.floating; break;
            case TokLessEq:   v.boolean = lhs.floating <= rhs.floating; break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return val_err();
          }

          return v;
        }

        case ValueKindBool: {
          // TODO: not sure about this
          v.type_idx = ValueKindBool;

          switch(bin.op) {
            case TokAnd:      v.boolean = lhs.boolean && rhs.boolean; break;
            case TokOr:       v.boolean = lhs.boolean || rhs.boolean; break;
            case TokEq:       v.boolean = lhs.boolean == rhs.boolean; break;
            case TokNotEq:    v.boolean = lhs.boolean != rhs.boolean; break;
            case TokGreat:    v.boolean = lhs.boolean >  rhs.boolean; break;
            case TokLess:     v.boolean = lhs.boolean <  rhs.boolean; break;
            case TokGreatEq:  v.boolean = lhs.boolean >= rhs.boolean; break;
            case TokLessEq:   v.boolean = lhs.boolean <= rhs.boolean; break;
            default:
              eval_log_err(ctx, "invalid bool binary operator");
              return val_err();
          }

          return v;
        }

        default:
          eval_log_err(ctx, "type doesn't support binary operation");
          return val_err();
      }
    }
  }

  return val_err();
}

void eval_block(Context* ctx, IntVec stmts) {
  SymTbl* tbl = ctx->tbl;
  Parser* p = ctx->p;

  VEC_FOR(stmts) {
    Stmt* s = parser_get_stmt(p, stmts.data[i]);

    switch(s->kind) {
      case StmtKindDecl: {
        char* start = p->src + s->decl.name->offset;
        int len = s->decl.name->len;
        Value val = eval_expr(ctx, s->decl.rhs_idx);

        symtbl_insert(tbl, start, len, val);
      } break;

      case StmtKindFnDecl: {
        // char* start = p->src + s->fn_decl.name->offset;
        // int len = s->fn_decl.name->len;

        // Value val = make_func_const(ctx, &s->fn_decl);

        // symtbl_insert(tbl, start, len, val);
      } break;

      case StmtKindAssign: {
        char* start = p->src + s->assign.var->offset;
        int len = s->assign.var->len;

        Value* var = symtbl_find(tbl, start, len);
        if (var == NULL) {
          eval_log_err(ctx, "undeclared variable");
          return;
        } else {
          Value res = eval_expr(ctx, s->assign.rhs_idx);
          if (!symtbl_type_eq(tbl, var, &res)) {
            eval_log_err(ctx, "assignment of different type");
            return;
          }

          *var = res;
        }
      } break;

      case StmtKindBlock: {
        symtbl_push_scope(tbl);
        eval_block(ctx, s->block.stmt_ids);
        symtbl_pop_scope(tbl);
      } break;

      case StmtKindIfElse: {
        StmtIfElse if_else = s->if_else;
        Value cond = eval_expr(ctx, if_else.cond_idx);

        if (symtbl_get_val_type(ctx->tbl, &cond)->kind != ValueKindBool) {
          eval_log_err(ctx, "if condition isn't bool");
          return;
        }

        int block_idx = cond.boolean ? if_else.if_idx : if_else.else_idx;

        if (block_idx != -1) {
          symtbl_push_scope(tbl);
          Stmt* block = parser_get_stmt(p, block_idx);
          eval_block(ctx, block->block.stmt_ids);
          symtbl_pop_scope(tbl);
        }
      } break;

      case StmtKindWhile: {
        StmtWhile wloop = s->wloop;
        Value cond = eval_expr(ctx, wloop.cond_idx);
        if (symtbl_get_val_type(tbl, &cond)->kind != ValueKindBool) {
          eval_log_err(ctx, "while condition isn't bool");
          return;
        }

        Stmt* block = parser_get_stmt(p, wloop.block_idx);
        while (cond.boolean) {
          symtbl_push_scope(tbl);
          eval_block(ctx, block->block.stmt_ids);
          symtbl_pop_scope(tbl);

          cond = eval_expr(ctx, wloop.cond_idx);
        }
      } break;

      case StmtKindExpr: {
        Value res = eval_expr(ctx, s->expr_idx);
        Type* type = symtbl_get_val_type(ctx->tbl, &res);
        switch (type->kind) {
          case ValueKindInt: printf("Int: %d\n", res.integer); break;
          case ValueKindFloat: printf("Float: %lf\n", res.floating); break;
          case ValueKindBool: printf("Bool: %s\n", res.boolean ? "true" : "false"); break;
          default: break;
        }
        break;
      }
    }
  }
}
 
void eval(Parser* p, SymTbl* tbl) {
  Context ctx = { p, tbl, NULL };
  eval_block(&ctx, p->top_lvl_stmts);
}