#pragma once
#include "table.h"

// TODO: returning Value might be costly

Value eval_expr(Parser* p, int expr_id, SymbolTable* ctx) {
  Expr* e = parser_get_expr(p, expr_id);

  switch (e->type) {
    case ExprTypeLiteral: {
      ExprLiteral lit = e->lit;
      Value v;
      switch (lit.type) {
        case LiteralTypeFloat: {
          char* start = p->src + lit.tok->offset;
          int len = lit.tok->len;
          // TODO: consider using a buffer instead of allocating
          char* str = str_clone(start, len);
          double val = atof(str);
          free(str);
          
          v = make_float_value(val);
        } break;

        case LiteralTypeTrue: v = make_bool_value(true); break;
        case LiteralTypeFalse: v = make_bool_value(false); break;
        case LiteralTypeArray: {
          IntVec ids = lit.arr.vals;
          if (ids.len == 0) {
            v = make_arr_value(ctx, ValueTypeBool, NULL, 0);
            break;
          }

          Value* buf = malloc(sizeof(Value) * ids.len);

          int expr_id = ids.data[0];
          Value prev = eval_expr(p, expr_id, ctx);
          buf[0] = prev;

          bool same = true;
          for(int i=1; i<ids.len; i++) {
            expr_id = ids.data[i];
            Value curr = eval_expr(p, expr_id, ctx);
            if (!symtable_type_eq(ctx, &prev, &curr)) {
              same = false;
              break;
            }
            prev = curr;
            buf[i] = curr;
          }

          if (!same) {
            free(buf);
            fprintf(stderr, "[EVAL ERR] array values must be of the same type at expr id %d\n", expr_id); 
            return val_err();
          }

          v = make_arr_value(ctx, prev.type_idx, buf, ids.len);
        } break;
      }

      return v;
    } break;

    case ExprTypeVariable: {
      char* start = p->src + e->ident->offset;
      int len = e->ident->len;

      Value* res = symtable_find(ctx, start, len);
      if (res == NULL) {
        fprintf(stderr, "[EVAL ERR] undefined variable at expr id %d\n", expr_id); 
        return val_err();
      } else {
        return *res;
      }
    } break;

    case ExprTypeUnary: {
      ExprUnary un = e->un;
      Value rhs = eval_expr(p, un.expr, ctx);
      Type* type = symtable_get_val_type(ctx, &rhs);

      Value v = {0};
      v.type_idx = rhs.type_idx;
      switch (type->type) {
        case ValueTypeFloat: {
          switch(un.op) {
            case TokSub: v.num = -rhs.num; break;
            default:
              fprintf(stderr, "[EVAL ERR] invalid numeric unary operator at expr id %d\n", expr_id); 
              return val_err();
          }
        } break;

        case ValueTypeBool: {
          switch(un.op) {
            case TokNot: v.boolean = !rhs.boolean; break;
            default:
              fprintf(stderr, "[EVAL ERR] invalid bool unary operator at expr id %d\n", expr_id); 
              return val_err();
          } 
        } break;

        default:
          fprintf(stderr, "[EVAL ERR] type doesn't support unary operation at expr id %d\n", expr_id); 
          return val_err();
      }

      return v;
    }

    case ExprTypeBinary: {
      ExprBinary bin = e->bin;
      Value lhs = eval_expr(p, bin.lhs_idx, ctx);
      Value rhs = eval_expr(p, bin.rhs_idx, ctx);
      Type* lt = symtable_get_val_type(ctx, &lhs);
      Type* rt = symtable_get_val_type(ctx, &rhs);
      
      if (lt->type == ValueTypeArray && rt->type == ValueTypeFloat) {
        // array access
        void* buf = lhs.arr.data;
        Value* res = ((Value*) buf) + ((int) rhs.num);
        return *res;
      }

      if (!symtable_type_eq(ctx, &lhs, &rhs)) {
        fprintf(stderr, "[EVAL ERR] binary expression on different types at expr id %d\n", expr_id);
        return val_err();
      }
      
      Value v = {0};
      switch (lt->type) {
        case ValueTypeFloat: {
          switch(bin.op) {
            case TokAdd:
            case TokSub:
            case TokMul:
            case TokDiv:
            case TokRem:
            case TokExp:
              // TODO: not sure about this
              v.type_idx = ValueTypeFloat;
              break;

            case TokEq:     
            case TokNotEq:
            case TokGreat:
            case TokLess:
            case TokGreatEq:
            case TokLessEq:
              // TODO: not sure about this
              v.type_idx = ValueTypeBool;
              break;

            default:
              fprintf(stderr, "[EVAL ERR] invalid numeric binary operator at expr id %d\n", expr_id); 
              return val_err();
          }

          switch(bin.op) {
            case TokAdd: v.num = lhs.num + rhs.num; break;
            case TokSub: v.num = lhs.num - rhs.num; break;
            case TokMul: v.num = lhs.num * rhs.num; break;
            case TokDiv: v.num = lhs.num / rhs.num; break;
            case TokRem: v.num = fmod(lhs.num, rhs.num); break;
            case TokExp: v.num = pow(lhs.num, rhs.num); break;

            case TokEq:       v.boolean = lhs.num == rhs.num; break;
            case TokNotEq:    v.boolean = lhs.num != rhs.num; break;
            case TokGreat:    v.boolean = lhs.num >  rhs.num; break;
            case TokLess:     v.boolean = lhs.num <  rhs.num; break;
            case TokGreatEq:  v.boolean = lhs.num >= rhs.num; break;
            case TokLessEq:   v.boolean = lhs.num <= rhs.num; break;

            default:
              fprintf(stderr, "[EVAL ERR] invalid numeric binary operator at expr id %d\n", expr_id); 
              return val_err();
          }

          return v;
        }

        case ValueTypeBool: {
          // TODO: not sure about this
          v.type_idx = ValueTypeBool;

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
              fprintf(stderr, "[EVAL ERR] invalid bool binary operator at expr id %d\n", expr_id); 
              return val_err();
          }

          return v;
        }

        default:
          fprintf(stderr, "[EVAL ERR] type doesn't support binary operation at expr id %d\n", expr_id); 
          return val_err();
      }
    }
  }

  return val_err();
}

void eval_block(Parser* p, IntVec stmts, SymbolTable* ctx) {
  VEC_FOR(stmts) {
    Stmt* s = parser_get_stmt(p, stmts.data[i]);

    switch(s->type) {
      case StmtTypeDecl: {
        char* start = p->src + s->assign.tok->offset;
        int len = s->assign.tok->len;
        Value val = eval_expr(p, s->assign.rhs_idx, ctx);

        symtable_insert(ctx, start, len, val);
      } break;

      case StmtTypeAssign: {
        char* start = p->src + s->decl.tok->offset;
        int len = s->decl.tok->len;

        Value* res = symtable_find(ctx, start, len);
        if (res == NULL) {
          fprintf(stderr, "[EVAL ERR] undeclared variable at expr id %ld\n", i); 
          return;
        } else {
          *res = eval_expr(p, s->decl.rhs_idx, ctx);
        }
      } break;

      case StmtTypeBlock: {
        symtable_push_scope(ctx);
        eval_block(p, s->block.stmt_ids, ctx);
        symtable_pop_scope(ctx);
      } break;

      case StmtTypeIfElse: {
        StmtIfElse if_else = s->if_else;
        Value cond = eval_expr(p, if_else.cond_idx, ctx);

        if (symtable_get_val_type(ctx, &cond)->type != ValueTypeBool) {
          fprintf(stderr, "[EVAL ERR] if condition isn't bool at expr id %ld\n", i); 
          return;
        }

        int block_idx = cond.boolean ? if_else.if_idx : if_else.else_idx;

        if (block_idx != -1) {
          symtable_push_scope(ctx);
          Stmt* block = parser_get_stmt(p, block_idx);
          eval_block(p, block->block.stmt_ids, ctx);
          symtable_pop_scope(ctx);
        }
      } break;

      case StmtTypeWhile: {
        StmtWhile wloop = s->wloop;
        Value cond = eval_expr(p, wloop.cond_idx, ctx);
        if (symtable_get_val_type(ctx, &cond)->type != ValueTypeBool) {
          fprintf(stderr, "[EVAL ERR] while condition isn't bool at expr id %ld\n", i); 
          return;
        }

        Stmt* block = parser_get_stmt(p, wloop.block_idx);
        while (cond.boolean) {
          symtable_push_scope(ctx);
          eval_block(p, block->block.stmt_ids, ctx);
          symtable_pop_scope(ctx);

          cond = eval_expr(p, wloop.cond_idx, ctx);
        }
      } break;

      case StmtTypeExpr: {
        Value res = eval_expr(p, s->expr_idx, ctx);
        Type* type = symtable_get_val_type(ctx, &res);
        switch (type->type) {
          case ValueTypeFloat: printf("%lf\n", res.num); break;
          case ValueTypeBool: printf("%s\n", res.boolean ? "true" : "false"); break;
          default: break;
        }
        break;
      }
    }
  }
}
 
void eval(Parser* p, SymbolTable* ctx) {
  eval_block(p, p->top_lvl_stmts, ctx);
}