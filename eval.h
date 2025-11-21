#pragma once
#include "table.h"

Value eval_expr(Parser* p, int expr_id, SymbolTable* ctx) {
  Expr* e = parser_get_expr(p, expr_id);

  switch (e->type) {
    case ExprTypeLiteral: {
      ExprLiteral lit = e->lit;
      char* start = p->src + lit.tok->offset;
      int len = lit.tok->len;

      Value v;
      switch (lit.type) {
        case LiteralTypeFloat: {
          // TODO: consider using a buffer instead of allocating
          char* str = str_clone(start, len);
          double val = atof(str);
          free(str);
          
          v = (Value) { .type = ValueTypeFloat, .num = val };
        } break;

        case LiteralTypeTrue: v = (Value) { .type = ValueTypeBool, .boolean = true }; break;
        case LiteralTypeFalse: v = (Value) { .type = ValueTypeBool, .boolean = false }; break;
      }

      return v;
    } break;

    case ExprTypeVariable: {
      char* start = p->src + e->ident->offset;
      int len = e->ident->len;

      Value* res = symtable_find(ctx, start, len);
      if (res == NULL) {
        fprintf(stderr, "[EVAL ERR] Undefined variable at expr id %d\n", expr_id); 
        return val_err();
      } else {
        return *res;
      }
    } break;

    case ExprTypeUnary: {
      ExprUnary un = e->un;
      Value rhs = eval_expr(p, un.expr, ctx);

      Value v = {0};
      v.type = rhs.type;
      switch (rhs.type) {
        case ValueTypeFloat: {
          switch(un.op) {
            case TokSub: v.num = -rhs.num; break;
            default:
              fprintf(stderr, "[EVAL ERR] Invalid numeric unary operator at expr id %d\n", expr_id); 
              return val_err();
          }
        } break;

        case ValueTypeBool: {
          switch(un.op) {
            case TokNot: v.boolean = !rhs.boolean; break;
            default:
              fprintf(stderr, "[EVAL ERR] Invalid bool unary operator at expr id %d\n", expr_id); 
              return val_err();
          } 
        } break;
      }

      return v;
    }

    case ExprTypeBinary: {
      ExprBinary bin = e->bin;
      Value lhs = eval_expr(p, bin.lhs_idx, ctx);
      Value rhs = eval_expr(p, bin.rhs_idx, ctx);

      if (lhs.type != rhs.type) {
        fprintf(stderr, "[EVAL ERR] Binary expression on different types at expr id %d\n", expr_id);
        return val_err();
      }

      Value v = {0};
      v.type = lhs.type;
      switch (lhs.type) {
        case ValueTypeFloat: {
          switch(bin.op) {
            case TokAdd: v.num = lhs.num + rhs.num; break;
            case TokSub: v.num = lhs.num - rhs.num; break;
            case TokMul: v.num = lhs.num * rhs.num; break;
            case TokDiv: v.num = lhs.num / rhs.num; break;
            case TokRem: v.num = fmod(lhs.num, rhs.num); break;
            case TokExp: v.num = pow(lhs.num, rhs.num); break;
            default:
              fprintf(stderr, "[EVAL ERR] Invalid numeric binary operator at expr id %d\n", expr_id); 
              return val_err();
          }

          return v;
        }

        case ValueTypeBool: {
          switch(bin.op) {
            case TokAnd: v.boolean = lhs.boolean && rhs.boolean; break;
            case TokOr: v.boolean = lhs.boolean || rhs.boolean; break;
            default:
              fprintf(stderr, "[EVAL ERR] Invalid bool binary operator at expr id %d\n", expr_id); 
              return val_err();
          }

          return v;
        }
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
        break;
      }

      case StmtTypeAssign: {
        char* start = p->src + s->decl.tok->offset;
        int len = s->decl.tok->len;

        Value* res = symtable_find(ctx, start, len);
        if (res == NULL) {
          fprintf(stderr, "[EVAL ERR] Undeclared variable at expr id %ld\n", i); 
          return;
        } else {
          Value res = eval_expr(p, s->decl.rhs_idx, ctx);
          symtable_insert(ctx, start, len, res);
        }
        break;
      }

      case StmtTypeBlock: {
        symtable_push_scope(ctx);
        eval_block(p, s->block.stmt_ids, ctx);
        symtable_pop_scope(ctx);
        break;
      }

      case StmtTypeExpr: {
        Value res = eval_expr(p, s->expr_idx, ctx);
        switch (res.type) {
          case ValueTypeFloat: printf("%lf\n", res.num); break;
          case ValueTypeBool: printf("%s\n", res.boolean ? "true" : "false"); break;
        }
        break;
      }
    }
  }
}
 
void eval(Parser* p, SymbolTable* ctx) {
  eval_block(p, p->top_lvl_stmts, ctx);
}