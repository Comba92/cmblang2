#pragma once
#include "table.h"

double eval_expr(Parser* p, int scope, int expr_id, SymbolTable* ctx) {
  Expr* e = parser_get_expr(p, expr_id);

  switch (e->type) {
    case ExprTypeLiteral: {
      char* start = p->src + e->tok->offset;
      int len = e->tok->len;

      // TODO: consider using a buffer instead of allocating
      char* str = str_clone(start, len);
      double val = atof(str);
      free(str);

      return val;
    }

    case ExprTypeVariable: {
      int start = e->tok->offset;
      int len = e->tok->len;

      Value* res = symtable_find(ctx, scope, p->src + start, len);

      if (res == NULL) {
        fprintf(stderr, "[EVAL ERR] Undefined variable at expr id %d\n", expr_id); 
        return NAN;
      } else {
        return res->num;
      }
    }

    case ExprTypeUnary: {
      double rhs = eval_expr(p, scope, e->un.expr, ctx);

      switch(e->un.op) {
        case TokSub: return -rhs;
        default:
          fprintf(stderr, "[EVAL ERR] Invalid unary operator at expr id %d\n", expr_id); 
          return NAN;
      }
    }

    case ExprTypeBinary: {
      double lhs = eval_expr(p, scope, e->bin.lhs_idx, ctx);
      double rhs = eval_expr(p, scope, e->bin.rhs_idx, ctx);

      switch(e->bin.op) {
        case TokAdd: return lhs + rhs;
        case TokSub: return lhs - rhs;
        case TokMul: return lhs * rhs;
        case TokDiv: return lhs / rhs;
        case TokRem: return fmod(lhs, rhs);
        case TokExp: return pow(lhs, rhs);
        default:
          fprintf(stderr, "[EVAL ERR] Invalid binary operator at expr id %d\n", expr_id); 
          return NAN;
      }
    }
  }
}

void eval_block(Parser* p, int scope, IntVec stmts, SymbolTable* ctx) {
  scope += 1;

  VEC_FOR(stmts) {
    Stmt* s = parser_get_stmt(p, stmts.data[i]);

    switch(s->type) {
      case StmtTypeDecl: {
        char* start = p->src + s->assign.tok->offset;
        int len = s->assign.tok->len;
        double val = eval_expr(p, scope, s->assign.rhs_idx, ctx);

        symtable_insert(ctx, scope, start, len, (Value) { .type = ValueTypeFloat, .num = val });
        break;
      }

      case StmtTypeAssign: {
        char* start = p->src + s->decl.tok->offset;
        int len = s->decl.tok->len;

        Value* res = symtable_find(ctx, scope, start, len);
        if (res == NULL) {
          fprintf(stderr, "[EVAL ERR] Undeclared variable at expr id %ld\n", i); 
          return;
        } else {
          res->num = eval_expr(p, scope, s->decl.rhs_idx, ctx);
        }
        break;
      }

      case StmtTypeBlock: {
        // symtable_push_scope(ctx);
        eval_block(p, scope, s->block.stmt_ids, ctx);
        // symtable_pop_scope(ctx);
        break;
      }

      case StmtTypeExpr: {
        double res = eval_expr(p, scope, s->expr_idx, ctx);
        printf("%lf\n", res);
        break;
      }
    }
  }
}
 
void eval(Parser* p, SymbolTable* ctx) {
  eval_block(p, -1, p->top_lvl_stmts, ctx);
}