#pragma once
#include "typecheck.h"

typedef enum {
  ValueKindInt,
  ValueKindFloat,
  ValueKindBool,
  ValueKindArray,
  ValueKindFunc,
  ValueKindErr,
} ValueKind;

typedef struct Value Value;
typedef struct {
  int len;
  Value* data;
} ValueArray;

// typedef struct {
//   FnParamVec params;
//   TypeId ret_id;
//   StmtId block_id;
// } ValueFunc;

struct Value {
  ValueKind kind;
  union {
    int i;
    double f;
    bool b;
    ValueArray arr;
    StmtFnDecl func;
  };
};

Value make_err_value() {
  return (Value) { .kind = ValueKindErr };
}

Value make_int_value(int val) {
  return (Value) { ValueKindInt, .i = val };
}

Value make_float_value(double val) {
  return (Value) { ValueKindFloat, .f = val };
}

Value make_bool_value(bool val) {
  return (Value) { ValueKindBool, .b = val };
}

Value make_func_value(StmtFnDecl func) {
  return (Value) { ValueKindFunc, .func = func };
}

Value make_arr_value(Value* data, int len) {
  return (Value) { ValueKindArray, .arr = { len, data }};
}

typedef struct {
  char* name;
  Value val;
} EnvVar;
VEC_DEF(EnvVar);
VEC_DEF_NAMED(EnvVarScopes, EnvVarVec);

EnvVarVec* env_top(EnvVarScopes* env) {
  return &env->data[env->len-1];
}

// void env_insert_with_str(EnvVarScopes* env, char* str, Value val) {
//   EnvVarVec* scope = env_top(env);

//   int present_idx = -1;
//   VEC_FOR(*scope) {
//     EnvVar it = scope->data[i];
//     if (strcmp(it.name, str) == 0) {
//       present_idx = i;
//       break;
//     }
//   }

//   if (present_idx == -1) {
//     // we own the identifier name string
//     char* name = strdup(str);
//     EnvVar v = { name, val };
//     VEC_PUSH(*scope, v);
//   } else {
//     scope->data[present_idx].val = val;
//   }
// }

void env_insert(EnvVarScopes* env, char* src, Token* tok, Value val) {
  char* start = src + tok->offset;
  int len = tok->len;
  
  EnvVarVec* scope = env_top(env);

  int present_idx = -1;
  VEC_FOR(*scope) {
    EnvVar it = scope->data[i];
    if (strncmp(it.name, start, len) == 0) {
      present_idx = i;
      break;
    }
  }

  if (present_idx == -1) {
    // we own the identifier name string
    char* name = str_clone(start, len);
    EnvVar v = { name, val };
    VEC_PUSH(*scope, v);
  } else {
    scope->data[present_idx].val = val;
  }
}

Value* env_find(EnvVarScopes* env, char* src, Token* tok) {
  char* start = src + tok->offset;
  int len = tok->len;

  EnvVar* present = NULL;
  for(int i=env->len-1; i >= 0; i--) {
    EnvVarVec scope = env->data[i];

    VEC_FOREACH(EnvVar, scope) {
      if (strncmp(it->name, start, len) == 0) {
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

void env_push_scope(EnvVarScopes* env) {
  VEC_PUSH(*env, (EnvVarVec) {0});
}

void env_pop_scope(EnvVarScopes* env) {
  EnvVarVec* scope = env_top(env);
  // free(scope->data);
  scope->len = 0;
  (void) VEC_POP(*env);
}

typedef struct {
  Parser* p;
  EnvVarScopes env;
  const char* err;
} Context;

Context ctx_init(Parser* p) {
  Context ctx = {0};
  ctx.p = p;
  // global scope
  env_push_scope(&ctx.env);
  return ctx;
}

void eval_log_err(Context* c, const char* err) {
  c->err = err;
  fprintf(stderr, "[EVAL ERR] %s\n", err); 
}

void eval_block(Context* ctx, IntVec stmts);
Value eval_expr(Context* ctx, int expr_id) {
  EnvVarScopes* env = &ctx->env;
  Parser* p = ctx->p;

  Expr e = parser_get_expr(p, expr_id);

  switch (e.kind) {
    case ExprKindLiteral: {
      ExprLiteral lit = e.lit;

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

        case LiteralKindTrue:  v = make_bool_value(true);  break;
        case LiteralKindFalse: v = make_bool_value(false); break;
        case LiteralKindArray: {
          IntVec ids = lit.arr.expr_ids;
          
          // shouldn't be possible
          // if (ids.len == 0) {
          //   v = make_arr_value(ctx, ValueKindBool, NULL, 0);
          //   break;
          // }

          Value* buf = malloc(sizeof(Value) * ids.len);

          VEC_FOR(ids) {
            expr_id = ids.data[i];
            buf[i] = eval_expr(ctx, expr_id);
          }

          v = make_arr_value(buf, ids.len);
        } break;
      }

      return v;
    } break;

    case ExprKindVariable: {
      Value* res = env_find(env, p->src, e.ident);
      if (res == NULL) {
        eval_log_err(ctx, "undefined variable");
        return make_err_value();
      } else {
        return *res;
      }
    } break;

    case ExprKindUnary: {
      ExprUnary un = e.un;
      Value rhs = eval_expr(ctx, un.expr);

      Value v = {0};
      v.kind = rhs.kind;
      switch (rhs.kind) {
        case ValueKindInt: {
          switch(un.op->kind) {
            case TokSub: v.i = -rhs.i; break;
            default:
              eval_log_err(ctx, "invalid integer unary operator");
              return make_err_value();
          }
        } break;

        case ValueKindFloat: {
          switch(un.op->kind) {
            case TokSub: v.f = -rhs.f; break;
            default:
              eval_log_err(ctx, "invalid floating unary operator");
              return make_err_value();
          }
        } break;

        case ValueKindBool: {
          switch(un.op->kind) {
            case TokNot: v.b = !rhs.b; break;
            default:
              eval_log_err(ctx, "invalid bool unary operator");
              return make_err_value();
          }
        } break;

        default:
          eval_log_err(ctx, "type doesn't support unary operation");
          return make_err_value();
      }

      return v;
    }

    case ExprKindBinary: {
      ExprBinary bin = e.bin;
      Value lhs = eval_expr(ctx, bin.lhs_id);
      Value rhs = eval_expr(ctx, bin.rhs_id);
      
      if (lhs.kind == ValueKindArray && rhs.kind == ValueKindInt) {
        // array access
        return lhs.arr.data[rhs.i];
      }
      
      Value v = {0};
      switch (lhs.kind) {
        case ValueKindInt: {
          switch(bin.op->kind) {
            case TokAdd:
            case TokSub:
            case TokMul:
            case TokDiv:
            case TokRem:
            case TokExp:
              v.kind = ValueKindInt;
              break;

            case TokEq:
            case TokNotEq:
            case TokGreat:
            case TokLess:
            case TokGreatEq:
            case TokLessEq:
              v.kind = ValueKindBool;
              break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return make_err_value();
          }

          switch(bin.op->kind) {
            case TokAdd: v.i = lhs.i + rhs.i; break;
            case TokSub: v.i = lhs.i - rhs.i; break;
            case TokMul: v.i = lhs.i * rhs.i; break;
            case TokDiv: v.i = lhs.i / rhs.i; break;
            case TokRem: v.i = lhs.i % rhs.i; break;
            case TokExp: v.i = pow(lhs.i, rhs.i); break;

            case TokEq:       v.b = lhs.i == rhs.i; break;
            case TokNotEq:    v.b = lhs.i != rhs.i; break;
            case TokGreat:    v.b = lhs.i >  rhs.i; break;
            case TokLess:     v.b = lhs.i <  rhs.i; break;
            case TokGreatEq:  v.b = lhs.i >= rhs.i; break;
            case TokLessEq:   v.b = lhs.i <= rhs.i; break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return make_err_value();
          }

          return v;
        }

        case ValueKindFloat: {
          switch(bin.op->kind) {
            case TokAdd:
            case TokSub:
            case TokMul:
            case TokDiv:
            case TokRem:
            case TokExp:
              v.kind = ValueKindFloat;
              break;

            case TokEq:
            case TokNotEq:
            case TokGreat:
            case TokLess:
            case TokGreatEq:
            case TokLessEq:
              v.kind = ValueKindBool;
              break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return make_err_value();
          }

          switch(bin.op->kind) {
            case TokAdd: v.f = lhs.f + rhs.f; break;
            case TokSub: v.f = lhs.f - rhs.f; break;
            case TokMul: v.f = lhs.f * rhs.f; break;
            case TokDiv: v.f = lhs.f / rhs.f; break;
            case TokRem: v.f = fmod(lhs.f, rhs.f); break;
            case TokExp: v.f = pow(lhs.f, rhs.f); break;

            case TokEq:       v.b = lhs.f == rhs.f; break;
            case TokNotEq:    v.b = lhs.f != rhs.f; break;
            case TokGreat:    v.b = lhs.f >  rhs.f; break;
            case TokLess:     v.b = lhs.f <  rhs.f; break;
            case TokGreatEq:  v.b = lhs.f >= rhs.f; break;
            case TokLessEq:   v.b = lhs.f <= rhs.f; break;

            default:
              eval_log_err(ctx, "invalid numeric binary operator");
              return make_err_value();
          }

          return v;
        }

        case ValueKindBool: {
          v.kind = ValueKindBool;
          switch(bin.op->kind) {
            case TokAnd:      v.b = lhs.b && rhs.b; break;
            case TokOr:       v.b = lhs.b || rhs.b; break;
            case TokEq:       v.b = lhs.b == rhs.b; break;
            case TokNotEq:    v.b = lhs.b != rhs.b; break;
            case TokGreat:    v.b = lhs.b >  rhs.b; break;
            case TokLess:     v.b = lhs.b <  rhs.b; break;
            case TokGreatEq:  v.b = lhs.b >= rhs.b; break;
            case TokLessEq:   v.b = lhs.b <= rhs.b; break;
            default:
              eval_log_err(ctx, "invalid bool binary operator");
              return make_err_value();
          }

          return v;
        }

        default:
          eval_log_err(ctx, "type doesn't support binary operation");
          return make_err_value();
      }
    } break;

    case ExprKindCall: {
      ExprCall call = e.call;
      Value val = eval_expr(ctx, call.callee_id);

      env_push_scope(env);
      // add arguments to scope
      VEC_FOR(call.args) {
        ExprId expr_id = call.args.data[i];
        Token* name = val.func.params_names.data[i];
        env_insert(env, p->src, name, eval_expr(ctx, expr_id)); 
      }

      Stmt block = parser_get_stmt(p, val.func.block_id);
      eval_block(ctx, block.block.stmt_ids);
      env_pop_scope(env);
    } break;
  }

  return make_err_value();
}

void eval_block(Context* ctx, IntVec stmts) {
  EnvVarScopes* env = &ctx->env;
  Parser* p = ctx->p;

  VEC_FOR(stmts) {
    Stmt s = parser_get_stmt(p, stmts.data[i]);

    switch(s.kind) {
      case StmtKindDecl: {
        Value val = eval_expr(ctx, s.decl.rhs_id);
        env_insert(env, p->src, s.decl.name, val);
      } break;

      case StmtKindFnDecl: {
        StmtFnDecl decl = s.func_decl;
        // TypeAnn signature = parser_get_type(p, decl.signature_id);
        Value v = make_func_value(decl);
        env_insert(env, p->src, decl.name, v);
      } break;

      case StmtKindAssign: {
        Value* var = env_find(env, p->src, s.assign.var);
        if (var == NULL) {
          eval_log_err(ctx, "undeclared variable");
          return;
        } else {
          Value res = eval_expr(ctx, s.assign.rhs_id);
          *var = res;
        }
      } break;

      case StmtKindBlock: {
        env_push_scope(env);
        eval_block(ctx, s.block.stmt_ids);
        env_pop_scope(env);
      } break;

      case StmtKindIfElse: {
        StmtIfElse if_else = s.if_else;
        Value cond = eval_expr(ctx, if_else.cond_id);

        int block_id = cond.b ? if_else.if_id : if_else.else_id;

        if (block_id != -1) {
          env_push_scope(env);
          Stmt block = parser_get_stmt(p, block_id);
          eval_block(ctx, block.block.stmt_ids);
          env_pop_scope(env);
        }
      } break;

      case StmtKindWhile: {
        StmtWhile wloop = s.wloop;
        Value cond = eval_expr(ctx, wloop.cond_id);

        Stmt block = parser_get_stmt(p, wloop.block_id);
        while (cond.b) {
          env_push_scope(env);
          eval_block(ctx, block.block.stmt_ids);
          env_pop_scope(env);

          cond = eval_expr(ctx, wloop.cond_id);
        }
      } break;

      case StmtKindExpr: {
        Value res = eval_expr(ctx, s.expr_id);
        switch (res.kind) {
          case ValueKindInt: printf("Int: %d\n", res.i); break;
          case ValueKindFloat: printf("Float: %lf\n", res.f); break;
          case ValueKindBool: printf("Bool: %s\n", res.b ? "true" : "false"); break;
          case ValueKindArray: printf("Array (TODO)\n"); break;
          case ValueKindFunc: printf("Func (TODO)\n"); break;
        }
        break;
      }
    }
  }
}
 
void eval(Context* c) {
  eval_block(c, c->p->top_lvl_stmts);
}