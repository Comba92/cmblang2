#pragma once

// TODO: consider storing indexes to Tokens instead of pointers

// TODO: use these instead of int
typedef int ExprId;
typedef int StmtId;
typedef int TypeId;

typedef enum {
  ExprKindLiteral,
  ExprKindVariable,
  ExprKindUnary,
  ExprKindBinary,
  ExprKindCall,
} ExprKind;

typedef enum {
  LiteralKindInt   = TokInt,
  LiteralKindFloat = TokFloat,
  LiteralKindTrue  = TokTrue,
  LiteralKindFalse = TokFalse,
  LiteralKindArray,
} LiteralKind;

typedef struct {
  IntVec expr_ids;
} LiteralArray;

typedef struct {
  LiteralKind kind;
  union {
    Token* tok;
    LiteralArray arr;
  };
} ExprLiteral;

typedef struct {
  Token* op;
  int expr;
} ExprUnary;

typedef struct {
  int lhs_idx;
  Token* op;
  int rhs_idx;
} ExprBinary;

typedef struct {
  int callee_idx;
  IntVec args;
} ExprCall;

typedef struct {
  ExprKind kind;
  union {
    ExprLiteral lit;
    Token* ident;
    ExprUnary un;
    ExprBinary bin;
    ExprCall call;
  };
} Expr;

#define PRIMITIVES_LIST \
  X(Int) \
  X(Float) \
  X(Bool) \

#define X(Name) TypeAnnKind##Name,
typedef enum {
  PRIMITIVES_LIST
  TypeAnnKindArray,
  TypeAnnKindFunc,
  TypeAnnKindUnknown,
} TypeAnnKind;
#undef X

typedef struct {
  Token* name;
  int type_idx;
} FnParam;
VEC_DEF(FnParam);

typedef struct {
  FnParamVec params;
  int ret_idx;
  int block_idx;
} TypeAnnFunc;

typedef struct {
  TypeAnnKind kind;
  union {
    LiteralKind prim;
    int inner_idx;
    TypeAnnFunc func;
  };
} TypeAnn;
VEC_DEF(TypeAnn);

typedef enum {
  StmtKindExpr,
  StmtKindDecl,
  StmtKindFnDecl,
  StmtKindAssign,
  StmtKindBlock,
  StmtKindIfElse,
  StmtKindWhile,
  StmtKindReturn,
} StmtKind;

// forward declare Stmt and StmtVec, so we can build self referencing stmts such as StmtBlock
typedef struct Stmt Stmt;
VEC_DEF(Stmt);

typedef struct {
  Token* name;
  int type_idx;
  int rhs_idx;
} StmtDecl;

typedef struct {
  Token* name;
  int type_idx;
} StmtFnDecl;

typedef struct {
  Token* var;
  int rhs_idx;
} StmtAssign;

typedef struct {
  IntVec stmt_ids;
} StmtBlock;

typedef struct {
  int cond_idx;
  int if_idx;
  int else_idx;
} StmtIfElse;

typedef struct {
  int cond_idx;
  int block_idx;
} StmtWhile;

struct Stmt {
  StmtKind kind;
  union {
    StmtDecl decl;
    StmtFnDecl fn_decl;
    StmtAssign assign;
    StmtBlock block;
    StmtIfElse if_else;
    StmtWhile wloop;
    int expr_idx;
  };
};

VEC_DEF(Expr);

void expr_dbg(ExprVec exprs, int idx) {
  Expr* e = &exprs.data[idx];
  printf("[EXPR %d] kind = %d ", idx, e->kind);

  switch(e->kind) {
    case ExprKindLiteral: printf("literal\n"); break;
    case ExprKindVariable: printf("variable\n"); break;
    case ExprKindUnary:
      printf("op = %c, rhs = %d\n", e->un.op->kind, e->un.expr);
      break;
    case ExprKindBinary: 
      printf("lhs = %d, op = %c, rhs = %d\n", e->bin.lhs_idx, e->bin.op->kind, e->bin.rhs_idx);
      break;
    default: break; 
  }
}

void stmt_dbg(StmtVec stmts, int idx) {
  Stmt* s = &stmts.data[idx];
  printf("[STMT %d] kind = %d\n", idx, s->kind);
}

typedef struct  {
  char* src;

  TokenVec tokens;
  int curr_token;
  
  // TODO: types should be unique; would be a great idea to store them in a hash table
  TypeAnnVec types;
  ExprVec exprs;
  StmtVec stmts;
  IntVec top_lvl_stmts;
  
  // TODO: might be a vector of errors
  const char* err;
} Parser;

void parse_log_err(Parser* p, const char* err) {
  p->err = err;
  
  int token_id = p->curr_token-1;
  Token* t = &p->tokens.data[token_id];
  int column = t->offset;
  fprintf(stderr, "[PARSE ERR] %s at token %d (type = %c), column %d\n", err, token_id, t->kind, column);
}

int parser_push_expr(Parser* p, Expr e) {
  VEC_PUSH(p->exprs, e);
  return p->exprs.len-1;
}

int parser_push_type(Parser* p, TypeAnn t) {
  VEC_PUSH(p->types, t);
  return p->types.len-1;
}

int parser_push_stmt(Parser* p, Stmt s) {
  VEC_PUSH(p->stmts, s);
  return p->stmts.len-1;
}

bool parser_is_at_end(Parser* p) {
  return p->curr_token >= p->tokens.len;
}

Token* parser_peek(Parser* p) {
  return &p->tokens.data[p->curr_token];
}

Token* parser_peek_nth(Parser* p, int n) {
  int idx = n + p->curr_token;
  return idx < p->tokens.len ? &p->tokens.data[idx] : &TOKEN_ERR;
}

Token* parser_eat(Parser* p) {
  return &p->tokens.data[p->curr_token++];
}

Token* parser_eat_match(Parser* p, TokenKind match, const char* err) {
  Token* t = parser_eat(p);
  if (t->kind != match) {
    parse_log_err(p, err);
    return NULL;
  }

  return t;
}

Token* parser_eat_if(Parser* p, TokenKind match) {
  if (parser_peek(p)->kind == match) return parser_eat(p);
  else return NULL;
}

// return by value
TypeAnn* parser_get_type(Parser* p, int idx) {
  return &p->types.data[idx];
}

// return by value
Expr* parser_get_expr(Parser* p, int idx) {
  return &p->exprs.data[idx];
}

// return by value
Stmt* parser_get_stmt(Parser* p, int idx) {
  return &p->stmts.data[idx];
}

void parser_clear(Parser *p) {
  p->src = NULL;
  p->tokens.len = 0;
  p->curr_token = 0;

  // do not free types
  // VEC_FOR(p->types) {
  //   TypeAnn* t = &p->types.data[i];
  //   if (t->kind == TypeAnnKindFunc) VEC_FREE(t->func.params);
  // }
  // p->types.len = 0;

  VEC_FOR(p->exprs) {
    Expr* e = &p->exprs.data[i];
    if (e->kind == ExprKindLiteral && e->lit.kind == LiteralKindArray) {
      VEC_FREE(e->lit.arr.expr_ids);
    } else if (e->kind == ExprKindCall) {
      VEC_FREE(e->call.args);
    }
  }
  p->exprs.len = 0;

  VEC_FOR(p->stmts) {
    Stmt* s = &p->stmts.data[i];
    if (s->kind == StmtKindBlock) VEC_FREE(s->block.stmt_ids);
  }
  p->stmts.len = 0;
  p->top_lvl_stmts.len = 0;

  p->err = NULL;
}

void parser_free(Parser *p) {
  parser_clear(p);
  VEC_FREE(p->tokens);

  VEC_FOR(p->types) {
    TypeAnn* t = &p->types.data[i];
    if (t->kind == TypeAnnKindFunc) VEC_FREE(t->func.params);
  }
  p->types.len = 0;
  
  VEC_FREE(p->types);
  VEC_FREE(p->exprs);
  VEC_FREE(p->stmts);
  VEC_FREE(p->top_lvl_stmts);
}

typedef struct {
  int left;
  int right;
} PrecLvl;
#define PREC(_a, _b) (PrecLvl) {(_a), (_b)}

int prefix_lvl(Token op) {
  switch (op.kind) {
    case TokSub: return 30;
    case TokNot: return 29;
    default: return -1;
  }
}

int postfix_lvl(Token op) {
  switch (op.kind) {
    case TokBraceLeft: return 32;
    case TokParenLeft: return 32;
    default: return -1;
  }
}

// https://www.alphacodingskills.com/rust/notes/rust-operators-precedence.php
PrecLvl infix_lvl(Token op) {
  switch (op.kind) {
    case TokAdd:
    case TokSub:
      return PREC(23,24);
    case TokEq:
    case TokNotEq:
      return PREC(16,17);
    case TokGreat:
    case TokGreatEq:
    case TokLess:
    case TokLessEq:
      return PREC(14, 15);
    case TokMul:
    case TokDiv:
    case TokRem: 
      return PREC(25,26);
    case TokAnd:
      return PREC(12,13);
    case TokOr:
      return PREC(10,11);
    case TokExp:
      return PREC(27,28);

    default: return PREC(-1,-1);
  }
}

Expr literal_primitive(Token* t) {
  ExprLiteral lit = {(LiteralKind) t->kind, .tok = t};
  return (Expr) { ExprKindLiteral, .lit = lit };
}

Expr literal_array(IntVec expr_ids) {
  LiteralArray arr = { expr_ids };
  ExprLiteral lit = {LiteralKindArray, .arr = arr};
  return (Expr) { ExprKindLiteral, .lit = lit };
}

Expr variable(Token* t) {
  return (Expr) { ExprKindVariable, .ident = t };
}

Expr unary(Token* op, int rhs) {
  ExprUnary un = {op, rhs};
  return (Expr) { ExprKindUnary, .un = un };
}

Expr binary(int lhs, Token* op, int rhs) {
  ExprBinary bin = {lhs, op, rhs};
  return (Expr) { ExprKindBinary, .bin = bin };
}

Expr call(int lhs, IntVec args) {
  ExprCall call = {lhs, args};
  return (Expr) { ExprKindCall, .call = call };
}

int parse_expr(Parser*, int);
// TODO: where to check for empty list? here or caller?
IntVec collect_expr_list(Parser* p, TokenKind separator, TokenKind terminator, const char* err) {
  // careful: memory leak if return early  
  IntVec expr_ids = {0};
  
  while (!parser_is_at_end(p)) {
    int val = parse_expr(p, 0);
    VEC_PUSH(expr_ids, val);

    Token* t = parser_peek(p);
    if (t->kind == terminator) break;
    else if (t->kind == separator) {
      parser_eat(p);
      // we can have a separator at the end without any expression after
      if (parser_peek(p)->kind == terminator) break;
    } else {
      VEC_FREE(expr_ids);
      parse_log_err(p, err);
      return (IntVec) {0};
    }
  }

  if (parser_eat_match(p, terminator, err) == NULL) {
    VEC_FREE(expr_ids);
    return (IntVec) {0};
  }

  return expr_ids;
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
int parse_expr(Parser* p, int prec_lvl) {
  Token* t = parser_eat(p);

  // parse lhs
  int lhs = -1;
  switch (t->kind) {
    case TokInt:
    case TokFloat:
    case TokTrue:
    case TokFalse:
      lhs = parser_push_expr(p, literal_primitive(t));
      break;

    case TokIdent:
      // variable
      lhs = parser_push_expr(p, variable(t));
      break;

    case TokSub:
    case TokNot:
      int lvl = prefix_lvl(*t);
      int rhs = parse_expr(p, lvl);
      lhs = parser_push_expr(p, unary(t, rhs));
      break;

    case TokParenLeft:
      lhs = parse_expr(p, 0);
      if (parser_eat_match(p, TokParenRight, "unclosed parenthesis ')' for expression") == NULL) return -1;
      break;

    case TokBraceLeft:
      // empty array
      if (parser_eat_if(p, TokBraceRight) != NULL) {
        parse_log_err(p, "empty arrays are illegal");
        return -1;
      }

      IntVec exprs = collect_expr_list(p, TokComma, TokBraceRight, "expected ',' or brace closing ']' for array literal");
      if (exprs.len == 0) return -1;

      return parser_push_expr(p, literal_array(exprs));
      break;

    default:
      parse_log_err(p, "invalid expression");
      return -1;
  }

  while (!parser_is_at_end(p)) {
    Token* op = parser_peek(p);
    if (!tok_is_op(*op)) break;

    int postfix = postfix_lvl(*op);
    if (postfix != -1) {
      if (postfix < prec_lvl) break;
      parser_eat(p);

      if (op->kind == TokBraceLeft) {
        // array indexing
        int rhs = parse_expr(p, 0);
        if (parser_eat_match(p, TokBraceRight, "unclosed brace '[' for array indexing expression") == NULL) return -1;
        
        lhs = parser_push_expr(p, binary(lhs, op, rhs));
      } else if (op->kind == TokParenLeft) {
        // function call

        if (parser_eat_if(p, TokParenRight) != NULL) {
          // no args
          lhs = parser_push_expr(p, call(lhs, (IntVec) {0}));
        } else {
          IntVec args = collect_expr_list(p, TokComma, TokParenRight, "expect ',' or parenthesis closing ')' for function call arguments");
          if (args.len == 0) return -1;
          lhs = parser_push_expr(p, call(lhs, args));
        }
      } else {
        lhs = parser_push_expr(p, unary(op, lhs));
      }

      continue;
    }

    PrecLvl infix = infix_lvl(*op);
    if (infix.left < prec_lvl) break;
    parser_eat(p);

    int rhs = parse_expr(p, infix.right);
    lhs = parser_push_expr(p, binary(lhs, op, rhs));
  }

  return lhs;
}

// OLD: old parse_assign
// int parse_assign(Parser* p, bool declaration) {
//   Token* name = parser_eat_match(p, TokIdent, ExpectIdentifier);
//   if (name == NULL) return -1;

//   if (parser_eat_match(p, TokAssign, ExpectAssign) == NULL) return -1;

//   int rhs = parse_expr(p, 0);

//   Stmt stmt;
//   if (declaration) {
//     StmtAssign decl = (StmtAssign) {name, rhs};
//     stmt = (Stmt) { StmtKindDecl, .decl = decl };
//   } else {
//     StmtAssign assign = (StmtAssign) {name, rhs};
//     stmt = (Stmt) { StmtKindAssign, .assign = assign };
//   }

//   return parser_push_stmt(p, stmt);
// }

// OLD: old parse_decl with var ident = expr;
// int parse_decl(Parser* p) {
//   // eat var keyword
//   parser_eat(p);
//   return parse_assign(p, true);
// }


int parse_decl(Parser* p) {
  Token* name = parser_eat(p);
  // eat ':='
  parser_eat(p);
  int rhs = parse_expr(p, 0);

  StmtDecl decl = {name, -1, rhs};
  Stmt stmt = (Stmt) { StmtKindDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
}

int parse_type(Parser* p) {
  // primitive
  // function
  // struct
  // array

  Token* tok = parser_eat(p);

  TypeAnn t;
  switch (tok->kind) {
    case TokInt: t.kind = TypeAnnKindInt; break;
    case TokFloat: t.kind = TypeAnnKindFloat; break;
    case TokBool: t.kind = TypeAnnKindBool; break;

    case TokIdent: {
      // this might be any kind of user defined type
      // TODO: should return unknown type?
      parse_log_err(p, "user defined types not yet supported");
      return -1;
    } break;

    case TokBraceLeft: {
      // array

      // parse inner type
      int inner = parse_type(p);
      if (parser_eat_match(p, TokBraceRight, "expect closing brace ']' in array type annotation") == NULL) return -1;

      t = (TypeAnn) { TypeAnnKindArray, .inner_idx = inner };
    } break;

    case TokParenLeft: {
      // function

      // careful: memory leak if return early  
      FnParamVec params = {0};
      
      while (!parser_is_at_end(p)) {
        int param_idx = parse_type(p);
        FnParam param = {NULL, param_idx};
        VEC_PUSH(params, param);

        Token* t = parser_peek(p);
        if (t->kind == TokParenRight) break;
        else if (t->kind == TokComma) {
          parser_eat(p);
          // we can have a separator at the end without any expression after
          if (parser_peek(p)->kind == TokParenRight) break;
        } else {
          VEC_FREE(params);
          parse_log_err(p, "expect comma ',' or closing parenthesis ')' in function type annotation");
          return -1;
        }
      }

      if (parser_eat_match(p, TokParenRight, "expect closing parenthesis ')' at end of function type annotation") == NULL) {
        VEC_FREE(params);
        return -1;
      }

      // TODO: function annotation might always require an arrow 
      int ret_idx = -1;
      if (parser_eat_if(p, TokArrow) != NULL) {
        ret_idx = parse_type(p);
      }

      TypeAnnFunc func = { params, ret_idx, -1 };
      t = (TypeAnn) { TypeAnnKindFunc, .func = func };
    } break;

    default: 
      parse_log_err(p, "invalid type annotation");
      return -1;
  }

  return parser_push_type(p, t);
}

int parse_decl_annot(Parser* p) {
  Token* name = parser_eat(p);

  // eat ':'
  parser_eat(p);
  int type = parse_type(p);

  if (parser_eat_match(p, TokAssign, "expect '=' after variable name and type annotation") == NULL) return -1;
  int rhs = parse_expr(p, 0);

  StmtDecl decl = (StmtDecl) {name, type, rhs};
  Stmt stmt = (Stmt) { StmtKindDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
}

int parse_assign(Parser* p) {
  Token* name = parser_eat(p);
  // eat '='
  parser_eat(p);
  int rhs = parse_expr(p, 0);

  StmtAssign assign = (StmtAssign) {name, rhs};
  Stmt stmt = (Stmt) { StmtKindAssign, .assign = assign };
  return parser_push_stmt(p, stmt);
}

int parse_stmt(Parser* p);
int parse_block(Parser* p) {
  // eat '{'
  if (parser_eat_match(p, TokCurlyLeft, "expected opening block curly brace '{'") == NULL) return -1;

  // careful: memory leak if return early
  IntVec stmt_ids = {0};
  while (!parser_is_at_end(p) && parser_peek(p)->kind != TokCurlyRight) {
    int s = parse_stmt(p);
    VEC_PUSH(stmt_ids, s);
  }
  
  if (parser_is_at_end(p)) {
    VEC_FREE(stmt_ids);
    parse_log_err(p, "unclosed block, expected curly brace '}'");
    return -1;
  }

  // we are sure to have reached a '}', we would have reached end of tokens otherwise
  // eat '}'
  parser_eat(p);
  return parser_push_stmt(p, (Stmt) {StmtKindBlock, .block = {stmt_ids}});
}

int parse_if_else(Parser* p) {
  // eat 'if'
  parser_eat(p);
  int expr_id = parse_expr(p, 0);
  int if_id = parse_block(p);

  int else_id = -1;
  if (parser_peek(p)->kind == TokElse) {
    // eat 'else'
    parser_eat(p);
    else_id = parse_block(p);
  }

  StmtIfElse stmt = { .cond_idx = expr_id, .if_idx = if_id, .else_idx = else_id };
  return parser_push_stmt(p, (Stmt) {StmtKindIfElse, .if_else = stmt});
}

int parse_while(Parser* p) {
  // eat 'while'
  parser_eat(p);
  int expr_id = parse_expr(p, 0);
  int while_id = parse_block(p);

  StmtWhile stmt = { .cond_idx = expr_id, .block_idx = while_id };
  return parser_push_stmt(p, (Stmt) {StmtKindWhile, .wloop = stmt});
}

int parse_return(Parser* p) {
  // eat 'return'
  parser_eat(p);

  Stmt stmt;
  if (tok_is_expr(*parser_peek(p))) {
    int expr = parse_expr(p, 0);
    stmt = (Stmt) { StmtKindReturn, .expr_idx = expr };
  } else {
    // no expr
    stmt = (Stmt) { StmtKindReturn, .expr_idx = -1 };  
  }

  return parser_push_stmt(p, stmt);
}

int parse_func_decl(Parser* p) {
  // eat 'fn'
  parser_eat(p);
  Token* func_name = parser_eat_match(p, TokIdent, "expect function name after 'fn' keyword");
  if (func_name == NULL) return -1;

  if (parser_eat_match(p, TokParenLeft, "expect open parenthesis '(' after fn keyword") == NULL) return -1;

  FnParamVec params = {0};

  while(!parser_is_at_end(p)) {
    // no params
    if (parser_peek(p)->kind == TokParenRight) {
      parser_eat(p);
      break;
    }

    Token* name = parser_eat_match(p, TokIdent, "expect function parameter name");
    if (name == NULL) goto error;
    if (parser_eat_match(p, TokColon, "expect ':' after function parameter name") == NULL) goto error;

    int type_idx = parse_type(p);
    if (type_idx == -1) goto error;

    FnParam param = { name, type_idx }; 
    VEC_PUSH(params, param);

    Token* t = parser_eat(p);
    if (t->kind == TokParenRight) break;
    else if (t->kind == TokComma) {
      // we can have a comma at the end without any expression after
      if (parser_peek(p)->kind == TokParenRight) { parser_eat(p); break; }
    } else {
      parse_log_err(p, "expect ',' or closed parenthesis ')' for function parameter listing");
      goto error;
    }
  }

  int ret_idx = -1;
  Token* arrow = parser_eat_if(p, TokArrow);
  if (arrow != NULL) {
    ret_idx = parse_type(p);
    if (ret_idx == -1) goto error;
  }

  int block_idx = parse_block(p);

  // TODO: this sucks and won't work
  IntVec ids = parser_get_stmt(p, block_idx)->block.stmt_ids;
  int last_stmt = ids.len > 0 ? ids.data[ids.len-1] : -1;

  // if we don't have a return type, we don't care
  // if we have a return value and block is empty, it is an error
  // if we have some stmts but no return value, it is an error
  if (ret_idx != -1 && (last_stmt == -1 || parser_get_stmt(p, last_stmt)->kind != StmtKindReturn)) {
    parse_log_err(p, "expect return expression at end of function implementation");
    goto error;
  }

  // build type
  TypeAnnFunc type = {params, ret_idx, block_idx};
  int type_idx = parser_push_type(p, (TypeAnn) { TypeAnnKindFunc, .func = type });

  StmtFnDecl func = { func_name, type_idx };
  return parser_push_stmt(p, (Stmt) { StmtKindFnDecl, .fn_decl = func });

  error:
    VEC_FREE(params);
    return -1;
}

int parse_stmt(Parser* p) {
  int stmt = -1;
  switch(parser_peek(p)->kind) {
    // case TokVar: return parse_decl(p);
    case TokCurlyLeft: stmt = parse_block(p); break;
    case TokIf: stmt = parse_if_else(p); break;
    case TokWhile: stmt = parse_while(p); break;
    case TokFn: stmt = parse_func_decl(p); break;
    case TokReturn: stmt = parse_return(p); break;

    default: {
      bool is_ident = parser_peek(p)->kind == TokIdent; 

      Token* next = parser_peek_nth(p, 1);
      if (is_ident && next->kind == TokAssign)
        stmt = parse_assign(p);
      else if (is_ident && next->kind == TokDecl)
        stmt = parse_decl(p);
      else if (is_ident && next->kind == TokColon)
        stmt = parse_decl_annot(p);
      else {
        // expression
        int id = parse_expr(p, 0);
        stmt = parser_push_stmt(p, (Stmt) {StmtKindExpr, .expr_idx = id});
      }
    } break;
  }

  // if (parser_eat_match(p, TokSemicolon, "expect ';' at end of statement") == NULL) return -1;
  // ignore ';'
  parser_eat_if(p, TokSemicolon);
  return stmt;
}

void parser_init_types(Parser* p) {
  TypeAnn t;

  #define X(Name) t.kind = TypeAnnKind##Name; VEC_PUSH(p->types, t);
  PRIMITIVES_LIST
  #undef X
}

void parse(Parser* p, char* str) {
  parser_clear(p);
  parser_init_types(p);

  TokenVec tokens = tokenize(str);
  p->src = str;
  p->tokens = tokens;

  while (!parser_is_at_end(p)) VEC_PUSH(p->top_lvl_stmts, parse_stmt(p));
}