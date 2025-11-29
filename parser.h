#pragma once
#include "deps/stb_ds.h"

// TODO: consider storing indexes to Tokens instead of pointers

typedef int ExprId;
typedef int StmtId;
typedef int TypeId;

#define EXPR_LIST \
  X(ExprKindLiteral) \
  X(ExprKindVariable) \
  X(ExprKindUnary) \
  X(ExprKindBinary) \
  X(ExprKindCall) \
  X(ExprKindMember) \
  X(ExprKindIndex) \

#define X(E) #E,
const char* EXPR_DBG[] = {
  EXPR_LIST
};
#undef X

#define X(E) E,
typedef enum {
  EXPR_LIST
} ExprKind;
#undef X

typedef enum {
  LiteralKindInt,
  LiteralKindFloat,
  LiteralKindTrue,
  LiteralKindFalse,
  LiteralKindArray,
  // LiteralKindArrayFill,
  // LiteralKindFunc,
} LiteralKind;

typedef struct {
  IntVec expr_ids;
} LiteralArray;

// typedef struct {
//   TokenRefVec args_names;
//   IntVec args_ids;
   
// } LiteralFunc;

typedef struct {
  LiteralKind kind;
  union {
    Token* tok;
    LiteralArray arr;
  };
} ExprLiteral;

typedef struct {
  Token* op;
  ExprId expr;
} ExprUnary;

typedef struct {
  ExprId lhs_id;
  Token* op;
  ExprId rhs_id;
} ExprBinary;

typedef struct {
  ExprId callee_id;
  IntVec args;
} ExprCall;

typedef struct {
  ExprId lhs;
  Token* field;
} ExprMember;

typedef struct {
  ExprId lhs;
  ExprId idx;
} ExprIndex;

typedef struct {
  ExprKind kind;
  union {
    ExprLiteral lit;
    Token* ident;
    ExprUnary un;
    ExprBinary bin;
    ExprCall call;
    ExprMember memb;
    ExprIndex idx;
  };
} Expr;
VEC_DEF(Expr);

void expr_dbg(Expr e) {
  printf("[EXPR] kind = %s\n", EXPR_DBG[e.kind]);
}

#define PRIMITIVES_LIST \
  X(Unknown) \
  X(Void) \
  X(Int) \
  X(Float) \
  X(Bool) \

#define TYPES_LIST \
  X(Array) \
  X(Func) \
  X(Struct) \

#define X(T) #T,
const char* TYPE_DBG[] = {
  PRIMITIVES_LIST
  TYPES_LIST
};
#undef X

#define X(Name) TypeAnnKind##Name,
typedef enum {
  PRIMITIVES_LIST
  TYPES_LIST
} TypeAnnKind;
#undef X

typedef struct {
  IntVec params_ids;
  TypeId ret_id;
} TypeAnnFunc;

typedef struct {
  TypeId inner_id;
  int len;
} TypeAnnArray;

typedef struct {
  Token* name;
  TypeId type_id;
} StructField;
VEC_DEF(StructField);

typedef struct {
  StructFieldVec fields;
} TypeAnnStruct;

typedef struct {
  TypeAnnKind kind;
  union {
    LiteralKind prim;
    TypeAnnArray arr;
    TypeAnnFunc func;
    TypeAnnStruct strct;
  };
} TypeAnn;
VEC_DEF(TypeAnn);

void type_dbg(TypeAnn t) {
  printf("[TYPE] kind = %s\n", TYPE_DBG[t.kind]);
}

TypeAnn new_func(IntVec params, TypeId ret) {
  TypeAnnFunc func = { params, ret };
  return (TypeAnn) {TypeAnnKindFunc, .func = func };
}

TypeAnn new_arr(TypeId inner, int len) {
  TypeAnnArray arr = { inner, len };
  return (TypeAnn) {TypeAnnKindArray, .arr = arr };
}

TypeAnn new_struct(StructFieldVec fields) {
  TypeAnnStruct strct = { fields };
  return (TypeAnn) {TypeAnnKindStruct, .strct = strct };
}

#define STMT_LIST \
  X(StmtKindExpr) \
  X(StmtKindDecl) \
  X(StmtKindFnDecl) \
  X(StmtKindStructDecl) \
  X(StmtKindAssign) \
  X(StmtKindBlock) \
  X(StmtKindIfElse) \
  X(StmtKindWhile) \
  X(StmtKindReturn) \

#define X(S) #S,
const char* STMT_DEBUG[] = {
  STMT_LIST
};
#undef X

#define X(S) S,
typedef enum {
  STMT_LIST
} StmtKind;
#undef X

// forward declare Stmt and StmtVec, so we can build self referencing stmts such as StmtBlock
typedef struct Stmt Stmt;
VEC_DEF(Stmt);

typedef struct {
  Token* name;
  TypeId type_id;
  ExprId rhs_id;
} StmtDecl;

typedef struct {
  Token* name;
  TokenRefVec params_names;
  TypeId signature_id;
  // TODO: probably should own the StmtBlock
  StmtId block_id;
} StmtFnDecl;

typedef struct {
  Token* name;
  TypeId type_id;
} StmtStructDecl;

typedef struct {
  ExprId lhs_id;
  ExprId rhs_id;
} StmtAssign;

typedef struct {
  IntVec stmt_ids;
} StmtBlock;

typedef struct {
  ExprId cond_id;
  StmtId if_id;
  StmtId else_id;
} StmtIfElse;

typedef struct {
  ExprId cond_id;
  StmtId block_id;
} StmtWhile;

typedef struct {
  ExprId expr_id;
  StmtId caller_id;
} StmtReturn;

struct Stmt {
  StmtKind kind;
  union {
    StmtDecl decl;
    StmtFnDecl func_decl;
    StmtStructDecl strct;
    StmtAssign assign;
    StmtBlock block;
    StmtIfElse if_else;
    StmtWhile wloop;
    StmtReturn ret;
    ExprId expr_id;
  };
};

void stmt_dbg(Stmt s) {
  printf("[STMT] kind = %s\n", STMT_DEBUG[s.kind]);
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
  bool is_panicking;
  bool had_errors;
} Parser;


TypeAnn parser_get_type(Parser* p, TypeId id) {
  return p->types.data[id];
}

Expr parser_get_expr(Parser* p, ExprId id) {
  return p->exprs.data[id];
}

Stmt parser_get_stmt(Parser* p, StmtId id) {
  return p->stmts.data[id];
}


ExprId parser_push_expr(Parser* p, Expr e) {
  VEC_PUSH(p->exprs, e);
  return p->exprs.len-1;
}

bool type_eq(Parser* p, TypeAnn a, TypeAnn b) {
  if (a.kind != b.kind) return false;

  switch(a.kind) {
    case TypeAnnKindUnknown: return false;

    case TypeAnnKindVoid:
    case TypeAnnKindInt:
    case TypeAnnKindFloat:
    case TypeAnnKindBool:
      return true;

    case TypeAnnKindFunc: {
      TypeAnnFunc func_a = a.func;
      TypeAnnFunc func_b = b.func;

      IntVec params_a = func_a.params_ids;
      IntVec params_b = func_b.params_ids;
      if (params_a.len != params_b.len) return false;
      for(int i=0; i<params_a.len; i++) {
        TypeAnn param_a = parser_get_type(p, params_a.data[i]);
        TypeAnn param_b = parser_get_type(p, params_b.data[i]);
        if (!type_eq(p, param_a, param_b))
          return false;
      }

      return func_a.ret_id == func_b.ret_id;
    } break;

    case TypeAnnKindArray: {
      TypeAnnArray arr_a = a.arr;
      TypeAnnArray arr_b = b.arr;

      TypeAnn inner_a = parser_get_type(p, arr_a.inner_id);
      TypeAnn inner_b = parser_get_type(p, arr_b.inner_id);
      return arr_a.len == arr_b.len && type_eq(p, inner_a, inner_b);
    } break;
  }

  return false;
}

TypeId parser_get_type_id(Parser* p, TypeAnn t) {
  VEC_FOR(p->types) {
    TypeAnn it = p->types.data[i];
    if (type_eq(p, it, t)) return i;
  }

  return -1;
}

TypeId parser_push_type(Parser* p, TypeAnn t) {
  VEC_FOR(p->types) {
    TypeAnn it = p->types.data[i];
    if (type_eq(p, it, t)) return i;
  }
  
  VEC_PUSH(p->types, t);
  return p->types.len-1;
}

StmtId parser_push_stmt(Parser* p, Stmt s) {
  VEC_PUSH(p->stmts, s);
  return p->stmts.len-1;
}

bool parser_is_at_end(Parser* p) {
  return p->curr_token >= p->tokens.len;
}

void parse_log_err(Parser* p, const char* err) {
  p->had_errors = true;
  if (p->is_panicking) return;

  p->err = err;
  
  int token_id = p->curr_token-1;
  Token tok = p->tokens.data[token_id];
  fprintf(stderr, "[PARSE ERR] %s at token %d: ", err, token_id);

  if (tok.kind < TokErr) {
    printf("kind = '%c' column = %d line = %d\n", tok.kind, tok.column, tok.line);
  } else {
    printf("kind = %s offset = %d line = %d len = %d '%.*s'\n", TOKEN_DBG[tok.kind - TokErr - 1], tok.column, tok.line, tok.len, tok.len, p->src + tok.offset);
  }

  p->is_panicking = true;
}

Token* parser_peek(Parser* p) {
  return &p->tokens.data[p->curr_token];
}

Token TOK_ERR = { TokErr, 1, -1, -1, -1 };
Token* parser_peek_nth(Parser* p, int n) {
  int idx = n + p->curr_token;
  return idx < p->tokens.len ? &p->tokens.data[idx] : &TOK_ERR;
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

void parser_eat_until_safe(Parser* p) {
  while (!parser_is_at_end(p)) {
    if (tok_is_safe(*parser_peek(p))) break;
    parser_eat(p);
  }

  p->is_panicking = false;
}

void parser_clear(Parser *p) {
  p->tokens.len = 0;
  p->curr_token = 0;

  // tokens will be overwritten next call to parse, so free them here
  VEC_FREE(p->tokens);

  VEC_FOR(p->types) {
    TypeAnn* t = &p->types.data[i];
    if (t->kind == TypeAnnKindFunc) VEC_FREE(t->func.params_ids);
    if (t->kind == TypeAnnKindStruct) VEC_FREE(t->strct.fields);
  }
  p->types.len = 0;

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
    if (s->kind == StmtKindBlock)  VEC_FREE(s->block.stmt_ids);
    if (s->kind == StmtKindFnDecl) VEC_FREE(s->func_decl.params_names);
  }
  p->stmts.len = 0;
  p->top_lvl_stmts.len = 0;

  p->err = NULL;
}

void parser_free(Parser *p) {
  parser_clear(p);
  VEC_FREE(p->tokens);
  VEC_FREE(p->types);  
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
    case TokDot: return 34;
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
  LiteralKind kind;
  switch (t->kind) {
    case TokTrue:     kind = LiteralKindTrue; break;
    case TokFalse:    kind = LiteralKindFalse; break;
    case TokIntLit:   kind = LiteralKindInt; break;
    case TokFloatLit: kind = LiteralKindFloat; break;
    default: return (Expr) {0};
  }

  ExprLiteral lit = {kind, .tok = t};
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

Expr unary(Token* op, ExprId rhs) {
  ExprUnary un = {op, rhs};
  return (Expr) { ExprKindUnary, .un = un };
}

Expr binary(ExprId lhs, Token* op, ExprId rhs) {
  ExprBinary bin = {lhs, op, rhs};
  return (Expr) { ExprKindBinary, .bin = bin };
}

Expr call(ExprId lhs, IntVec args) {
  ExprCall call = {lhs, args};
  return (Expr) { ExprKindCall, .call = call };
}

Expr indexing(ExprId lhs, ExprId idx) {
  ExprIndex indexing =  {lhs, idx};
  return (Expr) { ExprKindIndex, .idx = indexing };
}

Expr member(ExprId lhs, Token* field) {
  ExprMember member =  {lhs, field};
  return (Expr) { ExprKindMember, .memb = member };
}

ExprId parse_expr(Parser* parser, int prec_lvl);
// TODO: where to check for empty list? here or caller?
IntVec collect_expr_list(Parser* p, TokenKind separator, TokenKind terminator, const char* err) {
  // careful: memory leak if return early  
  IntVec expr_ids = {0};
  
  while (!parser_is_at_end(p)) {
    ExprId val = parse_expr(p, 0);
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
ExprId parse_expr(Parser* p, int prec_lvl) {
  Token* t = parser_eat(p);

  // parse lhs
  ExprId lhs = -1;
  switch (t->kind) {
    case TokIntLit:
    case TokFloatLit:
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
      IntVec exprs = {0};
    
      if (parser_eat_if(p, TokBraceRight) != NULL) {
        // empty array (uninit)
        return parser_push_expr(p, literal_array(exprs));
      }

      exprs = collect_expr_list(p, TokComma, TokBraceRight, "expected ',' or brace closing ']' for array literal");
      if (exprs.len == 0) return -1;

      // build type
      TypeAnn type = new_arr(TypeAnnKindUnknown, exprs.len);
      parser_push_type(p, type);
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
      // unary postfix op
      if (postfix < prec_lvl) break;
      parser_eat(p);

      if (op->kind == TokBraceLeft) {
        // array indexing
        ExprId rhs = parse_expr(p, 0);
        if (parser_eat_match(p, TokBraceRight, "unclosed brace ']' for array indexing expression") == NULL) return -1;
        lhs = parser_push_expr(p, indexing(lhs, rhs));
      } else if (op->kind == TokDot) {
        // member access
        // TODO 
      } else if (op->kind == TokParenLeft) {
        // function call

        if (parser_eat_if(p, TokParenRight) != NULL) {
          // no args
          lhs = parser_push_expr(p, call(lhs, (IntVec) {0}));
        } else {
          // collect args
          IntVec args = collect_expr_list(p, TokComma, TokParenRight, "expect ',' or parenthesis closing ')' for function call arguments");
          if (args.len == 0) {
            parse_log_err(p, "fucked up args parsing\n");
            return -1;
          }
          lhs = parser_push_expr(p, call(lhs, args));
        }
      } else {
        // any other postfix op
        lhs = parser_push_expr(p, unary(op, lhs));
      }

      continue;
    }

    // binary infix op
    PrecLvl infix = infix_lvl(*op);
    if (infix.left < prec_lvl) break;
    parser_eat(p);

    ExprId rhs = parse_expr(p, infix.right);
    lhs = parser_push_expr(p, binary(lhs, op, rhs));
  }

  return lhs;
}

StmtId parse_type(Parser* p) {
  // primitive
  // function
  // struct
  // array

  Token* tok = parser_eat(p);

  TypeAnn t;
  switch (tok->kind) {
    case TokInt:   t.kind  = TypeAnnKindInt; break;
    case TokFloat: t.kind  = TypeAnnKindFloat; break;
    case TokBool:  t.kind  = TypeAnnKindBool; break;

    case TokIdent: {
      // this might be any kind of user defined type
      // TODO: should return unknown type?
      parse_log_err(p, "user defined types not yet supported");
      return -1;
    } break;

    case TokBraceLeft: {
      // array

      // parse inner type
      TypeId inner = parse_type(p);
      if (parser_eat_match(p, TokSemicolon, "expect semicolon ';' in array type annotation") == NULL) return -1;
      Token* lit = parser_eat_match(p, TokIntLit, "expect constant integer size in array type annotation");
      if (lit == NULL) return -1;
      if (parser_eat_match(p, TokBraceRight, "expect closing brace ']' in array type annotation") == NULL) return -1;

      t = new_arr(inner, tok_parse_int(*lit, p->src));
    } break;

    case TokParenLeft: {
      // function

      // careful: memory leak if return early  
      IntVec params = {0};
      
      while (!parser_is_at_end(p)) {
        TypeId param_id = parse_type(p);
        VEC_PUSH(params, param_id);

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

      // TODO: function annotation might always require an arrow (and void if no return type)
      TypeId ret_id = TypeAnnKindVoid;
      if (parser_eat_if(p, TokArrow) != NULL) {
        ret_id = parse_type(p);
      }

      t = new_func(params, ret_id);
    } break;

    default: 
      parse_log_err(p, "invalid type annotation");
      return -1;
  }

  return parser_push_type(p, t);
}

StmtId parse_decl(Parser* p, ExprId lhs) {
  Token* name = parser_get_expr(p, lhs).ident;

  // eat ':='
  parser_eat(p);
  ExprId rhs = parse_expr(p, 0);

  StmtDecl decl = {name, TypeAnnKindUnknown, rhs};
  Stmt stmt = (Stmt) { StmtKindDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
}

StmtId parse_decl_annot(Parser* p, ExprId lhs) {
  Token* name = parser_get_expr(p, lhs).ident;

  // eat ':'
  parser_eat(p);
  TypeId type = parse_type(p);

  if (parser_eat_match(p, TokAssign, "expect '=' after variable name and type annotation") == NULL) return -1;
  ExprId rhs = parse_expr(p, 0);

  StmtDecl decl = (StmtDecl) {name, type, rhs};
  Stmt stmt = (Stmt) { StmtKindDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
}

StmtId parse_assign(Parser* p, ExprId lhs) {
  // eat '='
  parser_eat(p);
  ExprId rhs = parse_expr(p, 0);

  StmtAssign assign = (StmtAssign) {lhs, rhs};
  Stmt stmt = (Stmt) { StmtKindAssign, .assign = assign };
  return parser_push_stmt(p, stmt);
}

StmtId parse_stmt(Parser* p);
StmtId parse_block(Parser* p) {
  // eat '{'
  if (parser_eat_match(p, TokCurlyLeft, "expected opening block curly brace '{'") == NULL) return -1;

  // careful: memory leak if return early
  IntVec stmt_ids = {0};
  while (!parser_is_at_end(p) && parser_peek(p)->kind != TokCurlyRight) {
    StmtId s = parse_stmt(p);
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

StmtId parse_if_else(Parser* p) {
  // eat 'if'
  parser_eat(p);
  ExprId expr_id = parse_expr(p, 0);
  StmtId if_id = parse_block(p);

  StmtId else_id = -1;
  if (parser_peek(p)->kind == TokElse) {
    // eat 'else'
    parser_eat(p);
    else_id = parse_block(p);
  }

  StmtIfElse stmt = { .cond_id = expr_id, .if_id = if_id, .else_id = else_id };
  return parser_push_stmt(p, (Stmt) {StmtKindIfElse, .if_else = stmt});
}

StmtId parse_while(Parser* p) {
  // eat 'while'
  parser_eat(p);
  ExprId expr_id = parse_expr(p, 0);
  StmtId while_id = parse_block(p);

  StmtWhile stmt = { .cond_id = expr_id, .block_id = while_id };
  return parser_push_stmt(p, (Stmt) {StmtKindWhile, .wloop = stmt});
}

StmtId parse_return(Parser* p) {
  // eat 'return'
  parser_eat(p);

  Stmt stmt;
  if (tok_is_expr(*parser_peek(p))) {
    ExprId expr = parse_expr(p, 0);
    stmt = (Stmt) { StmtKindReturn, .expr_id = expr };
  } else {
    // no expr
    stmt = (Stmt) { StmtKindReturn, .expr_id = -1 };  
  }

  return parser_push_stmt(p, stmt);
}

StmtId parse_func_decl(Parser* p) {
  // eat 'fn'
  parser_eat(p);
  Token* func_name = parser_eat_match(p, TokIdent, "expect function name after 'fn' keyword");
  if (func_name == NULL) return -1;

  if (parser_eat_match(p, TokParenLeft, "expect open parenthesis '(' after fn keyword") == NULL) return -1;
  
  IntVec params = {0};
  TokenRefVec params_names = {0};

  while(!parser_is_at_end(p)) {
    // no params
    if (parser_peek(p)->kind == TokParenRight) {
      parser_eat(p);
      break;
    }

    Token* name = parser_eat_match(p, TokIdent, "expect function parameter name");
    if (name == NULL) goto error;
    if (parser_eat_match(p, TokColon, "expect ':' after function parameter name") == NULL) goto error;

    TypeId type_id = parse_type(p);
    if (type_id == -1) goto error;

    VEC_PUSH(params_names, name);
    VEC_PUSH(params, type_id);

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

  TypeId ret_id = TypeAnnKindVoid;
  Token* arrow = parser_eat_if(p, TokArrow);
  if (arrow != NULL) {
    ret_id = parse_type(p);
    if (ret_id == -1) goto error;
  }

  StmtId block_id = parse_block(p);

  // build type
  TypeId type_id = parser_push_type(p, new_func(params, ret_id));

  StmtFnDecl func = { func_name, params_names, type_id, block_id };
  return parser_push_stmt(p, (Stmt) { StmtKindFnDecl, .func_decl = func });

  error:
    VEC_FREE(params);
    return -1;
}

StmtId parse_struct_decl(Parser* p) {
  // eat 'struct'
  parser_eat(p);

  Token* struct_name = parser_eat_match(p, TokIdent, "expect struct name after struct keyword");
  if (struct_name == NULL) return -1;

  if (parser_eat_match(p, TokCurlyLeft, "expect curly open '{' after struct keyword") == NULL) return -1;
  
  // careful: causes leak if return early
  StructFieldVec fields = {0}; 
  while(!parser_is_at_end(p)) {
    // empty structs are not allowed 

    StructField field;
    Token* name = parser_eat_match(p, TokIdent, "expect struct field name in struct declaration");
    if (name == NULL) goto error;
    if (parser_eat_match(p, TokColon, "expect colon ':' after struct field name") == NULL) goto error;
    TypeId type = parse_type(p);

    Token* t = parser_eat(p);
    if (t->kind == TokCurlyRight) break;
    else if (t->kind == TokComma) {
      // we can have a comma at the end without any expression after
      if (parser_peek(p)->kind == TokCurlyRight) { parser_eat(p); break; }
    } else {
      parse_log_err(p, "expect ',' or closed curly '}' for struct declaration");
      goto error;
    }

    field.name = name;
    field.type_id = type;
    VEC_PUSH(fields, field);
  }

  // build type
  TypeId type_id = parser_push_type(p, new_struct(fields));

  StmtStructDecl strct = {struct_name, type_id};
  Stmt stmt = { StmtKindStructDecl, .strct = strct };
  return parser_push_stmt(p, stmt);

  error:
    VEC_FREE(fields);
    return -1;
}

StmtId parse_stmt(Parser* p) {
  // we are safe
  if (p->is_panicking) parser_eat_until_safe(p);
  if (parser_is_at_end(p)) return -1;

  int stmt = -1;
  switch(parser_peek(p)->kind) {
    // case TokVar: return parse_decl(p);
    case TokCurlyLeft: stmt = parse_block(p); break;
    case TokIf: stmt = parse_if_else(p); break;
    case TokWhile: stmt = parse_while(p); break;
    case TokFn: stmt = parse_func_decl(p); break;
    case TokStruct: stmt = parse_struct_decl(p); break;
    case TokReturn: stmt = parse_return(p); break;
    case TokIdent: {
      ExprId lhs = parse_expr(p, 0);

      Token* op = parser_peek(p);
      if (op->kind == TokAssign) {
        stmt = parse_assign(p, lhs);
      } else if (op->kind == TokDecl) {
        // TODO: expression statements can't start with an identifier anymore
        stmt = parse_decl(p, lhs);
      } else if (op->kind == TokColon) {
        stmt = parse_decl_annot(p, lhs);
      } else {
        // expression
        if (lhs == -1) {
          parse_log_err(p, "something wrong while parsing expression");
          return -1;
        }
        stmt = parser_push_stmt(p, (Stmt) {StmtKindExpr, .expr_id = lhs});
      }
    } break;

    default: {
      // expression
      int id = parse_expr(p, 0);

      if (id == -1) {
        parse_log_err(p, "something wrong while parsing expression");
        return -1;
      }
      stmt = parser_push_stmt(p, (Stmt) {StmtKindExpr, .expr_id = id});
    } break;
  }

  // if (parser_eat_match(p, TokSemicolon, "expect ';' at end of statement") == NULL) return -1;

  // ignore ';'
  if (parser_eat_if(p, TokSemicolon) != NULL) p->is_panicking = false;
  return stmt;
}

Parser parser_init(char* src) {
  Parser p = {0};
  p.src = src;

  TypeAnn t;

  #define X(Name) t.kind = TypeAnnKind##Name; VEC_PUSH(p.types, t);
  PRIMITIVES_LIST
  #undef X

  return p;
}

bool parse(Parser* p) {
  p->tokens = tokenize(p->src);
  while (!parser_is_at_end(p)) VEC_PUSH(p->top_lvl_stmts, parse_stmt(p));
  return !p->had_errors;
}