#pragma once

#include "common.h"

// TODO: consider storing indexes to Tokens instead of pointers

typedef enum {
  ExprKindLiteral,
  ExprKindVariable,
  ExprKindUnary,
  ExprKindBinary,
} ExprKind;

typedef enum {
  LiteralKindInt = TokInt,
  LiteralKindFloat = TokFloat,
  LiteralKindTrue = TokTrue,
  LiteralKindFalse = TokFalse,
  LiteralKindArray = TokBraceLeft,
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
  TokenKind op;
  int expr;
} ExprUnary;

typedef struct {
  int lhs_idx;
  TokenKind op;
  int rhs_idx;
} ExprBinary;

typedef struct {
  ExprKind kind;
  union {
    ExprLiteral lit;
    Token* ident;
    ExprUnary un;
    ExprBinary bin;
  };
} Expr;

typedef enum {
  StmtKindExpr,
  StmtKindDecl,
  StmtKindAssign,
  StmtKindBlock,
  StmtKindIfElse,
  StmtKindWhile,
} StmtKind;

// forward declare Stmt and StmtVec, so we can build self referencing stmts such as StmtBlock
typedef struct Stmt Stmt;
VEC_DEF(Stmt);

typedef struct {
  Token* name;
  // TODO: type here
  int rhs_idx;
} StmtDecl;

typedef struct {
  Token* tok;
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
    StmtAssign decl;
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
      printf("op = %c, rhs = %d\n", e->un.op, e->un.expr);
      break;
    case ExprKindBinary: 
      printf("lhs = %d, op = %c, rhs = %d\n", e->bin.lhs_idx, e->bin.op, e->bin.rhs_idx);
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
  
  ExprVec exprs;
  StmtVec stmts;
  IntVec top_lvl_stmts;
  
  const char* err;
} Parser;

// TODO: refactor this
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

int parser_push_stmt(Parser* p, Stmt e) {
  VEC_PUSH(p->stmts, e);
  return p->stmts.len-1;
}

bool parser_is_at_end(Parser* p) {
  return (size_t) p->curr_token >= p->tokens.len;
}

Token* parser_peek(Parser* p) {
  return &p->tokens.data[p->curr_token];
}

Token* parser_peek_nth(Parser* p, int n) {
  size_t idx = n + p->curr_token;
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

Expr* parser_get_expr(Parser* p, int idx) {
  return &p->exprs.data[idx];
}

Stmt* parser_get_stmt(Parser* p, int idx) {
  return &p->stmts.data[idx];
}

void parser_clear(Parser *p) {
  p->src = NULL;
  p->tokens.len = 0;
  p->curr_token = 0;

  // free array literals
  VEC_FOR(p->exprs) {
    Expr* e = &p->exprs.data[i];
    if (e->kind == ExprKindLiteral && e->lit.kind == LiteralKindArray) {
      VEC_FREE(e->lit.arr.expr_ids);
    }
  }
  p->exprs.len = 0;

  // free blocks IntVecs
  VEC_FOR(p->stmts) {
    Stmt* s = &p->stmts.data[i];
    if (s->kind == StmtKindBlock) {
      VEC_FREE(s->block.stmt_ids);
    }
  }
  p->stmts.len = 0;
  p->top_lvl_stmts.len = 0;

  p->err = NULL;
}

void parser_free(Parser *p) {
  parser_clear(p);
  VEC_FREE(p->tokens);
  VEC_FREE(p->exprs);
  VEC_FREE(p->stmts);
  VEC_FREE(p->top_lvl_stmts);
}

typedef struct {
  int left;
  int right;
} PrecLvl;
#define PREC(_a, _b) (PrecLvl) {(_a), (_b)}

int prefix_lvl(Token* op) {
  switch (op->kind) {
    case TokSub: return 30;
    case TokNot: return 29;
    default: return -1;
  }
}

int postfix_lvl(Token* op) {
  switch (op->kind) {
    case TokBraceLeft: return 32;
    default: return -1;
  }
}

// https://www.alphacodingskills.com/rust/notes/rust-operators-precedence.php
PrecLvl infix_lvl(Token* op) {
  switch (op->kind) {
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
  ExprUnary un = {op->kind, rhs};
  return (Expr) { ExprKindUnary, .un = un };
}

Expr binary(int lhs, Token* op, int rhs) {
  ExprBinary bin = {lhs, op->kind, rhs};
  return (Expr) { ExprKindBinary, .bin = bin };
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
int parse_expr(Parser* p, int prec_lvl) {
  Token* t = parser_eat(p);

  // parse lhs
  int lhs;
  switch (t->kind) {
    case TokInt:
    case TokFloat:
    case TokTrue:
    case TokFalse:
      lhs = parser_push_expr(p, literal_primitive(t));
      break;

    case TokIdent:
      lhs = parser_push_expr(p, variable(t));
      break;

    case TokSub:
    case TokNot:
      int lvl = prefix_lvl(t);
      int rhs = parse_expr(p, lvl);
      lhs = parser_push_expr(p, unary(t, rhs));
      break;

    case TokParenLeft:
      lhs = parse_expr(p, 0);
      if (parser_eat_match(p, TokParenRight, "unclosed parenthesis for expression") == NULL) return -1;
      break;

    case TokBraceLeft:
      IntVec expr_ids = {0};

      // empty array
      if (parser_eat_if(p, TokBraceRight) != NULL){
        parse_log_err(p, "empty arrays are illegal");
        return -1;
      }
  
      while (1) {
        int val = parse_expr(p, 0);
        VEC_PUSH(expr_ids, val);

        t = parser_eat(p);
        if (t->kind == TokBraceRight) break;
        else if (t->kind == TokComma) {
          // we can have a comma at the end without any expression after
          if (parser_peek(p)->kind == TokBraceRight) break;
        } else {
          parse_log_err(p, "expected comma or brace closing for array literal");
          return -1;
        }
      }
      return parser_push_expr(p, literal_array(expr_ids));
      break;

    default:
      parse_log_err(p, "invalid expression");
      return -1;
  }

  while (1) {
    if (parser_is_at_end(p)) break;

    Token* op = parser_peek(p);
    if (!tok_is_op(op)) break;

    int postfix = postfix_lvl(op);
    if (postfix != -1) {
      if (postfix < prec_lvl) break;
      parser_eat(p);

      if (op->kind == TokBraceLeft) {
        int rhs = parse_expr(p, 0);
        if (parser_eat_match(p, TokBraceRight, "unclosed brace for array literal") == NULL) return -1;
        
        lhs = parser_push_expr(p, binary(lhs, op, rhs));
      } else {
        lhs = parser_push_expr(p, unary(op, lhs));
      }

      continue;
    }

    PrecLvl infix = infix_lvl(op);
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

int parse_assign(Parser* p) {
  Token* name = parser_eat(p);
  // eat '='
  parser_eat(p);
  int rhs = parse_expr(p, 0);

  StmtAssign assign = (StmtAssign) {name, rhs};
  Stmt stmt = (Stmt) { StmtKindAssign, .assign = assign };
  return parser_push_stmt(p, stmt);
}

int parse_decl(Parser* p) {
  Token* name = parser_eat(p);
  // eat ':='
  parser_eat(p);
  int rhs = parse_expr(p, 0);

  StmtAssign decl = (StmtAssign) {name, rhs};
  Stmt stmt = (Stmt) { StmtKindDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
}

int parse_stmt(Parser* p);
int parse_block(Parser* p) {
  // eat '{'
  if (parser_eat_match(p, TokCurlyLeft, "expected opening block curly brace") == NULL) return -1;

  IntVec stmt_ids = {0};
  while (!parser_is_at_end(p) && parser_peek(p)->kind != TokCurlyRight) {
    int s = parse_stmt(p);
    VEC_PUSH(stmt_ids, s);
  }
  
  if (parser_is_at_end(p)) {
    parse_log_err(p, "unclosed block, expected curly brace");
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

int parse_stmt(Parser* p) {
  int stmt = -1;
  switch(parser_peek(p)->kind) {
    // case TokVar: return parse_decl(p);
    case TokCurlyLeft: stmt = parse_block(p); break;
    case TokIf: stmt = parse_if_else(p); break;
    case TokWhile: stmt = parse_while(p); break;

    default: {
      bool is_ident = parser_peek(p)->kind == TokIdent; 
      if (is_ident && parser_peek_nth(p, 1)->kind == TokAssign)
        stmt = parse_assign(p);
      else if (is_ident && parser_peek_nth(p, 1)->kind == TokDecl)
        stmt = parse_decl(p);
      else {
        // expression
        int id = parse_expr(p, 0);
        stmt = parser_push_stmt(p, (Stmt) {StmtKindExpr, .expr_idx = id});
      }
    } break;
  }

  // ignore ';'
  parser_eat_if(p, TokSemicolon);
  return stmt;
}

void parse(Parser* p, char* str) {
  parser_clear(p);

  TokenVec tokens = tokenize(str);
  p->src = str;
  p->tokens = tokens;

  while (!parser_is_at_end(p)) VEC_PUSH(p->top_lvl_stmts, parse_stmt(p));
}