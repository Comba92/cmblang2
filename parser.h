#pragma once

#include "common.h"

// TODO: consider storing indexes to Tokens instead of pointers

typedef enum {
  ExprTypeLiteral,
  ExprTypeVariable,
  ExprTypeUnary,
  ExprTypeBinary,
} ExprType;

typedef enum {
  LiteralTypeFloat = TokFloat,
  LiteralTypeTrue = TokTrue,
  LiteralTypeFalse = TokFalse,
} LiteralType;

typedef struct {
  LiteralType type;
  Token* tok;
} ExprLiteral;

typedef struct {
  TokenType op;
  int expr;
} ExprUnary;

typedef struct {
  int lhs_idx;
  TokenType op;
  int rhs_idx;
} ExprBinary;

typedef struct {
  ExprType type;
  union {
    ExprLiteral lit;
    Token* ident;
    ExprUnary un;
    ExprBinary bin;
  };
} Expr;

typedef enum {
  StmtTypeExpr,
  StmtTypeDecl,
  StmtTypeAssign,
  StmtTypeBlock,
  StmtTypeIfElse,
  StmtTypeWhile,
} StmtType;

// forward declare Stmt and StmtVec, so we can build self referencing stmts such as StmtBlock
typedef struct Stmt Stmt;
VEC_DEF(Stmt);

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
  StmtType type;
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

Expr literal(Token* t) {
  ExprLiteral lit = (ExprLiteral) {(LiteralType) t->type, t};
  return (Expr) { ExprTypeLiteral, .lit = lit };
}

Expr variable(Token* t) {
  return (Expr) { ExprTypeVariable, .ident = t };
}

Expr unary(Token* op, int rhs) {
  ExprUnary un = (ExprUnary) {op->type, rhs};
  return (Expr) { ExprTypeUnary, .un = un };
}

Expr binary(int lhs, Token* op, int rhs) {
  ExprBinary bin = (ExprBinary) {lhs, op->type, rhs};
  return (Expr) { ExprTypeBinary, .bin = bin };
}

Stmt assign(Token* t, int rhs) {
  StmtAssign assign = (StmtAssign) {t, rhs};
  return (Stmt) { StmtTypeAssign, .assign = assign };
}

Stmt decl(Token* t, int rhs) {
  StmtAssign decl = (StmtAssign) {t, rhs};
  return (Stmt) { StmtTypeDecl, .decl = decl };
}

void expr_dbg(ExprVec exprs, int idx) {
  Expr* e = &exprs.data[idx];
  printf("[EXPR %d] type = %d ", idx, e->type);

  switch(e->type) {
    case ExprTypeLiteral: printf("literal\n"); break;
    case ExprTypeVariable: printf("variable\n"); break;
    case ExprTypeUnary:
      printf("op = %c, rhs = %d\n", e->un.op, e->un.expr);
      break;
    case ExprTypeBinary: 
      printf("lhs = %d, op = %c, rhs = %d\n", e->bin.lhs_idx, e->bin.op, e->bin.rhs_idx);
      break;
    default: break; 
  }
}

void stmt_dbg(StmtVec stmts, int idx) {
  Stmt* s = &stmts.data[idx];
  printf("[STMT %d] type = %d\n", idx, s->type);
}

typedef enum {
  ErrNone = 0,
  NoTokens,
  BadToken,
  UnclosedParen,
  UnclosedBlock,
  ExpectOperator,
  ExpectBlock,
  BadExpr,
  ExpectAssign,
  ExpectIdentifier,
} ParseErr;

typedef struct  {
  char* src;

  TokenVec tokens;
  int curr_token;
  
  ExprVec exprs;
  StmtVec stmts;
  IntVec top_lvl_stmts;
  
  ParseErr err;
} Parser;

void parse_log_err(Parser* p, ParseErr err) {
  p->err = err;
  
  fprintf(stderr, "[PARSE ERR] ");
  switch (p->err) {
    case BadToken:
      break;
    
    case UnclosedParen:
      fprintf(stderr, "unclosed parenthesis");  
      break;

    case UnclosedBlock:
      fprintf(stderr, "unclosed block parenthesis");  
      break;

    case ExpectOperator:
      fprintf(stderr, "two consecutive expressions; expected operator");
      break;
      
    case BadExpr:
      fprintf(stderr, "invalid expression");
      break;

    case ExpectAssign:
      fprintf(stderr, "expected '=' token");
      break;

    case ExpectIdentifier:
      fprintf(stderr, "expected identifier token after 'var' keyword");
      break;
      
    default: fprintf(stderr, "unhandled error\n"); return;
  }
  
  int token_id = p->curr_token-1;
  Token* t = &p->tokens.data[token_id];
  int column = t->offset;
  fprintf(stderr, " at token %d (type = %c), column %d\n", token_id, t->type, column);
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
  // idx = idx < p->tokens.len ? idx : p->tokens.len-1;
  // return &p->tokens.data[idx];

  return idx < p->tokens.len ? &p->tokens.data[idx] : &TOKEN_ERR;
}

Token* parser_eat(Parser* p) {
  return &p->tokens.data[p->curr_token++];
}

Token* parser_eat_match(Parser* p, TokenType type, ParseErr err) {
  Token* t = parser_eat(p);
  if (t->type != type) {
    parse_log_err(p, err);
    return NULL;
  }

  return t;
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
  p->exprs.len = 0;

  // free blocks IntVecs
  VEC_FOR(p->stmts) {
    Stmt* s = &p->stmts.data[i];
    if (s->type == StmtTypeBlock) {
      free(s->block.stmt_ids.data);
    }
  }
  p->stmts.len = 0;
  p->top_lvl_stmts.len = 0;

  p->err = ErrNone;
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

PrecLvl prefix_lvl(Token* op) {
  switch (op->type) {
    case TokSub: return PREC(0, 26);
    case TokNot: return PREC(0, 25);
    default: return PREC(-1,-1);
  }
}

PrecLvl infix_lvl(Token* op) {
  switch (op->type) {
    case TokAdd:
    case TokSub:
      return PREC(12,13);
    case TokEq:
    case TokNotEq:
      return PREC(8,9);
    case TokGreat:
    case TokGreatEq:
    case TokLess:
    case TokLessEq:
      return PREC(10, 11);
    case TokMul:
    case TokDiv:
      return PREC(14,15);
    case TokAnd:
      return PREC(18,19);
    case TokOr:
      return PREC(16,17);
    case TokExp:
      return PREC(21,22);
    case TokRem: 
      return PREC(23,24);

    default: return PREC(-1,-1);
  }
}

PrecLvl postfix_lvl(Token* op) {
  switch (op->type) {
    default: return PREC(-1,-1);
  }
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
int parse_expr(Parser* p, int prec_lvl) {
  Token* t = parser_eat(p);

  // parse lhs
  int lhs;
  switch (t->type) {
    case TokFloat:
    case TokTrue:
    case TokFalse:
      lhs = parser_push_expr(p, literal(t));
      break;

    case TokIdent:
      lhs = parser_push_expr(p, variable(t));
      break;

    case TokSub:
    case TokNot:
      PrecLvl lvl = prefix_lvl(t);
      int rhs = parse_expr(p, lvl.right);
      lhs = parser_push_expr(p, unary(t, rhs));
      break;

    case TokParenLeft:
      lhs = parse_expr(p, 0);
      if (parser_eat(p)->type != TokParenRight) {
        parse_log_err(p, UnclosedParen);
        return -1;
      }
      break;

    default:
      parse_log_err(p, BadExpr);
      return -1;
  }

  while (1) {
    if (parser_is_at_end(p)) break;

    Token* op = parser_peek(p);
    if (!tok_is_op(op)) break;

    // postfix operator goes there

    PrecLvl lvl = infix_lvl(op);
    if (lvl.left < prec_lvl) break;
    parser_eat(p);

    int rhs = parse_expr(p, lvl.right);
    lhs = parser_push_expr(p, binary(lhs, op, rhs));
  }

  return lhs;
}

int parse_assign(Parser* p, bool declaration) {
  Token* name = parser_eat_match(p, TokIdent, ExpectIdentifier);
  if (name == NULL) return -1;

  if (parser_eat_match(p, TokAssign, ExpectAssign) == NULL) return -1;

  int rhs = parse_expr(p, 0);

  Stmt stmt;
  if (declaration) stmt = decl(name, rhs);
  else stmt = assign(name, rhs);

  return parser_push_stmt(p, stmt);
}

int parse_decl(Parser* p) {
  // eat var keyword
  parser_eat(p);
  return parse_assign(p, true);
}

int parse_stmt(Parser* p);
int parse_block(Parser* p) {
  // eat '{'
  if (parser_eat_match(p, TokCurlyLeft, ExpectBlock) == NULL) return -1;

  IntVec stmt_ids = {0};
  while (!parser_is_at_end(p) && parser_peek(p)->type != TokCurlyRight) {
    int s = parse_stmt(p);
    VEC_PUSH(stmt_ids, s);
  }
  
  if (parser_is_at_end(p)) {
    parse_log_err(p, UnclosedBlock);
    return -1;
  }

  // we are sure to have reached a '}', we would have reached end of tokens otherwise
  // eat '}'
  parser_eat(p);
  return parser_push_stmt(p, (Stmt) {StmtTypeBlock, .block = {stmt_ids}});
}

int parse_if_else(Parser* p) {
  // eat 'if'
  parser_eat(p);
  int expr_id = parse_expr(p, 0);
  int if_id = parse_block(p);

  int else_id = -1;
  if (parser_peek(p)->type == TokElse) {
    // eat 'else'
    parser_eat(p);
    else_id = parse_block(p);
  }

  StmtIfElse stmt = { .cond_idx = expr_id, .if_idx = if_id, .else_idx = else_id };
  return parser_push_stmt(p, (Stmt) {StmtTypeIfElse, .if_else = stmt});
}

int parse_while(Parser* p) {
  // eat 'while'
  parser_eat(p);
  int expr_id = parse_expr(p, 0);
  int while_id = parse_block(p);

  StmtWhile stmt = { .cond_idx = expr_id, .block_idx = while_id };
  return parser_push_stmt(p, (Stmt) {StmtTypeWhile, .wloop = stmt});
}

int parse_stmt(Parser* p) {
  switch(parser_peek(p)->type) {
    case TokVar: return parse_decl(p);
    case TokCurlyLeft: return parse_block(p);
    case TokIf: return parse_if_else(p);
    case TokWhile: return parse_while(p);

    default: {
      if (parser_peek(p)->type == TokIdent && parser_peek_nth(p, 1)->type == TokAssign)
        return parse_assign(p, false);
      else {
        // expression
        int id = parse_expr(p, 0);
        return parser_push_stmt(p, (Stmt) {StmtTypeExpr, .expr_idx = id});
      }
    }
  }
}

void parse(Parser* p, char* str) {
  parser_clear(p);

  TokenVec tokens = tokenize(str);
  p->src = str;
  p->tokens = tokens;
  
  if (tokens.len == 0) {
    p->err = NoTokens;
    return;
  }

  while (!parser_is_at_end(p)) VEC_PUSH(p->top_lvl_stmts, parse_stmt(p));
}