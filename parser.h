#pragma once
#include "common.h"
#include "lexer.h"

// TODO: consider storing indexes to Tokens instead of pointers

typedef enum {
  ExprTypeLiteral,
  ExprTypeVariable,
  ExprTypeUnary,
  ExprTypeBinary,
} ExprType;

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
    // TODO: pointer to token here
    Token* tok;
    ExprUnary un;
    ExprBinary bin;
  };
} Expr;

typedef enum {
  StmtTypeExpr,
  StmtTypeDecl,
  StmtTypeAssign,
  StmtTypeBlock,
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

struct Stmt {
  StmtType type;
  union {
    StmtAssign decl;
    StmtAssign assign;
    StmtBlock block;
    int expr_idx;
  };
};

VEC_DEF(Expr);

Expr literal(Token* num) {
  return (Expr) { ExprTypeLiteral, .tok = num };
}

Expr variable(Token* t) {
  return (Expr) { ExprTypeVariable, .tok = t };
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
  BadLeftExpr,
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
      
    case BadLeftExpr:
      fprintf(stderr, "invalid lhs expression");
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
    case TokSub: return PREC(0, 15);
    default: return PREC(-1,-1);
  }
}

PrecLvl infix_lvl(Token* op) {
  switch (op->type) {
    case TokAdd:
    case TokSub:
      return PREC(9,9);
    case TokMul:
    case TokDiv:
      return PREC(10,10);
    case TokExp:
      return PREC(11,12);
    case TokRem: 
      return PREC(14,14);

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
    case TokNumber:
      lhs = parser_push_expr(p, literal(t));
      break;

    case TokIdent:
      lhs = parser_push_expr(p, variable(t));
      break;

    case TokSub:
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
      parse_log_err(p, BadLeftExpr);
      return -1;
  }

  while (1) {
    if (parser_is_at_end(p)) break;

    Token* op = parser_peek(p);
    if (token_is_not_op(op)) break;

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
  return parse_assign(p, 1);
}

int parse_stmt(Parser* p);
int parse_block(Parser* p) {
  // eat '{'
  parser_eat(p);

  IntVec stmt_ids = {0};
  while (!parser_is_at_end(p) && parser_peek(p)->type != TokCurlyRight) {
    int s = parse_stmt(p);
    VEC_PUSH(stmt_ids, s);
  }
  
  if (parser_is_at_end(p)) {
    parse_log_err(p, UnclosedBlock);
    return -1;
  }

  // parser_eat_match(p, TokCurlyRight, UnclosedBlock);

  // we are sure to have reached a '}', we would have reached end of tokens otherwise
  // eat '}'
  parser_eat(p);
  return parser_push_stmt(p, (Stmt) {StmtTypeBlock, .block = {stmt_ids}});
}

int parse_stmt(Parser* p) {
  // TODO: change to switch
  if (parser_peek(p)->type == TokVar) return parse_decl(p);
  else if (parser_peek(p)->type == TokIdent && parser_peek_nth(p, 1)->type == TokAssign) return parse_assign(p, false);
  else if (parser_peek(p)->type == TokCurlyLeft) return parse_block(p);
  else {
    // expression
    int id = parse_expr(p, 0);
    return parser_push_stmt(p, (Stmt) {StmtTypeExpr, .expr_idx = id});
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