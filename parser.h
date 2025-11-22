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
  LiteralTypeArray = TokBraceLeft,
} LiteralType;

typedef struct {
  IntVec vals;
} LiteralArray;

typedef struct {
  LiteralType type;
  union {
    Token* tok;
    LiteralArray arr;
  };
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
  UnclosedBrace,
  ExpectOperator,
  ExpectCommaOrBraceClose,
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

// TODO: refactor this
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

    case ExpectCommaOrBraceClose:
      fprintf(stderr, "expected comma or brace closing in array literal");
      break;

    case UnclosedBrace:
      fprintf(stderr, "unclosed brace");
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
  return idx < p->tokens.len ? &p->tokens.data[idx] : &TOKEN_ERR;
}

Token* parser_eat(Parser* p) {
  return &p->tokens.data[p->curr_token++];
}

Token* parser_eat_match(Parser* p, TokenType match, ParseErr err) {
  Token* t = parser_eat(p);
  if (t->type != match) {
    parse_log_err(p, err);
    return NULL;
  }

  return t;
}

Token* parser_eat_if(Parser* p, TokenType match) {
  if (parser_peek(p)->type == match) return parser_eat(p);
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
    if (e->type == ExprTypeLiteral && e->lit.type == LiteralTypeArray) {
      free(e->lit.arr.vals.data);
    }
  }
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

int prefix_lvl(Token* op) {
  switch (op->type) {
    case TokSub: return 30;
    case TokNot: return 29;
    default: return -1;
  }
}

int postfix_lvl(Token* op) {
  switch (op->type) {
    case TokBraceLeft: return 32;
    default: return -1;
  }
}

// https://www.alphacodingskills.com/rust/notes/rust-operators-precedence.php
PrecLvl infix_lvl(Token* op) {
  switch (op->type) {
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
  ExprLiteral lit = {(LiteralType) t->type, .tok = t};
  return (Expr) { ExprTypeLiteral, .lit = lit };
}

Expr literal_array(IntVec vals) {
  LiteralArray arr = { vals };
  ExprLiteral lit = {LiteralTypeArray, .arr = arr};
  return (Expr) { ExprTypeLiteral, .lit = lit };
}

Expr variable(Token* t) {
  return (Expr) { ExprTypeVariable, .ident = t };
}

Expr unary(Token* op, int rhs) {
  ExprUnary un = {op->type, rhs};
  return (Expr) { ExprTypeUnary, .un = un };
}

Expr binary(int lhs, Token* op, int rhs) {
  ExprBinary bin = {lhs, op->type, rhs};
  return (Expr) { ExprTypeBinary, .bin = bin };
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
      if (parser_eat_match(p, TokParenRight, UnclosedParen) == NULL) return -1;
      break;

    case TokBraceLeft:
      IntVec vals = {0};

      // empty array
      if (parser_eat_if(p, TokBraceRight) != NULL)
        return parser_push_expr(p, literal_array(vals));
  
      while (1) {
        int val = parse_expr(p, 0);
        VEC_PUSH(vals, val);

        t = parser_eat(p);
        if (t->type == TokBraceRight) break;
        else if (t->type == TokComma) {
          if (parser_peek(p)->type == TokBraceRight) break;
        } else {
          parse_log_err(p, ExpectCommaOrBraceClose);
          return -1;
        }
      }
      return parser_push_expr(p, literal_array(vals));
      break;

    default:
      parse_log_err(p, BadExpr);
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

      if (op->type == TokBraceLeft) {
        int rhs = parse_expr(p, 0);
        if (parser_eat_match(p, TokBraceRight, UnclosedBrace) == NULL) return -1;
        
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
//     stmt = (Stmt) { StmtTypeDecl, .decl = decl };
//   } else {
//     StmtAssign assign = (StmtAssign) {name, rhs};
//     stmt = (Stmt) { StmtTypeAssign, .assign = assign };
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
  Stmt stmt = (Stmt) { StmtTypeAssign, .assign = assign };
  return parser_push_stmt(p, stmt);
}

int parse_decl(Parser* p) {
  Token* name = parser_eat(p);
  // eat ':='
  parser_eat(p);
  int rhs = parse_expr(p, 0);

  StmtAssign decl = (StmtAssign) {name, rhs};
  Stmt stmt = (Stmt) { StmtTypeDecl, .decl = decl };
  return parser_push_stmt(p, stmt);
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
  int stmt = -1;
  switch(parser_peek(p)->type) {
    // case TokVar: return parse_decl(p);
    case TokCurlyLeft: stmt = parse_block(p); break;
    case TokIf: stmt = parse_if_else(p); break;
    case TokWhile: stmt = parse_while(p); break;

    default: {
      bool is_ident = parser_peek(p)->type == TokIdent; 
      if (is_ident && parser_peek_nth(p, 1)->type == TokAssign)
        stmt = parse_assign(p);
      else if (is_ident && parser_peek_nth(p, 1)->type == TokDecl)
        stmt = parse_decl(p);
      else {
        // expression
        int id = parse_expr(p, 0);
        stmt = parser_push_stmt(p, (Stmt) {StmtTypeExpr, .expr_idx = id});
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