#pragma once
#include "common.h"

// TODO: if Token type stays small, consider passing always by copy instead of pointer

typedef enum {
  TokErr = 0,
  TokParenLeft = '(',
  TokParenRight = ')',
  TokBraceLeft = '[',
  TokBraceRight = ']',
  TokCurlyLeft = '{',
  TokCurlyRight = '}',
  TokNumber = 'n',
  TokIdent = 'i',
  TokAdd = '+',
  TokMul = '*',
  TokSub = '-',
  TokDiv = '/',
  TokRem = '%',
  TokExp = '^',
  TokVar = 'v',
  TokAssign = '=',
} TokenType;


typedef struct {
  TokenType type;
  int offset;
  int len;
} Token;

VEC_DEF(Token);

static Token TOKEN_ERR = { TokErr, -1, -1 };

Token token_sym(char c, int offset) {
  return (Token) { (TokenType) c,  offset, 1 };
}

int token_is_not_op(Token* t) {
  return t->type == TokVar || t->type == TokAssign || t->type == TokNumber || t->type == TokIdent;
}

void token_dbg(TokenVec tokens, int idx) {
  Token* t = &tokens.data[idx];
  printf("[TOKEN %d] kind = %c, col = %d, len = %d\n", idx, t->type, t->offset, t->len);
}

TokenVec tokenize(char* str) {
  TokenVec tokens = {0};
  int column = 0;
  int line = 0;

  while (1) {
    // skip whitespace
    while (isspace(*str)) {
      if (*str == '\n') {
        column = 0;
        line++;
      } else {
        column++;
      }

      str++;
    }

    char c = *str;
    Token t;

    switch(c) {
      case '(':
      case ')':
      case '{':
      case '}':
      case '+':
      case '*':
      case '-':
      case '/':
      case '%':
      case '^':
      case '=':
        t = token_sym(c, column);
        break;

      case '\0':
        return tokens;

      default: {
        if (isdigit(c)) {
          int len = 1;
          while (str[len] != '\0' && isdigit(str[len])) len++;
          if (str[len] == '.') {
            len++;
            while (str[len] != '\0' && isdigit(str[len])) len++;
          }
          
          t = (Token) {TokNumber, column, len};
        } else if (isalpha(c)) {
          // keywords
          if (strncmp(str, "var", 3) == 0) {
            t = (Token) {TokVar, column, 3};
            break;
          }

          // not a keyword, must be an ident
          int len = 1;
          while (str[len] != '\0' && (isalnum(str[len]) || str[len] == '_')) len++;
          t = (Token) {TokIdent, column, len};
        } else {
          // handle error
          fprintf(stderr, "[LEX ERR] Invalid token (%c) at col = %d\n", c, column);
          return (TokenVec) {0};
        }
      } break;
    }

    VEC_PUSH(tokens, t);
    str += t.len;
    column += t.len;
  }

  return tokens;
}