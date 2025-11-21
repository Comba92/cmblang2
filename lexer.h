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
  TokAdd = '+',
  TokMul = '*',
  TokSub = '-',
  TokDiv = '/',
  TokRem = '%',
  TokExp = '^',
  TokAssign = '=',
  TokBang = '!',
  TokGreat = '>',
  TokLess = '<',

  TokAnd = '&',
  TokOr = '|',
  TokNot = '~',

  TokFloat = 'n',
  TokIdent = 'i',

  TokVar = 'v',
  TokTrue = 't',
  TokFalse = 'f',
  TokIf,
  TokElse,
} TokenType;

typedef struct {
  const char* name;
  size_t len; 
  TokenType type;
} KeywordData;

static const KeywordData KEYWORDS[] = {
  { "var",    sizeof("var"),    TokVar },
  { "true",   sizeof("true"),   TokTrue },
  { "false",  sizeof("false"),  TokFalse },
  { "and",    sizeof("and"),    TokAnd },
  { "or",     sizeof("or"),     TokOr },
  { "not",    sizeof("not"),    TokNot },
  { "if",     sizeof("if"),     TokIf },
  { "else",   sizeof("else"),   TokElse },
};
const size_t KEYWORDS_LEN = sizeof(KEYWORDS) / sizeof(KeywordData);

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

int tok_is_op(Token* t) {
  return t->type == TokParenLeft
    || t->type == TokParenRight
    || t->type == TokAdd
    || t->type == TokMul
    || t->type == TokSub
    || t->type == TokDiv
    || t->type == TokRem
    || t->type == TokExp
    || t->type == TokAnd
    || t->type == TokOr
    || t->type == TokNot;
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
      case '>':
      case '<':
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
          
          t = (Token) {TokFloat, column, len};
        } else if (isalpha(c)) {
          bool is_keyword = false;
          // keywords
          for (size_t i=0; i<KEYWORDS_LEN; i++) {
            KeywordData keyword = KEYWORDS[i];
            if (strncmp(str, keyword.name, keyword.len-1) == 0) {
              t = (Token) {keyword.type, column, keyword.len-1};
              is_keyword = true;
              break;
            }
          }
          
          if (is_keyword) break;

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