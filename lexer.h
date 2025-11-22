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
  TokMark = '?',
  TokGreat = '>',
  TokLess = '<',
  TokColon = ':',
  TokSemicolon = ';',
  TokDot = '.',
  TokComma = ',',
  TokQuote = '\'',
  Tok2Quote = '"',
  TokDollar = '$',
  TokAsand = '&',
  TokHash = '#',
  
  TokAnd = 128,
  TokOr,
  TokNot,
  
  TokEq,
  TokNotEq,
  TokGreatEq,
  TokLessEq,
  TokArrow,
  Tok2Colon,
  TokDecl,
  Tok2Dot,
  Tok2Slash,
  
  TokFloat,
  TokIdent,

  // TokVar,
  TokTrue,
  TokFalse,
  TokIf,
  TokElse,
  TokWhile,
  TokFor,
  TokFn,
  TokReturn,
  TokStruct,
  TokAs,
} TokenType;

typedef struct {
  const char* name;
  size_t len; 
  TokenType type;
} KeywordData;

static const KeywordData KEYWORDS[] = {
  // { "var",    sizeof("var"),    TokVar },
  { "true",   sizeof("true"),   TokTrue },
  { "false",  sizeof("false"),  TokFalse },
  { "and",    sizeof("and"),    TokAnd },
  { "or",     sizeof("or"),     TokOr },
  { "not",    sizeof("not"),    TokNot },
  { "if",     sizeof("if"),     TokIf },
  { "else",   sizeof("else"),   TokElse },
  { "while",  sizeof("while"),  TokWhile },
  { "for",    sizeof("for"),    TokFor },
  { "fn",     sizeof("fn"),     TokFn },
  { "return", sizeof("return"), TokReturn },
  { "struct", sizeof("struct"), TokStruct },
  { "as",     sizeof("as"),     TokAs },
};
const size_t KEYWORDS_LEN = sizeof(KEYWORDS) / sizeof(KeywordData);

typedef struct {
  TokenType type;
  int offset;
  int len;
} Token;

VEC_DEF(Token);

static Token TOKEN_ERR = { TokErr, -1, -1 };

Token tok_sym(char c, int offset) {
  return (Token) { (TokenType) c,  offset, 1 };
}

Token tok_sym2(TokenType c, int offset) {
  return (Token) { c,  offset, 2 };
}

int tok_is_op(Token* t) {
  return t->type == TokParenLeft
    // || t->type == TokParenRight
    || t->type == TokBraceLeft
    // || t->type == TokBraceRight
    || t->type == TokAdd
    || t->type == TokMul
    || t->type == TokSub
    || t->type == TokDiv
    || t->type == TokRem
    || t->type == TokExp
    || t->type == TokAnd
    || t->type == TokOr
    || t->type == TokNot
    || t->type == TokEq
    || t->type == TokNotEq
    || t->type == TokGreat
    || t->type == TokGreatEq
    || t->type == TokLess
    || t->type == TokLessEq;
}

void token_dbg(TokenVec tokens, int idx) {
  Token* t = &tokens.data[idx];
  printf("[TOKEN %d] kind = %c, col = %d, len = %d\n", idx, t->type, t->offset, t->len);
}

Token lexer_match_or(char* s, char match, int column, TokenType tok, TokenType or) {
  if (*(s + 1) == match) {
    return tok_sym2(tok, column);
  } else {
    return tok_sym(or, column);
  }
}

TokenVec tokenize(char* str) {
  TokenVec tokens = {0};
  // TODO: consider removing this
  int consumed = 0;
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
      
      consumed++;
      str++;
    }

    char c = *str;
    Token t;

    switch(c) {
      case '(':
      case ')':
      case '[':
      case ']':
      case '{':
      case '}':
      case '+':
      case '*':
      case '%':
      case '^':
      case ',':
      case ';':
      case '\'':
      case '"':
      case '$':
      case '&':
      case '#':
        t = tok_sym(c, column);
        break;

      case '/': t = lexer_match_or(str, '/', column, Tok2Slash, TokDiv); break;
      case '.': t = lexer_match_or(str, '.', column, Tok2Dot, TokDot); break;
      case '-': t = lexer_match_or(str, '>', column, TokArrow, TokSub); break;
      case '=': t = lexer_match_or(str, '=', column, TokEq, TokAssign); break;
      case '<': t = lexer_match_or(str, '=', column, TokLessEq, TokLess); break;
      case '>': t = lexer_match_or(str, '=', column, TokGreatEq, TokGreat); break;
      case '!': t = lexer_match_or(str, '=', column, TokNotEq, TokBang); break;
      case ':': {
        char next = *(str + 1);
        if (next == ':') t = tok_sym2(Tok2Colon, column);
        else if (next == '=') t = tok_sym2(TokDecl, column);
        else t = tok_sym('.', column);
      }; break;

      case '\0':
        return tokens;

      default: {
        if (isdigit(c)) {
          // number literal
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
    consumed += t.len;
  }

  return tokens;
}