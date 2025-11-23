#pragma once

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
  TokQuestion = '?',
  TokGreat = '>',
  TokLess = '<',
  TokColon = ':',
  TokSemicolon = ';',
  TokDot = '.',
  TokComma = ',',
  TokQuote = '\'',
  Tok2Quote = '"',
  TokDollar = '$',
  TokSand = '&',
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
  
  TokInt,
  TokFloat,
  TokBool,
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
} TokenKind;

typedef struct {
  const char* name;
  size_t len; 
  TokenKind kind;
} KeywordData;

#define KEYWORDS_LIST \
  X(true, TokTrue) \
  X(false, TokFalse) \
  X(and, TokAnd) \
  X(or, TokOr) \
  X(not, TokNot) \
  X(if, TokIf) \
  X(else, TokElse) \
  X(while, TokWhile) \
  X(for, TokFor) \
  X(fn, TokFn) \
  X(return, TokReturn) \
  X(struct, TokStruct) \
  X(as, TokAs) \

  // X(int, TokInt) \
  // X(float, TokFloat) \
  // X(bool, TokBool) \

#define X(name, tok) { #name, sizeof(#name)-1, tok },
static const KeywordData KEYWORDS[] = {
  KEYWORDS_LIST
};
#undef X
const size_t KEYWORDS_LEN = sizeof(KEYWORDS) / sizeof(KeywordData);

typedef struct {
  TokenKind kind;
  int offset;
  int len;
} Token;
VEC_DEF(Token);

static Token TOKEN_ERR = { TokErr, -1, -1 };

Token tok_sym(char c, int offset) {
  return (Token) { (TokenKind) c,  offset, 1 };
}

Token tok_sym2(TokenKind c, int offset) {
  return (Token) { c,  offset, 2 };
}

bool tok_is_op(Token* t) {
  return t->kind == TokParenLeft
    // || t->kind == TokParenRight
    || t->kind == TokBraceLeft
    // || t->kind == TokBraceRight
    || t->kind == TokAdd
    || t->kind == TokMul
    || t->kind == TokSub
    || t->kind == TokDiv
    || t->kind == TokRem
    || t->kind == TokExp
    || t->kind == TokAnd
    || t->kind == TokOr
    || t->kind == TokNot
    || t->kind == TokEq
    || t->kind == TokNotEq
    || t->kind == TokGreat
    || t->kind == TokGreatEq
    || t->kind == TokLess
    || t->kind == TokLessEq;
}

bool tok_is_expr(Token* t) {
  return t->kind == TokIdent
    || t->kind == TokSub
    || t->kind == TokNot
    || t->kind == TokInt
    || t->kind == TokFloat
    || t->kind == TokTrue
    || t->kind == TokFalse
    || t->kind == TokBraceLeft
    || t->kind == TokParenLeft;
}

void token_dbg(TokenVec tokens, int idx) {
  Token* t = &tokens.data[idx];
  printf("[TOKEN %d] kind = %c, col = %d, len = %d\n", idx, t->kind, t->offset, t->len);
}

Token lexer_eat_if_or(char* s, char match, int column, TokenKind tok, TokenKind or) {
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

      case '/': t = lexer_eat_if_or(str, '/', column, Tok2Slash, TokDiv); break;
      case '.': t = lexer_eat_if_or(str, '.', column, Tok2Dot, TokDot); break;
      case '-': t = lexer_eat_if_or(str, '>', column, TokArrow, TokSub); break;
      case '=': t = lexer_eat_if_or(str, '=', column, TokEq, TokAssign); break;
      case '<': t = lexer_eat_if_or(str, '=', column, TokLessEq, TokLess); break;
      case '>': t = lexer_eat_if_or(str, '=', column, TokGreatEq, TokGreat); break;
      case '!': t = lexer_eat_if_or(str, '=', column, TokNotEq, TokBang); break;
      case ':': {
        char next = *(str + 1);
        if (next == ':') t = tok_sym2(Tok2Colon, column);
        else if (next == '=') t = tok_sym2(TokDecl, column);
        else t = tok_sym(TokColon, column);
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
            t = (Token) {TokFloat, column, len};
          } else {
            t = (Token) {TokInt, column, len};
          }
        } else if (isalpha(c)) {
          bool is_keyword = false;
          // keywords
          for (size_t i=0; i<KEYWORDS_LEN; i++) {
            KeywordData keyword = KEYWORDS[i];
            // len doesn't include null char
            if (strncmp(str, keyword.name, keyword.len) == 0) {
              t = (Token) {keyword.kind, column, keyword.len};
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