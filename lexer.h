#pragma once

// TODO: if Token type stays small, consider passing always by copy instead of pointer

#define TOKENS_LIST \
  X(TokAnd) \
  X(TokOr) \
  X(TokNot) \
  X(TokEq) \
  X(TokNotEq) \
  X(TokGreatEq) \
  X(TokLessEq) \
  X(TokArrow) \
  X(Tok2Colon) \
  X(TokDecl) \
  X(Tok2Dot) \
  X(Tok2Slash) \
  X(TokIntLit) \
  X(TokFloatLit) \
  X(TokInt) \
  X(TokFloat) \
  X(TokBool) \
  X(TokIdent) \
  X(TokTrue) \
  X(TokFalse) \
  X(TokIf) \
  X(TokElse) \
  X(TokWhile) \
  X(TokFor) \
  X(TokFn) \
  X(TokReturn) \
  X(TokStruct) \
  X(TokAs) \

#define X(Tok) #Tok,
const char* TOKEN_DBG[] = {
  TOKENS_LIST
};
#undef X

#define X(Tok) Tok,
typedef enum {
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
  
  TokErr = 128,
  TOKENS_LIST
} TokenKind;
#undef X

typedef struct {
  const char* name;
  int len; 
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
  X(int, TokInt) \
  X(float, TokFloat) \
  X(bool, TokBool) \

#define X(name, tok) { #name, sizeof(#name)-1, tok },
static const KeywordData KEYWORDS[] = {
  KEYWORDS_LIST
};
#undef X
const int KEYWORDS_LEN = sizeof(KEYWORDS) / sizeof(KeywordData);

typedef struct {
  TokenKind kind;
  int len;
  int offset;
  int line;
  int column;
} Token;
VEC_DEF(Token);
VEC_DEF_NAMED(TokenRefVec, Token*);


void tok_dbg(Token tok, char* src) {
  if (tok.kind < TokErr) {
    printf("[TOKEN] kind = '%c'%-10s\t column = %-8d line = %d\n", tok.kind, "", tok.column, tok.line);
  } else {
    printf("[TOKEN] kind = %-12s\t column = %-8d line = %d len = %d '%.*s'\n", TOKEN_DBG[tok.kind - TokErr - 1], tok.column, tok.line, tok.len, tok.len, src + tok.offset);
  }
}

Token tok_sym(char c) {
  return (Token) { (TokenKind) c, 1, 0, 0, 0 };
}

Token tok_sym2(TokenKind c) {
  return (Token) { c, 2, 0, 0, 0 };
}

bool tok_is_op(Token t) {
  return t.kind == TokParenLeft
    || t.kind == TokBraceLeft
    || t.kind == TokAdd
    || t.kind == TokMul
    || t.kind == TokSub
    || t.kind == TokDiv
    || t.kind == TokRem
    || t.kind == TokExp
    || t.kind == TokAnd
    || t.kind == TokOr
    || t.kind == TokNot
    || t.kind == TokEq
    || t.kind == TokNotEq
    || t.kind == TokGreat
    || t.kind == TokGreatEq
    || t.kind == TokLess
    || t.kind == TokLessEq;
}

bool tok_is_expr(Token t) {
  return t.kind == TokIdent
    || t.kind == TokSub
    || t.kind == TokNot
    || t.kind == TokIntLit
    || t.kind == TokFloatLit
    || t.kind == TokTrue
    || t.kind == TokFalse
    || t.kind == TokBraceLeft
    || t.kind == TokParenLeft;
}

bool tok_is_safe(Token t) {
  return t.kind == TokCurlyLeft
    || t.kind == TokIf
    || t.kind == TokWhile
    || t.kind == TokFn;
}

Token lexer_eat_if_or(char* s, char match, TokenKind tok, TokenKind or) {
  if (*(s + 1) == match) {
    return tok_sym2(tok);
  } else {
    return tok_sym(or);
  }
}

TokenVec tokenize(char* str) {
  TokenVec tokens = {0};

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
        t = tok_sym(c);
        break;

      case '/': t = lexer_eat_if_or(str, '/', Tok2Slash, TokDiv); break;
      case '.': t = lexer_eat_if_or(str, '.', Tok2Dot, TokDot); break;
      case '-': t = lexer_eat_if_or(str, '>', TokArrow, TokSub); break;
      case '=': t = lexer_eat_if_or(str, '=', TokEq, TokAssign); break;
      case '<': t = lexer_eat_if_or(str, '=', TokLessEq, TokLess); break;
      case '>': t = lexer_eat_if_or(str, '=', TokGreatEq, TokGreat); break;
      case '!': t = lexer_eat_if_or(str, '=', TokNotEq, TokBang); break;
      case ':': {
        char next = *(str + 1);
        if (next == ':') t = tok_sym2(Tok2Colon);
        else if (next == '=') t = tok_sym2(TokDecl);
        else t = tok_sym(TokColon);
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
            t.kind = TokFloatLit; 
            t.len = len;
          } else {
            t.kind = TokIntLit;
            t.len = len;
          }
        } else if (isalpha(c)) {
          bool is_keyword = false;
          // keywords
          for (int i=0; i<KEYWORDS_LEN; i++) {
            KeywordData keyword = KEYWORDS[i];
            // len doesn't include null char
            if (strncmp(str, keyword.name, keyword.len) == 0) {
              t.kind = keyword.kind; 
              t.len = keyword.len;
              is_keyword = true;
              break;
            }
          }
          
          if (is_keyword) break;

          // not a keyword, must be an ident
          int len = 1;
          while (str[len] != '\0' && (isalnum(str[len]) || str[len] == '_')) len++;
          t.kind = TokIdent;
          t.len = len;
        } else {
          t.offset = consumed;
          t.column = column;
          t.line = line;

          fprintf(stderr, "[LEX ERR] Invalid token ");
          tok_dbg(t, str);

          t = tok_sym(TokErr);
        }
      } break;
    }

    t.offset = consumed;
    t.column = column;
    t.line = line;

    VEC_PUSH(tokens, t);
    str += t.len;
    column += t.len;
    consumed += t.len;
  }

  return tokens;
}