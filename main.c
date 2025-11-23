#include "common.h"

#include "lexer.h"
#include "parser.h"
#include "eval.h"

int main() {
  printf("Hello!\n");

  #define BUF_SIZE 1024
  char buf[BUF_SIZE];
  SymTbl ctx = symtbl_init();
  Parser parser = {0};

  while(1) {
    fputs("> ", stdout);
    stdin_read_line(buf, BUF_SIZE);

    parse(&parser, buf);
    // VEC_FOR(parser.tokens) token_dbg(parser.tokens, i);
    // VEC_FOR(parser.exprs) expr_dbg(parser.exprs, i);
    // VEC_FOR(parser.stmts) stmt_dbg(parser.stmts, i);

    if (parser.err == NULL) {
      eval(&parser, &ctx);
    }
  }

  return 0;
}
