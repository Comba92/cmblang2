#include "common.h"
#include "lexer.h"
#include "parser.h"
#include "typecheck.h"
#include "eval.h"

#define DEBUG_LIST \
  X(IntVec) \
  X(Token) \
  X(LiteralArray) \
  X(ExprLiteral) \
  X(ExprUnary) \
  X(ExprBinary) \
  X(ExprCall) \
  X(Expr) \
  X(FnParam) \
  X(TypeAnnFunc) \
  X(TypeAnn) \
  X(StmtDecl) \
  X(StmtFnDecl) \
  X(StmtAssign) \
  X(StmtBlock) \
  X(StmtIfElse) \
  X(StmtWhile) \
  X(Stmt) \
  X(Parser) \
  X(Symtbl) \
  X(Sym) \
  X(ValueArray) \
  X(ValueFunc) \
  X(Value) \

int main() {
  printf("Hello!\n");

  // #define X(Name) printf("%s size: %ld bytes\n", #Name, sizeof(Name));
  // DEBUG_LIST
  // #undef X

  #define BUF_SIZE 1024
  char buf[BUF_SIZE];

  Parser parser = parser_init(&buf);
  Symtbl tbl = symtbl_init(&parser);
  Context ctx = ctx_init(&parser);

  while(1) {
    fputs("> ", stdout);
    stdin_read_line(buf, BUF_SIZE);

    parse(&parser);
    if (parser.err == NULL && typecheck(&tbl)) {
      eval(&ctx);
    }

    // VEC_FOR(parser.tokens) token_dbg(parser.tokens, i);
    // VEC_FOR(parser.exprs) expr_dbg(parser.exprs, i);
    // VEC_FOR(parser.stmts) stmt_dbg(parser.stmts, i);
  }

  return 0;
}
