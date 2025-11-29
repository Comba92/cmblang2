#include "common.h"
#include "lexer.h"
#include "parser.h"
#include "typecheck.h"
// #include "eval.h"

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

  int src_len = 0;
  char* text = file_read_to_string("test.cmb", &src_len);
  if (text == NULL) {
    fprintf(stderr, "Failed to open file\n");
    return 1;
  }

  printf("%s\n", text);

  Parser parser = parser_init(text);
  Symtbl tbl = symtbl_init(&parser);
  // Context ctx = ctx_init(&parser);

  // TokenVec tokens = tokenize(text);
  // VEC_FOREACH(Token, tokens) tok_dbg(*it, text);

  if (!parse(&parser)) return 2;
  // VEC_FOREACH(Expr, parser.exprs) expr_dbg(*it);
  // VEC_FOREACH(Stmt, parser.stmts) stmt_dbg(*it);
  VEC_FOREACH(TypeAnn, parser.types) type_dbg(*it);
  // putchar('\n');

  if (!typecheck(&tbl)) return 3;
  symtbl_dbg(&tbl);
  
  // eval(&ctx);

  return 0;
}
