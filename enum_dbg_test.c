#define ENUM_LIST \
  X(Kys) \
  X(Fag) \

#define X(Name) #Name,
const char* ENUM_DBG[] = {
  ENUM_LIST
};
#undef X

#define X(Name) Name,
enum MyEnum {
  ENUM_LIST
};

int main() {
  printf("Hello!\n");
  enum MyEnum a = Kys;

  printf("%s\n", ENUM_DBG[a]);
}