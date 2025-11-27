#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>

typedef uint8_t   u8;
typedef int8_t    i8;
typedef uint16_t  u16;
typedef int16_t   i16;
typedef uint32_t  u32;
typedef int32_t   i32;
typedef uint64_t  u64;
typedef int64_t   i64;
typedef float     f32;
typedef double    f64;

#define VEC_DEFAULT_CAP 8
#define VEC_PUSH(_vec, _val) \
{ \
  if ((_vec).len >= (_vec).cap) { \
    (_vec).cap = (_vec).cap == 0 ? VEC_DEFAULT_CAP : (_vec).cap * 2; \
    (_vec).data = realloc((_vec).data, (_vec).cap * sizeof((_val))); \
  } \
  (_vec).data[(_vec).len++] = (_val); \
}
#define VEC_POP(_vec) (_vec).data[(_vec).len--]
#define VEC_FREE(_vec) free((_vec).data)
#define VEC_RESERVE(_vec, _cap) \
{ \
  if ((_vec).cap < _cap) { \
    while((_vec).cap < (_cap)) (_vec).cap *= 2; \
    (_vec).data = realloc((_vec).data, (_vec).cap * sizeof((_vec).data[0])); \
  } \
} \

#define VEC_FIRST(_vec) (_vec).data[0]
#define VEC_LAST(_vec) (_vec).data[(_vec).len-1]

#define VEC_DEF_NAMED(_name, _type) \
typedef struct { \
  _type* data; \
  int len, cap; \
} _name
#define VEC_DEF(_type) VEC_DEF_NAMED(_type##Vec, _type)

#define VEC_FOR_I(_it, _vec) for(int _it=0; _it < (_vec).len; ++_it)
#define VEC_FOR(_vec) VEC_FOR_I(i, _vec)

#define VEC_FOREACH_IT(_type, _it, _vec) for (_type* _it = (_vec).data; _it < (_vec).data + (_vec).len; ++_it)
#define VEC_FOREACH(_type, _vec) VEC_FOREACH_IT(_type, it, _vec)

VEC_DEF_NAMED(IntVec, int);
VEC_DEF_NAMED(String, char);


#define str_fmt "%.*s"

char* str_clone(char* str, int len) {
  char* res = (char*) malloc(len + 1);
  memcpy(res, str, len);
  res[len] = '\0';
  return res;
}

static char FMT_BUF[2048];
char* fmt(char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int real_size = vsnprintf(NULL, 0, fmt, args);
  va_end(args);

  // real_size excludes null
  real_size += 1;
  
  va_start(args, fmt);
  // should write_real_size + null
  real_size = vsnprintf(FMT_BUF, real_size, fmt, args);
  va_end(args);

  return FMT_BUF;
}

void string_append(String* sb, char* sv) {
  // TODO: what about strcpy? + reserve
  while (*sv != '\0') {
    VEC_PUSH(*sb, *sv);
    sv++;
  }
}

void string_append_n(String* sb, char* sv, int n) {
  while (n-- > 0) {
    VEC_PUSH(*sb, *sv);
    sv++;
  }
}

char* file_read_to_string(char* path) {
  FILE* f = fopen(path, "rb");
  if (f == NULL) return NULL;
  
  if (fseek(f, 0, SEEK_END) != 0) return NULL;
  
  long size = ftell(f);
  if (size < 0) return NULL;

  if (fseek(f, 0, SEEK_SET) != 0) return NULL;

  char* buf = (char*) malloc(size);
  if (buf == NULL) return NULL;

  int read = fread(buf, 1, size, f);
  if (read != size) return NULL;
  else {
    // not necessarily null terminated
    buf[read] = '\0';
    return buf;
  };
}

void stdin_read_line(char* buf, int len) {
  int c;
  int count = 0;
  
  // we keep space for newline and zero terminator, so iter until len-2
  while(count < len-2) {
    c = getchar();
    if (c == '\n' || c == EOF) break;
    buf[count++] = c;
  }

  buf[count++] = '\n';
  buf[count] = '\0';

  // discard rest of stdin
  while(c != '\n') c = getchar();
}
