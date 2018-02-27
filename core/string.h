#ifndef L_CORE_STRING_H
#define L_CORE_STRING_H
#include "core/base.h"

typedef struct {
  const l_byte* start;
  l_int len;
} l_strn;

#undef l_strc
#undef l_strn_literal
#undef l_strn_empty

#define l_strc(s) ((l_byte*)(s)) /* zero terminated c string */
#define l_strn_literal(s) ((l_strn){l_strc("" s), sizeof(s) - 1})
#define l_strn_empty() ((l_strn){0, 0})

L_INLINE l_strn
l_strn_from(const void* s, l_int n) {
  return (l_strn){l_strc(s), n > 0 ? n : 0};
}

L_INLINE l_strn
l_strn_fromc(const void* s) { /* include <string.h> first before use it to make "strlen" visible */
  return (l_strn){l_strc(s), s ? strlen((char*)s) : 0};
}

L_INLINE l_strn
l_strn_fromp(const void* s, const void* e) {
  return l_strn_from(s, l_strc(e) - l_strc(s));
}

L_INLINE l_strn
l_strn_fromsub(const void* s, l_int m, l_int n) {
  return l_strn_from(l_strc(s) + m, n - m);
}

#endif /* L_CORE_STRING_H */

