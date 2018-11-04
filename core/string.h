#ifndef LNLYLIB_CORE_STRING_H
#define LNLYLIB_CORE_STRING_H
#ifdef __cplusplus
extern "C" {
#endif

#include "core/base.h"

typedef struct l_string l_string;

L_EXTERN l_string l_string_create(l_int alloc_type, l_int size);
L_EXTERN l_string l_string_create_from(l_int alloc_type, l_int size, l_strn from);
L_EXTERN l_ostream l_string_ostream(l_string* s);
L_EXTERN l_byte* l_string_strc(l_string* s);
L_EXTERN l_int l_string_size(const l_string* s);
L_EXTERN l_int l_string_capacity(const l_string* s);
L_EXTERN l_int l_string_append(l_string* s, l_strn from);
L_EXTERN l_int l_string_reset(l_string* s, l_strn from);
L_EXTERN void l_string_clear(l_string* s);
L_EXTERN void l_string_destroy(l_string* s);

L_INLINE l_strn
l_string_strn(const l_string* s)
{
  return l_strn_l(l_string_strc((l_string*)s), l_string_size(s));
}

L_INLINE l_bool
l_string_is_empty(const l_string* s)
{
  return l_string_size(s) == 0;
}

L_INLINE l_bool
l_string_nt_empty(const l_string* s)
{
  return l_string_size(s) != 0;
}

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
struct l_string {
  l_ulong implsz;
  l_int impltt;
  l_int extra;
  l_byte* lnstr;

  l_string():
    implsz(0), impltt(0), extra(0), lnstr(0)
    {}

  l_string(l_int alloc_type, l_int size) {
    *this = l_string_create(alloc_type, size);
  }

  l_string(l_int alloc_type, l_strn from) {
    *this = l_string_create_from(alloc_type, 0, from);
  }

  ~l_string() {
    l_string_destroy(this);
  }

  l_ostream ostream() {
    return l_string_ostream(this);
  }

  l_byte* strc() {
    return l_string_strc(this);
  }

  const l_byte* strc() const {
    return l_string_strc((l_string*)this);
  }

  l_strn strn() const {
    return l_string_strn(this);
  }

  l_int size() const {
    return l_string_size(this);
  }

  l_int capacity() const {
    return l_string_capacity(this);
  }

  l_bool empty() const {
    return l_string_is_empty(this);
  }

  void append(l_strn s) {
    l_string_append(this, s);
  }

  void clear() {
    l_string_clear(this);
  }

  void reset(l_strn s) {
    l_string_reset(this, s);
  }
};
#else
struct l_string {
  l_ulong implsz;
  l_int impltt;
  l_int extra;
  l_byte* lnstr;
};
#endif

#endif /* LNLYLIB_CORE_STRING_H */

