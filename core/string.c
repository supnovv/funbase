#include <string.h>
#include "core/string.h"

#define L_STRING_FIXLEN_CAP (sizeof(l_string) - sizeof(l_long) - 1)

static void
l_string_set_size(l_string* s, l_int n)
{
  s->implsz = (((l_ulong)n) << 6) | (s->implsz & 0x3f);
}

static void
l_string_set_long(l_string* s, l_int is_long)
{
  s->implsz = ((s->implsz >> 2) << 2) | (is_long ? 0x01 : 0x00);
}

static l_allocfunc
l_string_allocfunc(l_string* s)
{
  l_int alloc_type = (s->implsz >> 2) & 0x0f;
  return l_get_allocfunc(alloc_type);
}

static l_bool
l_string_is_long(const l_string* s)
{
  return (s->implsz & 0x03) > 0;
}

L_EXTERN l_byte*
l_string_strc(l_string* s)
{
  return l_string_is_long(s) ? s->lnstr : (l_byte*)&s->impltt;
}

L_EXTERN l_int
l_string_size(const l_string* s)
{
  return (l_int)((s)->implsz >> 6);
}

L_EXTERN l_int
l_string_capacity(const l_string* s)
{
  return l_string_is_long(s) ? s->impltt : (l_int)L_STRING_FIXLEN_CAP;
}

L_EXTERN l_string
l_string_create(l_int alloc_type, l_int size)
{
  l_string s = {0, 0, 0, 0};
  if (l_get_allocfunc(alloc_type)) {
    s.implsz |= ((alloc_type & 0x0f) << 2);
  }
  if (size > (l_int)L_STRING_FIXLEN_CAP) {
    l_byte* newp = 0;
    l_int capacity = (L_STRING_FIXLEN_CAP + 1) * 2 - 1;
    if (size > capacity) {
      capacity = size;
    }
    newp = (l_string_allocfunc(&s))(0, 0, 0, capacity + 1);
    newp[0] = 0;
    l_string_set_size(&s, 0);
    s.impltt = capacity;
    s.lnstr = newp;
    l_string_set_long(&s, true);
  }
  return s;
}

static l_int
l_impl_string_write(void* out, const void* p, l_int len)
{
  l_string* s = (l_string*)out;
  l_int size = l_string_size(s);
  l_byte* oldp = l_string_strc(s);

  if (p == 0 || len <= 0) {
    return 0;
  }

  if (l_string_is_long(s)) {
    if (size + len <= s->impltt) {
      l_copy_n(oldp + size, p, len);
      size += len;
      oldp[size] = 0;
      l_string_set_size(s, size);
    } else {
      l_byte* newp = 0;
      l_int capacity = (s->impltt + 1) * 2 - 1;
      if (size + len > capacity) {
        capacity = size + len;
      }
      if (capacity <= s->impltt) {
        l_loge_1(LNUL, "string is too long %d", ld(s->impltt));
        return 0;
      }
      newp = (l_string_allocfunc(s))(0, oldp, s->impltt + 1, capacity + 1);
      l_copy_n(newp + size, p, len);
      size += len;
      newp[size] = 0;
      l_string_set_size(s, size);
      s->impltt = capacity;
      s->lnstr = newp;
    }
  } else {
    if (size + len <= (l_int)L_STRING_FIXLEN_CAP) {
      l_copy_n(oldp + size, p, len);
      size += len;
      oldp[size] = 0;
      l_string_set_size(s, size);
    } else {
      l_byte* newp = 0;
      l_int capacity = (L_STRING_FIXLEN_CAP + 1) * 2 - 1;
      if (size + len > capacity) {
        capacity = size + len;
      }
      newp = (l_string_allocfunc(s))(0, 0, 0, capacity + 1);
      l_copy_n(newp, oldp, size);
      l_copy_n(newp + size, p, len);
      size += len;
      newp[size] = 0;
      l_string_set_size(s, size);
      s->impltt = capacity;
      s->lnstr = newp;
      l_string_set_long(s, true);
    }
  }

  return len;
}

static l_int
l_ostream_string_write(void* out, const void* p, l_int len)
{
  if (l_ostream_should_flush(p, len)) {
    return 0;
  } else {
    return l_impl_string_write(out, p, len);
  }
}

L_EXTERN l_ostream
l_string_ostream(l_string* s)
{
  return l_ostream_from(s, l_ostream_string_write);
}

L_EXTERN l_int
l_string_append(l_string* s, l_strn from)
{
  return l_impl_string_write(s, from.p, from.n);
}

L_EXTERN l_string
l_string_create_from(l_int alloc_type, l_int size, l_strn from)
{
  l_string s;
  if (size < from.n) {
    size = from.n;
  }
  s = l_string_create(alloc_type, size);
  l_string_append(&s, from);
  return s;
}

L_EXTERN void
l_string_clear(l_string* s)
{
  l_byte* p = l_string_strc(s);
  l_string_set_size(s, 0);
  p[0] = 0;
}

L_EXTERN l_int
l_string_reset(l_string* s, l_strn from)
{
  l_string_clear(s);
  return l_string_append(s, from);
}

L_EXTERN void
l_string_destroy(l_string* s)
{
  if (l_string_is_long(s) && s->lnstr) {
    (l_string_allocfunc(s))(0, s->lnstr, 0, 0);
    s->lnstr = 0;
  }

  *s = l_string_create((s->implsz >> 2) & 0x0f, 0);
}

