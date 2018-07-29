#define LNLYLIB_API_IMPL
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "core/base.h"

const l_strn l_digit_hex[] = {
  L_STR("0123456789abcdef"),
  L_STR("0123456789ABCDEF")
};

L_EXTERN l_int
l_string_parseDec(l_strt s)
{
  l_int times = 1;
  l_int value = 0;
  const l_byte* start = 0;
  const l_byte* end = 0;
  int negative = false;

  while (s.start < s.end) {
    if (*s.start >= '0' && *s.start <= '9') break;
    if (*s.start == '-') negative = true;
    ++s.start;
  }

  start = s.start;
  while (s.start < s.end) {
    if (*s.start < '0' || *s.start > '9') break;
    ++s.start;
  }

  end = s.start;
  while (start < end--) {
    value += *end * times;
    times *= 10;
  }

  return (negative ? -value : value);
}

L_EXTERN l_int
l_string_parseHex(l_strt s)
{
  l_int times = 1;
  l_int value = 0;
  const l_byte* start = 0;
  const l_byte* end = 0;
  int negative = false;

  while (s.start < s.end) {
    if (l_check_is_hex_digit(*s.start)) break;
    if (*s.start == '-') negative = true;
    ++s.start;
  }

  if (*s.start == '0' && s.start + 1 < s.end && (*(s.start + 1) == 'x' || *(s.start + 1) == 'X')) {
    s.start += 2;
    if (s.start >= s.end || !l_check_is_hex_digit(*s.start)) {
      return 0;
    }
  }

  start = s.start;
  while (s.start < s.end) {
    if (!l_check_is_hex_digit(*s.start)) break;
    ++s.start;
  }

  end = s.start;
  while (start < end--) {
    value += *end * times;
    times *= 16;
  }

  return (negative ? -value : value);
}

static l_int
l_ostream_format_fill(l_ostream* os, l_byte* a, l_byte* p, l_umedit flags)
{
  l_byte fill = L_GETF(flags);
  int width = L_GETW(flags);

  if (width > p - a) {
    if (fill == 0) {
      fill = ' ';
    }
    if (flags & L_LEFT) {
      while (width > p - a) {
        *p++ = fill;
      }
    } else {
      l_byte* e = a + width;
      while (p > a) {
        *(--e) = *(--p);
      }
      while (e > a) {
        *(--e) = fill;
      }
      p = a + width;
    }
  }

  return l_ostream_write(os, a, p - a);
}

L_EXTERN l_int
l_ostream_format_strn(l_ostream* os, l_strn s, l_umedit flags)
{
  int width = L_GETW(flags);
  if (l_strn_is_empty(&s)) {
    return 0;
  }
  if (s.n >= width) {
    return l_ostream_write(os, s.p, s.n);
  } else {
    l_byte a[160];
    memcpy(a, s.p, s.n);
    return l_ostream_format_fill(os, a, a + s.n, flags);
  }
}

L_EXTERN l_int
l_ostream_format_c(l_ostream* os, int c, l_umedit flags)
{
  l_byte ch = (l_byte)(c & 0xff);
  if (flags & L_UPPER) {
    if (ch >= 'a' && ch <= 'z') {
      ch -= 32;
    }
  } else if (flags & L_LOWER) {
    if (ch >= 'A' && ch <= 'Z') {
      ch += 32;
    }
  }
  return l_ostream_format_strn(os, l_strn_l(&ch, 1), flags);
}

#define L_HEX_FMT_BFSZ 159

L_EXTERN l_int
l_ostream_format_u(l_ostream* os, l_ulong u, l_umedit flags)
{
  /* 64-bit unsigned int max value 18446744073709552046 (20 chars) */
  l_byte a[L_HEX_FMT_BFSZ];
  l_byte basechar = 0;
  l_byte* e = a + L_HEX_FMT_BFSZ - 1;
  l_byte* p = e;
  const l_byte* hex = 0;
  l_umedit base = 0;
  l_byte precise = L_GETP(flags);
  l_byte width = L_GETW(flags);
  l_byte fill = L_GETF(flags);

  flags &= L_FORMAT_MASK;
  base = (flags & L_BASE_MASK);

  switch (l_right_most_bit(base)) {
  case 0:
    *--p = (u % 10) + '0';
    while ((u /= 10)) {
      *--p = (u % 10) + '0';
    }
    break;
  case L_HEX:
    hex = l_digit_hex[(flags & L_UPPER) != 0].p;
    *--p = hex[u & 0x0f];
    while ((u >>= 4)) {
      *--p = hex[u & 0x0f];
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'X' : 'x';
    }
    flags &= (~L_BASE_MASK);
    break;
  case L_OCT:
    *--p = (u & 0x07) + '0';
    while ((u >>= 3)) {
      *--p = (u & 0x07) + '0';
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'O' : 'o';
    }
    flags &= (~L_BASE_MASK);
    break;
  case L_BIN:
    *--p = (u & 0x01) + '0';
    while ((u >>= 1)) {
      *--p = (u & 0x01) + '0';
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'B' : 'b';
    }
    flags &= (~L_BASE_MASK);
    break;
  default:
    break;
  }

  while (precise > (p - a)) {
    *--p = '0';
  }

  if (basechar) {
    *--p = basechar;
    *--p = '0';
  }

  if (flags & L_MINUX) *--p = '-';
  else if (flags & L_PLUS) *--p = '+';
  else if (flags & L_BLANK) *--p = ' ';

  if (width > e - p) {
    if (fill == 0) {
      fill = ' ';
    }
    if (flags & L_LEFT) {
      l_byte* end = a + width;
      while (p < e) {
        *a++ = *p++;
      }
      while (a < end) {
        *a++ = fill;
      }
    } else {
      while (width > e - p) {
        *--p = fill;
      }
    }
  }

  return l_ostream_write(os, p, e - p);
}

L_EXTERN l_int
l_ostream_format_d(l_ostream* os, l_long d, l_umedit flags)
{
  l_ulong n = 0;
  if (d < 0) {
    n = -d;
    flags |= L_MINUS;
  } else {
    n = d;
    flags &= (~L_MINUS);
  }
  return l_ostream_format_u(os, n, flags);
}

L_EXTERN l_int
l_ostream_format_f(l_ostream* os, double f, l_umedit flags)
{
  l_value v = lf(f);
  l_byte a[159];
  l_byte sign = 0;
  l_byte* p = a;
  l_byte* dot = 0;
  l_ulong fraction = 0;
  l_ulong mantissa = 0;
  int exponent = 0;
  int negative = 0;
  l_umedit precise = ((flags & 0x7f0000) >> 16);

  /**
   * Floating Point Components
   * |                  | Sign   | Exponent   | Fraction   | Bias
   * |----              |-----   | -----      |  ----      | ----
   * | Single Precision | 1 [31] |  8 [30-23] | 23 [22-00] | 127
   * | Double Precision | 1 [63] | 11 [62-52] | 52 [51-00] | 1023
   * ------------------------------------------------------------
   * Sign - 0 positive, 1 negative
   * Exponent - represent both positive and negative exponents
   *          - the ectual exponent = Exponent - (127 or 1023)
   *          - exponents of -127/-1023 (all 0s) and 128/1024 (255/2047, all 1s) are reserved for special numbers
   * Mantissa - stored in normalized form, this basically puts the radix point after the first non-zero digit
   *          - the mantissa has effectively 24/53 bits of resolution, by way of 23/52 fraction bits: 1.Fraction
   */

  negative = (v.u & 0x8000000000000000) != 0;
  exponent = (v.u & 0x7ff0000000000000) >> 52;
  fraction = (v.u & 0x000fffffffffffff);

  if (negative) sign = '-';
  else if (flags & L_PLUS) sign = '+';
  else if (flags & L_BLANK) sign = ' ';

  if (exponent == 0 && fraction == 0) {
    if (sign) *p++ = sign;
    *p++ = '0'; *p++ = '.'; dot = p; *p++ = '0';
  } else if (exponent == 0x00000000000007ff) {
    if (fraction == 0) {
      if (sign) *p++ = sign;
      *p++ = 'I'; *p++ = 'N'; *p++ = 'F'; *p++ = 'I'; *p++ = 'N'; *p++ = 'I'; *p++ = 'T'; *p++ = 'Y';
    } else {
      if (flags & L_BLANK) *p++ = ' ';
      *p++ = 'N'; *p++ = 'A'; *p++ = 'N';
    }
  } else {
    if (sign) *p++ = sign;
    if (negative) v.u &= 0x7fffffffffffffff;

    exponent = exponent - 1023;
    mantissa = 0x0010000000000000 | fraction;
    /* intmasks = 0xfff0000000000000; */
    /*                         1.fraction
        [ , , , | , , , | , , ,1|f,r,a,c,t,i,o,n,n,n,...]
        <----------- 12 --------|-------- 52 ----------->    */
    if (exponent < 0) {
      /* only have fraction part */
      #if 0
      if (exponent < -8) {
        intmasks = 0xf000000000000000;
        exponent += 8; /* 0000.00000001fraction * 2^exponent */
        mantissa >>= (-exponent); /* lose lower digits */
      } else {
        intmasks <<= (-exponent);
      }
      *p++ = '0'; dot = p; *p++ = '.';
      l_log_print_fraction(mantissa, intmasks, p);
      #endif
      *p++ = '0'; *p++ = '.'; dot = p;
      p = l_string_print_fraction(v.f, p, precise);
    } else {
      if (exponent >= 52) {
        /* only have integer part */
        if (exponent <= 63) { /* 52 + 11 */
          mantissa <<= (exponent - 52);
          p = l_string_print_ulong(mantissa, p);
          *p++ = '.'; dot = p; *p++ = '0';
        } else {
          exponent -= 63;
          mantissa <<= 11;
          p = l_string_print_ulong(mantissa, p);
          *p++ = '*'; *p++ = '2'; *p++ = '^';
          p = l_string_print_ulong(exponent, p);
        }
      } else {
        /* have integer part and fraction part */
        #if 0
        intmasks >>= exponent;
        l_log_print_ulong((mantissa & intmasks) >> (52 - exponent), p);
        *p++ = '.';
        l_log_print_fraction(mantissa & (~intmasks), intmasks, p);
        #endif
        l_ulong ipart = l_cast(l_ulong, v.f);
        p = l_string_print_ulong(ipart, p);
        *p++ = '.'; dot = p;
        p = l_string_print_fraction(v.f - ipart, p, precise);
      }
    }
  }

  if (dot && precise) {
    while (p - dot < precise) {
      *p++ = '0';
    }
  }

  return l_ostream_format_fill(os, a, p, flags);
}

L_EXTERN l_int
l_ostream_format_s(l_ostream* os, const void* s, l_umedit flags)
{
  return l_ostream_format_strn(os, l_strn_c(s), flags);
}

L_EXTERN l_int
l_ostream_format_bool(l_ostream* os, int n, l_umedit flags)
{
  if (n) {
    if (flags & L_UPPER) {
      return l_ostream_format_strn(os, L_STR("TRUE"), flags);
    } else {
      return l_ostream_format_strn(os, L_STR("true"), flags);
    }
  } else {
    if (flags & L_UPPER) {
      return l_ostream_format_strn(os, L_STR("FALSE"), flags);
    } else {
      return l_ostream_format_strn(os, L_STR("false"), flags);
    }
  }
}

L_EXTERN l_int
l_ostream_format_n(l_ostream* os, const void* fmt, l_int n, const l_value* a)
{}

L_EXTERN l_int
l_impl_ostream_format_v(l_ostream* os, const void* fmt, l_int n, va_list vl)
{}

L_EXTERN l_int
l_impl_ostream_format(l_ostream* os, const void* fmt, l_int n, ...)
{}


L_EXTERN void
l_filename_init(l_filename* fn)
{
  fn->buff_len = FILENAME_MAX-8;
  fn->name_len = 0;
  fn->s[0] = 0;
}

L_EXTERN l_bool
l_filename_append(l_filename* fn, l_strn s)
{
  if (s.n > 0 && fn->name_len + s.n < fn->buff_len) {
    const l_byte* pend = s.p + s.n;
    while (s.p < pend) {
      fn->s[fn->name_len++] = *s.p++;
    }
    fn->s[fn->name_len] = 0;
    return true;
  } else {
    return false;
  }
}

L_EXTERN l_bool
l_filename_addname(l_filename* fn, l_strn name, l_strn suffix)
{
  return l_filename_append(fn, name) && l_filename_append(fn, suffix);
}

L_EXTERN l_bool
l_filename_addname_combine(l_filename* fn, l_strn part1, l_strn part2, l_strn sep)
{
  return l_filename_append(fn, part1) && l_filename_append(fn, sep) && l_filename_append(fn, part2);
}

L_EXTERN l_bool
l_filename_addpath(l_filename* fn, l_strn path)
{
  if (path.n > 0 && fn->name_len + path.n < fn->buff_len) {
    const l_byte* pend = path.p + path.n;
    if (fn->name_len > 0) {
      if (fn->s[fn->name_len - 1] == '/') {
        if (*path.p == '/') {
          path.p += 1;
        }
      } else {
        if (*path.p != '/') {
          fn->s[fn->name_len++] = '/';
        }
      }
    }
    while (path.p < pend) {
      fn->s[fn->name_len++] = *path.p++;
    }
    if (fn->s[fn->name_len - 1] != '/') {
      fn->s[fn->name_len++] = '/';
    }
    fn->s[fn->name_len] = 0;
    return true;
  } else {
    return false;
  }
}

static l_stdfile
l_impl_stdfile_open(const void* name, const char* mode)
{
  l_stdfile s = {0};
  if (name && mode) {
    s.file = fopen((const char*)name, mode);
    if (s.file == 0) {
      l_loge_2(LNUL, "fopen %s %s", ls(name), lserror(errno));
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
  }
  return s;
}

static l_stdfile
l_impl_stdfile_open_nobuf(const void* name, const char* mode)
{
  l_stdfile s = l_impl_stdfile_open(name, mode);
  if (s.file) {
    setbuf((FILE*)s.file, 0);
  }
  return s;
}

static void
l_impl_stdfile_reopen(FILE* file, const void* name, const char* mode)
{
  if (name && mode) {
    if (freopen((const char*)name, mode, file) == 0) {
      l_loge_2(LNUL, "freopen %s %s", ls(name), lserror(errno));
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
  }
}

L_EXTERN l_stdfile
l_stdfile_open_read(const void* name)
{
  return l_impl_stdfile_open(name, "rb");
}

L_EXTERN l_stdfile
l_stdfile_open_read_nobuf(const void* name)
{
  return l_impl_stdfile_open_nobuf(name, "rb");
}

L_EXTERN l_stdfile
l_stdfile_open_write(const void* name)
{
  return l_impl_stdfile_open(name, "wb");
}

L_EXTERN l_stdfile
l_stdfile_open_write_nobuf(const void* name)
{
  return l_impl_stdfile_open_nobuf(name, "wb");
}

L_EXTERN l_stdfile
l_stdfile_open_append(const void* name)
{
  return l_impl_stdfile_open(name, "ab");
}

L_EXTERN l_stdfile
l_stdfile_open_append_nobuf(const void* name)
{
  return l_impl_stdfile_open_nobuf(name, "ab");
}

L_EXTERN l_stdfile
l_stdfile_open_read_write(const void* name)
{
  return l_impl_stdfile_open(name, "rb+");
}

L_EXTERN void
l_stdfile_close(l_stdfile* s)
{
  if (s->file == 0) {
    return;
  }
  if (fclose((FILE*)s->file) != 0) {
    l_loge_1(LNUL, "fclose %s", lserror(errno));
  }
  s->file = 0;
}

L_EXTERN void
l_stdfile_clearerr(l_stdfile* s)
{
  clearerr((FILE*)s->file);
}

L_EXTERN l_bool
l_stdfile_flush(l_stdfile* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fflush((FILE*)s->file) == 0) {
    return true;
  } else {
    l_loge_1(LNUL, "fflush %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stdfile_rewind(l_stdfile* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fseek((FILE*)s->file, 0, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_1(LNUL, "fseek SET %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stdfile_seekto(l_stdfile* s, l_int pos)
{
  if (s->file == 0 || pos < 0 || pos > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(pos));
    return false;
  }
  if (fseek((FILE*)s->file, pos, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek SET %d %s", ld(pos), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stdfile_forword(l_stdfile* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stdfile_backward(l_stdfile* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, -offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_int
l_stdfile_read(l_stdfile* s, void* out, l_int size)
{
  if (s->file == 0 || out == 0 || size < 0 || size > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(size));
    return 0;
  }
  if (size == 0) {
    return 0;
  }
  { l_int n = 0;
    n = (l_int)fread(out, 1, (size_t)size, (FILE*)s->file);
    if (n == size) {
      return n;
    }
    if (!feof((FILE*)s->file)) {
      l_loge_1(LNUL, "fread %s", lserror(errno));
    }
    if (n < 0) {
      return 0;
    }
    return n;
  }
}

L_EXTERN l_int
l_stdfile_write(l_stdfile* s, const void* p, l_int len)
{
  if (s->file == 0 || p == 0 || len < 0 || len > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(len));
    return 0;
  }
  if (len == 0) {
    return 0;
  }
  { l_int n = (l_int)fwrite(p, 1, (size_t)len, (FILE*)s->file);
    if (n == len) {
      return n;
    }
    l_loge_1(LNUL, "fwrite %s", lserror(errno));
    if (n < 0) {
      return 0;
    }
    return n;
  }
}

L_EXTERN l_int
l_stdfile_write_strn(l_stdfile* out, l_strn s)
{
  return l_stdfile_write(out, s.p, s.n);
}

L_EXTERN l_int
l_stdfile_put(l_stdfile* s, l_byte ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fwrite(&ch, 1, 1, (FILE*)s->file) != 1) {
    l_loge_1(LNUL, "fwrite %s", lserror(errno));
    return 0;
  }
  return 1;
}

L_EXTERN l_int
l_stdfile_get(l_stdfile* s, l_byte* ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fread(ch, 1, 1, (FILE*)s->file) != 1) {
    if (!feof((FILE*)s->file)) {
      l_loge_1(LNUL, "fread %s", lserror(errno));
    }
    return 0;
  }
  return 1;
}

L_EXTERN l_bool
l_stdfile_remove(const void* name)
{
  if (name) {
    if (remove((const char*)name) == 0) {
      return true;
    } else {
      l_loge_1(LNUL, "remove %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
    return false;
  }
}

L_EXTERN l_bool
l_stdfile_rename(const void* from, const void* to)
{
  /* int rename(const char* oldname, const char* newname);
   * Changes the name of the file or directory specified by oldname
   * to newname. This is an operation performed directly on a file;
   * No streams are involved in the operation. If oldname and newname
   * specify different paths and this is supported by the system, the
   * file is moved to the new location. If newname names an existing
   * file, the function may either fail or override the existing file,
   * depending on the specific system and library implementation.
   */
  if (from && to) {
    if (rename((const char*)from, (const char*)to) == 0) {
      return true;
    } else {
      l_loge_1(LNUL, "rename %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
    return false;
  }
}

L_EXTERN void
l_stdfile_redirect_stdout(const void* name)
{
  l_impl_stdfile_reopen(stdout, name, "wb");
}

L_EXTERN void
l_stdfile_redirect_stderr(const void* name)
{
  l_impl_stdfile_reopen(stderr, name, "wb");
}

L_EXTERN void
l_stdfile_redirect_stdin(const void* name)
{
  l_impl_stdfile_reopen(stdin, name, "rb");
}

