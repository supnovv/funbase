#ifndef LNLYLIB_CORE_BASE_H
#define LNLYLIB_CORE_BASE_H
#include "autoconf.h"
#include "core/prefix.h"

/** pre-defines in makefile, prefix.h, and autoconf.c **
L_PLAT_LINUX or L_PLAT_MACOSX or L_PLAT_WINDOWS
L_NEWLINE - "\n" or "\r\n"
L_NL_SIZE - 1 or 2
L_PATH_SEP - "/" or "\\"
L_INLINE
L_EXTERN
L_THREAD_LOCAL
L_BUILD_SHARED
LNLYLIB_API_IMPL - shall be defined in lib src file
LNLYLIB_HOME_DIR - the lnlylib root folder when make
LNLYLIB_CLIB_DIR - c libraries folder
LNLYLIB_LUALIB_DIR - lua libraries folder
LNLYLIB_API_IMPL
L_MACH_32_BIT or L_MACH_64_BIT
L_LIT_ENDIAN or L_BIG_ENDIAN
l_bool false true - boolean
l_byte l_sbyte - 8-bit integer
l_short l_ushort - 16-bit integer
l_medit l_umedit - 32-bit integer
l_long l_ulong - 64-bit integer
l_int l_uint - pointer-size integer **/

#undef L_MAX_INT_UB
#undef L_MAX_INT_SB
#undef L_MIN_INT_SB
#undef L_MAX_INT_US
#undef L_MAX_INT_SS
#undef L_MIN_INT_SS
#undef L_MAX_INT_UM
#undef L_MAX_INT_SM
#undef L_MIN_INT_SM
#undef L_MAX_INT_UL
#undef L_MAX_INT_SL
#undef L_MIN_INT_SL
#undef L_MAX_INT_IO

#define L_MAX_INT_UB ((l_byte)0xff) /* 255 */
#define L_MAX_INT_SB ((l_sbyte)0x7f) /* 127 */
#define L_MIN_INT_SB ((l_sbyte)-127-1) /* 128 0x80 */
#define L_MAX_INT_US ((l_ushort)0xffff) /* 65535 */
#define L_MAX_INT_SS ((l_short)0x7fff) /* 32767 */
#define L_MIN_INT_SS ((l_short)-32767-1) /* 32768 0x8000 */
#define L_MAX_INT_UM ((l_umedit)0xffffffff) /* 4294967295 */
#define L_MAX_INT_SM ((l_medit)0x7fffffff) /* 2147483647 */
#define L_MIN_INT_SM ((l_medit)-2147483647-1) /* 2147483648 0x80000000 */
#define L_MAX_INT_UL ((l_ulong)0xffffffffffffffff) /* 18446744073709551615 */
#define L_MAX_INT_SL ((l_long)0x7fffffffffffffff) /* 9223372036854775807 */
#define L_MIN_INT_SL ((l_long)-9223372036854775807-1) /* 9223372036854775808 0x8000000000000000 */
#define L_MAX_INT_IO (0x7fff0000) /* 2147418112 */
/* on linux, write() and similar system calls will transfer at most 0x7ffff000 (2,147,479,522) bytes,
returning the number of bytes actually transferred. this is true on both 32-bit and 64-bit systems. */

#undef LNUL
#define LNUL (0)

#undef L_UNUSED
#define L_UNUSED(a) ((void)a)

typedef struct {
  const l_byte* p;
  l_int n;
} l_strn;

#undef l_cstr
#undef l_strn_c
#undef l_empty_strn
#undef l_const_strn

#define l_cstr(s) ((l_byte*)(s)) /* zero terminated c string */
#define l_strn_c(s) ((l_strn){l_cstr(s), (s) ? strlen((char*)(s)) : 0})
#define l_empty_strn() l_const_strn("")
#define l_const_strn(s) ((l_strn){l_cstr("" s), sizeof(s) - 1})

#undef L_STR
#undef L_EMPTY_STR
#define L_STR(s) l_const_strn(s)
#define L_EMPTY_STR l_empty_strn()

L_EXTERN l_bool l_strn_equal(const l_strn* a, l_strn b);
L_EXTERN l_bool l_strn_has(l_strn a, l_byte c);

L_INLINE l_bool
l_strn_nt_empty(const l_strn* s)
{
  return s->p && (s->n > 0);
}

L_INLINE l_bool
l_strn_is_empty(const l_strn* s)
{
  return !l_strn_nt_empty(s);
}

L_INLINE l_strn
l_strn_l(const void* s, l_int len)
{
  return (l_strn){l_cstr(s), len > 0 ? len : 0};
}

L_INLINE l_strn
l_strn_p(const void* s, const void* e)
{
  return l_strn_l(s, l_cstr(e) - l_cstr(s));
}

L_INLINE l_strn
l_strn_s(const void* s, l_int m, l_int n)
{
  return l_strn_l(l_cstr(s) + m, n - m);
}

/** memory operation **/

typedef void* (*l_allocfunc)(void* ud, void* p, l_ulong oldsz, l_ulong newsz);
L_EXTERN l_allocfunc l_alloc_func; /* note the allocated memory is not initialized */

#undef L_MALLOC
#undef L_MALLOC_TYPE
#undef L_MALLOC_TYPE_N
#undef L_RALLOC
#undef L_MFREE

#define L_MALLOC(E, size) l_alloc_func((E), 0, 0, (size))
#define L_MALLOC_TYPE(E, type) (type*)L_MALLOC((E), sizeof(type))
#define L_MALLOC_TYPE_N(E, type, n) (type*)L_MALLOC((E), sizeof(type) * (n))
#define L_RALLOC(E, p, newsz) l_alloc_func((E), (p), 0, (newsz))
#define L_MFREE(E, p) l_alloc_func((E), (p), 0, 0)

L_EXTERN l_bool l_zero_n(void* p, l_ulong size);
L_EXTERN l_ulong l_copy_n(void* dest, const void* from, l_ulong size);

/** debug and logging **/

#undef L_MKSTR
#undef L_X_MKSTR
#undef L_FILE_LINE

#define L_MKSTR(a) #a
#define L_X_MKSTR(a) L_MKSTR(a)
#define L_FILE_LINE __FILE__ " (" L_X_MKSTR(__LINE__) ") "

#undef l_impl_assert_pass
#undef l_impl_assert_fail
#undef l_assert
#undef l_loge_s
#undef l_loge_n
#undef l_loge_1
#undef l_loge_2
#undef l_loge_3
#undef l_loge_4
#undef l_loge_5
#undef l_loge_6
#undef l_loge_7
#undef l_loge_8
#undef l_loge_9
#undef l_logw_s
#undef l_logw_n
#undef l_logw_1
#undef l_logw_2
#undef l_logw_3
#undef l_logw_4
#undef l_logw_5
#undef l_logw_6
#undef l_logw_7
#undef l_logw_8
#undef l_logw_9
#undef l_logm_s
#undef l_logm_n
#undef l_logm_1
#undef l_logm_2
#undef l_logm_3
#undef l_logm_4
#undef l_logm_5
#undef l_logm_6
#undef l_logm_7
#undef l_logm_8
#undef l_logm_9
#undef l_logd_s
#undef l_logd_n
#undef l_logd_1
#undef l_logd_2
#undef l_logd_3
#undef l_logd_4
#undef l_logd_5
#undef l_logd_6
#undef l_logd_7
#undef l_logd_8
#undef l_logd_9

#define l_impl_assert_pass(E,expr) l_impl_logger_func(E, "41[D] " L_FILE_LINE, "assert pass: %s", ls(expr))
#define l_impl_assert_fail(E,expr) l_impl_logger_func(E, "01[E] " L_FILE_LINE, "assert fail: %s", ls(expr))

#define l_assert(E,e) ((e) ? l_impl_assert_pass(E, #e) : l_impl_assert_fail(E, #e)) /* 0:assert */
#define l_loge_s(E,s)                   l_impl_logger_s(E, "10[E] " L_FILE_LINE, (s)) /* 1:error */
#define l_loge_1(E,fmt,a)               l_impl_logger_1(E, "11[E] " L_FILE_LINE, (fmt), a)
#define l_loge_n(E,fmt,n,a)             l_impl_logger_n(E, "1n[E] " L_FILE_LINE, (fmt), n,a)
#define l_loge_2(E,fmt,a,b)             l_impl_logger_2(E, "12[E] " L_FILE_LINE, (fmt), a,b)
#define l_loge_3(E,fmt,a,b,c)           l_impl_logger_3(E, "13[E] " L_FILE_LINE, (fmt), a,b,c)
#define l_loge_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "14[E] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_loge_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "15[E] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_loge_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "16[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_loge_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "17[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_loge_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "18[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_loge_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "19[E] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logw_s(E,s)                   l_impl_logger_s(E, "20[W] " L_FILE_LINE, (s)) /* 2:warning */
#define l_logw_1(E,fmt,a)               l_impl_logger_1(E, "21[W] " L_FILE_LINE, (fmt), a)
#define l_logw_n(E,fmt,n,a)             l_impl_logger_n(E, "2n[W] " L_FILE_LINE, (fmt), n,a)
#define l_logw_2(E,fmt,a,b)             l_impl_logger_2(E, "22[W] " L_FILE_LINE, (fmt), a,b)
#define l_logw_3(E,fmt,a,b,c)           l_impl_logger_3(E, "23[W] " L_FILE_LINE, (fmt), a,b,c)
#define l_logw_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "24[W] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logw_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "25[W] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logw_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "26[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logw_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "27[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logw_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "28[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logw_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "29[W] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logm_s(E,s)                   l_impl_logger_s(E, "30[L] " L_FILE_LINE, (s)) /* 3:main flow */
#define l_logm_1(E,fmt,a)               l_impl_logger_1(E, "31[L] " L_FILE_LINE, (fmt), a)
#define l_logm_n(E,fmt,n,a)             l_impl_logger_n(E, "3n[L] " L_FILE_LINE, (fmt), n,a)
#define l_logm_2(E,fmt,a,b)             l_impl_logger_2(E, "32[L] " L_FILE_LINE, (fmt), a,b)
#define l_logm_3(E,fmt,a,b,c)           l_impl_logger_3(E, "33[L] " L_FILE_LINE, (fmt), a,b,c)
#define l_logm_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "34[L] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logm_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "35[L] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logm_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "36[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logm_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "37[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logm_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "38[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logm_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "39[L] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logd_s(E,s)                   l_impl_logger_s(E, "40[D] " L_FILE_LINE, (s)) /* 4:debug log */
#define l_logd_1(E,fmt,a)               l_impl_logger_1(E, "41[D] " L_FILE_LINE, (fmt), a)
#define l_logd_n(E,fmt,n,a)             l_impl_logger_n(E, "4n[D] " L_FILE_LINE, (fmt), n,a)
#define l_logd_2(E,fmt,a,b)             l_impl_logger_2(E, "42[D] " L_FILE_LINE, (fmt), a,b)
#define l_logd_3(E,fmt,a,b,c)           l_impl_logger_3(E, "43[D] " L_FILE_LINE, (fmt), a,b,c)
#define l_logd_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "44[D] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logd_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "45[D] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logd_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "46[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logd_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "47[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logd_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "48[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logd_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "49[D] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)

#undef ls
#undef lc
#undef lt
#undef lb
#undef lo
#undef lx
#undef lserror

#define ls(s) lp(s)
#define lc(a) ld(a)
#define lt(a) ld(a)
#define lb(a) lu(a)
#define lo(a) lu(a)
#define lx(a) lu(a)
#define lserror(n) ls(strerror(n)) /* include <string.h> first before use it to make "strerror" visible */

typedef union {
  l_long d;
  l_ulong u;
  double f;
  const void* p;
} l_value;

L_INLINE l_value
lp(const void* p)
{
  l_value a; a.p = p; return a;
}

L_INLINE l_value
ld(l_long d)
{
  l_value a; a.d = d; return a;
}

L_INLINE l_value
lu(l_ulong u)
{
  l_value a; a.u = u; return a;
}

L_INLINE l_value
lf(double f)
{
  l_value a; a.f = f; return a;
}

L_INLINE l_value
lstrn(const l_strn* s)
{
  return lp(s);
}

struct lnlylib_env;
L_EXTERN void l_impl_logger_func(struct lnlylib_env* E, const void* tag, const void* fmt, ...);

L_INLINE void
l_impl_logger_s(struct lnlylib_env* E, const void* tag, const void* s)
{
  l_impl_logger_func(E, tag, s, 0);
}

L_INLINE void
l_impl_logger_1(struct lnlylib_env* E, const void* tag, const void* s, l_value a)
{
  l_impl_logger_func(E, tag, s, a);
}

L_INLINE void
l_impl_logger_2(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b)
{
  l_impl_logger_func(E, tag, s, a, b);
}

L_INLINE void
l_impl_logger_3(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c)
{
  l_impl_logger_func(E, tag, s, a, b, c);
}

L_INLINE void
l_impl_logger_4(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d)
{
  l_impl_logger_func(E, tag, s, a, b, c, d);
}

L_INLINE void
l_impl_logger_5(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d, l_value e)
{
  l_impl_logger_func(E, tag, s, a, b, c, d, e);
}

L_INLINE void
l_impl_logger_6(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f)
{
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f);
}

L_INLINE void
l_impl_logger_7(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g)
{
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g);
}

L_INLINE void
l_impl_logger_8(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g, l_value h)
{
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g, h);
}

L_INLINE void
l_impl_logger_9(struct lnlylib_env* E, const void* tag, const void* s, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g, l_value h, l_value i)
{
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g, h, i);
}

L_INLINE void
l_impl_logger_n(struct lnlylib_env* E, const void* tag, const void* s, l_int n, const l_value* a)
{
  l_impl_logger_func(E, tag, s, n, a);
}

/** output stream **/

#define L_HEX         0x01000000
#define L_OCT         0x02000000
#define L_BIN         0x04000000
#define L_BASE_MASK   0x07000000
#define L_PLUS        0x10000000 /* use + as plus sign */
#define L_BLANK       0x20000000 /* blank char as plus sign */
#define L_MINUS       0x40000000
#define L_SIGN_MASK   0x70000000
#define L_LEFT        0x08000000
#define L_NOOX        0x80000000 /* dont append 0b 0o 0x prefix */
#define L_UPPER       0x00800000
#define L_LOWER       0x00008000
#define L_PRECISE     0x00000080
#define L_FORMAT_MASK 0xff808080

#define L_SETP(n) (((n) & 0x7f) << 16) /* precise */
#define L_SETW(n) (((n) & 0x7f) << 8) /* width */
#define L_SETF(n) ((n) & 0x7f) /* fill char */

#define L_GETP(f) ((l_byte)(((f) >> 16) & 0x7f))
#define L_GETW(f) ((l_byte)(((f) >> 8) & 0x7f))
#define L_GETF(f) ((l_byte)((f) & 0x7f))

typedef struct {
  void* out;
  l_int (*write)(void* out, const void* p, l_int n);
} l_ostream;

L_INLINE l_ostream
l_ostream_from(void* out, l_int (*write)(void*, const void*, l_int))
{
  return (l_ostream){out, write};
}

L_INLINE l_int
l_ostream_write(l_ostream* os, const void* p, l_int n)
{
  return os->write(os->out, p, n);
}

L_INLINE l_int
l_ostream_write_strn(l_ostream* os, l_strn s)
{
  return l_ostream_write(os, s.p, s.n);
}

L_EXTERN l_ostream l_stdout_ostream();
L_EXTERN l_ostream l_stderr_ostream();
L_EXTERN l_int l_ostream_format_c(l_ostream* os, int c, l_umedit flags);
L_EXTERN l_int l_ostream_format_d(l_ostream* os, l_long d, l_umedit flags);
L_EXTERN l_int l_ostream_format_u(l_ostream* os, l_ulong u, l_umedit flags);
L_EXTERN l_int l_ostream_format_f(l_ostream* os, double f, l_umedit flags);
L_EXTERN l_int l_ostream_format_s(l_ostream* os, const void* s, l_umedit flags);
L_EXTERN l_int l_ostream_format_strn(l_ostream* os, l_strn s, l_umedit flags);
L_EXTERN l_int l_ostream_format_bool(l_ostream* os, int n, l_umedit flags);
L_EXTERN int l_ostream_format_n(l_ostream* os, const void* fmt, l_int n, const l_value* a);
L_EXTERN int l_impl_ostream_format(l_ostream* os, const void* fmt, l_int n, ...);

L_INLINE int
l_ostream_format_1(l_ostream* os, const void* fmt, l_value a)
{
  return l_impl_ostream_format(os, fmt, 1, a);
}

L_INLINE int
l_ostream_format_2(l_ostream* os, const void* fmt, l_value a, l_value b)
{
  return l_impl_ostream_format(os, fmt, 2, a, b);
}

L_INLINE int
l_ostream_format_3(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c)
{
  return l_impl_ostream_format(os, fmt, 3, a, b, c);
}

L_INLINE int
l_ostream_format_4(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d)
{
  return l_impl_ostream_format(os, fmt, 4, a, b, c, d);
}

L_INLINE int
l_ostream_format_5(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d, l_value e)
{
  return l_impl_ostream_format(os, fmt, 5, a, b, c, d, e);
}

L_INLINE int
l_ostream_format_6(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f)
{
  return l_impl_ostream_format(os, fmt, 6, a, b, c, d, e, f);
}

L_INLINE int
l_ostream_format_7(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g)
{
  return l_impl_ostream_format(os, fmt, 7, a, b, c, d, e, f, g);
}

L_INLINE int
l_ostream_format_8(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g, l_value h)
{
  return l_impl_ostream_format(os, fmt, 8, a, b, c, d, e, f, g, h);
}

L_INLINE int
l_ostream_format_9(l_ostream* os, const void* fmt, l_value a, l_value b, l_value c, l_value d, l_value e, l_value f, l_value g, l_value h, l_value i)
{
  return l_impl_ostream_format(os, fmt, 9, a, b, c, d, e, f, g, h, i);
}

/** fixed length string buffer **/

typedef struct {
  l_uint a[2 + 16 / sizeof(l_uint)];
} l_sbuf16;

typedef struct {
  l_uint a[2 + 32 / sizeof(l_uint)];
} l_sbuf32;

typedef struct {
  l_uint a[2 + 64 / sizeof(l_uint)];
} l_sbuf64;

typedef struct {
  l_uint a[2 + 128 / sizeof(l_uint)];
} l_sbuf12;

typedef struct {
  l_uint a[2 + 256 / sizeof(l_uint)];
} l_sbuf25;

typedef struct {
  l_uint a[2 + 512 / sizeof(l_uint)];
} l_sbuf51;

typedef struct {
  l_uint a[2 + 1024 / sizeof(l_uint)];
} l_sbuf1k;

typedef struct {
  l_uint a[2 + 1024 * 2 / sizeof(l_uint)];
} l_sbuf2k;

typedef struct {
  l_uint a[2 + 1024 * 3 / sizeof(l_uint)];
} l_sbuf3k;

typedef struct {
  l_uint a[2 + 1024 * 4 / sizeof(l_uint)];
} l_sbuf4k;

typedef struct {
  l_uint a[2 + 1024 * 5 / sizeof(l_uint)];
} l_sbuf5k;

typedef struct {
  l_uint a[2 + 1024 * 6 / sizeof(l_uint)];
} l_sbuf6k;

typedef struct {
  l_uint a[2 + 1024 * 7 / sizeof(l_uint)];
} l_sbuf7k;

typedef struct {
  l_uint a[2 + 1024 * 8 / sizeof(l_uint)];
} l_sbuf8k;

typedef struct l_strbuf l_strbuf;

L_EXTERN l_strbuf* l_sbuf16_init(l_sbuf16* b);
L_EXTERN l_strbuf* l_sbuf32_init(l_sbuf32* b);
L_EXTERN l_strbuf* l_sbuf64_init(l_sbuf64* b);
L_EXTERN l_strbuf* l_sbuf12_init(l_sbuf12* b);
L_EXTERN l_strbuf* l_sbuf25_init(l_sbuf25* b);
L_EXTERN l_strbuf* l_sbuf51_init(l_sbuf51* b);
L_EXTERN l_strbuf* l_sbuf1k_init(l_sbuf1k* b);
L_EXTERN l_strbuf* l_sbuf2k_init(l_sbuf2k* b);
L_EXTERN l_strbuf* l_sbuf3k_init(l_sbuf3k* b);
L_EXTERN l_strbuf* l_sbuf4k_init(l_sbuf4k* b);
L_EXTERN l_strbuf* l_sbuf5k_init(l_sbuf5k* b);
L_EXTERN l_strbuf* l_sbuf6k_init(l_sbuf6k* b);
L_EXTERN l_strbuf* l_sbuf7k_init(l_sbuf7k* b);
L_EXTERN l_strbuf* l_sbuf8k_init(l_sbuf8k* b);

L_EXTERN l_strbuf* l_sbuf16_init_from(l_sbuf16* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf32_init_from(l_sbuf32* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf64_init_from(l_sbuf64* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf12_init_from(l_sbuf12* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf25_init_from(l_sbuf25* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf51_init_from(l_sbuf51* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf1k_init_from(l_sbuf1k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf2k_init_from(l_sbuf2k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf3k_init_from(l_sbuf3k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf4k_init_from(l_sbuf4k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf5k_init_from(l_sbuf5k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf6k_init_from(l_sbuf6k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf7k_init_from(l_sbuf7k* b, l_strn s);
L_EXTERN l_strbuf* l_sbuf8k_init_from(l_sbuf8k* b, l_strn s);

L_INLINE l_strbuf* l_sbuf16_p(l_sbuf16* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf32_p(l_sbuf32* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf64_p(l_sbuf64* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf12_p(l_sbuf12* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf25_p(l_sbuf25* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf51_p(l_sbuf51* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf1k_p(l_sbuf1k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf2k_p(l_sbuf2k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf3k_p(l_sbuf3k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf4k_p(l_sbuf4k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf5k_p(l_sbuf5k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf6k_p(l_sbuf6k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf7k_p(l_sbuf7k* b) { return (l_strbuf*)b; }
L_INLINE l_strbuf* l_sbuf8k_p(l_sbuf8k* b) { return (l_strbuf*)b; }

L_EXTERN l_ostream l_strbuf_ostream(l_strbuf* b);
L_EXTERN l_int l_strbuf_write(l_strbuf* b, l_strn s);
L_EXTERN l_int l_strbuf_reset(l_strbuf* b, l_strn s);
L_EXTERN void l_strbuf_clear(l_strbuf* b);
L_EXTERN l_byte* l_strbuf_cstr(l_strbuf* b);
L_EXTERN l_int l_strbuf_size(l_strbuf* b);
L_EXTERN l_strn l_strbuf_strn(l_strbuf* b);
L_EXTERN l_bool l_strbuf_is_empty(l_strbuf* b);
L_EXTERN l_bool l_strbuf_nt_empty(l_strbuf* b);
L_EXTERN l_int l_strbuf_add_path(l_strbuf* b, l_strn path);
L_EXTERN l_int l_strbuf_end_path(l_strbuf* b, l_strn fileanme);
L_EXTERN l_int l_strbuf_end_path_x(l_strbuf* b, l_strn filename_part1, l_strn filename_part2);

/** variable length string **/

typedef struct {
  l_allocfunc alloc;
  l_int implsz;
  l_int impltt;
  l_byte* lnstr;
} l_string;

L_EXTERN l_string l_string_init(l_allocfunc alloc, l_int size);
L_EXTERN l_string l_string_init_from(l_allocfunc alloc, l_int size, l_strn from);
L_EXTERN l_ostream l_string_ostream(l_string* s);
L_EXTERN l_int l_string_capacity(l_string* s);
L_EXTERN l_int l_string_write(l_string* s, l_strn from);
L_EXTERN l_int l_string_reset(l_string* s, l_strn from);
L_EXTERN void l_string_clear(l_string* s);

L_INLINE l_byte*
l_string_cstr(l_string* s)
{
  if (s->implsz <= 0) {
    return (l_byte*)&s->impltt;
  } else {
    return s->lnstr;
  }
}

L_INLINE l_int
l_string_size(l_string* s)
{
  if (s->implsz <= 0) {
    return -s->implsz;
  } else {
    return s->implsz;
  }
}

L_INLINE l_strn
l_string_strn(l_string* s)
{
  return l_strn_l(l_string_cstr(s), l_string_size(s));
}

L_INLINE l_bool
l_string_is_empty(l_string* s)
{
  return s->implsz == 0;
}

L_INLINE l_bool
l_string_nt_empty(l_string* s)
{
  return s->implsz != 0;
}

/** standard file stream **/

typedef struct {
  void* file;
} l_file;

L_EXTERN l_file l_file_open_read(const void* name);
L_EXTERN l_file l_file_open_read_nobuf(const void* name);
L_EXTERN l_file l_file_open_write(const void* name);
L_EXTERN l_file l_file_open_write_nobuf(const void* name);
L_EXTERN l_file l_file_open_append(const void* name);
L_EXTERN l_file l_file_open_append_nobuf(const void* name);
L_EXTERN l_file l_file_open_read_write(const void* name);
L_EXTERN void l_file_close(l_file* s);
L_EXTERN void l_file_clearerr(l_file* s);
L_EXTERN l_bool l_file_flush(l_file* s);
L_EXTERN l_bool l_file_rewind(l_file* s);
L_EXTERN l_bool l_file_seekto(l_file* s, l_int pos);
L_EXTERN l_bool l_file_forward(l_file* s, l_int offset);
L_EXTERN l_bool l_file_backward(l_file* s, l_int offset);
L_EXTERN l_int l_file_read(l_file* s, void* out, l_int size);
L_EXTERN l_int l_file_write(l_file* s, const void* p, l_int len);
L_EXTERN l_int l_file_write_strn(l_file* out, l_strn s);
L_EXTERN l_int l_file_put(l_file* s, l_byte ch);
L_EXTERN l_int l_file_get(l_file* s, l_byte* ch);
L_EXTERN l_bool l_file_remove(const void* name);
L_EXTERN l_bool l_file_rename(const void* from, const void* to);
L_EXTERN void l_file_redirect_stdout(const void* name);
L_EXTERN void l_file_redirect_stderr(const void* name);
L_EXTERN void l_file_redirect_stdin(const void* name);
L_EXTERN l_ostream l_file_ostream(l_file* s);

/** linked node **/

typedef struct l_smplnode {
  struct l_smplnode* next;
} l_smplnode;

L_INLINE void
l_smplnode_init(l_smplnode* node)
{
  node->next = node;
}

L_INLINE int
l_smplnode_is_empty(l_smplnode* node)
{
  return node->next == node;
}

L_INLINE void
l_smplnode_insert_after(l_smplnode* node, l_smplnode* newnode)
{
  newnode->next = node->next;
  node->next = newnode;
}

L_INLINE l_smplnode*
l_smplnode_remove_next(l_smplnode* node)
{
  l_smplnode* p = node->next;
  node->next = p->next;
  return p;
}

typedef struct l_linknode {
  struct l_linknode* next;
  struct l_linknode* prev;
} l_linknode;

L_INLINE void
l_linknode_init(l_linknode* node)
{
  node->next = node->prev = node;
}

L_INLINE int
l_linknode_is_empty(l_linknode* node)
{
  return node->next == node;
}

L_INLINE void
l_linknode_insert_after(l_linknode* node, l_linknode* newnode)
{
  newnode->next = node->next;
  node->next = newnode;
  newnode->prev = node;
  newnode->next->prev = newnode;
}

L_INLINE l_linknode*
l_linknode_remove(l_linknode* node)
{
  node->prev->next = node->next;
  node->next->prev = node->prev;
  return node;
}

/** linked queue **/

typedef struct l_squeue {
  l_smplnode head;
  l_smplnode* tail;
} l_squeue;

L_INLINE void
l_squeue_init(l_squeue* sq)
{
  l_smplnode_init(&sq->head);
  sq->tail = &sq->head;
}

L_INLINE l_bool
l_squeue_is_empty(l_squeue* sq)
{
  return (sq->head.next == &sq->head);
}

L_INLINE l_bool
l_squeue_nt_empty(l_squeue* sq)
{
  return (sq->head.next != &sq->head);
}

L_INLINE void
l_squeue_push(l_squeue* sq, l_smplnode* newnode)
{
  l_smplnode_insert_after(sq->tail, newnode);
  sq->tail = newnode;
}

L_INLINE void
l_squeue_push_queue(l_squeue* self, l_squeue* q)
{
  if (l_squeue_is_empty(q)) {
    return;
  }
  self->tail->next = q->head.next;
  self->tail = q->tail;
  self->tail->next = &self->head;
  l_squeue_init(q);
}

L_INLINE l_squeue
l_squeue_move(l_squeue* q)
{
  l_squeue newq;
  l_squeue_init(&newq);
  l_squeue_push_queue(&newq, q);
  return newq;
}

L_INLINE l_smplnode*
l_squeue_top(l_squeue* sq)
{
  if (l_squeue_is_empty(sq)) {
    return 0;
  } else {
    return sq->head.next;
  }
}

L_INLINE l_smplnode*
l_squeue_pop(l_squeue* sq)
{
  l_smplnode* node = 0;
  if (l_squeue_is_empty(sq)) {
    return 0;
  }
  node = l_smplnode_remove_next(&sq->head);
  if (node == sq->tail) {
    sq->tail = &sq->head;
  }
  return node;
}

typedef struct l_dqueue {
  l_linknode head;
} l_dqueue;

L_INLINE void
l_dqueue_init(l_dqueue* dq)
{
  l_linknode_init(&dq->head);
}

L_INLINE l_bool
l_dqueue_is_empty(l_dqueue* dq)
{
  return dq->head.next == &dq->head;
}

L_INLINE l_bool
l_dqueue_nt_empty(l_dqueue* dq)
{
  return dq->head.next != &dq->head;
}

L_INLINE void
l_dqueue_push(l_dqueue* dq, l_linknode* newnode)
{
  l_linknode_insert_after(&dq->head, newnode);
}

L_INLINE void
l_dqueue_push_queue(l_dqueue* self, l_dqueue* q)
{
  l_linknode* tail = 0;
  if (l_dqueue_is_empty(q)) return;
  /* chain self's tail with q's first element */
  tail = self->head.prev;
  tail->next = q->head.next;
  q->head.next->prev = tail;
  /* chain q's tail with self's head */
  tail = q->head.prev;
  tail->next = &self->head;
  self->head.prev = tail;
  /* init q to empty */
  l_dqueue_init(q);
}

L_INLINE l_dqueue
l_dqueue_move(l_dqueue* q)
{
  l_dqueue newq;
  l_dqueue_init(&newq);
  l_dqueue_push_queue(&newq, q);
  return newq;
}

L_INLINE l_linknode*
l_dqueue_pop(l_dqueue* dq)
{
  if (l_dqueue_is_empty(dq)) return 0;
  return l_linknode_remove(dq->head.prev);
}

#endif /* LNLYLIB_CORE_BASE_H */

