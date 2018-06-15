#ifndef L_CORE_BASE_H
#define L_CORE_BASE_H
#include "osi/autoconf.h"
#include "osi/prefix.h"

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

#undef L_SUCCESS
#undef L_ERROR
#undef L_EINVAL
#define L_SUCCESS (0)
#define L_ERROR (-1)
#define L_EINVAL (-2)

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

extern void l_impl_logger_func(const void* tag, const void* fmt, ...);
#define l_impl_assert_pass(expr) l_impl_logger_func("41[D] " L_FILE_LINE, "assert pass: %s", ls(expr))
#define l_impl_assert_fail(expr) l_impl_logger_func("01[E] " L_FILE_LINE, "assert fail: %s", ls(expr))

#define l_assert(e) ((e) ? l_impl_assert_pass(#e) : l_impl_assert_fail(#e)) /* 0:assert */
#define l_loge_s(s)                   l_impl_logger_s("10[E] " L_FILE_LINE, (s)) /* 1:error */
#define l_loge_1(fmt,a)               l_impl_logger_1("11[E] " L_FILE_LINE, (fmt), a)
#define l_loge_n(fmt,n,a)             l_impl_logger_n("1n[E] " L_FILE_LINE, (fmt), n,a)
#define l_loge_2(fmt,a,b)             l_impl_logger_2("12[E] " L_FILE_LINE, (fmt), a,b)
#define l_loge_3(fmt,a,b,c)           l_impl_logger_3("13[E] " L_FILE_LINE, (fmt), a,b,c)
#define l_loge_4(fmt,a,b,c,d)         l_impl_logger_4("14[E] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_loge_5(fmt,a,b,c,d,e)       l_impl_logger_5("15[E] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_loge_6(fmt,a,b,c,d,e,f)     l_impl_logger_6("16[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_loge_7(fmt,a,b,c,d,e,f,g)   l_impl_logger_7("17[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_loge_8(fmt,a,b,c,d,e,f,g,h) l_impl_logger_8("18[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_loge_9(t,a,b,c,d,e,f,g,h,i) l_impl_logger_9("19[E] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logw_s(s)                   l_impl_logger_s("20[W] " L_FILE_LINE, (s)) /* 2:warning */
#define l_logw_1(fmt,a)               l_impl_logger_1("21[W] " L_FILE_LINE, (fmt), a)
#define l_logw_n(fmt,n,a)             l_impl_logger_n("2n[W] " L_FILE_LINE, (fmt), n,a)
#define l_logw_2(fmt,a,b)             l_impl_logger_2("22[W] " L_FILE_LINE, (fmt), a,b)
#define l_logw_3(fmt,a,b,c)           l_impl_logger_3("23[W] " L_FILE_LINE, (fmt), a,b,c)
#define l_logw_4(fmt,a,b,c,d)         l_impl_logger_4("24[W] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logw_5(fmt,a,b,c,d,e)       l_impl_logger_5("25[W] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logw_6(fmt,a,b,c,d,e,f)     l_impl_logger_6("26[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logw_7(fmt,a,b,c,d,e,f,g)   l_impl_logger_7("27[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logw_8(fmt,a,b,c,d,e,f,g,h) l_impl_logger_8("28[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logw_9(t,a,b,c,d,e,f,g,h,i) l_impl_logger_9("29[W] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logm_s(s)                   l_impl_logger_s("30[L] " L_FILE_LINE, (s)) /* 3:main flow */
#define l_logm_1(fmt,a)               l_impl_logger_1("31[L] " L_FILE_LINE, (fmt), a)
#define l_logm_n(fmt,n,a)             l_impl_logger_n("3n[L] " L_FILE_LINE, (fmt), n,a)
#define l_logm_2(fmt,a,b)             l_impl_logger_2("32[L] " L_FILE_LINE, (fmt), a,b)
#define l_logm_3(fmt,a,b,c)           l_impl_logger_3("33[L] " L_FILE_LINE, (fmt), a,b,c)
#define l_logm_4(fmt,a,b,c,d)         l_impl_logger_4("34[L] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logm_5(fmt,a,b,c,d,e)       l_impl_logger_5("35[L] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logm_6(fmt,a,b,c,d,e,f)     l_impl_logger_6("36[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logm_7(fmt,a,b,c,d,e,f,g)   l_impl_logger_7("37[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logm_8(fmt,a,b,c,d,e,f,g,h) l_impl_logger_8("38[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logm_9(t,a,b,c,d,e,f,g,h,i) l_impl_logger_9("39[L] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logd_s(s)                   l_impl_logger_s("40[D] " L_FILE_LINE, (s)) /* 4:debug log */
#define l_logd_1(fmt,a)               l_impl_logger_1("41[D] " L_FILE_LINE, (fmt), a)
#define l_logd_n(fmt,n,a)             l_impl_logger_n("4n[D] " L_FILE_LINE, (fmt), n,a)
#define l_logd_2(fmt,a,b)             l_impl_logger_2("42[D] " L_FILE_LINE, (fmt), a,b)
#define l_logd_3(fmt,a,b,c)           l_impl_logger_3("43[D] " L_FILE_LINE, (fmt), a,b,c)
#define l_logd_4(fmt,a,b,c,d)         l_impl_logger_4("44[D] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logd_5(fmt,a,b,c,d,e)       l_impl_logger_5("45[D] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logd_6(fmt,a,b,c,d,e,f)     l_impl_logger_6("46[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logd_7(fmt,a,b,c,d,e,f,g)   l_impl_logger_7("47[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logd_8(fmt,a,b,c,d,e,f,g,h) l_impl_logger_8("48[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logd_9(t,a,b,c,d,e,f,g,h,i) l_impl_logger_9("49[D] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)

typedef struct {
  const l_byte* start;
  l_int len;
} l_strn;

#undef l_strc
#undef l_strn_literal
#undef l_strn_zero

#define l_strc(s) ((l_byte*)(s)) /* zero terminated c string */
#define l_strn_literal(s) ((l_strn){l_strc("" s), sizeof(s) - 1})
#define l_strn_zero() ((l_strn){0, 0})

L_INLINE l_strn
l_strn_l(const void* s, l_int len) {
  return (l_strn){l_strc(s), len > 0 ? len : 0};
}

L_INLINE l_strn
l_strn_c(const void* s) { /* include <string.h> first before use it to make "strlen" visible */
  return (l_strn){l_strc(s), s ? strlen((char*)s) : 0};
}

L_INLINE l_strn
l_strn_p(const void* s, const void* e) {
  return l_strn_l(s, l_strc(e) - l_strc(s));
}

L_INLINE l_strn
l_strn_s(const void* s, l_int m, l_int n) {
  return l_strn_l(l_strc(s) + m, n - m);
}

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
} l_logval;

L_INLINE l_logval
lp(const void* p) {
  l_logval a; a.p = p; return a;
}

L_INLINE l_logval
ld(l_long d) {
  l_logval a; a.d = d; return a;
}

L_INLINE l_logval
lu(l_ulong u) {
  l_logval a; a.u = u; return a;
}

L_INLINE l_logval
lf(double f) {
  l_logval a; a.f = f; return a;
}

L_INLINE l_logval
lstrn(const l_strn* s) {
  return lp(s);
}

L_INLINE void
l_impl_logger_s(const void* tag, const void* s) {
  l_impl_logger_func(tag, s, 0);
}

L_INLINE void
l_impl_logger_1(const void* tag, const void* s, l_logval a) {
  l_impl_logger_func(tag, s, a);
}

L_INLINE void
l_impl_logger_2(const void* tag, const void* s, l_logval a, l_logval b) {
  l_impl_logger_func(tag, s, a, b);
}

L_INLINE void
l_impl_logger_3(const void* tag, const void* s, l_logval a, l_logval b, l_logval c) {
  l_impl_logger_func(tag, s, a, b, c);
}

L_INLINE void
l_impl_logger_4(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d) {
  l_impl_logger_func(tag, s, a, b, c, d);
}

L_INLINE void
l_impl_logger_5(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e) {
  l_impl_logger_func(tag, s, a, b, c, d, e);
}

L_INLINE void
l_impl_logger_6(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f) {
  l_impl_logger_func(tag, s, a, b, c, d, e, f);
}

L_INLINE void
l_impl_logger_7(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g) {
  l_impl_logger_func(tag, s, a, b, c, d, e, f, g);
}

L_INLINE void
l_impl_logger_8(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g, l_logval h) {
  l_impl_logger_func(tag, s, a, b, c, d, e, f, g, h);
}

L_INLINE void
l_impl_logger_9(const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g, l_logval h, l_logval i) {
  l_impl_logger_func(tag, s, a, b, c, d, e, f, g, h, i);
}

L_INLINE void
l_impl_logger_n(const void* tag, const void* s, l_int n, const l_logval* a) {
  l_impl_logger_func(tag, s, n, a);
}

#endif /* L_CORE_BASE_H */

