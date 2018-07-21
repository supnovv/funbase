#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

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

#define l_impl_assert_pass(E, expr) l_impl_logger_func(E, "41[D] " L_FILE_LINE, "assert pass: %s", ls(expr))
#define l_impl_assert_fail(E, expr) l_impl_logger_func(E, "01[E] " L_FILE_LINE, "assert fail: %s", ls(expr))

#define l_assert(E, e) ((e) ? l_impl_assert_pass(E, #e) : l_impl_assert_fail(E, #e)) /* 0:assert */
#define l_loge_s(E, s)                   l_impl_logger_s(E, "10[E] " L_FILE_LINE, (s)) /* 1:error */
#define l_loge_1(E, fmt,a)               l_impl_logger_1(E, "11[E] " L_FILE_LINE, (fmt), a)
#define l_loge_n(E, fmt,n,a)             l_impl_logger_n(E, "1n[E] " L_FILE_LINE, (fmt), n,a)
#define l_loge_2(E, fmt,a,b)             l_impl_logger_2(E, "12[E] " L_FILE_LINE, (fmt), a,b)
#define l_loge_3(E, fmt,a,b,c)           l_impl_logger_3(E, "13[E] " L_FILE_LINE, (fmt), a,b,c)
#define l_loge_4(E, fmt,a,b,c,d)         l_impl_logger_4(E, "14[E] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_loge_5(E, fmt,a,b,c,d,e)       l_impl_logger_5(E, "15[E] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_loge_6(E, fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "16[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_loge_7(E, fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "17[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_loge_8(E, fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "18[E] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_loge_9(E, t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "19[E] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logw_s(E, s)                   l_impl_logger_s(E, "20[W] " L_FILE_LINE, (s)) /* 2:warning */
#define l_logw_1(E, fmt,a)               l_impl_logger_1(E, "21[W] " L_FILE_LINE, (fmt), a)
#define l_logw_n(E, fmt,n,a)             l_impl_logger_n(E, "2n[W] " L_FILE_LINE, (fmt), n,a)
#define l_logw_2(E, fmt,a,b)             l_impl_logger_2(E, "22[W] " L_FILE_LINE, (fmt), a,b)
#define l_logw_3(E, fmt,a,b,c)           l_impl_logger_3(E, "23[W] " L_FILE_LINE, (fmt), a,b,c)
#define l_logw_4(E, fmt,a,b,c,d)         l_impl_logger_4(E, "24[W] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logw_5(E, fmt,a,b,c,d,e)       l_impl_logger_5(E, "25[W] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logw_6(E, fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "26[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logw_7(E, fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "27[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logw_8(E, fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "28[W] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logw_9(E, t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "29[W] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logm_s(E, s)                   l_impl_logger_s(E, "30[L] " L_FILE_LINE, (s)) /* 3:main flow */
#define l_logm_1(E, fmt,a)               l_impl_logger_1(E, "31[L] " L_FILE_LINE, (fmt), a)
#define l_logm_n(E, fmt,n,a)             l_impl_logger_n(E, "3n[L] " L_FILE_LINE, (fmt), n,a)
#define l_logm_2(E, fmt,a,b)             l_impl_logger_2(E, "32[L] " L_FILE_LINE, (fmt), a,b)
#define l_logm_3(E, fmt,a,b,c)           l_impl_logger_3(E, "33[L] " L_FILE_LINE, (fmt), a,b,c)
#define l_logm_4(E, fmt,a,b,c,d)         l_impl_logger_4(E, "34[L] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logm_5(E, fmt,a,b,c,d,e)       l_impl_logger_5(E, "35[L] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logm_6(E, fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "36[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logm_7(E, fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "37[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logm_8(E, fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "38[L] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logm_9(E, t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "39[L] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
#define l_logd_s(E, s)                   l_impl_logger_s(E, "40[D] " L_FILE_LINE, (s)) /* 4:debug log */
#define l_logd_1(E, fmt,a)               l_impl_logger_1(E, "41[D] " L_FILE_LINE, (fmt), a)
#define l_logd_n(E, fmt,n,a)             l_impl_logger_n(E, "4n[D] " L_FILE_LINE, (fmt), n,a)
#define l_logd_2(E, fmt,a,b)             l_impl_logger_2(E, "42[D] " L_FILE_LINE, (fmt), a,b)
#define l_logd_3(E, fmt,a,b,c)           l_impl_logger_3(E, "43[D] " L_FILE_LINE, (fmt), a,b,c)
#define l_logd_4(E, fmt,a,b,c,d)         l_impl_logger_4(E, "44[D] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logd_5(E, fmt,a,b,c,d,e)       l_impl_logger_5(E, "45[D] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logd_6(E, fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "46[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logd_7(E, fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "47[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logd_8(E, fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "48[D] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logd_9(E, t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "49[D] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)

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

struct lnlylib_env;
extern void l_impl_logger_func(struct lnlylib_env* E, const void* tag, const void* fmt, ...);

L_INLINE void
l_impl_logger_s(struct lnlylib_env* E, const void* tag, const void* s) {
  l_impl_logger_func(E, tag, s, 0);
}

L_INLINE void
l_impl_logger_1(struct lnlylib_env* E, const void* tag, const void* s, l_logval a) {
  l_impl_logger_func(E, tag, s, a);
}

L_INLINE void
l_impl_logger_2(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b) {
  l_impl_logger_func(E, tag, s, a, b);
}

L_INLINE void
l_impl_logger_3(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c) {
  l_impl_logger_func(E, tag, s, a, b, c);
}

L_INLINE void
l_impl_logger_4(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d) {
  l_impl_logger_func(E, tag, s, a, b, c, d);
}

L_INLINE void
l_impl_logger_5(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e) {
  l_impl_logger_func(E, tag, s, a, b, c, d, e);
}

L_INLINE void
l_impl_logger_6(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f) {
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f);
}

L_INLINE void
l_impl_logger_7(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g) {
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g);
}

L_INLINE void
l_impl_logger_8(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g, l_logval h) {
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g, h);
}

L_INLINE void
l_impl_logger_9(struct lnlylib_env* E, const void* tag, const void* s, l_logval a, l_logval b, l_logval c, l_logval d,
    l_logval e, l_logval f, l_logval g, l_logval h, l_logval i) {
  l_impl_logger_func(E, tag, s, a, b, c, d, e, f, g, h, i);
}

L_INLINE void
l_impl_logger_n(struct lnlylib_env* E, const void* tag, const void* s, l_int n, const l_logval* a) {
  l_impl_logger_func(E, tag, s, n, a);
}

struct l_global;
struct l_worker;
struct l_service;
struct l_message;

typedef struct {
  const char* service_name;
  void* (*service_on_create)(struct lnlylib_env*);
  void (*service_on_destroy)(struct lnlylib_env*);
  void (*service_proc)(struct lnlylib_env*);
} l_service_callback;

L_EXTERN void l_send_message(struct lnlylib_env* E, l_uint dest_svid, l_uint mgid, l_umedit flags, void* data, l_umedit size);
L_EXTERN void l_create_service(struct lnlylib_env* E, l_service_callback cb, l_uint flags);
L_EXTERN void l_create_service_from_module(struct lnlylib_env* E, l_strn module_name, l_uint flags);
L_EXTERN void l_stop_service_specific(struct lnlylib_env* E, l_uint svid);
L_EXTERN void l_stop_service(struct lnlylib_env* E);

L_EXTERN int lnlylib_main(void (*start)(void), int argc, char** argv);

#endif /* LNLYLIB_CORE_BEAT_H */

