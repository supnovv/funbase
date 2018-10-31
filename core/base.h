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

#undef LNUL
#define LNUL (0)

#undef L_UNUSED
#define L_UNUSED(a) ((void)a)

typedef enum {
  L_STATUS_OK,
  L_STATUS_ERROR = (l_int)-10,
  L_STATUS_EARGS = L_STATUS_ERROR - 1,
  L_STATUS_EREAD = L_STATUS_ERROR - 2,
  L_STATUS_EWRITE = L_STATUS_ERROR - 3,
  L_STATUS_EAGAIN = L_STATUS_ERROR - 4,
  L_STATUS_ETIMEOUT = L_STATUS_ERROR - 5
} l_error;

typedef struct {
  const l_byte* p;
  l_int n;
} l_strn;

#undef l_strc
#undef l_strn_c
#undef l_empty_strn
#undef l_literal_strn

#define l_strc(s) ((l_byte*)(s)) /* zero terminated c string */
#define l_strn_c(s) ((l_strn){l_strc(s), (s) ? strlen((char*)(s)) : 0})
#define l_empty_strn() l_literal_strn("")
#define l_literal_strn(s) ((l_strn){l_strc("" s), sizeof(s) - 1})

L_EXTERN l_bool l_strn_equal(const l_strn* a, l_strn b);
L_EXTERN l_bool l_strn_equal_p(const l_strn* a, const l_strn* b);
L_EXTERN l_bool l_strn_contains(const l_strn* a, l_byte c);
L_EXTERN l_int l_strn_find(const l_strn* a, l_byte c); /* < 0 not found, >= 0 found */

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
  return (l_strn){l_strc(s), len > 0 ? len : 0};
}

L_INLINE l_strn
l_strn_p(const void* s, const void* e)
{
  return l_strn_l(s, l_strc(e) - l_strc(s));
}

L_INLINE l_strn
l_strn_s(const void* s, l_int m, l_int n)
{
  return l_strn_l(l_strc(s) + m, n - m);
}

typedef struct {
  const l_byte* s;
  const l_byte* e;
} l_strp;

#define l_strp_c(s) l_strp_l(l_strc(s), (s) ? strlen((char*)(s)) : 0)
#define l_empty_strp() l_literal_strp("")
#define l_literal_strp(s) l_strp_l(l_strc("" s), sizeof(s) - 1)

L_INLINE l_strp
l_strp_l(const void* p, l_int len)
{
  l_strp s;
  s.s = l_strc(p);
  s.e = s.s + (len > 0 ? len : 0);
  return s;
}

#define L_NUL '\0'   /* \x00 Null */
#define L_BEL '\a'   /* \x07 Bell */
#define L_BS  '\b'   /* \x08 Backspace */
#define L_HT  '\t'   /* \x09 Horizontal Tab */
#define L_LF  '\n'   /* \x0A Line Feed */
#define L_VT  '\v'   /* \x0B Vertical Tab */
#define L_FF  '\f'   /* \x0C Form Feed */
#define L_CR  '\r'   /* \x0D Carriage Return */
#define L_ESC '\x1B' /* \x1B Escape */
#define L_SP  '\x20' /* \x20 Space */
#define L_DEL '\x7F' /* \x7F Delete */
#define L_TAB L_HT

#define L_S_HT  "\t"   /* \x09 Horizontal Tab */
#define L_S_LF  "\n"   /* \x0A Line Feed */
#define L_S_VT  "\v"   /* \x0B Vertical Tab */
#define L_S_FF  "\f"   /* \x0C Form Feed */
#define L_S_CR  "\r"   /* \x0D Carriage Return */
#define L_S_SP  "\x20" /* \x20 Space */
#define L_S_TAB L_S_HT

L_EXTERN l_bool l_is_lower(l_byte ch);
L_EXTERN l_bool l_is_upper(l_byte ch);
L_EXTERN l_bool l_is_letter(l_byte ch);
L_EXTERN l_bool l_is_printable(l_byte ch);
L_EXTERN l_bool l_is_digit(l_byte ch);
L_EXTERN l_bool l_is_hex_digit(l_byte ch);
L_EXTERN l_bool l_is_alphanum(l_byte ch);
L_EXTERN l_bool l_is_alphanum_underscore(l_byte ch);
L_EXTERN l_bool l_is_alphanum_underscore_hyphen(l_byte ch);
L_EXTERN l_int l_dec_string_to_int(l_strn s);
L_EXTERN l_int l_hex_string_to_int(l_strn s);

L_INLINE l_bool
l_nt_lower(l_byte ch)
{
  return !l_is_lower(ch);
}

L_INLINE l_bool
l_nt_upper(l_byte ch)
{
  return !l_is_upper(ch);
}

L_INLINE l_bool
l_nt_letter(l_byte ch)
{
  return !l_is_letter(ch);
}

L_INLINE l_bool
l_nt_printable(l_byte ch)
{
  return !l_is_printable(ch);
}

L_INLINE l_bool
l_nt_digit(l_byte ch)
{
  return !l_is_digit(ch);
}

L_INLINE l_bool
l_nt_hex_digit(l_byte ch)
{
  return !l_is_hex_digit(ch);
}

L_INLINE l_bool
l_nt_alphanum(l_byte ch)
{
  return !l_is_alphanum(ch);
}

L_INLINE l_bool
l_nt_alphanum_underscore(l_byte ch)
{
  return !l_is_alphanum_underscore(ch);
}

L_INLINE l_bool
l_nt_alphanum_underscore_hyphen(l_byte ch)
{
  return !l_is_alphanum_underscore_hyphen(ch);
}

L_INLINE l_byte
l_to_lower(l_byte ch)
{
  return l_is_upper(ch) ? ch + 32 : ch;
}

L_INLINE l_byte
l_to_upper(l_byte ch)
{
  return l_is_lower(ch) ? ch - 32 : ch;
}

L_INLINE l_byte
l_flip_case(l_byte ch)
{
  return l_is_upper(ch) ? ch + 32 : (l_is_lower(ch) ? ch - 32 : ch);
}

#define l_enlarge_to_times_of_2(n)    ((((n - 1) >> 1) + 1) << 1) /* n should > 0 */
#define l_enlarge_to_times_of_4(n)    ((((n - 1) >> 2) + 1) << 2) /* n should > 0 */
#define l_enlarge_to_times_of_8(n)    ((((n - 1) >> 3) + 1) << 3) /* n should > 0 */
#define l_enlarge_to_times_of_16(n)   ((((n - 1) >> 4) + 1) << 4) /* n should > 0 */
#define l_enlarge_to_times_of_32(n)   ((((n - 1) >> 5) + 1) << 5) /* n should > 0 */
#define l_enlarge_to_times_of_64(n)   ((((n - 1) >> 6) + 1) << 6) /* n should > 0 */
#define l_enlarge_to_times_of_128(n)  ((((n - 1) >> 7) + 1) << 7) /* n should > 0 */
#define l_enlarge_to_times_of_256(n)  ((((n - 1) >> 8) + 1) << 8) /* n should > 0 */
#define l_enlarge_to_times_of_512(n)  ((((n - 1) >> 9) + 1) << 9) /* n should > 0 */
#define l_enlarge_to_times_of_1024(n) ((((n - 1) >> 10) + 1) << 10) /* n should > 0 */
#define l_enlarge_to_times_of_2048(n) ((((n - 1) >> 11) + 1) << 11) /* n should > 0 */
#define l_enlarge_to_times_of_4096(n) ((((n - 1) >> 12) + 1) << 12) /* n should > 0 */
#define l_enlarge_to_times_of_8192(n) ((((n - 1) >> 13) + 1) << 13) /* n should > 0 */

#define l_lower_most_bit(n) (n & (-n))
#define l_remove_lower_most_bit(n) (n & (n - 1))
#define l_is_power_of_two_int(n) (l_remote_lower_most_bit(n) == 0)

#define l_bit_belong_which_byte(which_bit_0_n) (which_bit_0_n >> 3)
#define l_bit_belong_which_ushort(which_bit_0_n) (which_bit_0_n >> 4)
#define l_bit_belong_which_umedit(which_bit_0_n) (which_bit_0_n >> 5)
#define l_bit_belong_which_ulong(which_bit_0_n) (which_bit_0_n >> 6)

#define l_bit_of_byte_need_moved_bits_to_high(which_bit_0_7) (7 - (which_bit_0_7 & 7))
#define l_bit_of_ushort_need_moved_bits_to_high(which_bit_0_15) (15 - (which_bit_0_15 & 15))
#define l_bit_of_umedit_need_moved_bits_to_high(which_bit_0_31) (31 - (which_bit_0_31 & 31))
#define l_bit_of_ulong_need_moved_bits_to_high(which_bit_0_63) (63 - (which_bit_0_63 & 63))

#define l_bit_mask_of_byte(which_bit_0_7) (((l_byte)1) << (which_bit_0_7 & 7))
#define l_bit_mask_of_ushort(which_bit_0_15) (((l_ushort)1) << (which_bit_0_15 & 15))
#define l_bit_mask_of_umedit(which_bit_0_31) (((l_umedit)1) << (which_bit_0_31 & 31))
#define l_bit_mask_of_ulong(which_bit_0_63) (((l_ulong)1) << (which_bit_0_63 & 63))

L_EXTERN l_byte l_bit_1_count_of_byte(l_byte ch);
L_EXTERN l_int l_bit_pos_of_power_of_two(l_ulong x); /* x should > 0 and be 2^n, return n */

#undef l_malloc
#undef l_ralloc
#undef l_mfree
#undef L_MALLOC_TYPE
#undef L_MALLOC_TYPE_N

#define l_malloc(E, size) l_alloc_func((E), 0, 0, (size))
#define l_ralloc(E, p, newsz) l_alloc_func((E), (p), 0, (newsz))
#define l_mfree(E, p) l_alloc_func((E), (p), 0, 0)
#define L_MALLOC_TYPE(E, type) (type*)l_malloc((E), sizeof(type))
#define L_MALLOC_TYPE_N(E, type, n) (type*)l_malloc((E), sizeof(type) * (n))

typedef void* (*l_allocfunc)(void* ud, void* p, l_ulong oldsz, l_ulong newsz);
L_EXTERN l_allocfunc l_alloc_func; /* note the allocated memory is not initialized */
L_EXTERN l_bool l_zero_n(void* p, l_ulong size);
L_EXTERN l_ulong l_copy_n(void* dest, const void* from, l_ulong size);

#undef L_MKSTR
#undef L_X_MKSTR
#undef L_FILE_LINE

#define L_MKSTR(a) #a
#define L_X_MKSTR(a) L_MKSTR(a)
#define L_FILE_LINE __FILE__ " (" L_X_MKSTR(__LINE__) ")"

#undef l_impl_assert_pass
#undef l_impl_assert_fail
#undef l_assert_s
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

#define l_impl_assert_pass(E,expr) l_impl_logger_func(E, "51[V] " L_FILE_LINE, "assert pass: %s", ls(expr))
#define l_impl_assert_fail(E,expr) l_impl_logger_func(E, "01[F] " L_FILE_LINE, "assert fail: %s", ls(expr))
#define l_impl_assert_s_fail(E,expr,s) l_impl_logger_func(E, "02[F] " L_FILE_LINE, "assert fail - %s: %s", ls(s), ls(expr))
#define l_impl_assert_eq_c_fail(E,expr,a,b) l_impl_logger_func(E, "03[F] " L_FILE_LINE, "assert fail - %c != %c - %s", lc(a), lc(b), ls(expr))
#define l_impl_assert_eq_d_fail(E,expr,a,b) l_impl_logger_func(E, "03[F] " L_FILE_LINE, "assert fail - %d != %d - %s", ld(a), ld(b), ls(expr))
#define l_impl_assert_eq_x_fail(E,expr,a,b) l_impl_logger_func(E, "03[F] " L_FILE_LINE, "assert fail - %x != %x - %s", lx(a), lx(b), ls(expr))
#define l_impl_assert_eq_p_fail(E,expr,a,b) l_impl_logger_func(E, "03[F] " L_FILE_LINE, "assert fail - %p != %p - %s", lp(a), lp(b), ls(expr))
#define l_impl_assert_eq_f_fail(E,expr,a,b) l_impl_logger_func(E, "03[F] " L_FILE_LINE, "assert fail - %f != %f - %s", lf(a), lf(b), ls(expr))

#define l_assert(E,e) ((e) ? l_impl_assert_pass(E, #e) : l_impl_assert_fail(E, #e)) /* 0: assert or fatal */
#define l_assert_s(E,e,s) ((e) ? l_impl_assert_pass(E, #e) : l_impl_assert_s_fail(E, #e, (s)))
#define l_assert_eq_c(E,a,b) (((a)==(b)) ? l_impl_assert_pass(E, #a " == " #b) : l_impl_assert_eq_c_fail(E, #a " == " #b, (a), (b))
#define l_assert_eq_d(E,a,b) (((a)==(b)) ? l_impl_assert_pass(E, #a " == " #b) : l_impl_assert_eq_d_fail(E, #a " == " #b, (a), (b))
#define l_assert_eq_x(E,a,b) (((a)==(b)) ? l_impl_assert_pass(E, #a " == " #b) : l_impl_assert_eq_x_fail(E, #a " == " #b, (a), (b))
#define l_assert_eq_p(E,a,b) (((a)==(b)) ? l_impl_assert_pass(E, #a " == " #b) : l_impl_assert_eq_p_fail(E, #a " == " #b, (a), (b))
#define l_assert_eq_f(E,a,b) (((a)==(b)) ? l_impl_assert_pass(E, #a " == " #b) : l_impl_assert_eq_f_fail(E, #a " == " #b, (a), (b))

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
#define l_logm_s(E,s)                   l_impl_logger_s(E, "30[m] " L_FILE_LINE, (s)) /* 3:main flow */
#define l_logm_1(E,fmt,a)               l_impl_logger_1(E, "31[m] " L_FILE_LINE, (fmt), a)
#define l_logm_n(E,fmt,n,a)             l_impl_logger_n(E, "3n[m] " L_FILE_LINE, (fmt), n,a)
#define l_logm_2(E,fmt,a,b)             l_impl_logger_2(E, "32[m] " L_FILE_LINE, (fmt), a,b)
#define l_logm_3(E,fmt,a,b,c)           l_impl_logger_3(E, "33[m] " L_FILE_LINE, (fmt), a,b,c)
#define l_logm_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "34[m] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logm_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "35[m] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logm_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "36[m] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logm_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "37[m] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logm_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "38[m] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logm_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "39[m] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)
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
#define l_logv_s(E,s)                   l_impl_logger_s(E, "50[V] " L_FILE_LINE, (s)) /* 5:verbose */
#define l_logv_1(E,fmt,a)               l_impl_logger_1(E, "51[V] " L_FILE_LINE, (fmt), a)
#define l_logv_n(E,fmt,n,a)             l_impl_logger_n(E, "5n[V] " L_FILE_LINE, (fmt), n,a)
#define l_logv_2(E,fmt,a,b)             l_impl_logger_2(E, "52[V] " L_FILE_LINE, (fmt), a,b)
#define l_logv_3(E,fmt,a,b,c)           l_impl_logger_3(E, "53[V] " L_FILE_LINE, (fmt), a,b,c)
#define l_logv_4(E,fmt,a,b,c,d)         l_impl_logger_4(E, "54[V] " L_FILE_LINE, (fmt), a,b,c,d)
#define l_logv_5(E,fmt,a,b,c,d,e)       l_impl_logger_5(E, "55[V] " L_FILE_LINE, (fmt), a,b,c,d,e)
#define l_logv_6(E,fmt,a,b,c,d,e,f)     l_impl_logger_6(E, "56[V] " L_FILE_LINE, (fmt), a,b,c,d,e,f)
#define l_logv_7(E,fmt,a,b,c,d,e,f,g)   l_impl_logger_7(E, "57[V] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g)
#define l_logv_8(E,fmt,a,b,c,d,e,f,g,h) l_impl_logger_8(E, "58[V] " L_FILE_LINE, (fmt), a,b,c,d,e,f,g,h)
#define l_logv_9(E,t,a,b,c,d,e,f,g,h,i) l_impl_logger_9(E, "59[V] " L_FILE_LINE, (t), a,b,c,d,e,f,g,h,i)

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

L_INLINE l_value
lsvid(l_ulong svid)
{
  return lx(svid);
}

struct lnlylib_env;
L_EXTERN int l_set_log_level(int n);
L_EXTERN void l_flush_logging(struct lnlylib_env* E);
L_EXTERN void l_impl_logger_func(struct lnlylib_env* E, const void* tag, const void* fmt, ...);

L_INLINE void
l_impl_logger_s(struct lnlylib_env* E, const void* tag, const void* s)
{
  l_impl_logger_func(E, tag, s, ls(""));
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
  if (n <= 0 || p == 0) {
    return 0;
  } else {
    return os->write(os->out, p, n);
  }
}

L_INLINE l_int
l_ostream_write_strn(l_ostream* os, l_strn s)
{
  return l_ostream_write(os, s.p, s.n);
}

L_EXTERN l_bool l_ostream_should_flush(const void* p, l_int n);
L_EXTERN void l_ostream_flush(l_ostream* os);

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

L_INLINE l_int
l_ostream_format_0(l_ostream* os, const void* s)
{
  return l_ostream_write_strn(os, l_strn_c(s));
}

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
L_EXTERN const l_byte* l_strbuf_strc(l_strbuf* b);
L_EXTERN l_int l_strbuf_size(l_strbuf* b);
L_EXTERN l_int l_strbuf_capacity(l_strbuf* b);
L_EXTERN l_byte* l_strbuf_getp(l_strbuf* b);
L_EXTERN void l_strbuf_add_len(l_strbuf* b, l_int n);
L_EXTERN void l_strbuf_adjust_len(l_strbuf* b);
L_EXTERN l_strn l_strbuf_strn(l_strbuf* b);
L_EXTERN l_bool l_strbuf_is_empty(l_strbuf* b);
L_EXTERN l_bool l_strbuf_nt_empty(l_strbuf* b);
L_EXTERN l_int l_strbuf_add_path(l_strbuf* b, l_strn path);
L_EXTERN l_int l_strbuf_end_path(l_strbuf* b, l_strn fileanme);
L_EXTERN l_int l_strbuf_end_path_x(l_strbuf* b, l_strn name_parta, l_strn name_partb);

typedef struct {
  l_int capacity;
  l_int size;
  l_byte* start;
} l_strmanip;

L_INLINE l_strmanip
l_get_strmanip(void* buffer_start, l_int total_size, l_int cur_size)
{
  return (l_strmanip){total_size < 0 ? 0 : total_size, cur_size < 0 ? 0 : cur_size, buffer_start};
}

L_EXTERN l_ostream l_strmanip_ostream(l_strmanip* b);

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
l_string_strc(l_string* s)
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
  return l_strn_l(l_string_strc(s), l_string_size(s));
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

typedef struct {
  void* file;
} l_file;

L_INLINE l_bool
l_file_is_open(const l_file* s)
{
  return s->file != 0;
}

L_INLINE l_bool
l_file_nt_open(const l_file* s)
{
  return s->file == 0;
}

L_EXTERN l_file l_file_open_read(const void* name); /* the file must exist */
L_EXTERN l_file l_file_open_read_nobuf(const void* name); /* the file must exist */
L_EXTERN l_file l_file_open_write(const void* name); /* create new file if not exist */
L_EXTERN l_file l_file_open_write_nobuf(const void* name); /* create new file if not exist */
L_EXTERN l_file l_file_open_append(const void* name); /* create new file if not exist */
L_EXTERN l_file l_file_open_append_nobuf(const void* name); /* create new file if not exist */
L_EXTERN l_file l_file_open_read_write(const void* name); /* the file must exist */
L_EXTERN l_file l_file_open_read_append(const void* name); /* the file must exist */
L_EXTERN l_bool l_file_read_line(l_file* s, l_byte* out, l_int len); /* return true means read something */
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

typedef struct l_smplnode {
  struct l_smplnode* next;
} l_smplnode;

L_INLINE void
l_smplnode_init(l_smplnode* node)
{
  node->next = 0;
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
  if (p) {
    node->next = p->next;
  }
  return p;
}

typedef struct l_squeue {
  l_smplnode head;
  l_smplnode* tail;
} l_squeue;

L_INLINE void
l_squeue_init(l_squeue* sq)
{
  sq->head.next = 0; /* init to 0 let the queue become copiable */
  sq->tail = 0;
}

L_INLINE l_bool
l_squeue_is_empty(l_squeue* sq)
{
  return (sq->head.next == 0);
}

L_INLINE l_bool
l_squeue_nt_empty(l_squeue* sq)
{
  return (sq->head.next != 0);
}

L_INLINE void /* push to the tail */
l_squeue_push(l_squeue* sq, l_smplnode* newnode)
{
  l_smplnode_insert_after(sq->tail == 0 ? &sq->head : sq->tail, newnode);
  sq->tail = newnode;
}

L_INLINE void
l_squeue_push_queue(l_squeue* self, l_squeue* q)
{
  if (l_squeue_is_empty(q)) {
    return;
  } else {
    l_smplnode* tail = self->tail == 0 ? &self->head : self->tail;
    tail->next = q->head.next;
    self->tail = q->tail;
    l_squeue_init(q);
  }
}

L_INLINE void
l_squeue_move_init(l_squeue* self, l_squeue* q)
{
  l_squeue_init(self);
  l_squeue_push_queue(self, q);
}

L_INLINE l_squeue
l_squeue_move(l_squeue* q)
{
  l_squeue out;
  l_squeue_move_init(&out, q);
  return out;
}

L_INLINE l_smplnode*
l_squeue_top(l_squeue* sq)
{
  return sq->head.next;
}

L_INLINE l_smplnode* /* pop from head */
l_squeue_pop(l_squeue* sq)
{
  l_smplnode* node = 0;
  node = l_smplnode_remove_next(&sq->head);
  if (node == sq->tail) {
    sq->tail = 0;
  }
  return node;
}

typedef struct {
  l_smplnode head;
  l_bool (*less)(l_smplnode*, l_smplnode*); /* if less is really less then the biggest is in the top */
} l_spriorq;

L_INLINE void
l_spriorq_init(l_spriorq* q, l_bool (*less)(l_smplnode*, l_smplnode*))
{
  q->head.next = 0;
  q->less = less;
}

L_INLINE l_bool
l_spriorq_is_empty(l_spriorq* q)
{
  return q->head.next == 0;
}

L_INLINE l_bool
l_spriorq_nt_empty(l_spriorq* q)
{
  return q->head.next != 0;
}

L_INLINE void
l_spriorq_push(l_spriorq* q, l_smplnode* newnode)
{
  l_smplnode* node = &q->head;
  while (node->next && q->less(newnode, node->next)) {
    node = node->next;
  }
  newnode->next = node->next;
  node->next = newnode;
}

L_INLINE l_smplnode*
l_spriorq_top(l_spriorq* q)
{
  return q->head.next;
}

L_INLINE l_smplnode* /* pop from head */
l_spriorq_pop(l_spriorq* q)
{
  return l_smplnode_remove_next(&q->head);
}

typedef struct l_linknode {
  struct l_linknode* next;
  struct l_linknode* prev;
} l_linknode;

L_INLINE void
l_linknode_init(l_linknode* node)
{
  node->next = node->prev = 0; /* init to 0 let the queue become copiable */
}

L_INLINE void
l_linknode_insert_after(l_linknode* node, l_linknode* newnode)
{
  newnode->next = node->next;
  node->next = newnode;
  newnode->prev = node;
  if (newnode->next) {
    newnode->next->prev = newnode;
  }
}

L_INLINE l_linknode*
l_linknode_remove(l_linknode* node)
{
  l_linknode* next = node->next;
  l_linknode* prev = node->prev;
  if (next) next->prev = prev;
  if (prev) prev->next = next;
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
  return dq->head.next == 0;
}

L_INLINE l_bool
l_dqueue_nt_empty(l_dqueue* dq)
{
  return dq->head.next != 0;
}

L_INLINE void
l_dqueue_push(l_dqueue* dq, l_linknode* newnode)
{
  l_linknode* node = dq->head.prev ? dq->head.prev : &dq->head;
  node->next = newnode;
  newnode->next = 0;
  newnode->prev = node;
  dq->head.prev = newnode;
}

L_INLINE void
l_dqueue_push_queue(l_dqueue* self, l_dqueue* q)
{
  l_linknode* tail = 0;
  if (l_dqueue_is_empty(q)) return;
  /* chain self's tail with q's first element */
  tail = self->head.prev ? self->head.prev : &self->head;
  tail->next = q->head.next;
  q->head.next->prev = tail;
  /* set tail to new tail of q */
  self->head.prev = q->head.prev;
  /* init q to empty */
  l_dqueue_init(q);
}

L_INLINE void
l_dqueue_move_init(l_dqueue* self, l_dqueue* q)
{
  l_dqueue_init(self);
  l_dqueue_push_queue(self, q);
}

L_INLINE l_dqueue
l_dqueue_move(l_dqueue* q)
{
  l_dqueue dq;
  l_dqueue_move_init(&dq, q);
  return dq;
}

L_INLINE l_linknode*
l_dqueue_top(l_dqueue* dq)
{
  return dq->head.next;
}

L_INLINE l_linknode*
l_dqueue_pop(l_dqueue* dq)
{
  l_linknode* node = dq->head.next;
  if (node) {
    l_linknode* prev = &dq->head;
    l_linknode* next = node->next;
    prev->next = next;
    if (next) {
      next->prev = prev;
    } else {
      dq->head.prev = 0;
    }
  }
  return node;
}

#endif /* LNLYLIB_CORE_BASE_H */

