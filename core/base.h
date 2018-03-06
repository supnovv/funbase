#ifndef L_CORE_BASE_H
#define L_CORE_BASE_H
#include "plat/autoconf.h"
#include "plat/prefix.h"

#undef L_MAX_IOSZ
#undef L_MAX_UBYTE
#undef L_MAX_SBYTE
#undef L_MIN_SBYTE
#undef L_MAX_USHORT
#undef L_MAX_SHORT
#undef L_MIN_SHORT
#undef L_MAX_UMEDIT
#undef L_MAX_MEDIT
#undef L_MIN_MEDIT
#undef L_MAX_ULONG
#undef L_MAX_LONG
#undef L_MIN_LONG

#define L_MAX_IOSZ   (0x7fff0000) /* 2147418112 */
#define L_MAX_UBYTE  ((l_byte)0xff) /* 255 */
#define L_MAX_SBYTE  ((l_sbyte)0x7f) /* 127 */
#define L_MIN_SBYTE  ((l_sbyte)-127-1) /* 128 0x80 */
#define L_MAX_USHORT ((l_ushort)0xffff) /* 65535 */
#define L_MAX_SHORT  ((l_short)0x7fff) /* 32767 */
#define L_MIN_SHORT  ((l_short)-32767-1) /* 32768 0x8000 */
#define L_MAX_UMEDIT ((l_umedit)0xffffffff) /* 4294967295 */
#define L_MAX_MEDIT  ((l_medit)0x7fffffff) /* 2147483647 */
#define L_MIN_MEDIT  ((l_medit)-2147483647-1) /* 2147483648 0x80000000 */
#define L_MAX_ULONG  ((l_ulong)0xffffffffffffffff) /* 18446744073709551615 */
#define L_MAX_LONG   ((l_long)0x7fffffffffffffff) /* 9223372036854775807 */
#define L_MIN_LONG   ((l_long)-9223372036854775807-1) /* 9223372036854775808 0x8000000000000000 */

#endif /* L_CORE_BASE_H */

