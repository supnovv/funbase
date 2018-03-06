#ifndef L_CORE_TIME_H
#define L_CORE_TIME_H
#include "core/base.h"

/**
 * The 64-bit signed integer max value is 9223372036854775807.
 * For seconds/milliseconds/microseconds/nanoseconds, it can
 * represent more than 291672107014/291672107/291672/291 years.
 * The 32-bit signed integer's biggest value is 2147483647.
 * For seconds/milliseconds/microseconds/nanoseconds, it can
 * represent more than 67-year/24-day/35-min/2-sec.
 * Year 38-bit can represent 274877906943 years.
 */

#undef L_NSEC_PERSEC
#undef L_USEC_PERSEC
#undef L_MSEC_PERSEC

#define L_NSEC_PERSEC 1000000000
#define L_USEC_PERSEC 1000000
#define L_MSEC_PERSEC 1000

typedef struct {
  l_long sec;
  l_medit nsec;
  l_byte zone;
} l_time;

typedef struct {
  l_long yhms;  /* Year (38-bit) Hour 0~23 (5-bit) Minite 0~59 (6-bit) Second 0~61 60 and 61 leap sec (6-bit) */
  l_medit rest; /* Timezone 0~23 (5-bit) Yearday 1~366 (9-bit) Weekday 0~6 0 is sunday (3-bit) Month 1~12 (4-bit) Day 1~31 (5-bit) */
  l_medit nsec; /* Nanoseconds that less than 1 sec */
} l_date;

L_INLINE l_byte
l_date_sec(const l_date* d) {
  return (l_byte)(d->yhms & 0x3f);
}

L_INLINE l_byte
l_date_min(const l_date* d) {
  return (l_byte)((d->yhms >> 6) & 0x3f);
}

L_INLINE l_byte
l_date_hour(const l_date* d) {
  return (l_byte)((d->yhms >> 12) & 0x1f);
}

L_INLINE l_long
l_date_year(const l_date* d) {
  return ((d->yhms >> 17) & 0x3ffff);
}

L_INLINE l_byte
l_date_day(const l_date* d) {
  return (l_byte)(d->rest & 0x1f);
}

L_INLINE l_byte
l_date_month(const l_date* d) {
  return (l_byte)((d->rest >> 5) & 0x0f);
}

L_INLINE l_byte
l_date_weekday(const l_date* d) {
  return (l_byte)((d->rest >> 9) & 0x07);
}

L_INLINE l_short
l_date_yearday(const l_date* d) {
  return (l_short)((d->rest >> 12) & 0x1ff);
}

L_INLINE l_byte
l_date_timezone(const l_date* d) {
  return (l_byte)((d->rest >> 21) & 0x1f);
}

L_EXTERN l_time l_system_time();
L_EXTERN l_time l_mono_time();
L_EXTERN l_date l_system_date();
L_EXTERN l_date l_date_fromsecs(l_long utcsecs);
L_EXTERN l_date l_date_fromtime(l_time utctime);

#endif /* L_CORE_TIME_H */

