#ifndef L_CORE_TIME_H
#define L_CORE_TIME_H
#include "core/base.h"

/**
 * The 64-bit signed integer's biggest value is 9223372036854775807.
 * For seconds/milliseconds/microseconds/nanoseconds, it can
 * represent more than 291672107014/291672107/291672/291 years.
 * The 32-bit signed integer's biggest value is 2147483647.
 * For seconds/milliseconds/microseconds/nanoseconds, it can
 * represent more than 67-year/24-day/35-min/2-sec. */

#undef L_NSECS_PER_SEC
#define L_NSECS_PER_SEC (1000000000L)

typedef struct {
  l_long sec;
  l_umedit nsec;
} l_time;

typedef struct {
  l_umedit year; /* 38-bit, can represent 274877906943 years */
  l_umedit nsec; /* nanoseconds that less than 1 sec */
  l_ushort yday; /* 1 ~ 366 */
  l_byte high;   /* extra bits for year */
  l_byte wdmon;  /* 1~12, high 4-bit is wday (0~6, 0 is sunday) */
  l_byte day;    /* 1~31 */
  l_byte hour;   /* 0~23 */
  l_byte min;    /* 0~59 */
  l_byte sec;    /* 0~61, 60 and 61 are the leap seconds */
} l_date;

#endif /* L_CORE_TIME_H */

