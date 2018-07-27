#ifndef LNLYLIB_OSI_BASE_H
#define LNLYLIB_OSI_BASE_H
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

/** file operations **/

typedef struct {
  void* ds;
} l_dirstm;

L_EXTERN int l_dirstm_opendir(l_dirstm* self, const void* name);
L_EXTERN void l_dirstm_close(l_dirstm* self);
L_EXTERN const l_byte* l_dirstm_read(l_dirstm* self);
L_EXTERN const l_byte* l_dirstm_read2(l_dirstm* self, int* isdir);

typedef struct {
  l_long size;
  l_long ctime;
  l_long atime;
  l_long mtime;
  l_byte isfile;
  l_byte isdir;
  l_byte islink;
} l_fileattr;

L_EXTERN int l_file_exist(const void* name);
L_EXTERN int l_file_exist_in(l_filehdl* dir, const void* name);
L_EXTERN int l_file_folder_exist(const void* foldername);
L_EXTERN l_ulong l_file_size(const void* name);
L_EXTERN l_bool l_file_attr(l_fileaddr* addr, const void* name);
L_EXTERN int l_file_opendir(l_filehdl* self, const void* name);
L_EXTERN void l_file_closefd(l_filehdl* self);

/** dynamic libarary loading **/

typedef union {
  void* impl;
  l_uint uhdl;
} l_dynhdl;

L_EXTERN l_bool l_dynhdl_is_empty(l_dynhdl* hdl);
L_EXTERN l_bool l_dynhdl_nt_empty(l_dynhdl* hdl);
L_EXTERN l_dynhdl l_empty_dynhdl();
L_EXTERN l_dynhdl l_dynhdl_open(l_strn name); /* the library file extension is auto determinted */
L_EXTERN l_dynhdl l_dynhdl_open_from(l_strn path, l_strn lib_name);
L_EXTERN void* l_dynhdl_load(l_dynhdl* hdl, l_strn sym_name);
L_EXTERN void l_dynhdl_close(l_dynhdl* hdl);

/** concurrency and synchronization **/

L_EXTERN void l_thrkey_init(l_thrkey* self);
L_EXTERN void l_thrkey_free(l_thrkey* self);
L_EXTERN void l_thrkey_set_data(l_thrkey* self, const void* data);
L_EXTERN void* l_thrkey_get_data(l_thrkey* self);

L_EXTERN void l_mutex_init(l_mutex* self);
L_EXTERN void l_mutex_free(l_mutex* self);
L_EXTERN int l_mutex_lock(l_mutex* self);
L_EXTERN int l_mutex_unlock(l_mutex* self);
L_EXTERN int l_mutex_try_lock(l_mutex* self);

L_EXTERN void l_rwlock_init(l_rwlock* self);
L_EXTERN void l_rwlock_free(l_rwlock* self);
L_EXTERN int l_rwlock_rdlock(l_rwlock* self);
L_EXTERN int l_rwlock_wrlock(l_rwlock* self);
L_EXTERN int l_rwlock_unlock(l_rwlock* self);
L_EXTERN int l_rwlock_try_read(l_rwlock* self);
L_EXTERN int l_rwlock_try_write(l_rwlock* self);

L_EXTERN void l_condv_init(l_condv* self);
L_EXTERN void l_condv_free(l_condv* self);
L_EXTERN int l_condv_wait(l_condv* self, l_mutex* mutex);
L_EXTERN int l_condv_timed_wait(l_condv* self, l_mutex* mutex, l_long ns);
L_EXTERN int l_condv_signal(l_condv* self);
L_EXTERN int l_condv_broadcast(l_condv* self);

L_EXTERN l_thrhdl l_thrhdl_self();
L_EXTERN int l_thrhdl_create(l_thrhdl* thrhdl, void* (*start)(void*), void* para);
L_EXTERN int l_thrhdl_cancel(l_thrhdl* thrhdl);
L_EXTERN int l_thrhdl_join(l_thrhdl* thrhdl);
L_EXTERN void l_thrhdl_sleep(l_long us);
L_EXTERN void l_thrhdl_exit();

/** socket **/

#define L_IO_EVENT_READ   0x01
#define L_IO_EVENT_WRITE  0x02
#define L_IO_EVENT_RDWR   0x03
#define L_IO_EVENT_PRI    0x04
#define L_IO_EVENT_RDH    0x08
#define L_IO_EVENT_HUP    0x10
#define L_IO_EVENT_ERR    0x20
#define L_IO_EVENT_MASK 0xffff

typedef l_filehdl l_socket;

#endif /* LNLYLIB_OSI_BASE_H */
