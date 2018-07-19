#ifndef LNLYLIB_OSI_BASE_H
#define LNLYLIB_OSI_BASE_H
#include "core/base.h"

#define L_IO_EVENT_READ   0x01
#define L_IO_EVENT_WRITE  0x02
#define L_IO_EVENT_RDWR   0x03
#define L_IO_EVENT_PRI    0x04
#define L_IO_EVENT_RDH    0x08
#define L_IO_EVENT_HUP    0x10
#define L_IO_EVENT_ERR    0x20
#define L_IO_EVENT_MASK 0xffff

typedef l_filehdl l_socket;

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

#endif /* LNLYLIB_OSI_BASE_H */
