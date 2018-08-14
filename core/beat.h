#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

#define L_MSG_MIN_USER_MSG_ID 0x0100 /* user defined messages shall use the id bigger than this value, while different services can use the same message id */

typedef struct lnlylib_env lnlylib_env;
typedef struct l_service l_service;
typedef struct l_message l_message;

L_EXTERN int lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv);

/* service */

typedef struct {
  void* (*service_on_create)(lnlylib_env*); /* create svud if needed, do other service creation related things */
  void (*service_on_destroy)(lnlylib_env*); /* release svud and do other service destroy related things */
  void (*service_proc)(lnlylib_env*); /* service main logic */
} l_service_callback;

typedef struct {
  const void* module;
  l_service_callback* cb;
  l_byte enable;
  l_byte listen;
  l_ushort port;
  l_filehdl hdl;
  const void* ip;
} l_service_create_para;

L_EXTERN l_service_create_para L_LISTEN_SERVICE(const void* ip, l_ushort port, l_service_callback* cb);
L_EXTERN l_service_create_para L_LISTEN_MODULE(const void* ip, l_ushort port, const void* module);
L_EXTERN l_service_create_para L_CONNECT_SERVICE(const void* ip, l_ushort port, l_service_callback* cb);
L_EXTERN l_service_create_para L_CONNECT_MODULE(const void* ip, l_ushort port, const void* module);
L_EXTERN l_service_create_para L_USEHDL_SERVICE(l_filehdl hdl, l_service_callback* cb);
L_EXTERN l_service_create_para L_USERHDL_MODULE(l_filehdl hdl, const void* module);
L_EXTERN l_service_create_para L_SERVICE(l_service_callback* cb);
L_EXTERN l_service_create_para L_MODULE(const void* module);

L_EXTERN void l_create_service(lnlylib_env* E, l_service_create_para para, void* svud);
L_EXTERN void l_stop_service(lnlylib_env* E);
L_EXTERN void l_stop_dest_service(lnlylib_env* E, l_ulong svid);

L_EXTERN l_service* l_current_service(lnlylib_env* E);
L_EXTERN void* l_current_svud(lnlylib_env* E);
L_EXTERN l_ulong l_current_svid(lnlylib_env* E);
L_EXTERN void* l_service_data(l_service* S);
L_EXTERN l_ulong l_service_id(l_service* S);

/* message */

L_EXTERN void l_send_message(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message_moved(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);

L_EXTERN l_message* l_current_msg(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgid(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgcu(lnlylib_env* E);
L_EXTERN l_strn l_current_mgdt(lnlylib_env* E);
L_EXTERN l_umedit l_message_id(l_message* msg);
L_EXTERN l_umedit l_message_cust(l_message* msg);
L_EXTERN l_strn l_message_data(l_message* msg);

/* time and timer */

#define L_MSG_TIMER_CREATE_RSP (L_MSG_MIN_USER_MSG_ID - 0x30)
#define L_MSG_TIMER_NOTIFY_IND (L_MSG_MIN_USER_MSG_ID - 0x31)

#define L_TIMER_MIN_VALID_TMID 0x0f
#define L_TIMER_IMMED_FIRED_ID 0x0f

typedef struct {
  l_ulong uniid;
} l_timer;

typedef struct {
  l_uint tmud; /* pointer size timer udata */
  l_timer timer; /* new created timer, 0x00 ~ 0x0e indicates fail */
} l_timer_create_rsp;

typedef struct {
  l_long stamp;
  l_ulong count;
  l_uint tmud;
  l_timer timer;
} l_timer_notify_ind;

L_EXTERN l_long l_time_msec(lnlylib_env* E);
L_EXTERN const l_byte* l_time_strc(lnlylib_env* E);

L_EXTERN void l_create_timer(lnlylib_env* E, l_uint tmud, l_long ms, void (*func)(lnlylib_env*, void*), void* parm);
L_EXTERN void l_create_repeated_timer(lnlylib_env* E, l_uint tmud, l_long ms, void (*func)(lnlylib_env*, void*), void* parm, l_long times);
L_EXTERN void l_create_notify_timer(lnlylib_env* E, l_uint tmud, l_long ms, l_long times);
L_EXTERN void l_cancel_timer(lnlylib_env* E, l_timer* timer);

#endif /* LNLYLIB_CORE_BEAT_H */

