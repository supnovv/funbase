#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

typedef struct lnlylib_env lnlylib_env;
typedef struct l_service l_service;
typedef struct l_message l_message;

L_EXTERN int lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv);

/* message */

#define L_MSG_MIN_USER_MSG_ID 0x0100 /* user defined messages shall use the id bigger than this value, while different services can use the same message id */

L_EXTERN void l_send_message(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message_moved(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);

L_EXTERN l_message* l_current_message(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgid(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgcu(lnlylib_env* E);
L_EXTERN void* l_current_mgdt(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgdt_size(lnlylib_env* E);
L_EXTERN l_ulong l_current_mgfr(lnlylib_env* E);

L_EXTERN l_umedit l_message_mgid(l_message* msg);
L_EXTERN l_umedit l_message_mgcu(l_message* msg);
L_EXTERN void* l_message_mgdt(l_message* msg);
L_EXTERN l_umedit l_message_mgdt_size(l_message* msg);
L_EXTERN l_ulong l_message_from(l_message* msg);

/* service */

#define L_MSG_SUBSRVC_CREATE_RSP  (L_MSG_MIN_USER_MSG_ID - 0x01) /* 0xff */
#define L_MSG_SERVICE_EXIT_REQ    (L_MSG_MIN_USER_MSG_ID - 0x02) /* 0xfe */
#define L_MSG_SERVICE_RESTART_REQ (L_MSG_MIN_USER_MSG_ID - 0x03) /* 0xfd */

typedef struct {
  const char* service_name;
  void* (*service_on_create)(lnlylib_env*); /* create svud if needed, do other service creation related things */
  void (*service_on_destroy)(lnlylib_env*); /* release svud and do other service destroy related things */
  void (*service_proc)(lnlylib_env*); /* service main logic */
} l_service_callback;

typedef l_service_callback l_service_define;

typedef struct {
  const void* module;
  l_service_callback* cb;
  l_byte enable;
  l_byte listen;
  l_ushort port;
  l_filehdl hdl;
  const void* ip;
} l_service_create_para;

typedef struct {
  l_ulong svid; /* non-zero svid indicates successful creation */
  void* svud;
  const char* name;
} l_subsrvc_create_rsp;

L_EXTERN l_service_create_para L_LISTEN_SERVICE(const void* ip, l_ushort port, l_service_callback* cb);
L_EXTERN l_service_create_para L_LISTEN_MODULE(const void* ip, l_ushort port, const void* module);
L_EXTERN l_service_create_para L_CONNECT_SERVICE(const void* ip, l_ushort port, l_service_callback* cb);
L_EXTERN l_service_create_para L_CONNECT_MODULE(const void* ip, l_ushort port, const void* module);
L_EXTERN l_service_create_para L_USEHDL_SERVICE(l_filehdl hdl, l_service_callback* cb);
L_EXTERN l_service_create_para L_USEHDL_MODULE(l_filehdl hdl, const void* module);
L_EXTERN l_service_create_para L_SERVICE(l_service_callback* cb);
L_EXTERN l_service_create_para L_MODULE(const void* module);

L_EXTERN void l_create_service(lnlylib_env* E, l_service_create_para para, void* svud);
L_EXTERN void l_stop_service(lnlylib_env* E);
L_EXTERN void l_stop_dest_service(lnlylib_env* E, l_ulong svid);

L_EXTERN l_service* l_current_service(lnlylib_env* E);
L_EXTERN void* l_current_svud(lnlylib_env* E);
L_EXTERN l_ulong l_current_svid(lnlylib_env* E);
L_EXTERN l_filehdl l_current_evhd(lnlylib_env* E);
L_EXTERN l_ulong l_current_svfr(lnlylib_env* E);
L_EXTERN void l_set_current_svud(lnlylib_env* E, void* svud);

L_EXTERN void* l_service_svud(l_service* S);
L_EXTERN l_ulong l_service_svid(l_service* S);
L_EXTERN l_filehdl l_service_evhd(l_service* S);
L_EXTERN l_ulong l_service_from(l_service* S);
L_EXTERN void l_set_service_svud(l_service* S, void* svud);

/* time and timer */

#define L_MSG_TIMER_CREATE_RSP (L_MSG_MIN_USER_MSG_ID - 0x11) /* 0xef */
#define L_MSG_TIMER_NOTIFY_IND (L_MSG_MIN_USER_MSG_ID - 0x12) /* 0xee */

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

/* socket service */

L_EXTERN void l_socket_service_read(lnlylib_env* E, l_ulong sock_srvc);
L_EXTERN void l_socket_service_write(lnlylib_env* E, l_ulong sock_srvc);
L_EXTERN void l_socket_service_recover(lnlylib_env* E, l_ulong sock_srvc);
L_EXTERN void l_socket_service_close(lnlylib_env* E, l_ulong sock_srvc);
L_EXTERN void l_stop_listen_server(lnlylib_env* E);

#define L_MSG_SOCK_READY_NTF   (L_MSG_MIN_USER_MSG_ID - 0x21) /* 0xdf */ /* socket service -> user service */
#define L_MSG_SOCK_DATA_RX_RSP (L_MSG_MIN_USER_MSG_ID - 0x22) /* 0xde */
#define L_MSG_SOCK_DATA_TX_RSP (L_MSG_MIN_USER_MSG_ID - 0x23) /* 0xdd */
#define L_MSG_SOCK_NTRDY_NTF   (L_MSG_MIN_USER_MSG_ID - 0x24) /* 0xdc */
#define L_MSG_SOCK_RECOVER_RSP (L_MSG_MIN_USER_MSG_ID - 0x25) /* 0xdb */

#endif /* LNLYLIB_CORE_BEAT_H */

