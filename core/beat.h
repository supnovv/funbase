#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

typedef struct lnlylib_env lnlylib_env;
typedef struct l_service l_service;
typedef struct l_message l_message;

L_EXTERN int lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv);

typedef struct l_service_access_point {
  l_smplnode node;
  l_ulong remote_svid;
  void (*access_proc)(lnlylib_env*);
  struct l_service_access_point* remote_apid;
} l_service_access_point;

/* message */

#define L_CORE_MSG_GR (0x00 << 8)
#define L_HTTP_MSG_GR (0x01 << 8)

#define L_MOVE_DATA_AUTO_FREE 0x80000000 /* the message data is moved and auto freed */
#define L_MOVE_DATA_DONT_FREE 0x40000000 /* the message data is moved but it is not freed */

L_EXTERN void l_send_response(lnlylib_env* E, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_response_for(lnlylib_env* E, l_message* msg, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message(lnlylib_env* E, l_service_access_point* sap, l_umedit session, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message_to(lnlylib_env* E, l_ulong dest_svid, l_umedit session, l_umedit mgid, void* data, l_umedit size);

L_EXTERN l_message* l_current_message(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgid(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgss(lnlylib_env* E);
L_EXTERN void* l_current_mgdt(lnlylib_env* E);
L_EXTERN l_umedit l_current_mgdt_size(lnlylib_env* E);
L_EXTERN l_ulong l_current_mgfr(lnlylib_env* E);

L_EXTERN l_umedit l_mgid(l_message* msg);
L_EXTERN l_umedit l_mgss(l_message* msg);
L_EXTERN void* l_mgdt(l_message* msg);
L_EXTERN l_umedit l_mgdt_size(l_message* msg);
L_EXTERN l_ulong l_mgfr(l_message* msg);

/* service */

#define L_MSG_SUBSRVC_CREATE_RSP  (L_CORE_MSG_GR + 0x0A)
#define L_MSG_SERVICE_EXIT_REQ    (L_CORE_MSG_GR + 0x0B)
#define L_MSG_SERVICE_RESTART_REQ (L_CORE_MSG_GR + 0x0C)

typedef struct {
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
  l_ulong svid; /* sub service id, non-zero svid indicates success */
  void* in_svud; /* user passed in svud for this sub service */
  void* in_ctx; /* unique data user passed in to identify this creation */
  void* out_svud; /* service created out itself real service data */
} l_subsrvc_create_rsp;

L_EXTERN void* l_empty_on_create(lnlylib_env* E);
L_EXTERN void l_empty_on_destroy(lnlylib_env* E);
L_EXTERN void l_empty_service_proc(lnlylib_env* E);

L_EXTERN l_service_create_para L_USEHDL_SERVICE(l_filehdl hdl, l_service_callback* cb);
L_EXTERN l_service_create_para L_USEHDL_MODULE(l_filehdl hdl, const void* module);
L_EXTERN l_service_create_para L_SERVICE(l_service_callback* cb);
L_EXTERN l_service_create_para L_MODULE(const void* module);

L_EXTERN void l_create_service(lnlylib_env* E, l_service_create_para para, void* svud, void* ctx);
L_EXTERN void l_create_access_point(lnlylib_env* E, l_service_access_point* sap, l_ulong remote_svid, void (*access_proc)(lnlylib_env*));
L_EXTERN void l_delete_access_point(lnlylib_env* E, l_service_access_point* sap);
L_EXTERN void l_stop_service(lnlylib_env* E);
L_EXTERN void l_stop_dest_service(lnlylib_env* E, l_ulong svid);

L_EXTERN l_service* l_current_service(lnlylib_env* E);
L_EXTERN void* l_current_svud(lnlylib_env* E);
L_EXTERN l_ulong l_current_svid(lnlylib_env* E);
L_EXTERN l_filehdl l_current_evhd(lnlylib_env* E);
L_EXTERN l_ulong l_current_svfr(lnlylib_env* E);
L_EXTERN void l_set_current_svud(lnlylib_env* E, void* svud);

L_EXTERN void* l_svud(l_service* S);
L_EXTERN l_ulong l_svid(l_service* S);
L_EXTERN l_filehdl l_evhd(l_service* S);
L_EXTERN l_ulong l_svfr(l_service* S);

/* time and timer */

#define L_MSG_TIMER_CREATE_RSP (L_CORE_MSG_GR + 0x1A)
#define L_MSG_TIMER_NOTIFY_IND (L_CORE_MSG_GR + 0x1B)

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

#define L_MSG_SOCK_READY_NTF     (L_CORE_MSG_GR + 0x2A) /* socket service -> user service */
#define L_MSG_SOCK_READ_RSP      (L_CORE_MSG_GR + 0x2B)
#define L_MSG_SOCK_WRITE_RSP     (L_CORE_MSG_GR + 0x2C)
#define L_MSG_SOCK_BAD_STATE_NTF (L_CORE_MSG_GR + 0x2D)
#define L_MSG_SOCK_RECOVER_RSP   (L_CORE_MSG_GR + 0x2E)

#endif /* LNLYLIB_CORE_BEAT_H */

