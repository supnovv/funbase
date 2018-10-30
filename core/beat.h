#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"
#include "osi/base.h"

typedef struct lnlylib_env lnlylib_env;
typedef struct l_service l_service;
typedef struct l_message l_message;

L_EXTERN lnlylib_env* lnlylib_setup(int argc, char** argv);
L_EXTERN void lnlylib_clean(lnlylib_env* main_env);
L_EXTERN int lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv);

typedef struct l_service_access_point {
  l_smplnode node;
  l_ulong peer_svid;
  struct l_service_access_point* peer_apid;
  void (*access_proc)(lnlylib_env*);
} l_service_access_point;

#define L_CORE_MSG_GR (0x00 << 8)
#define L_HTTP_MSG_GR (0x01 << 8)
#define L_MOVE_DATA_AUTO_FREE 0x80000000
#define L_MOVE_DATA_DONT_FREE 0x40000000

L_EXTERN void l_respond_message(lnlylib_env* E, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_respond_message_for(lnlylib_env* E, l_message* msg, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message(lnlylib_env* E, l_service_access_point* sap, l_umedit session, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_message_to(lnlylib_env* E, l_ulong dest_svid, l_umedit session, l_umedit mgid, void* data, l_umedit size);

#define L_CURRENT_MESSAGE_DATA(E, type) ((type*)l_current_message_data(E))
#define L_MESSAGE_DATA(msg, type) ((type*)l_message_data(msg))

L_EXTERN l_message* l_current_message(lnlylib_env* E);
L_EXTERN l_umedit l_current_message_id(lnlylib_env* E);
L_EXTERN l_umedit l_current_message_session(lnlylib_env* E);
L_EXTERN void* l_current_message_data(lnlylib_env* E);
L_EXTERN l_umedit l_current_message_data_size(lnlylib_env* E);
L_EXTERN l_ulong l_current_message_from(lnlylib_env* E);
L_EXTERN l_umedit l_message_id(l_message* msg);
L_EXTERN l_umedit l_message_session(l_message* msg);
L_EXTERN void* l_message_data(l_message* msg);
L_EXTERN l_umedit l_message_data_size(l_message* msg);
L_EXTERN l_ulong l_message_from(l_message* msg);

#define L_MSG_SERVICE_CREATE_RSP  (L_CORE_MSG_GR + 0x0D)
#define L_MSG_SERVICE_FORCE_QUIT  (L_CORE_MSG_GR + 0x0E)
#define L_MSG_SERVICE_RESTART_CMD (L_CORE_MSG_GR + 0x0F)

typedef struct {
  l_bool (*on_create)(lnlylib_env*); /* return false indicates create service failed, then the service destroyed immediately */
  void (*on_destroy)(lnlylib_env*);
  void (*service_proc)(lnlylib_env*);
} l_service_callback;

typedef struct {
  l_ulong srvc_id; /* 0 indicates service created fail */
  void* srvc_ud; /* service created service udata */
  void* in_svud; /* user passed in service udata */
  l_uint creation_ctx;
  l_filehdl hdl;
} l_service_create_rsp;

L_EXTERN l_bool l_empty_on_create(lnlylib_env* E);
L_EXTERN void l_empty_on_destroy(lnlylib_env* E);
L_EXTERN void l_empty_service_proc(lnlylib_env* E);

L_EXTERN void l_create_service(lnlylib_env* E, l_service_callback* srvc_cb, void* in_svud, l_uint creation_ctx);
L_EXTERN void l_create_event_poll_service(lnlylib_env* E, l_service_callback* srvc_cb, void* in_svud, l_filehdl hdl, l_uint creation_ctx);
L_EXTERN void l_create_access_point(lnlylib_env* E, l_service_access_point* sap, l_ulong peer_svid, void (*access_proc)(lnlylib_env*));
L_EXTERN void l_delete_access_point(lnlylib_env* E, l_service_access_point* sap);
L_EXTERN void l_service_add_event(lnlylib_env* E, l_filehdl hdl);
L_EXTERN void l_service_del_event(lnlylib_env* E);
L_EXTERN void l_stop_service(lnlylib_env* E);
L_EXTERN void l_stop_dest_service(lnlylib_env* E, l_ulong svid);

#define L_CURRENT_SERVICE_UDATA(E, type) ((type*)l_current_service_udata(E))
#define L_SERVICE_UDATA(S, type) ((type*)l_service_udata(S))

L_EXTERN l_service* l_current_service(lnlylib_env* E);
L_EXTERN void* l_current_service_udata(lnlylib_env* E);
L_EXTERN l_ulong l_current_service_id(lnlylib_env* E);
L_EXTERN l_filehdl l_current_service_evhdl(lnlylib_env* E);
L_EXTERN l_ulong l_current_service_from(lnlylib_env* E);
L_EXTERN l_message* l_current_create_req_message(lnlylib_env* E);
L_EXTERN void* l_service_udata(l_service* S);
L_EXTERN l_ulong l_service_id(l_service* S);
L_EXTERN l_filehdl l_service_evhdl(l_service* S);
L_EXTERN l_ulong l_service_from(l_service* S);
L_EXTERN l_message* l_get_create_req_message(l_service* S);
L_EXTERN l_bool l_set_service_udata(lnlylib_env* E, void* srvc_ud);

#define L_MSG_TIMER_CREATE_RSP (L_CORE_MSG_GR + 0x1A)
#define L_MSG_TIMER_NOTIFY_IND (L_CORE_MSG_GR + 0x1B)
#define L_TIMER_MIN_VALID_TMID 0x0f
#define L_TIMER_IMMED_FIRED_ID 0x0f

typedef struct {
  l_ulong id;
} l_timer;

typedef struct {
  l_uint timer_ctx; /* timer creation context */
  l_timer timer; /* new created timer, 0x00 ~ 0x0e indicates failure */
} l_timer_create_rsp;

typedef struct {
  l_long stamp;
  l_ulong count;
  l_uint timer_ctx;
  l_timer timer;
} l_timer_notify_ind;

L_EXTERN l_long l_time_msec(lnlylib_env* E);
L_EXTERN const l_byte* l_time_strc(lnlylib_env* E);

L_EXTERN void l_create_timer(lnlylib_env* E, l_uint timer_ctx, l_long ms, void (*func)(lnlylib_env*, void*), void* parm);
L_EXTERN void l_create_repeated_timer(lnlylib_env* E, l_uint timer_ctx, l_long ms, void (*func)(lnlylib_env*, void*), void* parm, l_long times);
L_EXTERN void l_create_notify_timer(lnlylib_env* E, l_uint timer_ctx, l_long ms, l_long times);
L_EXTERN void l_cancel_timer(lnlylib_env* E, l_timer* timer);

#define L_MSG_SOCK_READY_NTF     (L_CORE_MSG_GR + 0x2A) /* socket service -> user service */
#define L_MSG_SOCK_READ_RSP      (L_CORE_MSG_GR + 0x2B)
#define L_MSG_SOCK_WRITE_RSP     (L_CORE_MSG_GR + 0x2C)
#define L_MSG_SOCK_BAD_STATE_NTF (L_CORE_MSG_GR + 0x2D)
#define L_MSG_SOCK_RECOVER_RSP   (L_CORE_MSG_GR + 0x2E)

L_EXTERN void l_create_tcp_listen_service(lnlylib_env* E, const void* local_ip, l_ushort local_port, l_service_callback* response_service_callback, l_uint creation_ctx);
L_EXTERN void l_create_tcp_connect_service(lnlylib_env* E, const void* ip, l_ushort port, l_uint creation_ctx);
L_EXTERN void l_report_service_created(lnlylib_env* E, l_bool success);
L_EXTERN void l_stop_listen_service(lnlylib_env* E);

typedef struct {
  l_int bytes_read; /* bytes_read can be > 0 when status indicates error */
  l_error status; /* 0 success, L_STATUS_EARGS, L_STATUS_EREAD, or L_STATUS_ETIMEOUT */
} l_socket_read_rsp;

typedef struct {
  l_uint write_id;
  l_int size;
} l_socket_write_rsp;

L_EXTERN void l_socket_data_read_req(lnlylib_env* E, l_ulong sock_svid, void* in_buffer_s, void* in_buffer_e, l_int min_bytes_in, l_long timeout_ms);
L_EXTERN void l_send_socket_write_req(lnlylib_env* E, l_ulong sock_svid, const void* data, l_int size, l_uint write_id);
L_EXTERN void l_send_socket_close_cmd(lnlylib_env* E, l_ulong sock_svid);

#endif /* LNLYLIB_CORE_BEAT_H */

