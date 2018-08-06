#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

struct l_master;
struct l_thread;
struct l_worker;
struct l_service;
struct l_message;
struct l_corotable;
struct l_coroutine;

typedef struct lnlylib_env {
  struct l_master* M;
  struct l_thread* T;
  struct l_service* S;
  struct l_message* MSG;
  l_ostream* LOG;
  void* SVUD;
  void* ALLOC;
  struct l_corotable* CT;
  struct l_coroutine* CO;
} lnlylib_env;

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
L_EXTERN void l_stop_dest_service(lnlylib_env* E, l_ulong svid);
L_EXTERN void l_stop_service(lnlylib_env* E);
L_EXTERN void l_send_message(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);
L_EXTERN void l_send_data_moved_message(lnlylib_env* E, l_ulong dest_svid, l_umedit mgid, void* data, l_umedit size);

L_EXTERN struct l_message* l_get_message(lnlylib_env* E);
L_EXTERN struct l_service* l_get_service(lnlylib_env* E);
L_EXTERN struct l_master* l_get_master(lnlylib_env* E);
L_EXTERN struct l_thread* l_get_thread(lnlylib_env* E);

L_EXTERN l_umedit l_message_id(struct l_message* msg);
L_EXTERN l_umedit l_message_cust(struct l_message* msg);
L_EXTERN l_strn l_message_data(struct l_message* msg);
L_EXTERN l_umedit l_get_mssg_id(lnlylib_env* E);
L_EXTERN l_umedit l_get_mssg_cust(lnlylib_env* E);
L_EXTERN l_strn l_get_mssg_data(lnlylib_env* E);

L_EXTERN l_ulong l_service_id(struct l_service* S);
L_EXTERN void* l_service_data(struct l_service* S);
L_EXTERN l_ulong l_get_svid(lnlylib_env* E);
L_EXTERN void* l_get_svud(lnlylib_env* E);

L_EXTERN int lnlylib_main(void (*start)(lnlylib_env*), int argc, char** argv);

#endif /* LNLYLIB_CORE_BEAT_H */

