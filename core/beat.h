#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"
#include "core/lapi.h"

struct l_master;
struct l_thread;
struct l_worker;
struct l_service;
struct l_message;
struct l_coroutine;

typedef struct lnlylib_env {
  struct l_master* M;
  struct l_thread* T;
  struct l_service* S;
  struct l_message* MSG;
  l_ostream* LOG;
  void* SVUD;
  void* ALLOC;
  lua_State* L;
  struct l_coroutine* CO;
} lnlylib_env;

typedef struct {
  void* (*service_on_create)(lnlylib_env*);
  void (*service_on_destroy)(lnlylib_env*);
  void (*service_proc)(lnlylib_env*);
} l_service_callback;

L_EXTERN struct l_message* l_get_message(lnlylib_env* E);
L_EXTERN struct l_service* l_get_service(lnlylib_env* E);
L_EXTERN struct l_worker* l_get_worker(lnlylib_env* E);

L_EXTERN l_umedit l_message_id(struct l_message* msg);
L_EXTERN l_umedit l_message_cust(struct l_message* msg);
L_EXTERN l_strn l_message_data(struct l_message* msg);

L_EXTERN l_umedit l_get_mssg_id(lnlylib_env* E);
L_EXTERN l_umedit l_get_mgid_cust(lnlylib_env* E);
L_EXTERN l_strn l_get_mssg_data(lnlylib_env* E);

L_EXTERN l_ulong l_service_id(struct l_service* srvc);
L_EXTERN void* l_service_data(struct l_service* srvc);
L_EXTERN l_ulong l_get_srvc_id(lnlylib_env* E);
L_EXTERN void* l_get_srvc_data(lnlylib_env* E);

L_EXTERN void l_send_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong srvc_id, void* data, l_umedit size);
L_EXTERN void l_send_data_moved_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong srvc_id, void* data, l_umedit size);
L_EXTERN void l_stop_service_specific(lnlylib_env* E, l_ulong svrc_id);
L_EXTERN void l_stop_service(lnlylib_env* E);

L_EXTERN int lnlylib_main(void (*start)(void), int argc, char** argv);

#endif /* LNLYLIB_CORE_BEAT_H */

