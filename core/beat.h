#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

struct l_global;
struct l_worker;
struct l_service;
struct l_message;

typedef struct {
  const char* service_name;
  void* (*service_on_create)(struct lnlylib_env*);
  void (*service_on_destroy)(struct lnlylib_env*);
  void (*service_proc)(struct lnlylib_env*);
} l_service_callback;

L_EXTERN struct l_message* l_get_message(struct lnlylib_env* E);
L_EXTERN struct l_service* l_get_service(struct lnlylib_env* E);
L_EXTERN struct l_worker* l_get_worker(struct lnlylib_env* E);

L_EXTERN l_umedit l_message_id(struct message* msg);
L_EXTERN l_umedit l_message_cust(struct message* msg);
L_EXTERN l_strn l_message_data(struct message* msg);

L_EXTERN l_umedit l_get_mssg_id(struct lnlylib_env* E);
L_EXTERN l_umedit l_get_mgid_cust(struct lnlylib_env* E);
L_EXTERN l_strn l_get_mssg_data(struct lnlylib_env* E);

L_EXTERN l_ulong l_service_id(struct l_service* srvc);
L_EXTERN void* l_service_data(struct l_service* srvc);
L_EXTERN l_ulong l_get_srvc_id(struct lnlylib_env* E);
L_EXTERN void* l_get_srvc_data(struct lnlylib_env* E);

L_EXTERN void l_send_message(struct lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong srvc_id, void* data, l_umedit size);
L_EXTERN void l_send_data_moved_message(struct lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong srvc_id, void* data, l_umedit size);
L_EXTERN void l_stop_service_specific(struct lnlylib_env* E, l_ulong svrc_id);
L_EXTERN void l_stop_service(struct lnlylib_env* E);

L_EXTERN int lnlylib_main(void (*start)(void), int argc, char** argv);

#endif /* LNLYLIB_CORE_BEAT_H */

