#ifndef LNLYLIB_CORE_BEAT_H
#define LNLYLIB_CORE_BEAT_H
#include "core/base.h"

struct l_global;
struct l_worker;
struct l_service;
struct l_message;
struct l_coroutine;

typedef struct {
  void* svud;
  struct l_message* cmsg

  lua_State* L;
  struct l_coroutine* co;

  l_string* cstr;
  l_stanfile* logfile;

  struct l_service* csvc;
  struct l_worker* cthr;
  struct l_global* G;
} lnlylib_env;

typedef struct {
  void* (*service_on_create)(lnlylib_env*);
  void (*service_on_destroy)(lnlylib_env*);
  void (*service_proc)(lnlylib_env*);
} l_service_callback;

L_EXTERN void l_send_message(lnlylib_env* E, l_uint dest_svid, l_uint mgid, l_umedit flags, void* data, l_umedit size);
L_EXTERN void l_create_service(lnlylib_env* E, l_service_callback cb, l_uint flags);
L_EXTERN void l_create_service_from_module(lnlylib_env* E, l_strn module_name, l_uint flags);
L_EXTERN void l_stop_service_specific(lnlylib_env* E, l_uint svid);
L_EXTERN void l_stop_service(lnlylib_env* E);

L_EXTERN int lnlylib_main(void (*start)(void), int argc, char** argv);

#endif /* LNLYLIB_CORE_BEAT_H */

