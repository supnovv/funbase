#define LNLYLIB_API_IMPL
#include <stdlib.h>
#include "core/beat.h"
#include "core/lapi.h"
#include "osi/base.h"

#define L_SRVC_FLAG_ALIVE 0x01
#define L_SOCK_FLAG_LISTEN 0x02
#define L_SOCK_FLAG_CONNECT 0x04
#define L_SOCK_FLAG_INPROGRESS 0x08
#define L_MIN_USER_SERVICE_ID 256
#define L_MIN_SRVC_TABLE_SIZE 1024
#define L_SERVICE_LAUNCHER 0x01

#define L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ 0x02
#define L_SRVC_FLAG_STOP_SERVICE 0x04
#define L_SRVC_FLAG_CREATE_FROM_MODULE 0x08
#define L_SRVC_FLAG_CREATE_LUA_SERVICE 0x10
#define L_SRVC_FLAG_DESTROY_SERVICE 0x20

#define L_MSSG_FLAG_DONT_FREE 0x01 /* dont free message automatically */
#define L_MSSG_FLAG_FREE_DATA 0x02 /* free msgdata also when free message */
#define L_MSSG_FLAG_REMOTE_MSG 0x04

#define L_MASTER_THRIDX 1000
#define L_WORKER_THRIDX 1001
#define L_THREAD_THRIDX 2001
#define L_WORK_FLAG_QUIT 0x01
#define L_MAST_FLAG_QUIT 0x01

#define L_MIN_USER_MSG_ID 0x0100
#define L_MSG_WORKER_FEEDBACK    0x01
#define L_MSG_SERVICE_CREATE_REQ 0x02
#define L_MSG_SUBSRVC_CREATE_RSP 0x03
#define L_MSG_SERVICE_STOP_REQ   0x04
#define L_MSG_SERVICE_ON_CREATE  0x05
#define L_MSG_SERVICE_ON_DESTROY 0x06

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

typedef struct l_thread {
  l_thrhdl thrhdl;
  l_umedit thridx;
  int (*start)(lnlylib_env*);
  lnlylib_env* E;
  void* thrd_alloc;
  l_ostream logout;
} l_thread;

typedef struct {
  l_umedit num_workers;
  l_umedit init_stbl_size;
  l_string start_script;
  l_filename log_file_name;
} l_config;

typedef struct {
} l_cmdline;

typedef struct {
  l_smplnode node; /* chained to free q */
  l_squeue bkmq; /* service backup message q */
  struct l_service* service;
  l_umedit seed_num;
  l_ushort flags;
  l_ushort events;
} l_srvcslot;

typedef struct {
  l_umedit capacity;
  l_umedit num_services;
  l_squeue free_slots;
  l_srvcslot* slot_arr;
} l_srvctable;

typedef struct {
  struct l_worker* worker;
  l_squeue mast_rxmq;
  l_mutex mast_rxlk;
} l_worknode;

typedef struct l_master {
  l_thread T;
  l_config* conf;
  l_cmdline* cmds;
  l_mutex* qlock;
  l_condv* qcndv;
  l_squeue* globalq;
  l_squeue* mast_frmq;
  l_squeue* mast_frsq;
  l_squeue* temp_svcq;
  l_srvctable* stbl;
  l_umedit srvc_seed;
  l_umedit num_workers;
  l_worknode* node_arr;
  l_squeue queue[4];
  l_srvctable srvc_tbl;
  lnlylib_env main_env;
  l_config config;
  l_cmdline cmdline;
  l_mutex globalq_lock;
  l_condv globalq_cndv;
} l_master;

typedef struct l_worker {
  l_thread T;
  l_umedit work_flags;
  l_medit weight;
  l_squeue* work_frmq;
  l_squeue* work_txme; /* msgs send to current service */
  l_squeue* work_txmq; /* msgs send to other services */
  l_squeue* work_txms; /* msgs send to master */
  l_squeue* mast_rxmq;
  l_mutex* mast_rxlk;
  l_squeue msgq[4];
  lnlylib_env ENV;
} l_worker;

typedef struct l_coroutine {
  l_smplnode node; /* chain to free q */
  l_umedit seed_num;
  l_umedit wait_mgid;
  l_umedit mgid_cust;
  int coref;
  lua_State* co;
} l_coroutine;

/* lua state need about 5K memory, and a coroutine need about 1K memory */
typedef struct {
  l_smplnode node;
  l_umedit capacity;
  l_umedit coro_seed;
  l_squeue free_coro;
  l_coroutine* coro_arr;
  lua_State* L;
} l_corotable;

typedef struct l_service {
  l_smplnode node; /* chained in global q */
  l_squeue srvc_msgq;
  l_filehdl ioev_hdl;
  l_umedit srvc_flags;
  l_ulong srvc_id; /* the highest bit is for remote service or not */
  l_corotable* coro_tabl; /* only created for lua service */
  l_service_callback* cb;
  void* ud;
} l_service;

typedef struct {
  l_umedit a, b, c, d;
  l_ulong l, m, n, o;
  l_uint u, v, w, x;
  void *p, *q, *r, *s;
} l_msgdata;

typedef struct l_message {
  l_smplnode node;
  l_ulong mssg_dest;
  l_ulong sess_dest;
  l_ulong mssg_from;
  l_ulong sess_from;
  l_umedit mssg_id; /* high 32-bit is id, lower 32-bit's behavior is user defined */
  l_umedit mgid_cust; /* lower 32-bit's behavior is user defined */
  l_umedit mssg_flags;
  l_umedit data_size;
  void* mssg_data;
  l_msgdata extra;
} l_message;

typedef struct {
  l_bool enable;
  l_bool listen;
  l_filehdl hdl;
  const char* ip;
  l_ushort port;
} l_srvc_ioev_data;

typedef struct {
  const char* module;
  l_service_callback* cb;
  l_srvc_ioev_data ioev;
  void* svud;
} l_service_create_req;

typedef struct {
  l_service* service;
  l_squeue txmq;
  l_squeue txms;
} l_worker_feedback;

L_EXTERN l_srvc_ioev_data
L_LISTEN(const char* ip, l_ushort port)
{
  return (l_srvc_ioev_data){true, true, L_EMPTY_HDL, ip, port};
}

L_EXTERN l_srvc_ioev_data
L_CONNECT(const char* ip, l_ushort port)
{
  return (l_srvc_ioev_data){true, false, L_EMPTY_HDL, ip, port};
}

L_EXTERN l_srvc_ioev_data
L_USEHDL(l_filehdl hdl)
{
  return (l_srvc_ioev_data){true, false, hdl, 0, 0};
}

L_EXTERN l_srvc_ioev_data
L_NODATA()
{
  return (l_srvc_ioev_data){false, false, L_EMPTY_HDL, 0, 0};
}

/* l_send_message() copy data */
/* l_send_message_with_data_moved() move allocated data */

static void
l_message_free_data(lnlylib_env* E, l_message* msg)
{
  if (msg->mssg_data == 0 || msg->mssg_data == &msg->extra) {
    return;
  }

  if (msg->mssg_flags & L_MSSG_FLAG_FREE_DATA) {
    L_MFREE(E, msg->mssg_data);
  }
}

/** memory alloc functions in <stdlib.h> **
void* malloc(size_t size);
void* calloc(size_t num, size_t size);
void* realloc(void* p, size_t size);
void free(void* p);
---
realloc changes the size of the memory block pointed by buffer. It
may move the memory block to a new location (its address is returned
by the function). The content of the memory block is preserved up to
the lesser of the new and old sizes, even if the block is moved to a
new location. ***If the new size is larger, the value of the newly
allocated portion is indeterminate***.
In case of that buffer is a null pointer, the function behaves like malloc,
assigning a new block of size bytes and returning a pointer to its beginning.
If size is zero, the memory previously allocated at buffer is deallocated
as if a call to free was made, and a null pointer is returned. For c99/c11,
the return value depends on the particular library implementation, it may
either be a null pointer or some other location that shall not be dereference.
If the function fails to allocate the requested block of memory, a null
pointer is returned, and the memory block pointed to by buffer is not
deallocated (it is still valid, and with its contents unchanged). **/

static void*
lnlylib_rawalloc(void* ud, void* p, l_ulong oldsz, l_ulong newsz)
{
  L_UNUSED(ud);
  L_UNUSED(oldsz);
  if (newsz == 0) {
    if (p) free(p);
    return 0;
  } else if (p == 0) {
    return malloc(newsz);
  } else {
    return realloc(p, newsz);
  }
}

l_mallocfunc l_malloc_func = lnlylib_rawalloc;

static l_master* L_MASTER;

#if defined(L_THREAD_LOCAL)
static L_THREAD_LOCAL lnlylib_env* L_ENV;
#else
l_thrkey L_ENV_THRKEY;
#endif

static void
l_threadlocal_prepare()
{
#if defined(L_THREAD_LOCAL)
  L_ENV = 0;
#else
  l_thrkey_init(&L_ENV_THRKEY);
  l_thrkey_set_data(&L_ENV_THRKEY, 0);
#endif
}

static void
l_threadlocal_set(lnlylib_env* E)
{
#if defined(L_THREAD_LOCAL)
  L_ENV = E;
#else
  l_thrkey_set_data(&L_ENV_THRKEY, E);
#endif
}

static lnlylib_env*
l_threadlocal_get()
{
#if defined(L_THREAD_LOCAL)
  return L_ENV;
#else
  return l_thrkey_get_data(&L_ENV_THRKEY);
#endif
}

static void
l_thread_init(l_thread* T, l_umedit thridx, lnlylib_env* env, l_config* conf)
{
  T->thridx = thridx;
  T->start = 0;
  T->E = env;
  if (conf == 0) {
    T->logout = l_stdout_ostream();
    T->thrd_alloc = 0;
  } else {
    T->logout = l_config_logout(conf, thridx);
    T->thrd_alloc = l_thrdalloc_create();
  }
}

static void*
l_thread_proc(void* para)
{
  lnlylib_env* env = (lnlylib_env*)para;
  return (void*)(l_int)env->T->start(env);
}

static void
l_thread_start(l_thread* T, int (*start)(lnlylib_env*))
{
  T->start = start;
  l_thrhdl_create(&T->thrhdl, l_thread_proc, T->E);
}

static void
l_thread_join(l_thread* T)
{
  l_thrhdl_join(&T->thrhdl);
}

static void
l_srvcslot_init(l_srvcslot* slot)
{
  l_squeue_init(&slot->bkmq);
  slot->service = 0;
  slot->seed_num = 0;
  slot->flags = 0;
  slot->events = 0;
}

static void
l_srvctable_init(l_master* M, l_srvctable* stbl, l_umedit init_size)
{
  l_srvcslot* slot = 0;
  l_umedit i = 0;

  stbl->capacity = init_size;
  stbl->num_services = 0;
  l_squeue_init(&stbl->free_slots);
  stbl->slot_arr = L_MALLOC_TYPE_N(M->E, l_srvcslot, stbl->capacity);

  for (i = 0; i < stbl->capacity; ++i) {
    slot = stbl->slot_arr + i;
    l_srvcslot_init(slot);

    if (i >= L_MIN_USER_SERVICE_ID) {
      l_squeue_push(&stbl->free_slots, &slot->node);
    }
  }
}

static l_srvcslot*
l_srvctable_alloc_slot(l_master* M, l_srvctable* stbl)
{
  l_squeue* free_slots = 0;
  l_srvcslot* slot = 0;

  free_slots = &stbl->free_slots;
  if ((slot = (l_srvcslot*)l_squeue_pop(free_slots))) {
    stbl->num_services += 1;
    return slot;
  }

  l_logw_2("stbl is full: capacity %d num_services %d",
      ld(stbl->capacity), ld(stbl->num_services));

  { l_srvcslot* new_sarr = 0;
    l_umedit new_size = 0;
    l_umedit i = 0;

    new_size = stbl->capacity * 2;
    if (new_size <= stbl->capacity) {
      l_loge_2("current stbl is too large %d", stbl->capacity);
      return 0;
    }

    l_logw_1("stbl alloced to new size %d", ld(new_size));
    new_sarr = L_MALLOC_TYPE_N(M->E, l_srvcslot, new_size);

    /* copy the old slots and free the old */

    for (i = 0; i < stbl->capacity; ++i) {
      new_sarr[i] = stbl->slot_arr[i];
    }

    L_MFREE(M->E, stbl->slot_arr);

    /* init the new slots */

    for (; i < new_size; ++i) {
      slot = new_sarr + i;
      l_srvcslot_init(slot);

      if (i >= L_MIN_USER_SERVICE_ID) {
        l_squeue_push(&stbl->free_slots, &slot->node);
      }
    }

    stbl->capacity = new_size;
    stbl->slot_arr = new_sarr;
  }

  if ((slot = (l_srvcslot*)l_squeue_pop(free_slots))) {
    stbl->num_services += 1;
    return slot;
  }

  return 0;
}

static void
l_config_load(l_config* C)
{
  lua_State* L = ll_newstate();

  l_string_init(&C->start_script);

  C->num_workers = luastate_readint(c->L, "workers");
  if (C->num_workers < 1) {
    C->num_workers = 1;
  }

  C->min_stbl_size = l_config_read_int(L, "service_table_size");
  if (C->min_stbl_size < L_MIN_SRVC_TABLE_SIZE) {
    C->min_stbl_size = L_MIN_SRVC_TABLE_SIZE;
  }

  // ...

  luastate_close(&L);
}

static lnlylib_env*
l_master_init(int (*start)(lnlylib_env*), int argc, char** argv)
{
  l_master* M = 0;
  l_thrdalloc thrd_alloc;
  lnlylib_env* main_env = 0;
  l_worknode* work_node = 0;
  l_worker* worker = 0;
  l_umedit i = 0;

  l_threadlocal_prepare();

  M = L_MALLOC_TYPE(LNUL, l_master);
  L_MASTER = M;

  l_zero(&M, sizeof(l_master));

  M->conf = &M->config;
  M->cmds = &M->cmdline;

  M->qlock = &M->globalq_lock;
  M->qcndv = &M->globalq_cndv;
  l_mutex_init(M->qlock);
  l_condv_init(M->qcndv);

  l_squeue_init(M->queue + 0);
  l_squeue_init(M->queue + 1);
  l_squeue_init(M->queue + 2);
  l_squeue_init(M->queue + 3);
  M->globalq = M->queue + 0;
  M->mast_frmq = M->queue + 1;
  M->mast_frsq = M->queue + 2;
  M->temp_svcq = M->queue + 3;

  l_thread_init(&M->T,  L_MASTER_THRIDX, &M->main_env, 0);
  l_thrdalloc_init(&thrd_alloc);
  M->T.thrd_alloc = &thrd_alloc;
  M->T.thrhdl = l_thrhdl_self();
  M->T.start = start;

  main_env = &M->main_env;
  main_env->M = M;
  main_env->T = &M->T;
  main_env->LOG = &M->T.logout;
  main_env->ALLOC = M->T.thrd_alloc;

  l_threadlocal_init(main_env);

  l_config_load(conf);
  M->T.logout = l_config_logout(conf, L_MASTER_THRIDX);
  M->T.thrd_alloc = l_thrdalloc_create();
  main_env->LOG = &M->T.logout;
  main_env->ALLOC = M->T.thrd_alloc;

  l_parse_cmd_line(main_env, argc, argv);

  /* init service table */

  M->srvc_seed = 0;
  M->stbl = &M->srvc_tbl;
  l_srvctable_init(main_env, M->stbl, conf->min_stbl_size);

  /* init worker threads */

  M->num_workers = conf->num_workers;
  M->node_arr = L_MALLOC_TYPE_N(main_env, l_worknode, M->num_workers);

  for (i = 0; i < M->num_workers; ++i) {
    work_node = M->node_arr + i;
    worker = L_MALLOC_TYPE(main_env, l_worker);
    l_thread_init(&worker->T, L_WORKER_THRIDX + i, &worker->env, conf);
    worker->work_flags = 0;
    worker->weight = i / 4 - 1;

    l_squeue_init(worker->mq + 0);
    l_squeue_init(worker->mq + 1);
    l_squeue_init(worker->mq + 2);
    l_squeue_init(worker->mq + 3);
    worker->work_frmq = worker->mq + 0;
    worker->work_txme = worker->mq + 1;
    worker->work_txmq = worker->mq + 2;
    worker->work_txms = worker->mq + 3;

    worker->mast_rxmq = &work_node->mast_rxmq;
    worker->mast_rxlk = &work_node->mast_rxlk;
    l_squeue_init(worker->mast_rxmq);
    l_mutex_init(worker->mast_rxlk);

    worker->env.M = M;
    worker->env.T = &worker->T;
    worker->env.S = 0;
    worker->env.MSG = 0;
    worker->env.LOG = &worker->T.logout;
    worker->env.SVUD = 0;
    worker->env.ALLOC = worker->T.thrd_alloc;
    worker->env.L = 0;
    worker->env.CO = 0;

    work_node->worker = worker;
  }

  return main_env;
}

static int
l_master_loop(lnlylib_env* main_env)
{
  l_global* G = main_env->G;
  l_squeue* Q = &G->Q;
  l_mutex* QLOCK = &G->QLOCK;
  l_condv* QCNDV = &G->QCNDV;

  l_master* M = &G->master;
  l_srvctable* stbl = &M->stbl;
  l_squeue* temp_svcq = &M->temp_svcq;
  l_service* S = 0;
  l_srvcslot* srvc_slot = 0;

  l_umedit num_workers = M->num_workers;
  l_squeue* mast_frmq = &M->mast_frmq;
  l_squeue* mast_frsq = &M->mast_frsq;
  l_worknode* work_node = 0;
  l_message* MSG = 0;
  l_int global_q_is_empty = 0;
  l_int i = 0;
  l_int events = 0;

  l_service* launcher = 0;
  l_message* on_create_msg = 0;

  l_squeue rxmq;
  l_squeue rxms;
  l_squeue svcq;

  l_squeue_init(&rxmq);
  l_squeue_init(&rxms);
  l_squeue_init(&svcq);

  /* launcher is the service to bang the whole new world */
  launcher = l_master_create_reserved_service(M, L_SERVICE_LAUNCHER, &l_launcher_service_cb, 0);
  on_create_msg = l_master_create_message(M, L_MSG_SERVICE_ON_CREATE, launcher->srvc_id, 0, 0, 0);
  l_squeue_push(&launcher->srvc_msgq, &on_create_msg->node);
  stbl->slot_arr[launcher->srvc_id >> 32].service = 0; /* need detach the service from the table first before insert into global q to handle */
  l_squeue_push(&svcq, &launcher->node);

  for (; ;) {

    if (l_squeue_nt_empty(&svcq)) {
      l_mutex_lock(QLOCK);
      global_q_is_empty = l_squeue_is_empty(Q);
      l_squeue_push_queue(Q, &svcq->node);
      l_mutex_unlock(QLOCK);

      if (global_q_is_empty) {
        l_condv_broadcast(QCNDV);
      }
    } else {
      l_rawapi_sleep(30);
    }

    if (M->mast_flags & L_MAST_FLAG_QUIT) {
      break;
    }

    for (i = 0; i < num_workers; ++i) {
      work_node = M->node_arr + i;
      l_mutex_lock(&work_node->mast_rxlk);
      l_squeue_push_queue(&rxmq, &work_node->mast_rxmq);
      l_mutex_unlock(&work_node->mast_rxlk);
    }

    while ((MSG = (l_message*)l_sqeueue_pop(&rxmq))) {
      switch (MSG->mgid >> 32) {
      case L_MSG_WORKER_FEEDBACK: {
        l_worker_feedback_data* feedback = 0;
        feedback = (l_worker_feedback_data)&MSG->mssg_data;
        S = feedback->service;
        srvc_slot = stbl->slot + (S->srvc_id >> 32);
        srvc_slot->service = S; /* dock the service to the table first */

        /* insert backup q's meesage to service, and add service to tempq to handle */
        l_squeue_push_queue(&S->srvc_msgq, &srvc_slot->bkmq);
        l_squeue_push(temp_svcq, &S->node);
        l_assert((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0);
        S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;

        /* handle messages */
        l_master_deliver_messages(M, &feedback->txmq);
        l_master_handle_self_messages(M, &feedback->txms);

        /* handle io events if any pending */
        if (l_filhdl_nt_empty(&srvc_slot->iohdl) && srvc_slot->events) {
          l_master_generate_io_messages(S, srvc_slot);
        }

        /* check stop service flag */
        if (S->srvc_flags & L_SRVC_FLAG_STOP_SERVICE) {
          S->srvc_flags &= (~L_SRVC_FLAG_STOP_SERVICE);
          l_master_stop_service(M, S->srvc_id);
        }

        /* check destroy service flag */
        if (S->srvc_flags & L_SRVC_FLAG_DESTROY_SERVICE) {
          S->srvc_flags &= (~L_SRVC_FLAG_DESTROY_SERVICE);
          l_master_destroy_service(M, S->srvc_id);
        }}
        break;
      default:
        l_loge_3("unrecognized master message %d", ld(MSG->mgid));
        break;
      }

      l_squeue_push(mast_frmq, &MSG->node); /* TODO: how to free msgdata */
    }

    events = l_ioevmgr_try_wait(M->evmgr, l_master_dispatch_io_event);
    if (events > 0) {
      events += l_ioevmgr_try_wait(M->evmgr, l_master_dispatch_io_event);
    }

    while ((S = (l_service*)l_squeue_pop(temp_svcq))) {
      srvc_slot = stbl->slot + (S->srvc_id >> 32);

      if (l_squeue_is_empty(&S->srvc_msgq)) {
        l_assert(srvc_slot->service); /* keep the service docked */
      } else {
        srvc_slot->service = 0; /* detach the service that wait to handle */
        l_squeue_push(&svcq, &S->node);
      }

      S->srvc_flags &= ~L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
    }
  }

  return 0;
}

static int
l_worker_loop(lnlylib_env* ENV)
{
  l_worker* W = (l_worker*)ENV->T;
  l_squeue* Q = ENV->M->globalq;
  l_mutex* QLOCK = ENV->M->qlock;
  l_condv* QCNDV = ENV->M->qcndv;
  l_service* S = 0;
  l_message* MSG = 0;
  int i = 0, n = 0;

  l_logm_1("worker %d started", ld(W->T.thridx));

  for (; ;) {
    l_mutex_lock(QLOCK);
    while (l_squeue_is_empty(Q)) {
      l_condv_wait(QCNDV, QLOCK);
    }
    S = (l_service*)l_squeue_pop(Q);
    l_mutex_unlock(QLOCK);

    ENV->S = S;
    ENV->SVUD = S->ud;
    W->work_flags = 0;

    n = l_messages_should_handle(W);
    for (i = 0; i < n; ++i) {
      MSG = (l_message*)l_squeue_pop(&S->srvc_msgq);
      if (MSG == 0) { break; }
      ENV->MSG = MSG;
      switch (MSG->mssg_id) {
      case L_MSG_SERVICE_ON_CREATE:
        if (S->cb.service_on_create) {
          void* svud = 0;
          svud = S->cb.service_on_create(ENV);
          if (S->ud == 0) {
            ENV->SVUD = S->ud = svud;
          } else if (svud != 0) {
            l_loge_1("the srvc %d user data already assigned", ld(S->srvc_id));
          }
        }
        break;
      case L_MSG_SERVICE_ON_DESTROY:
        if (S->cb.service_on_destroy) {
          S->cb.service_on_destroy(ENV);
        }
        S->srvc_flags |= L_SRVC_FLAG_DESTROY_SERVICE;
        break;
      default:
        if (S->cb.service_proc) {
          S->cb.service_proc(ENV);
        }
        break;
      }
      l_squeue_push(W->work_frmq, &msg->node); /* TODO: how to free msgdata */
    }

    l_worker_flush_messages(ENV);

    if (W->work_flags & L_WORK_FLAG_QUIT) {
      break;
    }
  }

  l_logm_1("worker %d exited", ld(W->T.thridx));
  return 0;
}

L_EXTERN int
lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv)
{
  lnlylib_env* main_env = l_master_init(start, argc, argv);
  l_master* M = main_env->M;
  l_worker* W = 0;
  l_umedit i = 0;
  int exit_code = 0;

  l_logm_s("master started");

  for (i = 0; i < M->num_workers; ++i) {
    W = M->node_arr[i].worker;
    l_thread_start(&W->T, l_worker_loop);
  }

  exit_code = l_master_loop(main_env);
  l_logm_s("master loop complete %d", ld(exit_code));

  for (i = 0; i < M->num_workers; ++i) {
    W = M->node_arr[i].worker;
    l_thread_join(&W->T);
    l_logm_s("worker %d joined", ld(W->T.thridx));
  }

  l_logm_s("master exited");
  l_master_exit(main_env);
  return 0;
}

static l_umedit
l_get_srvc_seed(l_master* M)
{
  l_umedit seed = ++M->srvc_seed;
  if (seed == 0) {
    return ++M->srvc_seed;
  } else {
    return seed;
  }
}

static void
l_service_init(l_service* S, l_umedit svid, l_umedit seed, l_service_callback* cb, l_umedit flags)
{
  l_squeue_init(&S->srvc_msgq);
  S->ioev_hdl = L_EMPTY_HDL;
  S->srvc_flags = flags;
  S->srvc_id = (((l_ulong)svid) << 32) | seed;
  S->coro_tabl = 0;
  S->cb = cb;
  S->ud = 0;
}

static void
l_service_free_co(l_service* S)
{
}

static l_service_callback*
l_master_load_service_module(const char* module)
{
  l_service_callback* cb = 0;
  l_dynlib hdl = l_empty_dynlib();

  hdl = l_dynlib_open2(l_const_strn(LNLYLIB_CLIB_DIR), module_name); /* TODO: 1. do module cache? 2. multiple service can exist in one module */
  if (l_dynlib_is_empty(&hdl)) {
    l_loge_1("open library %strn failed", lstrn(&module_name));
    return 0;
  }

#if 0
  cb.service_proc = (void (*)(lnlylib_env*))l_dynlib_sym2(&hdl, module_name, l_const_strn("service_proc"));
  if (cb.service_proc == 0) {
    l_loge_1("load %strn_service_proc failed", l_strn(&module_name));
    return 0;
  }

  cb.service_on_create = (void* (*)(lnlylib_env*))l_dynlib_sym2(&hdl, module_name, l_const_strn("service_on_create"));
  cb.service_on_destroy = (void (*)(lnlylib_env*))l_dynlib_sym2(&hdl, module_name, l_const_strn("service_on_destroy"));
#endif

  cb = (l_service_callback*)l_dynlib_sym2(&hdl, module_name, l_const_strn("callback"));
  return cb;
}

typedef struct {
  l_sockaddr local;
  l_service_callback* inconn_cb;
  l_umedit conns;
  l_dqueue connq;
  l_dqueue freeq;
} l_socket_listen_svud;

static void*
l_socket_listen_service_on_create(lnlylib_env* E)
{
  l_socket_listen_svud* data = 0;
  data = L_MALLOC_TYPE(E, l_socket_listen_svud);
  data->conss = 0;
  l_dqueue_init(&data->connq);
  l_dqueue_init(&data->freeq);
  return data;
}

static void
l_socket_listen_service_on_destroy(lnlylib_env* E)
{
  l_socket_listen_svud* data = 0;

  data = (l_socket_listen_svud*)E->svud;
  if (data) {
    L_MFREE(E, data);
  }

  E->svud = 0;
}

static void
l_socket_listen_service_accept_conn(void* ud, l_sockconn* conn)
{
  lnlylib_env* E = (lnlylib_env*)ud;
  l_socket_listen_svud* data = 0;
  l_socket_inconn_svud* inconn_svud = 0;
  l_sockaddr* remote = 0;

  data = (l_socket_listen_svud*)E->svud;
  inconn_svud = (l_socket_inconn_svud*)l_dqueue_pop(&data->freeq);
  if (inconn_svud == 0) {
    inconn_svud = L_MALLOC_TYPE(E, l_socket_inconn_svud);
  }

  remote = &conn->remote;
  inconn_svud.listen_srvc_data = data;
  inconn_svud.listen_svid = E->S->srvc_id;
  inconn_svud.upper_svid = 0;
  inconn_svud.rmt_port = l_sockaddr_port(remote);
  l_socketaddr_ipstr(remote, &inconn_svud.rmt_ip, sizeof(l_ipaddr));

  l_create_service(E, (l_service_create_req){
    l_socket_inconn_service_callback, 0,
    L_USEHDL(conn->sock),
    inconn_svud
  });
}

static void
l_socket_listen_service_proc(lnlylib_env* E)
{
  l_umedit mgid = 0;
  l_service* S = E->S;
  l_socket_listen_svud* srvc = 0;

  mgid = E->MSG->mssg_id;
  srvc = (l_socket_listen_svud*)E->svud;

  switch (mgid) {
  case L_MSG_SERVICE_STOP_REQ:
    break;
  case L_MSG_SERVICE_RESTART:
    break;
  /* messages from master */
  case L_MSG_SOCK_ACCEPT_IND:
    l_socket_accept(&S->ioev_hdl, l_socket_listen_service_accept_conn, E);
    break;
  case L_MSG_SOCK_ERROR:
    break;
  case L_MSG_SUBSRVC_CREATE_RSP: {
      l_subsrvc_create_rsp* rsp = 0;
      rsp = (l_subsrvc_create_rsp*)MSG->mssg_data;
      if (rsp->succ) {
        l_send_message(E, L_MSG_SOCK_CONNECTED, 0, rsp->svid, 0, 0, 0);
      } else {
        l_socket_inconn_svud* inconn_svud = 0;
        inconn_svud = (l_socket_inconn_svud*)rsp->svud;
        l_dqueue_push(&srvc->freeq, &inconn_svud->node);
      }
    }
    break;
  /* messages from accepted connection services */
  case L_MSG_SOCK_DISC_NTF:
    break;
  default:
    break;
  }
}

typedef struct {
  l_linknode node;
  l_socket_listen_svud* listen_svud;
  l_ulong listen_svid;
  l_ulong upper_svid;
  l_ipaddr rmt_ip;
  l_ushort rmt_port;
  l_squeue wrmq;
  l_squeue rdmq;
  l_umedit wrid;
  l_umedit rdid;
} l_socket_inconn_svud;

static void*
l_socket_inconn_service_on_create(lnlylib_env* E)
{
  l_socket_inconn_svud* svud = 0;
  svud = (l_socket_inconn_svud*)E->svud;
  l_create_service(E, (l_service_create_req){
    0, svud->listen_svud->inconn_cb,
    L_NODATA(),
    0});
  return 0;
}

typedef struct {
  l_ulong svid;
  void* svud;
  l_bool succ;
} l_subsrvc_create_rsp;

static void
l_socket_inconn_service_proc(lnlylib_env* E)
{
  /** message exchanges, RSP only send after REQ, NTF can send without REQ **
  L_MSG_SOCK_CONNIND => 
  L_MSG_SOCK_CONNECT <=
                     => L_MSG_SOCK_CONNDONE  =>  L_MSG_SOCK_CONN_NTF
                        L_MSG_SOCK_DISC_REQ  <=
                                             =>  L_MSG_SOCK_DISC_NTF (after disc, the data service is destroyed)
                     => L_MSG_SOCK_DISCDONE  =>  L_MSG_SOCK_DISC_NTF
                     => L_MSG_SOCK_ERROR     =>  L_MSG_SOCK_DISC_NTF
                     => L_MSG_DATA_READY_RX  =>  L_MSG_READ_DATA_IND
                        L_MSG_READ_DATA_REQ  <=
                                             =>  L_MSG_READ_DATA_RSP
                        L_MSG_WRITE_DATA_REQ <=
                     => L_MSG_DATA_READY_TX  =>  L_MSG_WRITE_DATA_RSP
  ********************************************************************/

  l_message* MSG = E->MSG;
  l_service* S = E->S;
  l_socket_inconn_svud* svud = 0;

  l_message* msg = 0;
  l_byte* data = 0;
  l_umedit size = 0;
  l_umedit done = 0;

  svud = (l_socket_inconn_svud*)E->svud;

  switch (MSG->mssg_id) {
  case L_MSG_SUBSRVC_CREATE_RSP: {
      l_subsrvc_create_rsp* rsp = 0;
      rsp = (l_subsrvc_create_rsp*)MSG->mssg_data;
      if (rsp->succ) {
        svud->upper_svid = rsp->svid;
      } else {
        /* TODO: disc the socket and stop the service */
      }
    }
    break;
  /* messages from lower socket events */
  case L_MSG_SOCK_CONNECTED: /* the link is connected, send L_MSG_SOCK_CONN_NTF to upper */
    break;
  case L_MSG_SOCK_DISCONNECTED: /* the link is disconnected, send L_MSG_SOCK_DISC_NTF to upper */
    break;
  case L_MSG_SOCK_ERROR: /* send L_MSG_SOCK_DISC_NTF to upper */
    break;
  case L_MSG_DATA_READY_RX: /* read data and may send L_MSG_READ_DATA_RSP to upper */
    if ((msg = l_squeue_top(&svud->rdmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_read(svud->fd, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        /* TODO */
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  case L_MSG_DATA_READY_TX: /* write data and may send L_MSG_WRITE_DATA_RSP to upper */
    if ((msg = l_squeue_top(&svud->wrmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_write(svud->fd, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        l_message_free_data(msg);
        l_message_init(msg, S->srvc_id, svud->up_srvc, L_MSG_WRITE_DATA_DONE, 0);
        msg->extra.a = ++svud->wrid;
        l_send_message_impl(E, msg);
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  /* messages from upper service */
  case L_MSG_READ_DATA_REQ: /* queue the read request */
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data alrady read */
      l_squeue_push(&svud->rdmq, &msg->node);
    }
    break;
  case L_MSG_WRITE_DATA_REQ: /* queue the write request */
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data already written */
      l_squeue_push(&svud->wrmq, &msg->node);
    }
    break;
  case L_MSG_SOCK_DISC_REQ: /* send L_MSG_SOCK_DISC_NTF to upper */
    break;
  default:
    break;
  }
}

typedef struct {
  l_sockaddr local;
  l_ipaddr rmt_ip;
  l_ushort rmt_port;
} l_socket_outconn_svud;

static void
l_socket_outconn_service_proc(lnlylib_env* E)
{
  l_umedit mgid = E->MSG->mssg_id;
  l_service* S = E->S;
  l_socket_outconn_svud* outconn = 0;

  outconn = (l_socket_outconn_svud*)E->svud;

  switch (mgid) {
  case L_MSG_SOCK_CONNECT_IND:
    if (l_socket_cmpl_connect(slot->iohdl)) {
      /* socket connected success */
      l_send_message(E, L_MSG_DATA_READY_TX, 0, S->srvc_id, 0, 0, 0);
    } else {
      l_send_message(E, L_MSG_SOCK_DISCONNECTED, 0, S->srvc_id, 0, 0, 0);
    }
    break;
  default:
    break;
  }
}

static l_service*
l_master_create_service(l_master* M, l_service_create_req* req, l_umedit svid)
{
  l_service* S = 0;
  l_srvcslot* slot = 0;
  l_srvctable* stbl = 0;
  l_service_callback* cb = 0;
  l_fildhdl ioev_hdl = L_EMPTY_HDL;
  l_ushort flags = 0;
  l_ushort events = 0;

  if (req->module) {
    cb = l_master_load_service_module(req->module);
  } else {
    cb = req->cb;
  }

  if (cb == 0) {
    l_loge_s("service create fail due to null cb");
    return 0;
  }

  if (req->ioev.enable) {
    if (l_fildhdl_is_empty(req->ioev.hdl)) {
      l_sockaddr sa;
      if (!req->ioev.ip || !req->ioev.poart) {
        l_loge_s("service create fail due to empty ip or port");
        return 0;
      }
      if (!l_sockaddr_init(&sa, l_strn_c(req->ioev.ip), req->ioev.port)) {
        l_loge_s("service create fail due to invalid address");
        return 0;
      }
      if (req->ioev.listen) {
        l_socket sock;
        sock = l_socket_tcp_listen(&sa, 0);
        if (l_socket_is_empty(&sock)) {
          l_loge_s("service create fail due to listen fail");
          return 0;
        }
        ioev_hdl = sock;
        flags |= L_SOCK_FLAG_LISTEN;
        events |= L_IO_EVENT_READ | L_IO_EVENT_ERR;
      } else {
        l_socket sock;
        l_bool done = false;
        sock = l_socket_tcp_connect(&sa, &done);
        if (l_socket_is_empty(&sock)) {
          l_loge_s("service create fail due to connect fail");
          return 0;
        }
        ioev_hdl = sock;
        flags |= L_SOCK_FLAG_CONNECT;
        if (!done) flags |= L_SOCK_FLAG_INPROGRESS;
        events |= L_IO_EVENT_RDWR | L_IO_EVENT_ERR | L_IO_EVENT_HUP | L_IO_EVENT_RHP;
      }
    } else {
      ioev_hdl = req->ioev.hdl;
    }
  }

  stbl = M->stbl;

  if (svid != 0) {
    if (svid >= L_MIN_USER_SERVICE_ID || svid >= stbl->capacity) {
      l_loge_1("invalid reserved service id %d", ld(svid));
      return 0;
    }

    slot = stbl->slot_arr + svid;
    if (slot->service || (slot->flags & L_SRVC_FLAG_ALIVE)) {
      l_loge_1("reserved service %d already created", ld(svid));
      return 0;
    }
  } else {
    slot = l_srvctable_alloc_slot(M, stbl);
    if (slot == 0) {
      l_loge_s("service create fail due to slot alloc");
      return 0;
    }
  }

  S = (l_service*)l_squeue_pop(&M->mast_frsq);
  if (S == 0) {
    S = L_MALLOC_TYPE(M->E, l_service);
  }

  if (S == 0) {
    l_loge_s("service create fail due to malloc");
    return 0;
  }

  slot->seed_num = l_get_srvc_seed(M);
  l_service_init(S, slot->srvc_index, slot->seed_num, cb, 0);
  S->ioev_hdl = ioev_hel;

  slot->service = service; /* dock the service to the table */
  slot->flags |= L_SRVC_FLAG_ALIVE | flags;
  slot->events |= events;
  slot->iohdl = ioev_hdl;

  if (l_filehdl_nt_empty(&ioev_hdl)) { /* add this hdl to receive events */
    l_ioevmgr_add(&M->evmgr, ioev_hdl, S->srvc_id, slot->events);
    slot->events = 0; /* clear and prepare to receive incoming events */
  }

  return service;
}

L_EXTERN void
l_stop_service(lnlylib_env* E)
{
  E->csvc->srvc_flags |= L_SRVC_FLAG_STOP_SERVICE;
}

static void*
l_launcher_on_create(lnlylib_env* E)
{
  l_global* G = E->G;
  l_master* M = &G->master;
  l_config* C = &G->conf;

  l_logm_s("launcher on create");

  if (l_string_nt_empty(&C->start_script)) {
    ll_pcall(C->L, l_string_strn(&C->start_script));
  }

  if (M->start) {
    M->start();
  }

  l_stop_service(E);
  return 0;
}

static void
l_launcher_on_destroy(lnlylib_env* E)
{
  l_logm_1("launcher on destroy (ud %d)", ld(E->svud));
}

static void
l_launcher_service_proc(lnlylib_env* E)
{
  l_logm_1("launcher handle msg %d", ld(E->cmsg->mgid));
}

static l_service_callback
l_launcher_service_cb = {
  "LAUNCHER",
  l_launcher_on_create,
  l_launcher_on_destroy,
  l_launcher_service_proc
};

static l_message*
l_master_create_message(l_master* M, l_umedit mgid, l_ulong dest_svid, l_umedit flags, void* data, l_umedit size)
{
  l_message* msg = 0;

  msg = (l_message*)l_squeue_pop(M->mast_frmq);
  if (msg == 0) {
    msg = L_MALLOC_TYPE(M->E, l_message);
  }

  msg->mgid = (((l_ulong)mgid) << 32);
  msg->mssg_flags = flags;
  msg->mssg_from = 0;
  msg->from_coro = 0;

  /* TODO: consider flags */

  if (data && size > 0) {
    msg->data_size = size;
    l_assert(size <= sizeof(l_msgdata);
    l_copy_n(&msg->extra, data, size);
    msg->mssg_data = &msg->extra;
  } else if (size) {
    msg->data_size = 4;
    msg->extra.a = size; /* size as a 4-byte data */
    msg->mssg_data = &msg->extra;
  } else {
    msg->data_size = 0;
    msg->mssg_data = 0;
  }

  l_assert(l_service_nt_remote(dest_svid));
  msg->mssg_dest = dest_svid;
  msg->dest_coro = 0;
  return msg;
}

static void
l_master_insert_message(l_master* M, l_message* msg)
{
  l_srvctable* stbl = &M->stbl;
  l_uint mssg_dest = msg->mssg_dest;
  l_srvcslot* srvc_slot = 0;

  srvc_slot = stbl->slot_arr + mssg_dest;
  if (mssg_dest >= stbl->capacity || (srvc_slot->flags & L_SRVC_FLAG_ALIVE) == 0) {
    /* invalid message, just insert into free q. TODO: how to free msgdata */
    l_loge_3("invalid message %d from %d to %d", ld(msg->mgid), ld(msg->mssg_from), ld(mssg_dest));
    l_squeue_push(&M->mast_frmq, &msg->node);
  } else {
    if ((S = srvc_slot->service)) {
      /* service is docked in the service table, docked service is waiting to handle,
         push the message to service's message q and insert the service to temp q if needed */
      l_squeue_push(&S->srvc_msgq, &msg->node);
      if ((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0) {
        l_squeue_push(&M->temp_svcq, &S->node);
        S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
      }
    } else {
      /* service is already in global q or is handling in worker thread,
         just push the message to its backup message queue */
      l_squeue_push(&srvc_slot->bkmq, &msg->node);
    }
  }
}

static void
l_master_generate_io_messages(l_service* S, l_srvcslot* slot)
{
  l_master* M = L_MASTER;
  l_squeue* msgq = S->srvc_msgq;
  l_ushort events = 0;
  l_message* msg = 0;

  events = slot->events;
  slot->events = 0;

  if (events & L_IO_EVENT_WRITE) { /* send L_MSG_SOCK_CONN_IND or L_MSG_DATA_READY_TX message */
    if (slot->flags & L_SOCK_FLAG_CONNECT) {
      if (l_socket_cmpl_connect(slot->iohdl)) {
        msg = l_master_create_message(M, L_MSG_SOCK_CONNECTED, S->srvc_id, 0, 0, slot->flags & L_SOCK_FLAG_INPROGRESS);
        if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
        msg = l_master_create_message(M, L_MSG_DATA_READY_TX, S->srvc_id, 0, 0, 0);
        if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
      } else {
        msg = l_master_create_message(M, L_MSG_SOCK_DISCONNECTED, S->srvc_id, 0, 0, 0);
        if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
      }
      slot->flags &= ~(L_SOCK_FLAG_CONNECT | L_SOCK_FLAG_INPROGRESS);
    } else {
      msg = l_master_create_message(M, L_MSG_DATA_READY_TX, S->srvc_id, 0, 0, 0);
      if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
    }
  }

  if (events & L_IO_EVENT_READ) { /* send L_MSG_SOCK_ACCEPT_IND or L_MSG_DATA_READY_RX message */
    if (slot->flags & L_SOCK_FLAG_LISTEN) {
      msg = l_master_create_message(M, L_MSG_SOCK_ACCEPT_IND, S->srvc_id, 0, 0, 0);
      if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
    } else {
      msg = l_master_create_message(M, L_MSG_DATA_READY_RX, S->srvc_id, 0, 0, 0);
      if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
    }
  }

  if (events & (L_IO_EVENT_HUP | L_IO_EVENT_RHP)) { /* send L_MSG_SOCK_DISCONNECTED message */
    msg = l_master_create_message(M, L_MSG_SOCK_DISCONNECTED, S->srvc_id, 0, 0, events & L_IO_EVENT_RHP);
    if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
  }

  if (events & L_IO_EVENT_ERR) { /* send L_MSG_SOCK_ERROR message */
    msg = l_master_create_message(M, L_MSG_SOCK_ERROR, S->srvc_id, 0, 0, 0);
    if (msg) l_squeue_push(&S->srvc_msgq, &msg->node);
  }
}

static void
l_master_dispatch_io_event(l_ulong ud, l_ushort events)
{
  l_master* M = L_MASTER;
  l_srvctable* stbl = M->stbl;
  l_squeue* temp_svcq = &M->temp_svcq;
  l_service* S = 0;
  l_srvcslot* slot = 0;
  l_umedit svid = (l_umedit)(ud >> 32);
  l_umedit seed = (l_umedit)(ud & 0xffffffff);

  slot = stbl->slot_arr + svid;
  if (events == 0 || l_filehdl_is_empty(&slot->iohdl) || slot->seed_num != seed) {
    return;
  }

  slot->events |= events;

  S = slot->service;
  if (S == 0) { /* currently the service is waiting for handle or is hanlding, */
    return;   /* the events are handle when the service finish the current handle */
  }

  l_master_generate_io_messages(S, slot);

  if ((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0) {
    S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
    l_squeue_push(temp_svcq, &S->node);
  }
}

static void
l_master_deliver_messages(l_master* M, l_squeue* txmq)
{
  l_message* msg = 0;
  while ((msg = (l_message*)l_squeue_pop(txmq))) {
    l_master_insert_message(M, msg);
  }
}

static void
l_master_stop_service(l_master* M, l_ulong srvc_id)
{
  l_srvctable* stbl = &M->stbl;
  l_srvcslot* svc_slot = 0;
  l_umedit svid = srvc_id >> 32;

  srvc_slot = stbl->slot_arr + svid;
  if (svid >= stbl->capacity || (srvc_slot->flags & L_SRVC_FLAG_ALIVE) == 0) {
    l_loge_1("try to stop invalid service %d", ld(svid));
  } else {
    l_message* on_destroy_msg = 0;
    on_destroy_msg = l_master_create_message(M, L_MSG_SERVICE_ON_DESTROY, srvc_id, 0, 0, 0);
    l_master_insert_message(M, on_destroy_msg);
  }
}

static void
l_master_destroy_service(l_master* M, l_ulong srvc_id)
{
  l_srvctable* stbl = &M->stbl;
  l_srvcslot* srvc_slot = 0;
  l_service* S = 0;
  l_message* srvc_msg = 0;
  l_umedit svid = srvc_id >> 32;

  srvc_slot = stbl->slot_arr + svid;
  if (svid >= stbl->capacity || (srvc_slot->flags & L_SRVC_FLAG_ALIVE) == 0) {
    l_loge_1("try to destroy invalid service %d", ld(svid));
  } else {
    l_assert(srvc_slot->service); /* the service must docked when destroy */
    S = srvc_slot->service;
    srvc_slot->service = 0;
    srvc_slot->flags &= (~L_SRVC_FLAG_ALIVE);
    if (l_squeue_nt_empty(&S->srvc_msgq)) {
      l_loge_1("service %d destroyed with unhandled messages", ld(svid));
      while ((srvc_msg = (l_message*)l_squeue_pop(&S->srvc_msgq))) {
        l_squeue_push(&M->mast_frmq, &srvc_msg->node); /* TODO: how to free msgdata */
      }
    }
    l_service_free_co(S);
    l_squeue_push(&M->mast_frsq, &S->node);
  }
}

static void
l_master_handle_self_messages(l_master* M, l_squeue* txms)
{
  l_srvctable* stbl = &M->stbl;
  l_message* msg = 0;

  while ((msg = (l_message*)l_squeue_pop(txms))) {
    switch (msg->mgid >> 32) {
    case L_MSG_CREATE_SERVICE_REQ: {
      l_service* S = 0;
      l_service_create_req* create_data = 0;
      l_message* on_create_msg = 0;
      create_data = (l_service_create_req*)msg->mssg_data;
      /* create the service according to the request */
      if (create_data->flags & L_SRVC_FLAG_CREATE_FROM_MODULE) {
        S = l_master_create_service_from_module(M, create_data->module_name, create_data->flags);
      } else if (create_data->flags & L_SRVC_FLAG_CREATE_LUA_SERVICE) {
        /* TODO: how to create lua service */
      } else {
        S = l_master_create_service(M, create_data->cb, create_data->flags);
      }

      /*TODO: need response the service that send L_MSG_CREATE_SERVICE_REQ
      no matter the service created success or not */

      if (S == 0) {
      } else {
        // S->parent_svid = msg->mssg_from;
        /* make service's first message *ON_CREATE* */
        on_create_msg = l_master_create_message(M, L_MSG_SERVICE_ON_CREATE, S->srvc_id, 0, 0, 0);
        l_squeue_push(&S->srvc_msgq, &on_create_msg->node);
        /* insert the service to tempq to handle */
        l_master_insert_message(M, on_create_msg);
      }}
      break;
    case L_MSG_STOP_SERVICE_REQ:
      l_master_stop_service(M, msg->mssg_dest);
      break;
    default:
      l_loge_3("invalid master message %d", ld(msg->mgid));
      break;
    }
    l_squeue_push(mast_frmq, &msg->node); /* no need to free msgdata */
  }
}

static l_message*
l_create_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong dest_svid, l_umedit flags, void* data, l_umedit size)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_message* msg = 0;

  msg = (l_message*)l_squeue_pop(W->frmq);
  if (msg == 0) {
    msg = L_MALLOC_TYPE(E, l_message);
  }

  msg->mgid = (((l_ulong)mgid) << 32) | mgid_cust;
  msg->mssg_flags = flags;
  msg->mssg_from = S->srvc_id;
  msg->from_coro = E->coro ? E->coro->coid : 0;

  /* TODO: consider flags */

  if (data && size > 0) {
    msg->data_size = size;
    l_assert(size <= sizeof(l_msgdata);
    l_copy_n(&msg->mssg_data, data, size);
  } else {
    msg->data_size = 0;
  }

  msg->mssg_dest = dest_svid;
  if (l_service_is_remote(dest_svid)) {
    msg->mssg_flags |= L_MSSG_FLAG_REMOTE_MSG;
    /* TODO */
  } else {
    msg->mssg_flags &= (~L_MSSG_FLAG_REMOTE_MSG);
    msg->mssg_dest = dest_svid;
    msg->dest_coro = 0;
  }
  return msg;
}

static void
l_send_message_impl(lnlylib_env* E, l_message* msg)
{
  l_worker* W = E->cthr;

  if (msg == 0) {
    return;
  }

  if (msg->mssg_flags & L_MSSG_FLAG_REMOTE_MSG) {
    /* TODO */
  } else if (msg->mssg_dest == msg->mssg_from) {
    l_squeue_push(W->work_txme, &msg->node);
  } else {
    l_squeue_push(W->work_txmq, &msg->node);
  }
}

static void
l_send_master_message(lnlylib_env* E, l_umedit mgid, l_ulong dest_svid, l_umedit flags, void* data, l_umedit size)
{
  l_message* msg = 0;
  l_worker* W = E->cthr;

  msg = l_create_message(E, mgid, 0, dest_svid, flags, data, size);
  if (msg == 0) {
    return;
  }

  l_assert(dest_svid != msg->mssg_from);
  l_assert(l_service_nt_remote(dest_svid));
  l_squeue_push(W->work_txms, &msg->node);
}

static void /* lua message has dest coroutine need to be specified */
l_send_lua_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong mssg_dest, l_ulong dest_coro, l_umedit flags, void* data, l_umedit size)
{
  if (mgid < L_MIN_USER_MSG_ID) {
    l_loge_1("invalid message id %d", ld(mgid));
  } else {
    l_message* msg = l_create_message(E, mgid, mgid_cust, mssg_dest, flags, data, size);
    if (msg) {
      msg->dest_coro = dest_coro;
      l_send_message_impl(E, msg);
    }
  }
}

L_EXTERN void
l_send_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong dest_svid, l_umedit flags, void* data, l_umedit size)
{
  if (mgid < L_MIN_USER_MSG_ID) {
    l_loge_1("invalid message id %d", ld(mgid));
  } else {
    l_send_message_impl(E, l_create_message(E, mgid, mgid_cust, svid, flags, data, size));
  }
}

L_EXTERN l_bool
l_create_service(lnlylib_env* E, l_service_create_req* data)
{}

L_EXTERN void
l_create_service(lnlylib_env* E, l_service_callback* cb, l_uint flags)
{
  l_service_create_req create_service_req;
  flags &= ~(L_SRVC_FLAG_CREATE_FROM_MODULE | L_SRVC_FLAG_CREATE_LUA_SERVICE);
  create_service_req.flags = flags;
  create_service_req.cb = cb;
  l_send_master_message(E, L_MSG_CREATE_SERVICE_REQ, 0, 0, &create_service_req, sizeof(l_service_create_req));
}

L_EXTERN l_ulong
l_create_listen_service(lnlylib_env* E, l_strn ip, l_ushort port, l_service_callback* accept_service_cb)
{}

L_EXTERN l_ulong
l_create_connect_service(lnlylib_env* E, l_strn ip, l_ushort port, l_service_callback* connect_service_cb)
{}

L_EXTERN void
l_create_service_from_module(lnlylib_env* E, l_strn module_name, l_uint flags)
{
  l_service_create_req create_service_req;
  flags &= ~(L_SRVC_FLAG_CREATE_LUA_SERVICE);
  create_service_req.flags = flags | L_SRVC_FLAG_CREATE_FROM_MODULE;
  create_service_req.module_name = module_name;
  l_send_master_message(E, L_MSG_CREATE_SERVICE_REQ, 0, 0, &create_service_req, sizeof(l_service_create_req));
}

L_EXTERN void
l_create_lua_service(lnlylib_env* E, l_strn lualib_name)
{ /* TODO: how to create lua service */
}

L_EXTERN void
l_stop_service_specific(lnlylib_env* E, l_ulong svid)
{
  l_send_master_message(E, L_MSG_STOP_SERVICE_REQ, svid, 0, 0, 0);
}

static void
l_worker_flush_messages(lnlylib_env* E)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_message* msg = 0;
  l_worker_feedback_data feedback;

  l_squeue_push_queue(&S->srvc_msgq, W->work_txme);
  feedback.service = S;
  feedback.txmq = l_squeue_move(W->work_txmq);
  feedback.txms = l_squeue_move(W->work_txms);

  msg = l_create_message(E, L_MSG_WORKER_FEEDBACK, 0, 0, 0, &feedback, sizeof(l_worker_feedback_data));
  if (msg == 0) {
    return;
  }

  /* deliver the msg to master */
  l_mutex_lock(W->mast_rxlk);
  l_squeue_push(W->mast_rxmq, &msg->node);
  l_mutex_unlock(W->mast_rxlk);
}

/** string and logging **/

static void
l_start_logging(lnlylib_env* E, const l_byte* tag)
{
  l_ostream* out = 0;
  int thridx = 0;

  l_ostream_format_2(out, "%s %2x ", ls(tag), ld(thridx));
  return out;
}

L_EXTERN int l_impl_ostream_format_v(l_ostream* os, const void* fmt, l_int n, va_list vl);

L_EXTERN void
l_impl_logger_func(lnlylib_env* E, const void* tag, const void* fmt, ...)
{
  int level = l_cstr(tag)[0] - '0';
  int nargs = l_cstr(tag)[1];
  l_ostream* out = 0;
  va_list vl;

  if (!fmt || level > l_cur_log_level()) {
    return;
  }

  out = l_start_logging(E, l_cstr(tag) + 2);

  if (nargs == 'n') {
    va_start(vl, fmt);
    nargs = va_arg(vl, l_int);
    l_ostream_format_n(out, fmt, nargs, va_arg(vl, l_value*));
    va_end(vl);
  } else {
    va_start(vl, fmt);
    l_impl_ostream_format_v(out, fmt, nargs - '0', vl);
    va_end(vl);
  }

  l_ostream_write(out, L_NEWLINE, L_NL_SIZE);
}

/** interface for lua **/

static void
ll_set_extra(lua_State* co, lnlylib_env* env)
{
  /** void* lua_getextraspace(lua_State* L) **
  Returns a pointer to a raw memory area associated with the given Lua
  state. The application can use this area for any purpose; Lua does
  not use it for anything. Each new thread has this area initialized
  with a copy of the area of the main thread. By default, this area
  has the size of a pointer to void, but you can recompile Lua with
  a different size for this area. (See LUA_EXTRASPACE in luaconf.h)
  ********************************************************************/
  l_uint* extra = (l_uint*)lua_getextraspace(co);
  *extra = (l_uint)env;
}

static lnlylib_env*
ll_get_extra(lua_State* co)
{
  l_uint* extra = (l_uint*)lua_getextraspace(co);
  return (lnlylib_env*)(*extra);
}

typedef struct {
  lua_Unsigned mgid;
  lua_Unsigned service;
  lua_Unsigned session;
  lua_Unsigned format;
  void* data;
} l_msg_userdata;

/* c function registers for lua */
static const struct luaL_Reg clanglib[] = {
  {"name", cfuncname},
  {"new", cfuncnew},
  {NULL, NULL}
};

int luaopen_clanglib(lua_State* L)
{
  luaL_newlib(L, clanglib);
  return 1;
}

/* add metatable for type check and oop style access */

int luaopen_clanglib(lua_State* L)
{
  luaL_newmetatable(L, "package.clanglib");
  luaL_newlib(L, clanglib);
  return 1;
}

static int clanglibnew(lua_State* L)
{
  // new the object
  luaL_getmetatable(L, "package.clanglib");
  lua_setmetatable(L, -2);
  return 1; /* new userdata is already on the stack */
}

typedef union {
  lua_Integer i;
  lua_Unsigned u;
} l_Integer_Union;

static lua_Unsigned
ll_Integer2Unsigned(lua_Integer i)
{
  l_Integer_Union a;
  a.i = i;
  return a.u;
}

static lua_Integer
ll_Unsigned2Integer(lua_Unsigned u)
{
  l_Integer_Union a;
  a.u = u;
  return a.i;
}

static l_ulong
ll_checkunsigned(lua_State* L, int stackindex)
{
  lua_Integer i = luaL_checkinteger(L, stackindex);
  return (l_ulong)ll_Integer2Unsigned(i);
}

typedef struct {
  l_umedit size;
  l_umedit data[1];
} ll_packdata;

static int
ll_send_msg(lua_State* co)
{
  lnlylib_env* E = ll_get_extra(co);
  l_ulong mgid = ll_checkunsigned(co, 1);
  l_ulong dest = ll_checkunsigned(co, 2);
  l_umedit flags = (l_umedit)luaL_checkinteger(co, 3);
  ll_packdata* pack = (ll_packdata*)luaL_checkudata(co, 4, "lnlylib.packdata");
  l_send_lua_message(E, mgid, dest, 0, flags, pack->data, pack->size);
  return 0;
}

local heart = lnlylib_heartbeat
msg = heart.wait_message(msgid) -- the msg content is moved to stack by c layer
send_msg(mgid, mssg_dest, flags, data)
send_rsp(msg, mgid, flags, data)

static int
ll_send_rsp(lua_State* co)
{
  lnlylib_env* E = ll_get_extra(co);
  ll_mssgdata* dest = (ll_mssgdata*)luaL_checkudata(co, 1, "lnlylib.mssgdata");
  l_ulong mgid = ll_checkunsigned(co, 2);
  l_umedit flags = (l_umedit)luaL_checkinteger(co, 3);
  ll_packdata* pack = (ll_packdata*)luaL_checkudata(co, 4, "lnlylib.packdata");
  l_send_lua_message(E, mgid, dest->service, dest->session, flags, pack->data, pack->size);
  return 0;
}

static int
ll_wait_msg(lua_State* co)
{
  lnlylib_env* E = ll_get_extra(co);
  l_ulong mgid = ll_checkunsigned(co, 1);
  E->coro->wait_mgid = mgid;
  l_assert(E->coro->co == co);
  ll_yield_impl(co, 0);
  return 0;
}

L_EXTERN void
ll_yield_impl(lua_State* co, int nresults)
{
  int status = lua_yield(co, nresults);
  l_loge_1("lua_yield never returns to here %d", ld(status));
}

L_EXTERN int
ll_yield(lua_State* co)
{
  return ll_yield_impl(co, lua_gettop(L));
}

