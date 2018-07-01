#include "core/heartbeat.h"

#define L_SERVICE_ALIVE 0x01
#define L_MIN_USER_SERVICE_ID 256
#define L_MIN_SRVC_TABLE_SIZE 1024
#define L_SERVICE_LAUNCHER 0x01

#define L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ 0x01
#define L_SRVC_FLAG_STOP_SERVICE 0x02
#define L_SRVC_FLAG_DESTROY_SERVICE 0x04
#define L_MSSG_FLAG_FREE_EXTRA_DATA 0x01
#define L_WORK_FLAG_QUIT 0x01
#define L_MAST_FLAG_QUIT 0x01

#define L_MIN_USER_MSG_ID 0x0100
#define L_MSG_WORKER_FEEDBACK_IND 0x01
#define L_MSG_CREATE_SERVICE_REQ  0x02
#define L_MSG_STOP_SERVICE_REQ    0x03
#define L_MSG_SERVICE_ON_CREATE   0x04
#define L_MSG_SERVICE_ON_DESTROY  0x05

struct l_service;
typedef struct {
  l_smplnode node;
  struct l_service* service;
  l_uint slot_index; /* the lowest bit is for service alive or not */
  l_squeue bkmq; /* service backup message q */
} l_srvcslot;

typedef struct {
  l_uint capacity;
  l_uint num_services;
  l_squeue free_slots;
  l_srvcslot* slot_arr;
} l_srvctable;

struct l_worker;
typedef struct {
  struct l_worker* worker;
  l_squeue mast_rxmq;
  l_mutex mast_rxlk;
} l_worknode;

struct l_global;
struct l_message;
typedef struct {
  struct l_global* G;
  struct l_worker* cthr;
  struct l_service* csvc;
  struct l_message* cmsg
  void* svud;
} lnlylib_env;

typedef struct {
  l_thrhdl thrhdl;
  void (*start)(void);
  l_uint mast_flags;
  l_squeue mast_frsq; /* free service q */
  l_squeue mast_frmq; /* free message q */
  l_int num_workers; /* size of work node array */
  l_worknode* node_arr;
  l_srvctable stbl;
  lnlylib_env main_env;
} l_master;

typedef struct {
  l_int buff_max_len;
  l_int name_len;
  l_byte a[FILENAME_MAX+1];
} l_filename;

typedef struct {
  l_int num_workers;
  l_int init_stbl_size;
  l_filename log_file_name;
} l_config;

typedef struct {
} l_cmdline;

typedef struct l_global {
  l_config conf;
  l_cmdline cmds;
  l_master master;
  l_squeue Q;
  l_mutex QLOCK;
  l_condv QCNDV;
} l_global;

typedef struct l_service {
  l_smplnode node; /* chained in global q */
  l_uint svid;
  l_squeue srvc_msgq;
  l_uint srvc_flags;
  lua_State* L;
  l_squeue srvc_frco;
  l_uint turns;
  void (*proc)(lnlylib_env*);
  void* ud;
} l_service;

typedef struct {
  l_service* service;
  l_squeue txmq;
} l_worker_feedback_ind;

typedef struct {
  l_umedit ma, mb, mc, md;
  l_ulong la, lb, lc, ld;
  l_uint a, b, c, d;
  void* extra;
} l_msgdata;

typedef struct l_message {
  l_smplnode node;
  l_uint mgid;
  l_uint mssg_dest;
  l_uint mssg_from;
  l_umedit mssg_flags;
  l_umedit data_size;
  l_msgdata mssg_data;
} l_message;

typedef struct l_worker {
  l_thrhdl thrhdl;
  l_int weight;
  l_int index;
  l_squeue* work_txmq;
  l_squeue* work_txme;
  l_squeue* work_frmq;
  l_uint work_flags;
  l_mutex* mast_rxlk;
  l_squeue* mast_rxmq;
  l_squeue mq[3];
  lnlylib_env ENV;
} l_worker;

static void
l_srvctable_init(l_srvctable* stbl, l_int init_size)
{
  l_srvcslot* slot = 0;
  l_uint i = 0;

  stbl->capacity = init_size;
  stbl->num_services = 0;
  l_squeue_init(&stbl->free_slots);

  stbl->slot_arr = L_MALLOC_N(l_srvcslot, stbl->capacity);
  for (i = 0; i < stbl->capacity; ++i) {
    /* init the service slot */
    slot = stbl->slot_arr + i;
    slot->service = 0;
    slot->slot_index = i << 1; /* the lowerest bit for service alive or not */
    l_squeue_init(&slot->bkmq);
    /* insert the slot to free queue */
    if (i >= L_MIN_USER_SERVICE_ID) {
      l_squeue_push(&stbl->free_slots, &slot->node);
    }
  }
}

static l_srvcslot*
l_srvctable_alloc_slot(l_srvctable* stbl)
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
    l_uint new_size = 0;
    l_uint i = 0;

    new_size = stbl->capacity * 2;
    if (new_size <= stbl->capacity) {
      l_loge_2("current stbl is too large %d", stbl->capacity);
      return 0;
    }

    l_logw_1("stbl alloced to new size %d", ld(new_size));
    new_sarr = L_MALLOC_N(l_srvcslot, new_size);

    /* copy the old slots and free the old */

    for (i = 0; i < stbl->capacity; ++i) {
      new_sarr[i] = stbl->slot_arr[i];
    }

    l_rawapi_mfree(stbl->slot_arr);

    /* init the new slots */

    for (; i < new_size; ++i) {
      slot = new_sarr + i;
      slot->service = 0;
      slot->slot_index = i << 1; /* the lowerest bit is for service alive or not */
      l_squeue_init(&slot->bkmq);
      /* insert new slot to free queue */
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
  lua_State* L = luastate_new();

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
l_master_init(void (*start)(void), int argc, char** argv)
{
  l_global* G = L_MALLOC(l_global);
  l_master* M = &G->master;
  l_config* C = &G->conf;
  l_workernode* work_node = 0;
  l_worker* worker = 0;
  lnlylib_env* main_env = 0;
  l_int i = 0;

  l_parse_cmd_line(&G->cmds, argc, argv);

  l_config_load(C);

  l_squeue_init(&G->Q);
  l_mutex_init(&G->QLOCK);
  l_condv_init(&G->QCNDV);

  M->thrhdl = l_rawapi_thread_self();
  M->start = start;
  M->mast_flags = 0;
  l_squeue_init(&M->mast_frsq);
  l_squeue_init(&M->mast_frmq);

  M->num_workers = C->num_workers;
  M->node_arr = L_MALLOC_N(l_worknode, M->num_workers);

  for (i = 0; i < M->num_workers; ++i) {
    work_node = M->node_arr + i;
    l_squeue_init(&work_node->mast_rxmq);
    l_mutex_init(&work_node->mast_rxlk);

    work_node->worker= L_MALLOC(l_worker);
    worker = work_node->worker;
    worker->weight = 0; /* TODO: consider thread weight */
    worker->index = i;

    l_squeue_init(worker->mq + 0);
    l_squeue_init(worker->mq + 1);
    l_squeue_init(worker->mq + 2);
  }

  l_srvctable_init(&M->stbl, C->min_stbl_size);

  main_env = &M->main_env;
  main_env->G = G;
  main_env->cthr = 0;
  main_env->csvc = 0;
  main_env->cmsg = 0;
  main_env->svud = 0;

  return main_env;
}

static void
l_service_init(l_service* S, l_uint svid, void (*proc)(lnlylib_env*), void* ud, l_uint flags)
{
  S->svid = svid;
  l_squeue_init(&S->srvc_msgq);
  S->srvc_flags = flags;
  S->L = 0;
  l_squeue_init(&S->srvc_frco);
  S->turns = 0;
  S->proc = proc;
  S->ud = ud;
}

static l_service*
l_master_create_reserved_service(l_master* M, l_uint svid, void (*proc)(lnlylib_env*), void* ud, l_uint flags)
{
  l_service* S = 0;
  l_srvcslot* slot = 0;
  l_srvctable* stbl = &M->stbl;

  S = (l_service*)l_squeue_pop(&M->mast_frsq);
  if (S == 0) {
    S = L_MALLOC(l_service);
  }

  if (svid >= L_MIN_USER_SERVICE_ID || svid >= stbl->capacity) {
    l_loge_1("invalid reserved service id %d", ld(svid));
    return 0;
  }

  slot = stbl->slot_arr + svid;
  if (slot->service || (slot->slot_index & L_SERVICE_ALIVE)) {
    l_loge_1("reserved service %d already created", ld(svid));
    return 0;
  }

  l_service_init(S, svid, proc, ud, flags);
  slot->service = service; /* dock the service to the table */
  slot->slot_index |= L_SERVICE_ALIVE;
  return service;

}

static l_service*
l_master_create_service(l_master* M, void (*proc)(lnlylib_env*), void* ud, l_uint flags)
{
  l_service* S = 0;
  l_srvcslot* slot = 0;
  l_srvctable* stbl = &M->stbl;

  S = (l_service*)l_squeue_pop(&M->mast_frsq);
  if (S == 0) {
    S = L_MALLOC(l_service);
  }

  slot = l_srvctable_alloc_slot(stbl);
  if (slot == 0) {
    return 0;
  }

  l_service_init(S, slot->slot_index >> 1, proc, ud, flags);
  slot->service = service; /* dock the service to the table */
  slot->slot_index |= L_SERVICE_ALIVE;
  return service;
}

static void
l_master_destroy_service(l_master* M, l_service* S)
{
}

static void
l_launcher_service_proc(lnlylib_env* E)
{
  l_uint msgid = E->cmsg->mgid;
  switch (mgid) {
  case L_MSG_SERVICE_ON_CREATE:
    l_logm_s("enter launcher");
    E->G->master.start();
    l_stop_service(E);
    break;
  case L_MSG_SERVICE_ON_DESTROY:
    l_logm_s("launcher completed");
    break;
  default:
    l_loge_1("invalid launcher msg %d", ld(mgid));
    break;
  }
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
  l_service* S = 0;
  l_srvcslot* srvc_slot = 0;

  l_int num_workers = M->num_workers;
  l_worknode* work_node = 0;
  l_worker_feedback_ind* feedback_ind = 0;
  l_message* MSG = 0;
  l_message* tx_msg = 0;
  l_uint mssg_dest = 0;
  l_int i = 0;

  l_squeue rxmq;
  l_squeue svcq;
  l_squeue temp_svcq;

  l_squeue_init(&rxmq);
  l_squeue_init(&svcq);
  l_squeue_init(&temp_svcq);

  { /* launcher is the service to bang the whole new world */

    l_service* launcher = 0;
    l_message* on_create_msg = 0;

    launcher = l_master_create_reserved_service(M, L_SERVICE_LAUNCHER, l_launcher_service_proc, 0, 0);

    on_create_msg = L_MALLOC(l_message);
    on_create_msg->mssg_dest = launcher->svid;
    on_create_msg->mssg_from = 0;
    on_create_msg.mgid = L_MSG_SERVICE_ON_CREATE;
    on_create_msg.mssg_flags = 0;
    on_create_msg.data_size = 0;
    l_squeue_push(&launcher->srvc_msgq, &on_create_msg->node);

    stbl->slot_arr[launcher->svid].service = 0; /* need detach the service from the table first before insert into global q to handle */
    l_squeue_push(&svcq, &launcher->node);
  }

  for (; ;) {

    if (l_squeue_nt_empty(&svcq)) {
      l_int global_q_is_empty = 0;

      l_mutex_lock(QLOCK);
      global_q_is_empty = l_squeue_is_empty(Q);
      l_squeue_push_queue(Q, &svcq->node);
      l_mutex_unlock(QLOCK);

      if (global_q_is_empty) {
        l_condv_broadcast(QCNDV);
      }
    }

    if (M->mast_flags & L_MAST_FLAG_QUIT) {
      break;
    }

check_feeded_messages:

    for (i = 0; i < num_workers; ++i) {
      work_node = M->node_arr + i;
      l_mutex_lock(&work_node->mast_rxlk);
      l_squeue_push_queue(&rxmq, &work_node->mast_rxmq);
      l_mutex_unlock(&work_node->mast_rxlk);
    }

    if (l_squeue_is_empty(&rxmq)) {
      l_rawapi_sleep(30);
      goto check_feeded_messages;
    }

    while ((MSG = (l_message*)l_sqeueue_pop(&rxmq))) {
      switch (MSG->mgid) {
      case L_MSG_WORKER_FEEDBACK_IND:
        feedback_ind = (l_worker_feedback_ind)&MSG->mssg_data;
        S = feedback_ind->service;
        srvc_slot = stbl->slot + S->svid;
        srvc_slot->service = S; /* dock the service to the table first */
        l_squeue_push(&S->srvc_msgq, &(srvc_slot->bkmq.node));

        l_assert((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0);
        l_squeue_push(&temp_svcq, &S->node);
        S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;

        while ((tx_msg = (l_message*)l_squeue_pop(&feedback_ind->txmq))) {
          mssg_dest = tx_msg->mssg_dest;
          srvc_slot = stbl->slot + mssg_dest;
          if (mssg_dest < stbl->capacity && srvc_slot->slot_index & L_SERVICE_ALIVE) {
            S = srvc_slot->service;
            if (S) {
              /* service is docked in the service table, docked service is waiting to handle,
                 push the message to service's message q and insert the service to temp q if needed */
              l_squeue_push(&S->srvc_msgq, &tx_msg->node);
              if ((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0) {
                l_squeue_push(&temp_svcq, &S->node);
                S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
              }
            } else {
              /* service is already in global q or is handling in worker thread,
                 just push the message to its backup message queue */
              l_squeue_push(&srvc_slot->bkmq, &tx_msg->node);
            }
          } else {
            /* invalid message, just insert into free q. TODO: how to free message data */
            l_loge_3("invalid message %d from %d to %d", ld(tx_msg->mgid), ld(tx_msg->mssg_from), ld(mssg_dest));
            l_squeue_push(&M->mast_frmq, &tx_msg->node);
          }
        }
        break;
      default:
        /* unrecognized message, just insert into free q. TODO: how to free message data */
        l_loge_3("unrecognized message %d from %d to %d", ld(tx_msg->mgid), ld(tx_msg->mssg_from), ld(mssg_dest));
        l_squeue_push(&M->mast_frmq, &tx_msg->node);
        break;
      }
    }

    while ((S = (l_service*)l_squeue_pop(&temp_svcq))) {
      srvc_slot = stbl->slot + S->svid;
      if (l_squeue_is_empty(&S->srvc_msgq)) {
        l_assert(srvc_slot->service); /* keep the service docked */
        S->srvc_flags &= ~L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
      } else {
        S->srvc_flags &= ~L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;
        srvc_slot->service = 0; /* detach the service that wait to handle */
        l_squeue_push(&svcq, &S->node);
      }
    }

    if (l_squeue_is_empty(&svcq) && (M->mast_flags & L_MAST_FLAG_QUIT) == 0) {
      goto check_feeded_messages;
    }
  }

  return 0;
}

static l_message*
l_create_message(lnlylib_env* E, l_uint dest_svid, l_uint mgid, l_umedit flags, void* data, l_umedit size)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_message* msg = 0;

  msg = (l_message*)l_squeue_pop(W->frmq);
  if (msg == 0) {
    msg = L_MALLOC(l_message);
  }

  msg->mgid = mgid;
  msg->mssg_dest = dest_svid;
  msg->mssg_from = S->svid;
  msg->mssg_flags = flags;

  /* TODO: consider flags */

  if (data && size > 0) {
    msg->data_size = size;
    l_assert(size <= sizeof(l_msgdata);
    l_copy_n(&msg->mssg_data, data, size);
  } else {
    msg->data_size = 0;
  }

  return msg;
}

static void
l_send_message_impl(lnlylib_env* E, l_uint dest_svid, l_uint mgid, l_umedit flags, void* data, l_umedit size)
{
  l_message* msg = 0;

  msg = l_create_message(E, dest_svid, mgid, flags, data, size);
  if (msg == 0) {
    return;
  }

  if (dest_svid == msg->mssg_from) {
    l_squeue_push(W->txme, &msg->node);
  } else {
    l_squeue_push(W->txmq, &msg->node);
  }
}

L_EXTERN void
l_send_message(lnlylib_env* E, l_uint dest_svid, l_uint mgid, l_umedit flags, void* data, l_umedit size)
{
  if (mgid < L_MIN_USER_MSG_ID) {
    l_loge_1("invalid message id %d", ld(mgid));
  } else {
    l_send_message_impl(E, svid, mgid, flags, data, size);
  }
}

static void
l_worker_flush_messages(lnlylib_env* E)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_message* msg = 0;
  l_worker_feedback_ind feedback_ind;

  l_squeue_push_queue(&S->srvc_msgq, W->work_txme);
  feedback_ind.service = S;
  feedback_ind.txmq = l_squeue_move(W->work_txmq);

  msg = l_create_message(E, 0, L_MSG_WORKER_FEEDBACK_IND, 0, &feedback_ind, sizeof(l_worker_feedback_ind));
  if (msg == 0) {
    return;
  }

  /* deliver the msg to master */
  l_mutex_lock(W->mast_rxlk);
  l_squeue_push(W->mast_rxmq, &msg->node);
  l_mutex_unlock(W->mast_rxlk);
}

static int
l_worker_loop(l_svcenv* env)
{
  l_worker* W = env->cthr;
  l_global* G = env->G;
  l_squeue* Q = &G->Q;
  l_mutex* QLOCK = &G->QLOCK;
  l_condv* QCNDV = &Q->QCNDV;
  l_srvenv* ENV = &W->ENV;
  l_service* S = 0;
  l_message* msg = 0;
  l_int i = 0, n = 0;

  W->txmq = W->mq + 0;
  W->txme = W->mq + 1;
  W->frmq = W->mq + 2;

  W->mast_rxlk = &(G->master.node[W->index].mast_rxlk);
  W->mast_rxmq = &(G->master.node[W->index].mast_rxmq);

  ENV->G = env->G;
  ENV->cthr = W;
  ENV->csvc = 0;
  ENV->cmsg = 0;
  ENV->svud = 0;

  l_logm_1("worker %d started", ld(W->index));

  for (; ;) {
    l_mutex_lock(QLOCK);
    while (l_squeue_is_empty(Q)) {
      l_condv_wait(QCNDV, QLOCK);
    }
    S = (l_service*)l_squeue_pop(Q);
    l_mutex_unlock(QLOCK);

    ENV->csvc = S;
    ENV->svud = S->ud;
    W->work_flags = 0;

    n = l_messages_should_handle(W);
    for (i = 0; i < n; ++i) {
      if ((msg = (l_message*)l_squeue_pop(&S->srvc_msgq))) {
        ENV->cmsg = msg;
        S->proc(ENV);
        S->turns += 1;
      } else {
        break;
      }
    }

    l_worker_flush_messages(ENV);

    if (W->work_flags & L_WORK_FLAG_QUIT) {
      break;
    }
  }

  l_logm_1("worker %d exited", ld(W->index));
  return 0;
}

static void*
l_worker_thread_proc(void* para)
{
  lnlylib_env* main_env = (lnlylib_env*)para;
  return (void*)(l_int)l_worker_loop(main_env);
}

L_EXTERN int
lnlylib_main(void (*start)(void), int argc, char** argv)
{
  lnlylib_env* main_env = l_master_init(start, argc, argv);
  l_master* M = &main_env->G->master;
  l_worker* W = 0;
  l_int i = 0;
  int exit_code = 0;

  l_logm_s("master started");

  for (i = 0; i < M->num_workers; ++i) {
    W = M->node_arr[i].worker;
    main_env->cthr = W;
    l_rawapi_thread_create(&W->thrhdl, l_worker_thread_proc, main_env);
  }

  exit_code = l_master_loop(main_env);
  l_logm_s("master loop complete %d", ld(exit_code));

  for (i = 0; i < M->workers; ++i) {
    W = M->node_arr[i].worker;
    l_rawapi_thread_join(W);
    l_logm_s("worker %d joined", ld(W->index));
  }

  l_logm_s("master exited");
  l_master_exit(main_env);
  return 0;
}

