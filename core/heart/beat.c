#define L_MALLOC(type) ((type)*)l_rawapi_malloc(sizeof(type))
#define L_CALLOC(type) ((type)*)l_rawapi_calloc(sizeof(type))
#define L_MALLOC_N(type, n) ((type)*)l_rawapi_malloc(sizeof(type) * (n))
#define L_CALLOC_N(type, n) ((type)*)l_rawapi_calloc(sizeof(type) * (n))

typedef union {
  l_byte start;
  l_eightbyte a[10];
} l_strint;

L_EXTERN l_strn
l_strint_strn(l_strint* s)
{
  return l_strn_p(((l_byte*)s) + s->start, s+1);
}

L_EXTERN l_strint
l_ulong_to_string(l_ulong n) {
  l_strint s;
  l_byte* p = (l_byte*)((&s) + 1);

  *(--p) = (n % 10) + '0';

  while ((n /= 10)) {
    *(--p) = (n % 10) + '0';
  }

  s->start = p - (l_byte*)&s;
  return s;
}

typedef struct {
  l_smplnode node;
  l_int bfsize;
} L_BUFHEAD;

typedef struct {
  L_BUFHEAD HEAD;
  l_int size;
  l_int fixed_max_size;
} l_strbuf;

typedef struct {
  l_int num_workers;
  l_int max_stbl_size;
  l_int max_lbuf_size;
  l_byte filename[FILENAME_MAX+1];
  l_byte* name_prefix_end;
} l_config;

static void
l_config_load(l_config* C)
{
  lua_State* L = luastate_new();
  C->num_workers = luastate_readint(c->L, "workers");
  if (C->num_workers < 0) {
    C->num_workers = 0;
  }
  // ...

  luastate_close(&L);
}

struct l_global;
typedef struct {
  struct l_global* G;
  lua_State* L;
  l_scheme* S;
  l_thread* curthr;
  l_thread* master;
  l_string logstr;
  l_stanfile logout;
} l_curenv;

typedef struct {
  l_squeue queue; /* free buffer q */
  l_int size;  /* size of the queue */
  l_int frmem; /* free memory size */
  l_int limit; /* free memory limit */
} l_freebq;

struct l_service;
typedef struct {
  l_linknode node;
  l_umedit weight;
  l_ushort index;
  /* shared with master */
  l_mutex* svmx; /* mutex to guard service */
  l_mutex* trmx; /* mutex to guard thread */
  l_condv* cndv; /* condition variable */
  l_squeue* rxmq;
  int msgwait;
  /* thread own use */
  l_squeue* txmq;
  l_squeue* txms;
  l_squeue* txme;
  l_freebq* frbq;
  struct l_service* cursvc;
  int (*start)(l_curenv*);
  l_thrhdl thrhdl;
  l_curenv E;
  l_mutex mx[2];
  l_condv cond;
  l_squeue q[4];
  l_freebq bufq;
} l_thread;

static void
l_thread_init(l_thread* t)
{
  if (t->svmx) {
    return;
  }

  t->svmx = &t->mx[0];
  t->trmx = &t->mx[1];
  t->cndv = &t->cond;

  t->rxmq = &t->q[0];
  t->txmq = &t->q[1];
  t->txms = &t->q[2];
  t->txme = &t->q[3];

  t->frbq = &t->bufq;

  l_mutex_init(t->svmx);
  l_mutex_init(t->trmx);
  l_condv_init(t->cndv);

  l_squeue_init(t->rxmq);
  l_squeue_init(t->txmq);
  l_squeue_init(t->txms);
  l_squeue_init(t->txme);

  l_squeue_init(&t->frbq.queue);
}

typedef struct l_global {
  l_config conf;
  /* thread global */
  l_thread tmas;
  l_thread* wrks;
  l_priorq thrq;
  /* io event global */
  l_ioevmgr emgr;
  /* message global */
  l_mutex mgmx;
  l_squeue rxmq;
  /* service global */
  l_mutex svmx;
  l_umedit seed;
  l_svctbl stbl;
} l_global;

static void
l_thread_setup(l_curenv* env)
{
  l_config* C = &env->G.conf;
  l_strn start_log = l_strn_literal("--------" L_NEWLINE);
  if (l_strn_eq(l_strn_literal("stdout"), l_strn_c(C->filename))) {
    env->logstr.impl = 1;
    env->logout.file = stdout;
    l_stanfile_write_strn(&env->logout, start_log);
  } else if (l_strn_eq(l_strn_literal("stderr"), l_strn_c(C->filename))) {
    env->logstr.impl = 2;
    env->logout.file = stderr;
    l_stanfile_write_strn(&env->logout, start_log);
  } else {
    l_string_create(&env->logstr, C->max_lbuf_size);
    { l_byte* name = C->name_prefix_end;
      l_strint index = l_ulong_to_string(C->curthr->index);
      l_strn idxstr = l_strint_strn(&index);
      *name++ = '_';
      name = l_copy_strn(idxstr, name);
      name = l_copy_strn(l_strn_literal(".lnlylib.log"), name);
      *name = 0;
      env->logout = l_stanfile_open_append_nobuf(C->filename);
      l_string_append(&env->logstr, start_log);
    }
  }
}

static l_global*
l_global_setup()
{
  l_global* G = L_CALLOC(l_global);
  l_config* C = &G->conf;
  l_config_load(&G->conf);

  /* thread */

  l_thread_init(&G->tmas, C);

  l_priorq_init(&G->thrq);

  if (C->num_workers) {
    G->wrks = L_CALLOC_N(l_thread, C->num_workers);
    { l_thread* t = 0;
      int i = 0;
      for (; i < C->num_workers; ++i) {
        t = G->wrks + i;
        t->index = i + 1; /* worker thread index start with 1 */
        l_thread_init(t, C);
        l_priorq_push(&G->thrq, &t->node);
      }
    }
  }

  /* io event */

  l_ioevmgr_init(&G->emgr);

  /* message */

  l_mutex_init(&G->mgmx);
  l_squeue_init(&G->rxmq);

  /* service */

  l_mutex_init(&G->svmx);
  G->seed = L_SERVICE_START_ID;
  l_svctbl_init(&G->stbl, C->max_stbl_size);

  /* others */

  { l_strn logfile = l_strn_p(C->filename, C->name_prefix_end);
    l_logm_5("workers %d max_lbuf_size %d max_stbl_size 2^%d logfile %strn",
      ld(C->num_workers), ld(C->max_lbuf_size), ld(C->max_stbl_size),
      lstrn(&logfile));
  }

  return G;
}

static void
l_global_clean(l_global* G)
{
}

static void*
l_thread_proc(void* para)
{
  l_thread* t = (l_thread*)para;
  return (void*)(l_int)t->start(t->env);
}

static void
l_thread_start(l_curenv* env, l_thread* t, int (*start)(l_curenv*))
{
  if (env->curthr != env->master) {
    l_loge_s("should start thread by master");
    return;
  }
  t->start = start;
  t->E.G = env->G;
  t->E.L = luastate_new();
  t->E.curthr = t;
  t->E.master = env->master;
  l_thread_setup(&t->E);
  l_rawapi_thread_create(&t->thrhdl, l_thread_proc, t);
}

static l_curenv* l_master_setup();
static void l_master_clean(l_curenv* env);

L_EXTERN int
l_start_main_thread(int (*start)(l_curenv*), int argc, char** argv)
{
  l_curenv* env = l_master_setup(start);
  l_master_parse_cmdline(env, argc, argv);
  l_logm_s("master startup");

  { l_int i = 0;
    l_int num_workers = env->G->conf.num_workers;
    l_thread* t = env->G->wrks;

    for (; i < num_workers; ++i) {
        l_thread_start(env, t + i, l_worker_start);
    }

    l_logm_s("worker %d start", ld(num_workers));

    i = l_master_loop(env);
    l_logm_s("master exit %d", ld(i));

    for (i = 0; i < num_workers; ++i) {
      l_thread_join(env, t + i);
      logm_s("worker %d exit", ld(i+1));
    }
  }

  l_logm_s("cleanup");
  l_master_clean(env);
  return 0;
}

static l_curenv*
l_master_setup(int (*start)(l_curenv*))
{
  l_global* G = l_global_setup();
  l_thread* master = &G->tmas;

  master->start = start;
  master->thdhdl = l_rawapi_thread_self();
  master->E.G = G;
  master->E.L = luastate_new();
  master->E.curthr = master;
  master->E.master = master;
  l_thread_setup(&master->E, master);
  return E;
}

static void
l_master_clean(l_curenv* env)
{
}

#define l_msg_ptr(b) ((l_message*)(b)->p)
#define L_MSG_MASTER_MIN_ID
#define L_MSG_MASTER_MAX_ID
#define L_MSG_START_SRVC_REQ
#define L_MSG_CLOSE_SRVC_REQ
#define L_MSG_ADD_SRVC_EVENT
#define L_MSG_DEL_SRVC_EVENT
#define L_MSG_START_LAUNCHER
#define L_MSG_CLOSE_MASTER_CMD
#define L_MSG_CLOSE_WORKER_REQ
#define L_MSG_CLOSE_WORKER_RSP
#define L_MSG_SOCKET_EVENT_IND
#define L_MSG_SOCK_CONNECT_RSP
#define L_MSG_SOCK_CONNECT_IND
#define L_MSG_USER_MIN_ID (0xffff+1)

#define L_SERVICE_MASTER   0x00
#define L_SERVICE_WORKER   0x01
#define L_SERVICE_LAUNCHER 0x02
#define L_SERVICE_USER_MIN_ID (0xffff+1)

static int
l_launcher_service_proc(l_curenv* env, l_message* msg)
{
  switch (msg->mgid) {
  case L_MSG_START_SRVC_REQ:
    l_logm_s(env, "launcher service started");
    break;
  case L_MSG_CLOSE_SRVC_REQ:
    l_logm_s(env, "launcher service closed");
    break;
  case L_MSG_START_LAUNCHER:
    l_logm_1("launcher service %d", ld(env->cursvc->svid));
    if (msg->ptra) {
      typedef int (*start_proc)(l_curenv*);
      start_proc start = (start_proc)(void*)msg->ptra;
      return start(env);
    }
    break;
  default:
    break;
  }
  return 0;
}

static int
l_master_loop(l_curenv* E)
{
  l_squeue rxmq;
  l_squeue frmq;
  l_squeue* mq = 0;
  l_service* srvc = 0;
  l_message* msg = 0;
  l_int i = 0;
  l_int n = 0;

  l_logm_s(E, "master run");

  if (E->G->conf.num_workers) {
    n = E->G->conf.num_workers;
    mq = L_CALLOC_N(l_squeue, n);
    for (i = 0; i < n; ++i) {
      l_squeue_init(mq + i);
    }
  }

  l_squeue_init(&rxmq);
  l_squeue_init(&frmq);

  
}

typedef struct {
  l_smplnode node;
  l_int bfsz;
} L_BUFHEAD;

typedef struct {
  L_BUFHEAD HEAD;
  l_ulong dest;
  l_umedit mgid;
  l_umedit data;
  l_ulong numa;
  l_ulong numb;
  l_uint ptra;
  l_uint ptrb;
} l_message;

typedef struct {
  void* impl;
} l_buffer;

typedef struct {
  L_BUFHEAD HEAD;
  l_filedesc evfd; /* guard by svmx */
  l_ushort evmk;   /* guard by svmx */
  l_ushort flag;   /* guard by svmx */
  l_thread* thrd;  /* only set once when init */
  int (*proc)(l_service*, l_message*);
  l_ulong svid;    /* only set once when init */
  l_umedit hint;   /* only access by a worker */
  l_luastate* co;  /* need save int coref */
  int (*func)(l_service*);
  int (*kfun)(l_service*);
} l_service;

static l_umedit
l_create_fresh_svid(l_global_env* env)
{
  l_umedit svid = 0;

  l_mutex_lock(env->svmx);
  env->seed += 1;
  if (env->seed < L_SERVICE_USER_MIN_ID) {
    env->seed = L_SERVICE_USER_MIN_ID;
  }
  svid = env->seed;
  l_mutex_unlock(env->svmx);

  return svid;
}

/** thread memory management
fixed size memory allocation: the max size of this kind of memory
can be known beforehead. or if the actual data size is larger than
the memory size, a new larger memory is allocated, the data is
copied to the new memory, the older one is discard and wasted.
i.e., the discarded memory cannot be used anymore, unless it is
anssigned to a new object to use explicity.
how to move fixed size memory between thread? the fixed size
memory is allocated once and used once. it is not reallocated
and reused. if the thread that own the memory doesn't use the
memory anymore, the fixed size memory can be dilivered to other
threads by the messages.
each service in a thread can manage a fixed size memory pool
for its own use. but it is configureable, a service also can
use thread's global fixed size memory pool. but if a service
need reuse the memory pool, and need move memory to other
threads via messages, it need use thread's global memory pool
to allocate this kind of data. because thread's global memory
is never be reallocated. or we already allocation messeage's
data in thread's global memory pool.
because the fixed size memory is allocated once and used once,
and don't care when it is freed. so if the memory pool is full,
a new memory need allocated and chained with the old memories.
it needs to be chained is that we must keep the allocated
memory address not be changed.
the fixed size memory allocation usually suitable for small
object allocation. if the discarded memory wasted, it will
not cost much.
---
reusable small objects allocation:
---
large object allocation: larger than 8K or like, allocate
directly from system.
---
string allocation:
*/

#define l_srvc_buf(b) ((l_service*)(b)->p)

L_EXTERN l_service*
l_create_service(l_curenv* env, l_int size, int (*proc)(l_curenv*, l_message*))
{
  if (size < (l_int)sizeof(l_service)) {
    l_loge_1("size %d", ld(size));
    return 0;
  }
}

L_EXTERN l_service*
l_create_service_from(l_service* from, l_int size, int (*proc)(l_service*, l_message*))
{

  if (size < (l_int)sizeof(l_service)) {
    l_loge_1("size %d", ld(size));
    return 0;
  }

  { l_thread* thrd = 0;

    if (from && from->thrd) {
      thrd = from->thrd;
    } else {
      thrd = l_thread_self();
    }

    { l_buffer* b = 0;

      if (!l_buffer_new(&b, size, thrd)) {
        return 0;
      }

      l_srvc_buf(b)->evfd = l_filedesc_();
      l_srvc_buf(b)->svid = l_create_fresh_svid();
      l_srvc_buf(b)->thrd = thrd;
      l_srvc_buf(b)->proc = proc;

      return l_srvc_buf(b);
    }
  }
}

static void
l_master_wakeup(l_global_env* env) {
  /* wakeup master to handle messages */
  l_ioevmgr_wakeup(&env->evmgr);
}

static void /* send message to dest service from current thread */
l_send_message(l_thread* cur, l_ulong destid, l_umedit msgid, l_umedit u32, l_umedit u64, l_message* msg) {
  msg->dest = destid;
  msg->msgid = msgid;
  msg->data = u32;
  msg->extra = u64;

  if (l_msg_dest_tidx(msg) == cur->index) {
    l_squeue_push(cur->txme, &msg->HEAD.node); /* send to self */
  }
  else if ((msgid >= L_MSG_MASTER_MIN_ID && msgid <= L_MSG_MASTER_MAX_ID) || l_msg_dest_svid(msg) == 0) {
    l_squeue_push(cur->txms, &msg->HEAD.node); /* master messages or send to master */
  }
  else {
    l_squeue_push(cur->txmq, &msg->HEAD.node); /* send to other threads */
  }
}

static void
l_flush_message(l_thread* cur) {
}



#if defined(L_THREAD_LOCAL)
L_THREAD_LOCAL l_thread* l_self_thread;
#else
static l_thrkey l_thrkey_g;
#endif

static int l_initialized = false;
static l_thread l_master_thread;
static int l_num_workers;
static l_thread* l_worker_thread;
static l_priorq l_worker_queue;

static l_thread*
l_thread_self() {
#if defined(L_THREAD_LOCAL)
  return l_self_thread;
#else
  return (l_thread*)l_thrkey_getData(&l_thrkey_g);
#endif
}

static void
l_thread_setSelf(l_thread* self) {
#if defined(L_THREAD_LOCAL)
  l_self_thread = self;
#else
  l_thrkey_setData(&l_thread_g, self);
#endif
}

static l_thread*
l_thread_master() {
  return &l_master_thread;
}

static void
l_thread_lock(l_thread* t) {
  l_mutex_lock(t->mutex);
}

static void
l_thread_unlock(l_thread* t) {
  l_mutex_unlock(t->mutex);
}

static void
l_thread_free(l_thread* t) {
  l_smplnode* node = 0;
  if (!t->block) {
    return; /* already freed */
  }

  { /* free all messages */
    l_squeue msgq;
    l_squeue_init(&msgq);

    l_thread_lock(t);
    l_squeue_pushQueue(&msgq, t->rxmq);
    l_thread_unlock(t);

    l_squeue_pushQueue(&msgq, t->txmq);
    l_squeue_pushQueue(&msgq, t->txms);
    l_squeue_pushQueue(&msgq, t->txme);

    while ((node = l_squeue_pop(&msgq))) {
      l_raw_mfree(node);
    }
  }

  { /* free all buffers */
    l_squeue* frbq = 0;
    frbq = &t->freebq->queue;
    while ((node = l_squeue_pop(frbq))) {
      l_raw_mfree(node);
    }
  }

  /* free locks */
  l_mutex_free(t->svmtx);
  l_mutex_free(t->mutex);
  l_condv_free(t->condv);

  /* others */
  l_thread_freeLog(t);
  if (t->L) {
    l_luastate_close(t->L);
    t->L = 0;
  }
  l_raw_mfree(t->block);
  t->block = 0;
}

static void*
l_thread_proc(void* para) {
  int n = 0;
  l_thread* self = (l_thread*)para;
  l_thread_setSelf(self);
  n = self->start();
  l_thread_setSelf(0);
  return (void*)(l_int)n;
}

static int
l_thread_join(l_thread* t) {
  return l_thrhdl_join(&t->thrhdl);
}

static int
l_thread_less(void* lhs, void* rhs) {
  return ((l_thread*)lhs)->weight < ((l_thread*)rhs)->weight;
}

static void
l_master_init() {
  int i = 0;
  l_strt prefix;
  l_config* conf = 0;
  l_thread* master = &l_master_thread;
  l_thread* thread = 0;

  if (l_initialized) return;

  l_thrkey_init(&l_thrkey_g);

  /* master thread */

#if defined(L_THREAD_LOCAL_SUPPORTED)
  l_self_thread = 0;
#endif
  l_thrkey_setData(&l_thrkey_g, 0);

  conf = l_config_create();
  prefix = l_strt_from(conf->logfile, conf->prefixend);

  l_thread_init(master, conf);
  master->id = l_raw_thread_self(); /* master thread created by os */
  master->L = conf->L;
  conf->L = 0;

#if defined(L_THREAD_LOCAL_SUPPORTED)
  l_self_thread = master;
#endif
  l_thrkey_setData(&l_thrkey_g, master);

  /* worker thread pool */

  l_priorq_init(&l_thread_pool, l_thread_less);

  if (conf->workers > 0) {
    l_num_workers = conf->workers;
    l_worker_thread = (l_thread*)l_raw_calloc(sizeof(l_thread) * conf->workers);

    for (i = 0; i < conf->workers; ++i) {
      thread = l_worker_thread + i;
      thread->index = i + 1; /* worker index should not 0 */
      l_thread_init(thread, conf);
      thread->L = l_luastate_new();
      l_priorq_push(&l_thread_pool, &thread->node);
    }

  } else {
    l_num_workers = 0;
    l_worker_thread = 0;
  }

  /* socket */

  l_socket_init();
  l_eventmgr_init(&l_eventmgr_g);

  /* message */

  l_squeue_init(&l_msg_rxq);
  l_mutex_init(&l_msg_mtx);

  /* service */

  l_mutex_init(&l_srvc_mtx);
  l_svid_seed = L_SERVICE_START_ID;
  l_srvctable_init(&l_srvc_table, conf->service_table_size);

  l_initialized = true;

  /* others */

  l_logm_5("workers %d log_buffer_size %d service_table_size 2^%d thread_max_free_memory %d logfile_prefix %strt",
      ld(conf->workers), ld(conf->log_buffer_size), ld(conf->service_table_size), ld(conf->thread_max_free_memory), lstrt(&prefix));

  l_config_free(conf);
}

static void
l_master_clean() {
  l_smplnode* node = 0;
  l_thread* thread = 0;
  l_thread* master = l_thread_master();

  if (!l_initialized) return;

  /* socket */

  l_eventmgr_free(&l_eventmgr_g);

  /* clean messages */

  l_mutex_lock(&l_msg_mtx);
  while ((node = l_squeue_pop(&l_msg_rxq))) {
    l_raw_mfree(node);
  }
  l_mutex_unlock(&l_msg_mtx);
  l_mutex_free(&l_msg_mtx);

  /* clean services */

  l_srvctable_free(&l_srvc_table, l_raw_alloc_func);
  l_mutex_free(&l_srvc_mtx);

  /* clean threads */

  l_thread_free(master);

  while ((thread = (l_thread*)l_priorq_pop(&l_thread_pool))) {
    l_thread_free(thread);
  }

  if (l_worker_thread) {
    l_raw_mfree(l_worker_thread);
    l_worker_thread = 0;
  }

  l_num_workers = 0;

  /* others */

  l_thrkey_free(&l_thrkey_g);

#if defined(L_THREAD_LOCAL_SUPPORTED)
  l_self_thread = 0;
#endif

  l_initialized = false;
}

/* -------- new version heart beat implementation -------- */

#define L_SERVICE_FLAG_ALIVE 0x0001

typedef struct {
  l_squeue node;
  l_service* service;
  l_umedit index;
  l_umedit srvc_alive;
  l_squeue bkmq;
} l_srvcslot;

typedef struct {
  l_umedit capacity; /* total services the slot array can contain */
  l_umedit size; /* number of current exist services */
  l_squeue free_slots;
  l_srvcslot* slot;
} l_srvctable;

static l_srvcslot*
l_srvctable_alloc_slot(l_srvctable* stbl)
{
  l_squeue* free_slots = &stbl->free_slots;
  l_srvcslot* slot = 0;
  l_srvcslot* new_slot = 0;
  l_umedit new_size = 0;
  l_umedit i = 0;

  if ((slot = (l_srvcslot*)l_squeue_pop(free_slots))) {
    stbl->size += 1;
    return slot;
  }

  l_logw_2("stbl is full: capacity %d size %d",
      ld(stbl->capacity), ld(stbl->size));

  new_size = stbl->capacity * 2;
  if (new_size <= stbl->capacity) {
    l_loge_2("current stbl is too large %d", stbl->capacity);
    return 0;
  }

  l_logw_1("stbl alloced to new size %d", ld(new_size));
  new_slot = L_MALLOC_N(l_srvcslot, new_size);

  /* copy the old slots and free the old */
  for (i = 0; i < stbl->capacity; ++i) {
    new_slot[i] = stbl->slot[i];
  }

  l_rawapi_mfree(stbl->slot);

  /* init the new slots */
  for (; i < new_size; ++i) {
    slot = new_slot + i;
    slot->service = 0;
    slot->index = i;
    slot->srvc_alive = 0;
    l_squeue_init(&slot->bkmq);
    /* insert new slot to free queue */
    l_squeue_push(free_slots, &slot->node);
  }

  stbl->capacity = new_size;
  stbl->slot = new_slot;

  if ((slot = (l_srvcslot*)l_squeue_pop(free_slots))) {
    stbl->size += 1;
    return slot;
  }

  return 0;
}

static void
l_srvctable_init(l_srvctable* stbl, l_int init_size)
{
  l_srvcslot* slot = 0;
  l_umedit i = 0;

  stbl->capacity = init_size;
  stbl->size = 0;
  l_squeue_init(&stbl->frslot);

  stbl->slot = L_MALLOC_N(l_srvcslot, stbl->capacity);
  for (i = 0; i < stbl->capacity; ++i) {
    /* init the service slot */
    slot = stbl->slot + i;
    slot->service = 0;
    slot->srvc_alive = 0;
    slot->index = i;
    l_squeue_init(&slot->bkmq);

    /* insert the slot to free queue */
    l_squeue_push(&stbl->frslot, &slot->node);
  }
}

typedef struct {
  l_worker* worker;
  l_squeue rxmq;
  l_mutex rxlk;
} l_worknode;

typedef struct {
  l_thrhdl thrhdl;
  void (*start)(lnlylib_env*);
  l_uint mast_flags;
  l_squeue mast_frsq; /* free service queue */
  l_squeue mast_frmq; /* free message queue in master */
  l_int workers; /* size of work node array */
  l_worknode* node;
  l_srvctable stbl;
  lnlylib_env main_env;
} l_master;

typedef struct {
  l_int capacity;
  l_int size;
  l_byte a[FILENAME_MAX+1];
} l_filename;

typedef struct {
  l_int num_workers;
  l_int init_stbl_size;
  l_filename log_file_name;
} l_config;

typedef struct {
} l_cmdline;

typedef struct {
  l_config conf;
  l_cmdline cmds;
  l_master master;
  l_squeue Q;
  l_mutex QLOCK;
  l_condv QCNDV;
} l_global;


static void
l_config_load(l_config* C)
{
  lua_State* L = luastate_new();
  C->num_workers = luastate_readint(c->L, "workers");
  if (C->num_workers < 0) {
    C->num_workers = 0;
  }
  // ...

  luastate_close(&L);
}

static lnlylib_env*
l_master_init(void (*start)(lnlylib_env*), int argc, char** argv)
{
  l_global* G = L_MALLOC(l_global);
  l_master* M = &G->master;
  l_config* C = &G->conf;
  l_workernode* node = 0;
  l_worker* worker = 0;
  lnlylib_env* man_env = 0;
  l_int i = 0;

  l_parse_cmd_line(&G->cmds, argc, argv);

  l_config_load(C);

  l_squeue_init(&G->Q);
  l_mutex_init(&G->QLOCK);
  l_condv_init(&G->QCNDV);

  M->thrhdl = l_rawapi_thrhdl_self();
  M->start = start;
  l_squeue_init(&M->frsq);

  M->workers = C->num_workers;
  M->node = L_MALLOC_N(l_worknode, M->workers);

  for (i = 0; i < M->workers; ++i) {
    node = M->node + i;
    l_squeue_init(&node->rxmq);
    l_mutex_init(&node->rxlk);

    worker = &node->worker;
    worker->index = i;
    worker->weight = 0; /* TODO: consider thread weight */

    l_condv_init(&worker->cond);
    l_squeue_init(worker->mq + 0);
    l_squeue_init(worker->mq + 1);
    l_squeue_init(worker->mq + 2);
  }

  l_srvctable_init(&M->stbl, C->min_stbl_size);

  main_env = &M->main_env;
  main_env->G = G;
  main_env->csvc = 0;
  main_env->cthr = 0;
  main_env->cmsg = 0;

  return main_env;
}

typedef struct {
  l_smplnode node; /* chained in global q */
  l_umedit svid;
  l_squeue srvc_msgq;
  l_uint srvc_flags;
  lua_State* L;
  l_squeue co;
  l_uint turns;
  void (*cb)(lnlylib_env*);
  void* ud;
} l_service;

static void
l_service_init(l_service* srvc, l_umedit svid, void (*cb)(lnlylib_env*), void* ud, l_umedit flags)
{
  srvc->svid = svid;
  l_squeue_init(&srvc->msgq);
  srvc->L = 0;
  l_squeue_init(&srvc->co);
  srvc->flags = flags;
  srvc->turns = 0;
  srvc->cb = cb;
  srvc->ud = ud;
}

static l_service*
l_master_create_service_impl(l_master* M, void (*cb)(lnlylib_env*), void* ud, l_umedit flags)
{
  l_service* service = 0;
  l_srvcslot* slot = 0;
  l_srvctable* stbl = &M->stbl;

  service = (l_service*)l_squeue_pop(&M->frsq);
  if (service == 0) {
    service = L_MALLOC(l_service);
  }

  slot = l_srvctable_alloc_slot(stbl);
  if (slot == 0) {
    return 0;
  }

  l_service_init(service, slot->index, cb, ud, flags);
  slot->service = 0;
  slot->srvc_alive = 1;
  return service;
}

static int
l_master_loop(lnlylib_env* main_env)
{
  l_global* G = main_env->G;
  l_squeue* Q = &G->Q;
  l_mutex* QLOCK = &G->QLOCK;
  l_condv* QCNDV = &G->QCNDV;

  l_master* M = &G->master;
  l_worknode* node = 0;
  l_int workers = M->workers;
  l_message* msg = 0;
  l_message* cur_msg = 0;
  l_service* service = 0;
  l_srvctable* stbl = &M->stbl;
  l_srvcslot* srvc_slot = 0;
  l_msg_current_work_done* done;
  l_int i = 0;
  l_int mssg_dest = 0;

  l_service* launcher = 0;
  l_message* oncreate = 0;
  l_int global_q_is_empty = 0;
  l_squeue rxmq;
  l_squeue svcq;
  l_squeue temp_svcq;

  l_squeue_init(&svcq);
  l_squeue_init(&rxmq);
  l_squeue_init(&temp_svcq);

  /* create 1st service to bang the new world */
  launcher = l_master_create_service_impl(M, M->start, 0, 0);
  oncreate = L_MALLOC(l_message);
  oncreate.dest = launcher->svid;
  oncreate.from = 0;
  oncreate.mgid = L_MSG_SERVICE_ONCREATE;
  oncreate.flags = 0;
  oncreate.size = 0;
  oncreate.data = 0;

  l_squeue_push(&launcher->msgq, &oncreate->node);
  l_squeue_push(&svcq, &launcher->node);

  for (; ;) {

    /* insert services in svcq to global q to handle */
    l_mutex_lock(QLOCK);
    global_q_is_empty = l_squeue_is_empty(Q);
    l_squeue_push_queue(Q, &svcq->node);
    l_mutex_unlock(QLOCK);

    if (global_q_is_empty) {
      l_condv_broadcast(QCNDV);
    }

    for (; ;) {
      for (i = 0; i < workers; ++i) {
        node = M->node + i;
        l_mutex_lock(&node->rxlk);
        l_squeue_push_queue(&rxmq, &node->rxmq);
        l_mutex_unlock(&node->rxlk);
      }

      if (l_squeue_is_empty(&rxmq)) {
        l_rawapi_sleep(30);
        continue;
      }

      while ((msg = (l_message*)l_sqeueue_pop(&rxmq))) {
        switch (msg->mgid) {
        case L_MSG_CURTASK_DONE:
          done = (l_msg_current_work_done)msg->mssg_data;
          service = done->service;
          srvc_slot = stbl->slot + service->svid;
          srvc_slot->service = service;
          l_squeue_push(&service->srvc_msgq, &(srvc_slot->bkmq.node));

          l_assert((service->srvc_flags & L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ) == 0);
          l_squeue_push(&temp_svcq, &service->node);
          service->srvc_flags |= L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ;

          while ((cur_msg = (l_message*)l_squeue_pop(&done->txmq))) {
            mssg_dest = cur_msg->mssg_dest;
            srvc_slot = stbl->slot + mssg_dest;
            if (mssg_dest < stbl->capacity && srvc_slot->srvc_alive) {
              service = srvc_slot->service;
              if (service) {
                /* service is in the service table and not handled yet,
                   push the message to service's message queue */
                l_squeue_push(&service->srvc_msgq, &cur_msg->node);
                if ((service->srvc_flags & L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ) == 0) {
                  l_squeue_push(&temp_svcq, &service->node);
                  service->srvc_flags |= L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ;
                }
              } else {
                /* service is in global queue or is handling in worker thread,
                   just push the message to its backup message queue */
                l_squeue_push(&srvc_slot->bkmq, &cur_msg->node);
              }
            } else {
              /* invalide message, just free it. TODO: free message data */
              l_squeue_push(&M->mast_frmq, &cur_msg->node);
            }
          }
          break;
        default:
          /* unrecognized message, just free it. TODO: free message data */
          l_squeue_push(&M->mast_frmq, &cur_msg->node);
          break;
        }
      }

      while ((service = (l_service*)l_squeue_pop(&temp_svcq))) {
        srvc_slot = stbl->slot + service->svid;
        if (l_squeue_is_empty(&service->srvc_msgq)) {
          service->srvc_flags &= ~L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ;
          l_assert(srvc_slot->service);
        } else {
          service->srvc_flags &= ~L_FLAG_NOT_HANDLED_SERVICE_INSERTED_INTO_TEMPQ;
          srvc_slot->service = 0;
          l_squeue_push(&svcq, &service->node);
        }
      }

      if (l_squeue_nt_empty(&svcq)) {
        break;
      }
    }
  }
}



typedef struct {
  l_service* srvc;
  l_squeue txmq;
} l_msg_current_work_done;


typedef struct {
  l_worker* worker;
  l_squeue rxmq;
  l_mutex rxlk;
} l_worknode;

typedef struct {
  l_thrhdl thrhdl;
  void (*start)(lnlylib_env*);
  l_uint mast_flags;
  l_squeue mast_frsq; /* free service queue */
  l_squeue mast_frmq; /* free message queue in master */
  l_int workers; /* size of work node array */
  l_worknode* node;
  l_srvctable stbl;
  lnlylib_env main_env;
} l_master;

static void*
l_worker_thread_proc(void* para)
{
  lnlylib_env* main_env = (lnlylib_env*)para;
  return (void*)(l_int)l_worker_loop(main_env);
}

L_EXTERN int
lnlylib_main(void (*start)(lnlylib_env*), int argc, char** argv)
{
  lnlylib_env* main_env = l_master_init(start, argc, argv);
  l_master* M = &main_env->G->master;
  l_worker* W = 0;
  l_int i = 0;
  int exit = 0;

  l_logm_s("master started");

  for (i = 0; i < M->workers; ++i) {
    W = &M->node[i].worker;
    main_env->cthr = W;
    l_rawapi_thread_create(&W->thrhdl, l_worker_thread_proc, main_env);
  }

  exit = l_master_loop(main_env);
  l_logm_s("master loop finished %d", ld(exit));

  for (i = 0; i < M->workers; ++i) {
    W = &M->node[i].worker;
    l_rawapi_thread_join(W);
    l_logm_s("worker %d joined", ld(W->index));
  }

  l_logm_s("master exited");
  l_master_exit(main_env);
  return 0;
}

typedef struct {
  l_smplnode node;
  l_umedit mgid;
  l_umedit mssg_dest;
  l_umedit mssg_from;
  l_umedit mssg_flags;
  l_int mssg_size;
  void* mssg_data;
} l_message;

struct l_global;
typedef struct {
  struct l_global* G;
  l_service* csvc;
  l_worker* cthr;
  l_message* cmsg;
  void* ud;
} lnlylib_env;

#define L_FLAG_WORKER_QUIT 0x01

typedef struct {
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

static l_message*
l_create_message(l_svcenv* E, l_umedit dest_srvc_id, l_umedit msg_id, l_umedit flags, void* data, l_int size)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_message* M = 0;

  M = (l_message*)l_squeue_pop(W->frmq);
  if (M == 0) {
    M = L_MALLOC(l_message);
  }

  msg->dest = dest_srvc_id;
  msg->mgid = msg_id;
  msg->from = S->svid;

  /* TODO: consider flags */

  if (data && size > 0) {
    msg->size = size;
    msg->data = l_rawapi_malloc(size);
    l_copy_n(msg->data, data, size);
  } else {
    msg->size = 0;
    msg->data = 0;
  }

  return msg;
}

L_EXTERN void
l_send_message(l_svcenv* E, l_umedit dest_srvc_id, l_umedit msg_id, l_umedit flags, void* data, l_int size)
{
  l_message* msg = 0;

  msg = l_create_message(E, dest_srvc_id, msg_id, flags, data, size);
  if (msg == 0) {
    return;
  }

  if (dest_srvc_id == msg->from) {
    l_squeue_push(W->txme, &msg->node);
  } else {
    l_squeue_push(W->txmq, &msg->node);
  }
}

#define L_MSG_CURRENT_WORK_DONE 0x0010

typedef struct {
  l_service* srvc;
  l_squeue txmq;
} l_msg_current_work_done;

static void
l_worker_flush_messages(l_srvenv* E)
{
  l_worker* W = E->cthr;
  l_service* S = E->csvc;
  l_msg_current_work_done data;
  l_message* msg = 0;

  l_squeue_push_queue(&S->msgq, W->txme);
  data.srvc = S;
  data.txmq = l_squeue_move(W->txmq);

  msg = l_create_new_message(E, 0, L_MSG_CURRENT_WORK_DONE, 0, &data, sizeof(l_msg_current_work_done));
  if (msg == 0) {
    return;
  }

  /* deliver the msg to master to dispatch */
  l_mutex_lock(W->rxlk);
  l_squeue_push(W->rxmq, msg);
  l_mutex_unlock(W->rxlk);
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
  l_message* M = 0;
  l_int i = 0, n = 0;

  W->txmq = W->mq + 0;
  W->txme = W->mq + 1;
  W->frmq = W->mq + 2;

  W->rxlk = &(G->master.node[W->index].rxlk);
  W->rxmq = &(G->master.node[W->index].rxmq);

  ENV->G = env->G;
  ENV->cthr = W;
  ENV->csvc = 0;
  ENV->cmsg = 0;

  l_logm_1("worker %d started", ld(W->index));

  for (; ;) {
    l_mutex_lock(QLOCK);
    while (l_squeue_is_empty(Q)) {
      l_condv_wait(QCNDV, QLOCK);
    }
    S = (l_service*)l_squeue_pop(Q);
    l_mutex_unlock(QLOCK);

    ENV->csvc = S;
    ENV->ud = S->ud;
    W->flags = 0;

    n = l_messages_should_handle(W);
    for (i = 0; i < n; ++i) {
      if ((M = (l_message*)l_squeue_pop(&S->msgq))) {
        ENV->cmsg = M;
        S->cb(ENV);
      } else {
        break;
      }
    }

    l_worker_flush_messages(ENV);

    if (W->flags & L_FLAG_WORKER_QUIT) {
      break;
    }
  }

  l_logm_1("worker %d exited", ld(W->index));
  return 0;
}

