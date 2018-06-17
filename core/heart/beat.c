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

