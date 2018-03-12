


typedef struct {
  l_mutex mtxa;
  l_mutex mtxb;
  l_condv cnda;
  l_squeue qa;
  l_squeue qb;
  l_squeue qc;
  l_freebq frbq;
} l_thrblock;

typedef struct {
  l_linknode node;
  l_umedit weight;
  l_ushort index;
  /* shared with master */
  l_mutex* svmtx;
  l_mutex* mutex;
  l_condv* condv;
  l_squeue* rxmq;
  int msgwait;
  /* thread own use */
  lua_State* L;
  l_squeue* txmq;
  l_squeue* txms;
  l_string log;
  l_file logfile;
  l_freebq* freebq;
  l_thrhdl thrhdl;
  int (*start)();
  l_thrblock* block;
} l_thread;

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
l_thread_init(l_thread* t, l_config* conf) {
  l_thrblock* b = 0;
  if (t->block) return; /* already initialized */

  t->weight = 0;
  t->msgwait = 0;

  t->block = l_raw_malloc(sizeof(l_thrblock));
  b = t->block;

  t->svmtx = &b->mtxa;
  t->mutex = &b->mtxb;
  t->condv = &b->cnda;
  l_mutex_init(t->svmtx);
  l_mutex_init(t->mutex);
  l_condv_init(t->condv);

  t->rxmq = &b->qa;
  t->txmq = &b->qb;
  t->txms = &b->qc;
  l_squeue_init(t->rxmq);
  l_squeue_init(t->txmq);
  l_squeue_init(t->txms);

  t->freebq = &b->frbq;
  l_zero_n(t->freebq, sizeof(l_freebq));
  l_squeue_init(&b->frbq.queue);
  b->frbq.limit = conf->thread_max_free_memory;

  l_thread_initLog(t, conf);
}

static void
l_thread_free(l_thread* t) {
  l_smplnode* node = 0;
  l_squeue* frbq = 0;
  l_squeue msgq;
  l_squeue_init(&msgq);

  if (!t->block)
    return; /* already freed */

  /* free all messages */

  l_thread_lock(t);
  l_squeue_pushQueue(&msgq, t->rxmq);
  l_thread_unlock(t);

  l_squeue_pushQueue(&msgq, t->txmq);
  l_squeue_pushQueue(&msgq, t->txms);

  while ((node = l_squeue_pop(&msgq))) {
    l_raw_mfree(node);
  }

  /* free all buffers */

  frbq = &t->freebq->queue;
  while ((node = l_squeue_pop(frbq))) {
    l_raw_mfree(node);
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
l_thread_start(l_thread* t, int (*start)()) {
  t->start = start;
  return l_thrhdl_create(&t->thrdhl, l_thread_proc, t);
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

