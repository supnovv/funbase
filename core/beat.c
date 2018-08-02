#define LNLYLIB_API_IMPL
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <float.h>
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
#define L_MAX_NUM_WORKERS 1000

#define L_WORK_FLAG_QUIT 0x01
#define L_MAST_FLAG_QUIT 0x01

#define L_MIN_USER_MSG_ID 0x0100
#define L_MSG_WORKER_FEEDBACK    0x01
#define L_MSG_SERVICE_CREATE_REQ 0x02
#define L_MSG_SUBSRVC_CREATE_RSP 0x03
#define L_MSG_SERVICE_STOP_REQ   0x04
#define L_MSG_SERVICE_ON_CREATE  0x05
#define L_MSG_SERVICE_ON_DESTROY 0x06

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
  l_filename logfilename;
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
  l_ulong srvc_id; /* the highest bit is for remote service or not, high 32-bit is the index, low 32-bit is the seed num */
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

L_EXTERN l_bool
l_zero_n(void* p, l_ulong size)
{
  if (p == 0 || size == 0) {
    return false;
  }
  if (p == memset(p, 0, size)) {
    return true;
  } else {
    l_loge_1(LNUL, "memset fail %d", ld(size));
    return false;
  }
}

L_EXTERN l_ulong
l_copy_n(void* dest, const void* from, l_ulong size)
{
  if (dest == 0 || from == 0 || size == 0) {
    return 0;
  }
  if (l_cstr(dest) + size <= l_cstr(from) || l_cstr(from) + size <= l_cstr(dest)) {
    if (dest == memcpy(dest, from, size)) {
      return size;
    } else {
      l_loge_3(LNUL, "memcpy fail %p <- %p %d", lp(dest), lp(from), ld(size));
      return 0;
    }
  } else {
    if (dest == memmove(dest, from, size)) {
      return size;
    } else {
      l_loge_3(LNUL, "memmove fail %p <- %p %d", lp(dest), lp(from), ld(size));
      return 0;
    }
  }
}

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

lnlylib_env* l_get_lnlylib_env()
{
  return l_threadlocal_get();
}

static void
l_config_load(l_config* conf)
{
  lua_State* L = ll_new_state();
  l_funcindex func;
  l_tableindex env;
  l_strn script, logfile;

  conf->num_workers = 1;
  conf->init_stbl_size = L_MIN_SRVC_TABLE_SIZE;
  conf->start_script = l_empty_string();
  l_filename_set(&conf->logfilename, L_STR("stdout"));

  func = ll_load_file(L, LNLYLIB_HOME_DIR "/conf/config.lua");
  if (func.index <= 0) {
    l_loge_s(LNUL, "load config file failed");
    return;
  }

  env = ll_new_table(L);
  if (env.index <= 0) {
    l_loge_s(LNUL, "create env table failed");
    return;
  }

  ll_set_funcenv(L, func, env);
  ll_push(L, func.index);
  ll_pcall_func(L, func, 0);

  conf->num_workers = ll_table_get_int(L, env, "workers");
  if (conf->num_workers < 1) {
    conf->num_workers = 1;
  } else if (conf->num_workers > L_MAX_NUM_WORKERS) {
    conf->num_workers = L_MAX_NUM_WORKERS;
  }

  conf->init_stbl_size = ll_table_get_int(L, env, "services");
  if (conf->init_stbl_size < L_MIN_SRVC_TABLE_SIZE) {
    conf->init_stbl_size = L_MIN_SRVC_TABLE_SIZE;
  }

  script = ll_table_get_str(L, env, "script");
  l_string_set(&conf->start_script, script);

  logfile = ll_table_get_str(L, env, "logfile");
  if (!l_filename_set(&conf->logfilename, logfile)) {
    l_filename_set(&conf->logfilename, L_STR("stdout"));
  }

  ll_close_state(L);
}

L_EXTERN l_int l_impl_file_write(void* out, const void* p, l_int len);

static l_ostream
l_ostream_from(void* out, l_int (*write)(void* out, const void* p, l_int n))
{
  return (l_ostream){out, 0, write};
}

typedef struct l_strbuf {
  l_int total;
  l_int n;
  l_byte s[1];
} l_strbuf;

static l_int
l_strbuf_write(void* out, const void* p, l_int len)
{
  l_strbuf* b = (l_strbuf*)out;
  if (p == 0 || n <= 0) {
    return 0;
  }
  if (b->n + len < b->total) {
    l_copy_n(b->s + b->n, p, len);
    b->n += len;
    b->s[b->n] = 0;
    return len;
  } else {
    return 0;
  }
}

static void
l_strbuf_init(struct l_strbuf* b, l_int total)
{
  b->total = total;
  b->n = 0;
  b->s[0] = 0;
}

L_EXTERN l_int
l_strbuf_clear(struct l_strbuf* b)
{
  l_int n = b->n;
  b->n = 0;
  b->s[0] = 0;
  return n;
}

L_EXTERN l_int
l_strbuf_reset(struct l_strbuf* b, l_strn s)
{
  l_strbuf_clear(b);
  return l_strbuf_write(b, s.p, s.n);
}

L_EXTERN const l_byte*
l_strbuf_cstr(struct l_strbuf* b)
{
  return b->s;
}

L_EXTERN l_strn
l_strbuf_strn(struct l_strbuf* b)
{
  return l_strn_l(b->s, b->n);
}

L_EXTERN l_int
l_strbuf_add_path(struct l_strbuf* b, l_strn path)
{
}

L_EXTERN l_int
l_strbuf_end_path(struct l_strbuf* b, l_strn fileanme)
{
}

L_EXTERN l_ostream
l_sbuf16_init(l_sbuf16* b)
{
  l_strbuf_init(l_sbuf16_p(b), 16);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf32_init(l_sbuf32* b)
{
  l_strbuf_init(l_sbuf32_p(b), 32);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf64_init(l_sbuf64* b)
{
  l_strbuf_init(l_sbuf64_p(b), 64);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf12_init(l_sbuf12* b)
{
  l_strbuf_init(l_sbuf12_p(b), 128);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf25_init(l_sbuf25* b)
{
  l_strbuf_init(l_sbuf25_p(b), 256);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf51_init(l_sbuf51* b)
{
  l_strbuf_init(l_sbuf51_p(b), 512);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf1k_init(l_sbuf1k* b)
{
  l_strbuf_init(l_sbuf1k_p(b), 1024);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf2k_init(l_sbuf2k* b)
{
  l_strbuf_init(l_sbuf2k_p(b), 1024*2);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf3k_init(l_sbuf3k* b)
{
  l_strbuf_init(l_sbuf3k_p(b), 1024*3);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf4k_init(l_sbuf4k* b)
{
  l_strbuf_init(l_sbuf4k_p(b), 1024*4);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf5k_init(l_sbuf5k* b)
{
  l_strbuf_init(l_sbuf5k_p(b), 1024*5);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf6k_init(l_sbuf6k* b)
{
  l_strbuf_init(l_sbuf6k_p(b), 1024*6);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf7k_init(l_sbuf7k* b)
{
  l_strbuf_init(l_sbuf7k_p(b), 1024*7);
  return l_ostream_from(b, l_impl_strbuf_write);
}

L_EXTERN l_ostream
l_sbuf8k_init(l_sbuf8k* b)
{
  l_strbuf_init(l_sbuf8k_p(b), 1024*8);
  return l_ostream_from(b, l_impl_strbuf_write);
}

/**
 * [non-zero] pritable charater
 * [00111010] G ~ Z   0x3a
 * [00111011] g ~ z   0x3b
 * [00111101] 0 ~ 9   0x3d
 * [00111110] A ~ F   0x3e
 * [00111111] a ~ f   0x3f
 * [00110000] _       0x30
 * [00100000] -       0x20
 * [0000XX1X] letter                 (ch & 0x02)
 * [0000XX10] upper letter           (ch & 0x03) == 2
 * [0000XX11] lower letter           (ch & 0x03) == 3
 * [0000X1XX] hex digit              (ch & 0x04)
 * [00001XXX] alphanum               (ch & 0x08)
 * [XXX1XXXX] alphanum and _         (ch & 0x10)
 * [XX1XXXXX] alphanum and _ and -   (ch & 0x20)
 */
static const l_byte l_char_table[] = {
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x80,0x80,0x80,0x80,0x80,0x80,0x80,0x80, 0x80,0x80,0x80,0x80,0x80,0x20,0x80,0x80, 0x3d,0x3d,0x3d,0x3d,0x3d,0x3d,0x3d,0x3d, 0x3d,0x3d,0x80,0x80,0x80,0x80,0x80,0x80, /* (20) - (3d) 0 ~ 9 */
  0x80,0x3e,0x3e,0x3e,0x3e,0x3e,0x3e,0x3a, 0x3a,0x3a,0x3a,0x3a,0x3a,0x3a,0x3a,0x3a, 0x3a,0x3a,0x3a,0x3a,0x3a,0x3a,0x3a,0x3a, 0x3a,0x3a,0x3a,0x80,0x80,0x80,0x80,0x30, /* (3e,3a) A ~ Z (30) _ */
  0x80,0x3f,0x3f,0x3f,0x3f,0x3f,0x3f,0x3b, 0x3b,0x3b,0x3b,0x3b,0x3b,0x3b,0x3b,0x3b, 0x3b,0x3b,0x3b,0x3b,0x3b,0x3b,0x3b,0x3b, 0x3b,0x3b,0x3b,0x80,0x80,0x80,0x80,0x00, /* (3f,3b) a ~ z */
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
};

static const l_strn l_hex_digit[] = {
  L_STR("0123456789abcdef"),
  L_STR("0123456789ABCDEF")
};

L_EXTERN int
l_is_printable(l_byte c)
{
  return l_char_table[c];
}

L_EXTERN int
l_is_dec_digit(l_byte c)
{
  return l_char_table[c] == 0x3d;
}

L_EXTERN int
l_is_letter(l_byte c)
{
  return l_char_table[c] & 0x02;
}

L_EXTERN int
l_is_upper_letter(l_byte c)
{
  return (l_char_table[c] & 0x03) == 2;
}

L_EXTERN int
l_is_lower_letter(l_byte c)
{
  return (l_char_table[c] & 0x03) == 3;
}

L_EXTERN int
l_is_hex_digit(l_byte c)
{
  return l_char_table[c] & 0x04;
}

L_EXTERN int
l_is_alphanum(l_byte c)
{
  return l_char_table[c] & 0x08;
}

L_EXTERN int
l_is_alphanum_underscore(l_byte c)
{
  return l_char_table[c] & 0x10;
}

L_EXTERN int
l_is_alphanum_underscore_hyphen(l_byte c)
{
  return l_char_table[c] & 0x20;
}

L_EXTERN l_int
l_dec_string_to_int(l_strn s)
{
  l_int times = 1;
  l_int value = 0;
  const l_byte* start = 0;
  const l_byte* end = 0;
  const l_byte* s_end = s.p + s.n;
  int negative = false;

  while (s.p < s_end) {
    if (*s.p >= '0' && *s.p <= '9') break;
    if (*s.p == '-') negative = true;
    ++s.p;
  }

  start = s.p;
  while (s.p < s_end) {
    if (*s.p < '0' || *s.p > '9') break;
    ++s.p;
  }

  end = s.p;
  while (start < end--) {
    value += *end * times;
    times *= 10;
  }

  return (negative ? -value : value);
}

L_EXTERN l_int
l_hex_string_to_int(l_strn s)
{
  l_int times = 1;
  l_int value = 0;
  const l_byte* start = 0;
  const l_byte* end = 0;
  const l_byte* s_end = s.p + s.n;
  int negative = false;

  while (s.p < s_end) {
    if (l_is_hex_digit(*s.p)) break;
    if (*s.p == '-') negative = true;
    ++s.p;
  }

  if (*s.p == '0' && s.p + 1 < s_end && (*(s.p + 1) == 'x' || *(s.p + 1) == 'X')) {
    s.p += 2;
    if (s.p >= s_end || !l_is_hex_digit(*s.p)) {
      return 0;
    }
  }

  start = s.p;
  while (s.p < s_end) {
    if (!l_is_hex_digit(*s.p)) break;
    ++s.p;
  }

  end = s.p;
  while (start < end--) {
    value += *end * times;
    times *= 16;
  }

  return (negative ? -value : value);
}

static l_int
l_ostream_format_fill(l_ostream* os, l_byte* a, l_byte* p, l_umedit flags)
{
  l_byte fill = L_GETF(flags);
  int width = L_GETW(flags);

  if (width > p - a) {
    if (fill == 0) {
      fill = ' ';
    }
    if (flags & L_LEFT) {
      while (width > p - a) {
        *p++ = fill;
      }
    } else {
      l_byte* e = a + width;
      while (p > a) {
        *(--e) = *(--p);
      }
      while (e > a) {
        *(--e) = fill;
      }
      p = a + width;
    }
  }

  return l_ostream_write(os, a, p - a);
}

L_EXTERN l_int
l_ostream_format_strn(l_ostream* os, l_strn s, l_umedit flags)
{
  int width = L_GETW(flags);
  if (l_strn_is_empty(&s)) {
    return 0;
  }
  if (s.n >= width) {
    return l_ostream_write(os, s.p, s.n);
  } else {
    l_byte a[160];
    memcpy(a, s.p, s.n);
    return l_ostream_format_fill(os, a, a + s.n, flags);
  }
}

L_EXTERN l_int
l_ostream_format_c(l_ostream* os, int c, l_umedit flags)
{
  l_byte ch = (l_byte)(c & 0xff);
  if (flags & L_UPPER) {
    if (ch >= 'a' && ch <= 'z') {
      ch -= 32;
    }
  } else if (flags & L_LOWER) {
    if (ch >= 'A' && ch <= 'Z') {
      ch += 32;
    }
  }
  return l_ostream_format_strn(os, l_strn_l(&ch, 1), flags);
}

#define L_HEX_FMT_BFSZ 159

static l_umedit
l_right_most_bit(l_umedit n)
{
  return n & (-n);
}

L_EXTERN l_int
l_ostream_format_u(l_ostream* os, l_ulong u, l_umedit flags)
{
  /* 64-bit unsigned int max value 18446744073709552046 (20 chars) */
  l_byte a[L_HEX_FMT_BFSZ];
  l_byte basechar = 0;
  l_byte* e = a + L_HEX_FMT_BFSZ - 1;
  l_byte* p = e;
  const l_byte* hex = 0;
  l_umedit base = 0;
  l_byte precise = L_GETP(flags);
  l_byte width = L_GETW(flags);
  l_byte fill = L_GETF(flags);

  flags &= L_FORMAT_MASK;
  base = (flags & L_BASE_MASK);

  switch (l_right_most_bit(base)) {
  case 0:
    *--p = (u % 10) + '0';
    while ((u /= 10)) {
      *--p = (u % 10) + '0';
    }
    break;
  case L_HEX:
    hex = l_hex_digit[(flags & L_UPPER) != 0].p;
    *--p = hex[u & 0x0f];
    while ((u >>= 4)) {
      *--p = hex[u & 0x0f];
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'X' : 'x';
    }
    flags &= (~L_BASE_MASK);
    break;
  case L_OCT:
    *--p = (u & 0x07) + '0';
    while ((u >>= 3)) {
      *--p = (u & 0x07) + '0';
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'O' : 'o';
    }
    flags &= (~L_BASE_MASK);
    break;
  case L_BIN:
    *--p = (u & 0x01) + '0';
    while ((u >>= 1)) {
      *--p = (u & 0x01) + '0';
    }
    if (!(flags & L_NOOX)) {
      basechar = (flags & L_UPPER) ? 'B' : 'b';
    }
    flags &= (~L_BASE_MASK);
    break;
  default:
    break;
  }

  while (precise > (p - a)) {
    *--p = '0';
  }

  if (basechar) {
    *--p = basechar;
    *--p = '0';
  }

  if (flags & L_MINUS) *--p = '-';
  else if (flags & L_PLUS) *--p = '+';
  else if (flags & L_BLANK) *--p = ' ';

  if (width > e - p) {
    if (fill == 0) {
      fill = ' ';
    }
    if (flags & L_LEFT) {
      l_byte* pa = a;
      l_byte* end = a + width;
      while (p < e) {
        *pa++ = *p++;
      }
      while (pa < end) {
        *pa++ = fill;
      }
    } else {
      while (width > e - p) {
        *--p = fill;
      }
    }
  }

  return l_ostream_write(os, p, e - p);
}

L_EXTERN l_int
l_ostream_format_d(l_ostream* os, l_long d, l_umedit flags)
{
  l_ulong n = 0;
  if (d < 0) {
    n = -d;
    flags |= L_MINUS;
  } else {
    n = d;
    flags &= (~L_MINUS);
  }
  return l_ostream_format_u(os, n, flags);
}

static l_byte*
l_format_ulong_dec(l_ulong n, l_byte* p)
{
  l_byte a[80];
  l_byte* s = a;

  *s++ = (n % 10) + '0';

  while ((n /= 10)) {
    *s++ = (n % 10) + '0';
  }

  while (s-- > a) {
    *p++ = *s;
  }

  return p;
}

static l_byte*
l_format_fraction_dec(double f, l_byte* p, int precise)
{
  l_ulong ipart = 0;

  if (f < DBL_EPSILON) {
    *p++ = '0';
    return p;
  }

  if (precise == 0) {
    precise = 80;
  }

  while (f >= DBL_EPSILON && precise-- > 0) {
    ipart = (l_ulong)(f * 10);
    *p++ = (l_byte)(ipart + '0');
    f = f * 10 - ipart;
  }

  return p;
}

L_EXTERN l_int
l_ostream_format_f(l_ostream* os, double f, l_umedit flags)
{
  l_value v = lf(f);
  l_byte a[159];
  l_byte sign = 0;
  l_byte* p = a;
  l_byte* dot = 0;
  l_ulong fraction = 0;
  l_ulong mantissa = 0;
  int exponent = 0;
  int negative = 0;
  l_umedit precise = L_GETP(flags);

  /**
   * Floating Point Components
   * |                  | Sign   | Exponent   | Fraction   | Bias
   * |----              |-----   | -----      |  ----      | ----
   * | Single Precision | 1 [31] |  8 [30-23] | 23 [22-00] | 127
   * | Double Precision | 1 [63] | 11 [62-52] | 52 [51-00] | 1023
   * ------------------------------------------------------------
   * Sign - 0 positive, 1 negative
   * Exponent - represent both positive and negative exponents
   *          - the ectual exponent = Exponent - (127 or 1023)
   *          - exponents of -127/-1023 (all 0s) and 128/1024 (255/2047, all 1s) are reserved for special numbers
   * Mantissa - stored in normalized form, this basically puts the radix point after the first non-zero digit
   *          - the mantissa has effectively 24/53 bits of resolution, by way of 23/52 fraction bits: 1.Fraction
   */

  negative = (v.u & 0x8000000000000000) != 0;
  exponent = (v.u & 0x7ff0000000000000) >> 52;
  fraction = (v.u & 0x000fffffffffffff);

  if (negative) sign = '-';
  else if (flags & L_PLUS) sign = '+';
  else if (flags & L_BLANK) sign = ' ';

  if (exponent == 0 && fraction == 0) {
    if (sign) *p++ = sign;
    *p++ = '0'; *p++ = '.'; dot = p; *p++ = '0';
  } else if (exponent == 0x00000000000007ff) {
    if (fraction == 0) {
      if (sign) *p++ = sign;
      *p++ = 'I'; *p++ = 'N'; *p++ = 'F'; *p++ = 'I'; *p++ = 'N'; *p++ = 'I'; *p++ = 'T'; *p++ = 'Y';
    } else {
      if (flags & L_BLANK) *p++ = ' ';
      *p++ = 'N'; *p++ = 'A'; *p++ = 'N';
    }
  } else {
    if (sign) *p++ = sign;
    if (negative) v.u &= 0x7fffffffffffffff;

    exponent = exponent - 1023;
    mantissa = 0x0010000000000000 | fraction;
    /* intmasks = 0xfff0000000000000; */
    /*                         1.fraction
        [ , , , | , , , | , , ,1|f,r,a,c,t,i,o,n,n,n,...]
        <----------- 12 --------|-------- 52 ----------->    */
    if (exponent < 0) {
      /* only have fraction part */
      #if 0
      if (exponent < -8) {
        intmasks = 0xf000000000000000;
        exponent += 8; /* 0000.00000001fraction * 2^exponent */
        mantissa >>= (-exponent); /* lose lower digits */
      } else {
        intmasks <<= (-exponent);
      }
      *p++ = '0'; dot = p; *p++ = '.';
      l_format_fraction_dec(mantissa, intmasks, p);
      #endif
      *p++ = '0'; *p++ = '.'; dot = p;
      p = l_format_fraction_dec(v.f, p, precise);
    } else {
      if (exponent >= 52) {
        /* only have integer part */
        if (exponent <= 63) { /* 52 + 11 */
          mantissa <<= (exponent - 52);
          p = l_format_ulong_dec(mantissa, p);
          *p++ = '.'; dot = p; *p++ = '0';
        } else {
          exponent -= 63;
          mantissa <<= 11;
          p = l_format_ulong_dec(mantissa, p);
          *p++ = '*'; *p++ = '2'; *p++ = '^';
          p = l_format_ulong_dec(exponent, p);
        }
      } else {
        /* have integer part and fraction part */
        #if 0
        intmasks >>= exponent;
        l_format_ulong_dec((mantissa & intmasks) >> (52 - exponent), p);
        *p++ = '.';
        l_format_fraction_dec(mantissa & (~intmasks), intmasks, p);
        #endif
        l_ulong ipart = (l_ulong)v.f;
        p = l_format_ulong_dec(ipart, p);
        *p++ = '.'; dot = p;
        p = l_format_fraction_dec(v.f - ipart, p, precise);
      }
    }
  }

  if (dot && precise) {
    while (p - dot < precise) {
      *p++ = '0';
    }
  }

  return l_ostream_format_fill(os, a, p, flags);
}

L_EXTERN l_int
l_ostream_format_s(l_ostream* os, const void* s, l_umedit flags)
{
  return l_ostream_format_strn(os, l_strn_c(s), flags);
}

L_EXTERN l_int
l_ostream_format_bool(l_ostream* os, int n, l_umedit flags)
{
  if (n) {
    if (flags & L_UPPER) {
      return l_ostream_format_strn(os, L_STR("TRUE"), flags);
    } else {
      return l_ostream_format_strn(os, L_STR("true"), flags);
    }
  } else {
    if (flags & L_UPPER) {
      return l_ostream_format_strn(os, L_STR("FALSE"), flags);
    } else {
      return l_ostream_format_strn(os, L_STR("false"), flags);
    }
  }
}

static const l_byte*
l_ostream_format_a_value(l_ostream* os, const l_byte* start, const l_byte* end, l_value a)
{
  /** format flags **
  s - const void*
  f - double
  d - l_long
  u - l_ulong
  strn - l_strn*
  bool - print true or false
  p - print as pointer value
  b - bin
  o - oct
  x - hex
  base - (b)in (o)ct (h)ex
  sign - ( )blank (+)plus (z)dont print 0b 0o 0x prefix
  justify - (l)eft, default is right
  width - 1 ~ 2 digit
  precision - (.) and 1 ~ 2 digit
  fill - fill to reach the width length
  ********************************************************************/
  l_umedit flags = 0; /* start point to '%' and next char is not '%' */
  const l_byte* cur = start;

  while (++cur < end) {
    switch (*cur) {
    case ' ':
      flags |= L_BLANK;
      continue;
    case '+':
      flags |= L_PLUS;
      continue;
    case '.':
      flags |= L_PRECISE;
      continue;
    case 'l': case 'L':
      flags |= L_LEFT;
      continue;
    case 'z': case 'Z':
      flags |= L_NOOX;
      continue;
    case '0': case '~': case '-': case '=': case '#':
      flags |= L_SETF(*cur); /* fill char */
      continue;
    case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': {
      l_umedit width = *cur - '0';
      l_byte ch = 0;
      if (cur + 1 < end && (ch = *(cur + 1)) >= '0' && ch <= '9') {
        /* cur + 1 is the 2nd digit */
        width = width * 10 + ch - '0';
        ++cur;
        /* skip next extra digits */
        while (cur + 1 < end && (ch = *(cur + 1)) >= '0' && ch <= '9') {
          ++cur;
        }
      }
      /* current char is digit, and next is not digit or end */
      if (flags & L_PRECISE) {
        flags |= L_SETP(width);
        flags &= (~L_PRECISE);
      } else {
        flags |= L_SETW(width);
      }
    }
    continue;
    case 'S':
      flags |= L_UPPER;
      /* fallthrough */
    case 's':
      if (end - cur >= 4 && *(cur+1) == 't' && *(cur+2) == 'r' && *(cur+3) == 'n') {
        l_ostream_format_s(os, a.p, flags);
        return cur + 4;
      } else {
        l_ostream_format_s(os, a.p, flags);
        return cur + 1;
      }
    case 'f': case 'F':
      l_ostream_format_f(os, a.f, flags);
      return cur + 1;
    case 'u': case 'U':
      l_ostream_format_u(os, a.u, flags);
      return cur + 1;
    case 'd': case 'D':
      l_ostream_format_d(os, a.d, flags);
      return cur + 1;
    case 'C':
      flags |= L_UPPER;
      /* fallthrough */
    case 'c':
      l_ostream_format_c(os, (int)a.u, flags);
      return cur + 1;
    case 'B':
      flags |= L_UPPER;
      /* fallthrough */
    case 'b':
      if (end - cur >= 4 && *(cur+1) == 'o' && *(cur+2) == 'o' && *(cur+3) == 'l') {
        l_ostream_format_bool(os, (int)a.u, flags);
        return cur + 4;
      } else {
        flags |= L_BIN;
        l_ostream_format_u(os, a.u, flags);
        return cur + 1;
      }
    case 'O':
      flags |= L_UPPER;
      /* fallthrough */
    case 'o':
      flags |= L_OCT;
      l_ostream_format_u(os, a.u, flags);
      return cur + 1;
    case 'X': case 'P':
      flags |= L_UPPER;
      /* fallthrough */
    case 'x': case 'p':
      flags |= L_HEX;
      l_ostream_format_u(os, a.u, flags);
      return cur + 1;
    default:
      break;
    }

    /* wrong format if goes here */
    break;
  }

  l_ostream_write(os, start, end - start);
  return 0;
}

static l_value
l_format_n_get_value(const void* p, l_int n)
{
  const l_value* a = (const l_value*)p;
  return a[n];
}

/** variable arguments **
The type va_list is an object type suitable for holding information
needed by the macros va_start, va_arg, va_end, and va_copy. If access
to the varying arguments is desired, the called function shall decalre
an object such as ap having type va_list. The object ap may be passed
as an argument to another function; if that function invokes the va_arg
macro with parameter ap, the value of ap in the calling function is
indeterminate and shall be passed to the va_end macro prior to any
further reference to ap. 215) It is permitted to create a pointer to a
va_list and pass that pointer to another function, in which case the
original function may make further use of the original list after
the other function returns.
Each invocation of the va_arg macro modifies ap so that the values of
successive arguments are returned in turn. **/

static l_value
l_format_v_get_value(const void* p, l_int n)
{
  va_list* vl = (va_list*)p;
  L_UNUSED(n);
  return va_arg(*vl, l_value);
}

static int
l_impl_ostream_format_n(l_ostream* os, const void* fmt, l_int n, const void* p, l_value (*getval)(const void*, l_int))
{
  int nfmts = 0;
  const l_byte* cur = 0;
  const l_byte* end = 0;
  const l_byte* beg = 0;
  l_strn fmt_str;

  if (fmt == 0) {
    return 0;
  }

  fmt_str = l_strn_c(fmt);

  if (n <= 0 || p == 0) {
    l_ostream_write(os, fmt_str.p, fmt_str.n);
    return 0;
  }

  cur = fmt_str.p;
  end = cur + fmt_str.n;
  beg = cur;

  while (cur < end) {
    if (*cur != '%') {
      ++cur;
      continue;
    }

    /* cur char is '%' */
    l_ostream_write(os, beg, cur - beg);
    beg = cur;

    /* cur is '%' and next char also is '%' */
    if (cur + 1 < end && *(cur + 1) == '%') {
      beg = cur + 1;
      cur = cur + 2;
      continue;
    }

    /* cur is '%' and next is not '%' or end */
    if (!(cur = l_ostream_format_a_value(os, cur, end, getval(p, nfmts)))) {
      break;
    }

    ++nfmts;

    if (nfmts >= n) {
      break;
    }
  }

  if (beg < cur) {
    l_ostream_write(os, beg, cur - beg);
  }

  return nfmts;
}

L_EXTERN int
l_ostream_format_n(l_ostream* os, const void* fmt, l_int n, const l_value* a)
{
  return l_impl_ostream_format_n(os, fmt, n, a, l_format_n_get_value);
}

L_EXTERN int
l_impl_ostream_format_v(l_ostream* os, const void* fmt, l_int n, va_list vl)
{
  return l_impl_ostream_format_n(os, fmt, n, &vl, l_format_v_get_value);
}

L_EXTERN int
l_impl_ostream_format(l_ostream* os, const void* fmt, l_int n, ...)
{
  int nfmts = 0;
  va_list vl;
  va_start(vl, n);
  nfmts = l_impl_ostream_format_v(os, fmt, n, vl);
  va_end(vl);
  return nfmts;
}

L_EXTERN void
l_filename_init(l_filename* nm)
{
  nm->buff_len = L_MAX_FILENAME - 8;
  nm->name_len = 0;
  nm->s[0] = 0;
}

L_EXTERN l_bool
l_filename_set(l_filename* nm, l_strn s)
{
  l_filename_init(nm);
  return l_filename_append(nm, s);
}

L_EXTERN l_bool
l_filename_append(l_filename* nm, l_strn s)
{
  if (s.n > 0 && nm->name_len + s.n < nm->buff_len) {
    l_copy_n(nm->s + nm->name_len, s.p, s.n);
    nm->name_len += s.n;
    nm->s[nm->name_len] = 0;
    return true;
  } else {
    return false;
  }
}

static l_int
l_filename_write(void* out, const void* p, l_int len)
{
  l_filename* self = (l_filename*)out;
  if (p == 0 || len <= 0) {
    return 0;
  }
  if (self->name_len + len < self->buff_len) {
    l_copy_n(self->s + self->name_len, p, len);
    self->name_len += len;
    self->s[self->name_len] = 0;
    return len;
  } else {
    return 0;
  }
}

L_EXTERN l_ostream
l_filename_ostream(l_filename* out)
{
  l_ostream os;
  os.out = out;
  os.size = 0;
  os.write = l_filename_write;
  return os;
}

L_EXTERN l_bool
l_filename_addname(l_filename* nm, l_strn name, l_strn suffix)
{
  return l_filename_append(nm, name) && l_filename_append(nm, suffix);
}

L_EXTERN l_bool
l_filename_addname_combine(l_filename* nm, l_strn part1, l_strn part2, l_strn sep)
{
  return l_filename_append(nm, part1) && l_filename_append(nm, sep) && l_filename_append(nm, part2);
}

L_EXTERN l_bool
l_filename_addpath(l_filename* nm, l_strn path)
{
  if (path.n > 0 && nm->name_len + path.n < nm->buff_len) {
    const l_byte* pend = path.p + path.n;
    if (nm->name_len > 0) {
      if (nm->s[nm->name_len - 1] == '/') {
        if (*path.p == '/') {
          path.p += 1;
        }
      } else {
        if (*path.p != '/') {
          nm->s[nm->name_len++] = '/';
        }
      }
    }
    while (path.p < pend) {
      nm->s[nm->name_len++] = *path.p++;
    }
    if (nm->s[nm->name_len - 1] != '/') {
      nm->s[nm->name_len++] = '/';
    }
    nm->s[nm->name_len] = 0;
    return true;
  } else {
    return false;
  }
}

static l_file
l_impl_file_open(const void* name, const char* mode)
{
  l_file s = {0};
  if (name && mode) {
    s.file = fopen((const char*)name, mode);
    if (s.file == 0) {
      l_loge_2(LNUL, "fopen %s %s", ls(name), lserror(errno));
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
  }
  return s;
}

static l_file
l_impl_file_open_nobuf(const void* name, const char* mode)
{
  l_file s = l_impl_file_open(name, mode);
  if (s.file) {
    setbuf((FILE*)s.file, 0);
  }
  return s;
}

static void
l_impl_file_reopen(FILE* file, const void* name, const char* mode)
{
  if (name && mode) {
    if (freopen((const char*)name, mode, file) == 0) {
      l_loge_2(LNUL, "freopen %s %s", ls(name), lserror(errno));
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
  }
}

L_EXTERN l_file
l_file_open_read(const void* name)
{
  return l_impl_file_open(name, "rb");
}

L_EXTERN l_file
l_file_open_read_nobuf(const void* name)
{
  return l_impl_file_open_nobuf(name, "rb");
}

L_EXTERN l_file
l_file_open_write(const void* name)
{
  return l_impl_file_open(name, "wb");
}

L_EXTERN l_file
l_file_open_write_nobuf(const void* name)
{
  return l_impl_file_open_nobuf(name, "wb");
}

L_EXTERN l_file
l_file_open_append(const void* name)
{
  return l_impl_file_open(name, "ab");
}

L_EXTERN l_file
l_file_open_append_nobuf(const void* name)
{
  return l_impl_file_open_nobuf(name, "ab");
}

L_EXTERN l_file
l_file_open_read_write(const void* name)
{
  return l_impl_file_open(name, "rb+");
}

L_EXTERN void
l_file_close(l_file* s)
{
  if (s->file == 0) {
    return;
  }
  if (fclose((FILE*)s->file) != 0) {
    l_loge_1(LNUL, "fclose %s", lserror(errno));
  }
  s->file = 0;
}

L_EXTERN void
l_file_clearerr(l_file* s)
{
  clearerr((FILE*)s->file);
}

L_EXTERN l_bool
l_file_flush(l_file* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fflush((FILE*)s->file) == 0) {
    return true;
  } else {
    l_loge_1(LNUL, "fflush %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_file_rewind(l_file* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fseek((FILE*)s->file, 0, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_1(LNUL, "fseek SET %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_file_seekto(l_file* s, l_int pos)
{
  if (s->file == 0 || pos < 0 || pos > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(pos));
    return false;
  }
  if (fseek((FILE*)s->file, pos, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek SET %d %s", ld(pos), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_file_forword(l_file* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_file_backward(l_file* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, -offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_2(LNUL, "fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_int
l_file_read(l_file* s, void* out, l_int size)
{
  if (s->file == 0 || out == 0 || size < 0 || size > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(size));
    return 0;
  }
  if (size == 0) {
    return 0;
  }
  { l_int n = 0;
    n = (l_int)fread(out, 1, (size_t)size, (FILE*)s->file);
    if (n == size) {
      return n;
    }
    if (!feof((FILE*)s->file)) {
      l_loge_1(LNUL, "fread %s", lserror(errno));
    }
    if (n < 0) {
      return 0;
    }
    return n;
  }
}

L_EXTERN l_int
l_file_write(l_file* s, const void* p, l_int len)
{
  l_int n = 0;

  if (s->file == 0 || p == 0 || len <= 0 || len > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %d", ld(len));
    return 0;
  }

  n = (l_int)fwrite(p, 1, (size_t)len, (FILE*)s->file);
  if (n == len) {
    return n;
  }

  l_loge_1(LNUL, "fwrite %s", lserror(errno));
  return (n < 0 ? 0 : n);
}

L_EXTERN l_int
l_impl_file_write(void* out, const void* p, l_int len)
{
  l_int n = 0;

  if (out == 0 || p == 0 || len <= 0 || len > L_MAX_INT_IO) {
    l_loge_1(LNUL, "EINVAL %s", ld(len));
    return 0;
  }

  n = (l_int)fwrite(p, 1, (size_t)len, (FILE*)out);
  if (n == len) {
    return n;
  }

  l_loge_1(LNUL, "fwrite %s", lserror(errno));
  return (n < 0 ? 0 : n);
}

L_EXTERN l_ostream
l_stdout_ostream()
{
  l_ostream os;
  os.out = stdout;
  os.size = 0;
  os.write = l_impl_file_write;
  return os;
}

L_EXTERN l_ostream
l_stderr_ostream()
{
  l_ostream os;
  os.out = stderr;
  os.size = 0;
  os.write = l_impl_file_write;
  return os;
}

L_EXTERN l_int
l_file_write_strn(l_file* out, l_strn s)
{
  return l_file_write(out, s.p, s.n);
}

L_EXTERN l_int
l_file_put(l_file* s, l_byte ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fwrite(&ch, 1, 1, (FILE*)s->file) != 1) {
    l_loge_1(LNUL, "fwrite %s", lserror(errno));
    return 0;
  }
  return 1;
}

L_EXTERN l_int
l_file_get(l_file* s, l_byte* ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fread(ch, 1, 1, (FILE*)s->file) != 1) {
    if (!feof((FILE*)s->file)) {
      l_loge_1(LNUL, "fread %s", lserror(errno));
    }
    return 0;
  }
  return 1;
}

L_EXTERN l_bool
l_file_remove(const void* name)
{
  if (name) {
    if (remove((const char*)name) == 0) {
      return true;
    } else {
      l_loge_1(LNUL, "remove %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
    return false;
  }
}

L_EXTERN l_bool
l_file_rename(const void* from, const void* to)
{
  /* int rename(const char* oldname, const char* newname);
   * Changes the name of the file or directory specified by oldname
   * to newname. This is an operation performed directly on a file;
   * No streams are involved in the operation. If oldname and newname
   * specify different paths and this is supported by the system, the
   * file is moved to the new location. If newname names an existing
   * file, the function may either fail or override the existing file,
   * depending on the specific system and library implementation.
   */
  if (from && to) {
    if (rename((const char*)from, (const char*)to) == 0) {
      return true;
    } else {
      l_loge_1(LNUL, "rename %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s(LNUL, "EINVAL");
    return false;
  }
}

L_EXTERN void
l_file_redirect_stdout(const void* name)
{
  l_impl_file_reopen(stdout, name, "wb");
}

L_EXTERN void
l_file_redirect_stderr(const void* name)
{
  l_impl_file_reopen(stderr, name, "wb");
}

L_EXTERN void
l_file_redirect_stdin(const void* name)
{
  l_impl_file_reopen(stdin, name, "rb");
}

static l_ostream
l_config_logout(l_config* conf, l_umedit thridx)
{
  l_strn logfile_prefix;

  logfile_prefix = l_filename_strn(&conf->logfilename);

  if (l_strn_equal(&logfile_prefix, L_STR("stdout"))) {
    return l_stdout_ostream();

  } else if (l_strn_equal(&logfile_prefix, L_STR("stderr"))) {
    return l_stderr_ostream();

  } else {
    l_filename result_name;
    l_ostream result_out;

    l_filename_init(&result_name);
    result_out = l_filename_ostream(&result_name);

    if (l_ostream_format_2(&result_out, "%strn.%d.log", lstrn(&logfile_prefix), ld(thridx)) != 2) {
      l_loge_2(LNUL, "create log ostream fail %strn %d", lstrn(&logfile_prefix), ld(thridx));
      return l_stdout_ostream();

    } else {
      l_ostream logout;

      logout.out = fopen(result_name.p, "wb");

      if (logout.out == 0) {
        l_loge_1(LNUL, "open log file fail %s", ls(result_name.s));
        return l_stdout_ostream();

      } else {
        logout.size = 0;
        logout.write = l_impl_file_write;
        return logout;
      }
    }
  }
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

static void
l_thread_free(l_thread* T)
{
  if (T->logout.out && T->logout.out != stdout && T->logout.out != stderr) {
    fclose((FILE*)T->logout.out);
    T->logout.out = 0;
  }

  if (T->thrd_alloc) {
    l_thrdalloc_destroy(T->thrd_alloc);
    T->thrd_alloc = 0;
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

  l_logw_2(E, "stbl is full: capacity %d num_services %d",
      ld(stbl->capacity), ld(stbl->num_services));

  { l_srvcslot* new_sarr = 0;
    l_umedit new_size = 0;
    l_umedit i = 0;

    new_size = stbl->capacity * 2;
    if (new_size <= stbl->capacity) {
      l_loge_2(E, "current stbl is too large %d", stbl->capacity);
      return 0;
    }

    l_logw_1(E, "stbl alloced to new size %d", ld(new_size));
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
  l_master* M = main_env->M;
  l_squeue* Q = M->globalq;
  l_mutex* QLOCK = M->qlock;
  l_condv* QCNDV = M->qcndv;

  l_srvctable* stbl = M->stbl;
  l_squeue* temp_svcq = M->temp_svcq;
  l_service* S = 0;
  l_srvcslot* srvc_slot = 0;

  l_umedit num_workers = M->num_workers;
  l_squeue* mast_frmq = M->mast_frmq;
  l_squeue* mast_frsq = M->mast_frsq;
  l_worknode* work_node = 0;
  l_message* MSG = 0;
  l_umedit i = 0;
  int global_q_is_empty = 0;
  int num_events = 0;
  int master_exit = 0;

  l_service* launcher = 0;
  l_message* on_create_msg = 0;

  l_squeue rxmq;
  l_squeue rxms;
  l_squeue svcq;

  l_squeue_init(&rxmq);
  l_squeue_init(&rxms);
  l_squeue_init(&svcq);

  /* launcher is the 1st service to bang the whole new world */

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

    if (master_exit) {
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
        l_worker_feedback* feedback = 0;
        feedback = (l_worker_feedback)&MSG->mssg_data;
        S = feedback->service;
        srvc_slot = stbl->slot + (S->srvc_id >> 32);
        srvc_slot->service = S; /* dock the service to the table first */

        /* insert backup q's meesage to service, and add service to tempq to handle */
        l_squeue_push_queue(&S->srvc_msgq, &srvc_slot->bkmq);
        l_squeue_push(temp_svcq, &S->node);
        l_assert((S->srvc_flags & L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ) == 0);
        S->srvc_flags |= L_SRVC_FLAG_DOCKED_SERVICE_INSERTED_TO_TEMPQ;

        l_master_deliver_messages(M, &feedback->txmq);

        master_exit = l_master_handle_self_messages(M, &feedback->txms);

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
        l_loge_3(E, "unrecognized master message %d", ld(MSG->mgid));
        break;
      }

      l_squeue_push(mast_frmq, &MSG->node); /* TODO: how to free msgdata */
    }

    num_events = l_ioevmgr_try_wait(M->evmgr, l_master_dispatch_io_event);
    if (num_events > 0) {
      num_events += l_ioevmgr_try_wait(M->evmgr, l_master_dispatch_io_event);
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

  l_logm_1(ENV, "worker %d started", ld(W->T.thridx));

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
            l_loge_1(ENV, "the srvc %d user data already assigned", ld(S->srvc_id));
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

  l_logm_1(ENV, "worker %d exited", ld(W->T.thridx));
  return 0;
}

L_EXTERN int
lnlylib_main(int (*start)(lnlylib_env*), int argc, char** argv)
{
  lnlylib_env* main_env = l_master_init(start, argc, argv);
  l_master* M = main_env->M;
  l_worker* W = 0;
  l_umedit num_workers = 0;
  l_umedit i = 0;
  int exit_code = 0;

  l_logm_s(main_env, "master started");

  num_workers = M->num_workers;

  for (i = 0; i < num_workers; ++i) {
    W = M->node_arr[i].worker;
    l_thread_start(&W->T, l_worker_loop);
  }

  exit_code = l_master_loop(main_env);
  l_logm_s(main_env, "master loop complete %d", ld(exit_code));

  for (i = 0; i < num_workers; ++i) {
    W = M->node_arr[i].worker;
    l_thread_join(&W->T);
    l_logm_s(main_env, "worker %d joined", ld(W->T.thridx));
  }

  l_logm_s(main_env, "master exited");
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
    l_loge_1(LNUL, "open library %strn failed", lstrn(&module_name));
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
  lnlylib_env* E = M->T.E;
  l_fildhdl ioev_hdl = L_EMPTY_HDL;
  l_ushort flags = 0;
  l_ushort events = 0;

  if (req->module) {
    cb = l_master_load_service_module(req->module);
  } else {
    cb = req->cb;
  }

  if (cb == 0) {
    l_loge_s(E, "service create fail due to null cb");
    return 0;
  }

  if (req->ioev.enable) {
    if (l_fildhdl_is_empty(req->ioev.hdl)) {
      l_sockaddr sa;
      if (!req->ioev.ip || !req->ioev.poart) {
        l_loge_s(E, "service create fail due to empty ip or port");
        return 0;
      }
      if (!l_sockaddr_init(&sa, l_strn_c(req->ioev.ip), req->ioev.port)) {
        l_loge_s(E, "service create fail due to invalid address");
        return 0;
      }
      if (req->ioev.listen) {
        l_socket sock;
        sock = l_socket_tcp_listen(&sa, 0);
        if (l_socket_is_empty(&sock)) {
          l_loge_s(E, "service create fail due to listen fail");
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
          l_loge_s(E, "service create fail due to connect fail");
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
      l_loge_1(E, "invalid reserved service id %d", ld(svid));
      return 0;
    }

    slot = stbl->slot_arr + svid;
    if (slot->service || (slot->flags & L_SRVC_FLAG_ALIVE)) {
      l_loge_1(E, "reserved service %d already created", ld(svid));
      return 0;
    }
  } else {
    slot = l_srvctable_alloc_slot(M, stbl);
    if (slot == 0) {
      l_loge_s(E, "service create fail due to slot alloc");
      return 0;
    }
  }

  S = (l_service*)l_squeue_pop(&M->mast_frsq);
  if (S == 0) {
    S = L_MALLOC_TYPE(M->E, l_service);
  }

  if (S == 0) {
    l_loge_s(E, "service create fail due to malloc");
    return 0;
  }

  slot->seed_num = l_get_srvc_seed(M);
  l_service_init(S, slot - stbl->slot_arr, slot->seed_num, cb, 0);
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

  l_logm_s(E, "launcher on create");

  if (l_string_nt_empty(&C->start_script)) {
    ll_pcall_func(C->L, l_string_strn(&C->start_script));
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
  l_logm_1(E, "launcher on destroy (ud %d)", ld(E->svud));
}

static void
l_launcher_service_proc(lnlylib_env* E)
{
  l_logm_1(E, "launcher handle msg %d", ld(E->cmsg->mgid));
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
    l_loge_3(E, "invalid message %d from %d to %d", ld(msg->mgid), ld(msg->mssg_from), ld(mssg_dest));
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
    l_loge_1(E, "try to stop invalid service %d", ld(svid));
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
    l_loge_1(E, "try to destroy invalid service %d", ld(svid));
  } else {
    l_assert(srvc_slot->service); /* the service must docked when destroy */
    S = srvc_slot->service;
    srvc_slot->service = 0;
    srvc_slot->flags &= (~L_SRVC_FLAG_ALIVE);
    if (l_squeue_nt_empty(&S->srvc_msgq)) {
      l_loge_1(E, "service %d destroyed with unhandled messages", ld(svid));
      while ((srvc_msg = (l_message*)l_squeue_pop(&S->srvc_msgq))) {
        l_squeue_push(&M->mast_frmq, &srvc_msg->node); /* TODO: how to free msgdata */
      }
    }
    l_service_free_co(S);
    l_squeue_push(&M->mast_frsq, &S->node);
  }
}

static int
l_master_handle_self_messages(l_master* M, l_squeue* txms)
{
  l_srvctable* stbl = &M->stbl;
  l_message* msg = 0;
  int master_exit = 0;

  while ((msg = (l_message*)l_squeue_pop(txms))) {
    switch (msg->mssg_id) {
    case L_MSG_MASTER_EXIT_REQ:
      /* send L_MSG_WORKER_EXIT_REQ to each workers */
      break;
    case L_MSG_WORKER_EXIT_RSP:
      /* worker will send L_MSG_WORKER_EXIT_RSP to master before it is exit */
      l_assert(M->num_workers != 0);
      M->num_workers -= 1;
      if (M->num_workers == 0) {
        master_exit = 1;
      }
      break;
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
      l_loge_3(E, "invalid master message %d", ld(msg->mgid));
      break;
    }
    l_squeue_push(mast_frmq, &msg->node); /* no need to free msgdata */
  }

  return master_exit;
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
l_send_lua_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong mssg_dest, l_ulong dest_coro, void* data, l_umedit size)
{
  if (mgid < L_MIN_USER_MSG_ID) {
    l_loge_1(E, "invalid message id %d", ld(mgid));
  } else {
    l_message* msg = l_create_message(E, mgid, mgid_cust, mssg_dest, flags, data, size);
    if (msg) {
      msg->dest_coro = dest_coro;
      l_send_message_impl(E, msg);
    }
  }
}

L_EXTERN void
l_send_message(lnlylib_env* E, l_umedit mgid, l_umedit mgid_cust, l_ulong dest_svid, void* data, l_umedit size)
{
  if (mgid < L_MIN_USER_MSG_ID) {
    l_loge_1(E, "invalid message id %d", ld(mgid));
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
  l_worker_feedback feedback;

  l_squeue_push_queue(&S->srvc_msgq, W->work_txme);
  feedback.service = S;
  feedback.txmq = l_squeue_move(W->work_txmq);
  feedback.txms = l_squeue_move(W->work_txms);

  msg = l_create_message(E, L_MSG_WORKER_FEEDBACK, 0, 0, 0, &feedback, sizeof(l_worker_feedback));
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
  l_loge_1(E, "lua_yield never returns to here %d", ld(status));
}

L_EXTERN int
ll_yield(lua_State* co)
{
  return ll_yield_impl(co, lua_gettop(L));
}

