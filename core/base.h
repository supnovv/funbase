#ifndef LNLYLIB_CORE_BASE_H
#define LNLYLIB_CORE_BASE_H
#include "autoconf.h"
#include "core/prefix.h"

/** pre-defines in makefile, prefix.h, and autoconf.c **
L_PLAT_LINUX or L_PLAT_MACOSX or L_PLAT_WINDOWS
L_NEWLINE // "\n" or "\r\n"
L_NL_SIZE // 1 or 2
L_PATH_SEP // "/" or "\\"
L_INLINE
L_EXTERN
L_THREAD_LOCAL
L_BUILD_SHARED
LNLYLIB_AUTOCONF // only defined in autoconf.c
LNLYLIB_API_IMPL // shall be defined in lib src file
LNLYLIB_HOME_DIR // the lnlylib root folder when make
LNLYLIB_CLIB_DIR // c libraries folder
LNLYLIB_API_IMPL
LNLYLIB_LUALIB_DIR // lua libraries folder
L_MACH_32_BIT or L_MACH_64_BIT
L_LIT_ENDIAN or L_BIG_ENDIAN
l_bool false true // boolean
l_byte l_sbyte // 8-bit integer
l_short l_ushort // 16-bit integer
l_medit l_umedit // 32-bit integer
l_long l_ulong // 64-bit integer
l_int l_uint // pointer-size integer
**********************************************************************/

#undef L_MAX_INT_UB
#undef L_MAX_INT_SB
#undef L_MIN_INT_SB
#undef L_MAX_INT_US
#undef L_MAX_INT_SS
#undef L_MIN_INT_SS
#undef L_MAX_INT_UM
#undef L_MAX_INT_SM
#undef L_MIN_INT_SM
#undef L_MAX_INT_UL
#undef L_MAX_INT_SL
#undef L_MIN_INT_SL
#undef L_MAX_INT_IO

#define L_MAX_INT_UB ((l_byte)0xff) /* 255 */
#define L_MAX_INT_SB ((l_sbyte)0x7f) /* 127 */
#define L_MIN_INT_SB ((l_sbyte)-127-1) /* 128 0x80 */
#define L_MAX_INT_US ((l_ushort)0xffff) /* 65535 */
#define L_MAX_INT_SS ((l_short)0x7fff) /* 32767 */
#define L_MIN_INT_SS ((l_short)-32767-1) /* 32768 0x8000 */
#define L_MAX_INT_UM ((l_umedit)0xffffffff) /* 4294967295 */
#define L_MAX_INT_SM ((l_medit)0x7fffffff) /* 2147483647 */
#define L_MIN_INT_SM ((l_medit)-2147483647-1) /* 2147483648 0x80000000 */
#define L_MAX_INT_UL ((l_ulong)0xffffffffffffffff) /* 18446744073709551615 */
#define L_MAX_INT_SL ((l_long)0x7fffffffffffffff) /* 9223372036854775807 */
#define L_MIN_INT_SL ((l_long)-9223372036854775807-1) /* 9223372036854775808 0x8000000000000000 */
#define L_MAX_INT_IO (0x7fff0000) /* 2147418112 */
/* on linux, write() and similar system calls will transfer at most 0x7ffff000 (2,147,479,522) bytes,
returning the number of bytes actually transferred. this is true on both 32-bit and 64-bit systems. */

#undef LNUL
#define LNUL (0)

typedef union {
  double d;
  l_byte a[8];
} l_eightbyte;

typedef struct {
  const l_byte* p;
  l_int n;
} l_strn;

#undef l_strc
#undef l_empty_strn
#undef l_const_strn

#define l_strc(s) ((l_byte*)(s)) /* zero terminated c string */
#define l_empty_strn() l_const_strn("")
#define l_const_strn(s) ((l_strn){l_strc("" s), sizeof(s) - 1})

L_INLINE l_strn
l_strn_l(const void* s, l_int len) {
  return (l_strn){l_strc(s), len > 0 ? len : 0};
}

L_INLINE l_strn
l_strn_c(const void* s) { /* include <string.h> first before use it to make "strlen" visible */
  return (l_strn){l_strc(s), s ? strlen((char*)s) : 0};
}

L_INLINE l_strn
l_strn_p(const void* s, const void* e) {
  return l_strn_l(s, l_strc(e) - l_strc(s));
}

L_INLINE l_strn
l_strn_s(const void* s, l_int m, l_int n) {
  return l_strn_l(l_strc(s) + m, n - m);
}

/** simple link list */

typedef struct l_smplnode {
  struct l_smplnode* next;
} l_smplnode;

L_INLINE void
l_smplnode_init(l_smplnode* node) {
  node->next = node;
}

L_INLINE int
l_smplnode_is_empty(l_smplnode* node) {
  return node->next == node;
}

L_INLINE void
l_smplnode_insert_after(l_smplnode* node, l_smplnode* newnode) {
  newnode->next = node->next;
  node->next = newnode;
}

L_INLINE l_smplnode*
l_smplnode_remove_next(l_smplnode* node) {
  l_smplnode* p = node->next;
  node->next = p->next;
  return p;
}

/** bidirectional link list */

typedef struct l_linknode {
  struct l_linknode* next;
  struct l_linknode* prev;
} l_linknode;

L_INLINE void
l_linknode_init(l_linknode* node) {
  node->next = node->prev = node;
}

L_INLINE int
l_linknode_is_empty(l_linknode* node) {
  return node->next == node;
}

L_INLINE void
l_linknode_insert_after(l_linknode* node, l_linknode* newnode) {
  newnode->next = node->next;
  node->next = newnode;
  newnode->prev = node;
  newnode->next->prev = newnode;
}

L_INLINE l_linknode*
l_linknode_remove(l_linknode* node) {
  node->prev->next = node->next;
  node->next->prev = node->prev;
  return node;
}

/** simple linked queue */

typedef struct l_squeue {
  l_smplnode head;
  l_smplnode* tail;
} l_squeue;

L_INLINE void
l_squeue_init(l_squeue* self)
{
  l_smplnode_init(&self->head);
  self->tail = &self->head;
}

L_INLINE void
l_squeue_push(l_squeue* self, l_smplnode* newnode)
{
  l_smplnode_insert_after(self->tail, newnode);
  self->tail = newnode;
}

L_INLINE void
l_squeue_push_queue(l_squeue* self, l_squeue* q)
{
  if (l_squeue_is_empty(q)) {
    return;
  }
  self->tail->next = q->head.next;
  self->tail = q->tail;
  self->tail->next = &self->head;
  l_squeue_init(q);
}

L_INLINE l_squeue
l_squeue_move(l_squeue* q)
{
  l_squeue newq;
  l_squeue_init(&newq);
  l_squeue_push_queue(&newq, q);
  return newq;
}

L_INLINE l_bool
l_squeue_is_empty(l_squeue* self)
{
  return (self->head.next == &self->head);
}

L_INLINE l_smplnode*
l_squeue_top(l_squeue* self)
{
  if (l_squeue_is_empty(self)) {
    return 0;
  } else {
    return self->head.next;
  }
}

L_INLINE l_smplnode*
l_squeue_pop(l_squeue* self)
{
  l_smplnode* node = 0;
  if (l_squeue_is_empty(self)) {
    return 0;
  }
  node = l_smplnode_remove_next(&self->head);
  if (node == self->tail) {
    self->tail = &self->head;
  }
  return node;
}

/** bidirectional queue */

typedef struct l_dqueue {
  l_linknode head;
} l_dqueue;

L_INLINE void
l_dqueue_init(l_dqueue* self)
{
  l_linknode_init(&self->head);
}

L_INLINE void
l_dqueue_push(l_dqueue* self, l_linknode* newnode)
{
  l_linknode_insert_after(&self->head, newnode);
}

L_INLINE void
l_dqueue_push_queue(l_dqueue* self, l_dqueue* q)
{
  l_linknode* tail = 0;
  if (l_dqueue_is_empty(q)) return;
  /* chain self's tail with q's first element */
  tail = self->head.prev;
  tail->next = q->head.next;
  q->head.next->prev = tail;
  /* chain q's tail with self's head */
  tail = q->head.prev;
  tail->next = &self->head;
  self->head.prev = tail;
  /* init q to empty */
  l_dqueue_init(q);
}

L_INLINE l_dqueue
l_dqueue_move(l_dqueue* q)
{
  l_dqueue newq;
  l_dqueue_init(&newq);
  l_dqueue_push_queue(&newq, q);
  return newq;
}

L_INLINE l_bool
l_dqueue_is_empty(l_dqueue* self)
{
  return l_linknode_is_empty(&self->head);
}

L_INLINE l_linknode*
l_dqueue_pop(l_dqueue* self)
{
  if (l_dqueue_is_empty(self)) return 0;
  return l_linknode_remove(self->head.prev);
}

#endif /* LNLYLIB_CORE_BASE_H */

