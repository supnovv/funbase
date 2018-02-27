#ifndef L_CORE_LIST_H
#define L_CORE_LIST_H
#include "core/base.h"

/** simple link list */

typedef struct l_smplnode {
  struct l_smplnode* next;
} l_smplnode;

L_INLINE void
l_smplnode_init(l_smplnode* node) {
  node->next = node;
}

L_INLINE int
l_smplnode_isEmpty(l_smplnode* node) {
  return node->next == node;
}

L_INLINE void
l_smplnode_insertAfter(l_smplnode* node, l_smplnode* newnode) {
  newnode->next = node->next;
  node->next = newnode;
}

L_INLINE l_smplnode*
l_smplnode_removeNext(l_smplnode* node) {
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
l_linknode_isEmpty(l_linknode* node) {
  return node->next == node;
}

L_INLINE void
l_linknode_insertAfter(l_linknode* node, l_linknode* newnode) {
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

#endif /* L_CORE_LIST_H */

