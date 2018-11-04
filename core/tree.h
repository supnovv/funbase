#ifndef LNLYLIB_CORE_TREE_H
#define LNLYLIB_CORE_TREE_H
#include "core/base.h"

typedef struct l_bintree {
  struct l_bintree* parent;
  struct l_bintree* left;
  struct l_bintree* right;
  l_uint ud;
} l_bintree;

typedef struct l_treenode {
  struct l_treenode* parent;
  struct l_treenode* next_sibling;
  struct l_treenode* prev_sibling;
  struct l_treenode* first_child;
  struct l_treenode* last_child;
  l_uint ud;
} l_treenode;

L_EXTERN void l_treenode_init(l_treenode* node, l_uint ud);
L_EXTERN void l_foreach_treenode(l_treenode* tree, void (*func)(void* parm, l_treenode* node, l_bool start), void* parm);

L_EXTERN l_bool l_treenode_append_child(l_treenode* node, l_treenode* child);
L_EXTERN l_bool l_treenode_insert_child(l_treenode* node, l_int i, l_treenode* child);
L_EXTERN l_bool l_treenode_insert_sibling_after(l_treenode* node, l_treenode* sibling);
L_EXTERN l_bool l_treenode_insert_sibling_before(l_treenode* node, l_treenode* sibling);

L_EXTERN l_treenode* l_treenode_remove_first_child(l_treenode* node);
L_EXTERN l_treenode* l_treenode_remove_last_child(l_treenode* node);
L_EXTERN l_treenode* l_treenode_remove_node(l_treenode* node);
L_EXTERN l_treenode* l_treenode_remove_child(l_treenode* node, l_int i);
L_EXTERN l_treenode* l_treenode_remove_sibling_after(l_treenode* node);
L_EXTERN l_treenode* l_treenode_remove_sibling_before(l_treenode* node);

#endif /* LNLYLIB_CORE_TREE_H */

