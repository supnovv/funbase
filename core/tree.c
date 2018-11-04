#include <string.h>
#include "core/tree.h"

L_EXTERN void
l_treenode_init(l_treenode* node, l_uint ud)
{
  l_zero_n(node, sizeof(l_treenode));
  node->ud = ud;
}

L_EXTERN l_bool
l_treenode_append_child(l_treenode* node, l_treenode* child)
{
  l_treenode* first = 0;
  l_treenode* last = 0;

  first = child;
  while (first->prev_sibling) {
    first = first->prev_sibling;
    first->parent = node;
  }

  last = child;
  while (last->next_sibling) {
    last = last->next_sibling;
    last->parent = node;
  }

  if (node->last_child) {
    node->last_child->next_sibling = first;
    first->prev_sibling = node->last_child;
    node->last_child = last;
  } else {
    node->first_child = first;
    node->last_child = last;
  }

  child->parent = node;
  return true;
}

L_EXTERN l_bool
l_treenode_insert_child(l_treenode* node, l_int i, l_treenode* child)
{
  l_int node_i = 0;
  l_treenode* cur_child = 0;

  cur_child = node->first_child;

  while (node_i < i) {
    if (cur_child == 0) {
      break;
    }
    cur_child = cur_child->next_sibling;
    node_i += 1;
  }

  if (node_i < i) {
    return false;
  }

  if (cur_child) {
    return l_treenode_insert_sibling_before(cur_child, child);
  } else {
    return l_treenode_append_child(node, child);
  }
}

L_EXTERN l_bool
l_treenode_insert_sibling_after(l_treenode* node, l_treenode* sibling)
{
  l_treenode* parent = node->parent;
  l_treenode* first = sibling;
  l_treenode* last = sibling;

  if (parent == 0) {
    return false;
  }

  while (first->prev_sibling) {
    first = first->prev_sibling;
    first->parent = parent;
  }

  while (last->next_sibling) {
    last = last->next_sibling;
    last->parent = parent;
  }

  if (node->next_sibling) {
    last->next_sibling = node->next_sibling;
    node->next_sibling->prev_sibling = last;
    node->next_sibling = first;
    first->prev_sibling = node;
  } else {
    node->next_sibling = first;
    first->prev_sibling = node;
    /* parent last child changed */
    parent->last_child = last;
  }

  sibling->parent = parent;
  return true;
}

L_EXTERN l_bool
l_treenode_insert_sibling_before(l_treenode* node, l_treenode* sibling)
{
  l_treenode* parent = node->parent;
  l_treenode* first = sibling;
  l_treenode* last = sibling;

  if (parent == 0) {
    return false;
  }

  while (first->prev_sibling) {
    first = first->prev_sibling;
    first->parent = parent;
  }

  while (last->next_sibling) {
    last = last->next_sibling;
    last->parent = parent;
  }

  if (node->prev_sibling) {
    first->prev_sibling = node->prev_sibling;
    node->prev_sibling->next_sibling = first;
    node->prev_sibling = last;
    last->next_sibling = node;
  } else {
    node->prev_sibling = last;
    last->next_sibling = node;
    /* parent first child changed */
    parent->first_child = first;
  }

  sibling->parent = parent;
  return true;
}

L_EXTERN void
l_foreach_treenode(l_treenode* tree, void (*func)(void* parm, l_treenode* node, l_bool start), void* parm)
{
  l_treenode* child = 0;

  func(parm, tree, true);

  child = tree->first_child;
  while (child) {
    l_foreach_treenode(child, func, parm);
    child = child->next_sibling;
  }

  func(parm, tree, false);
}

L_EXTERN l_treenode*
l_treenode_remove_first_child(l_treenode* node)
{
  l_treenode* first = 0;

  if (node->first_child == 0) {
    return 0;
  }

  first = node->first_child;

  if (first->next_sibling) {
    node->first_child = first->next_sibling;
    first->next_sibling->prev_sibling = 0;
    first->next_sibling = 0;
  } else {
    node->first_child = 0;
    node->last_child = 0;
  }

  first->parent = 0;
  return first;
}

L_EXTERN l_treenode*
l_treenode_remove_last_child(l_treenode* node)
{
  l_treenode* last = 0;

  if (node->last_child == 0) {
    return 0;
  }

  last = node->last_child;

  if (last->prev_sibling) {
    node->last_child = last->prev_sibling;
    last->prev_sibling->next_sibling = 0;
    last->prev_sibling = 0;
  } else {
    node->first_child = 0;
    node->last_child = 0;
  }

  last->parent = 0;
  return last;
}

L_EXTERN l_treenode*
l_treenode_remove_node(l_treenode* node)
{
  l_treenode* parent = node->parent;
  l_treenode* prev = 0;
  l_treenode* next = 0;

  if (parent == 0) {
    return 0;
  }

  prev = node->prev_sibling;
  if (prev == 0) {
    l_assert(LNUL, parent->first_child == node);
    return l_treenode_remove_first_child(parent);
  }

  next = node->next_sibling;
  if (next == 0) {
    l_assert(LNUL, parent->last_child == node);
    return l_treenode_remove_last_child(parent);
  }

  prev->next_sibling = next;
  next->prev_sibling = prev;

  node->next_sibling = 0;
  node->prev_sibling = 0;
  node->parent = 0;

  return node;
}

L_EXTERN l_treenode*
l_treenode_remove_child(l_treenode* node, l_int i)
{
  l_int node_i = 0;
  l_treenode* child = 0;

  child = node->first_child;
  if (child == 0 || i < 0) {
    return 0;
  }

  while (node_i < i) {
    node_i += 1;
    child = child->next_sibling;
    if (child == 0) {
      return 0;
    }
  }

  return l_treenode_remove_node(child);
}

L_EXTERN l_treenode*
l_treenode_remove_sibling_after(l_treenode* node)
{
  if (node->next_sibling) {
    return l_treenode_remove_node(node->next_sibling);
  } else {
    return 0;
  }
}

L_EXTERN l_treenode*
l_treenode_remove_sibling_before(l_treenode* node)
{
  if (node->prev_sibling) {
    return l_treenode_remove_node(node->prev_sibling);
  } else {
    return 0;
  }
}

