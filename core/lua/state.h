#ifndef L_CORE_LUA_STATE_H
#define L_CORE_LUA_STATE_H

typedef struct {
  int index;
} l_stackindex;

typedef union {
  l_stackindex stackindex;
  int index;
} l_tableindex;

typedef union {
  l_stackindex stackindex;
  int index;
} l_funcindex;

#endif /* L_CORE_LUA_STATE_H */

