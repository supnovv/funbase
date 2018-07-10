#ifndef LNLYLIB_CORE_LAPI_H
#define LNLYLIB_CORE_LAPI_H
#include <lualib.h>
#include <luaxlib.h>
#include "core/base.h"

#define ll_pop_err(L) {\
  l_loge_1("luaerr %s", ls(lua_tostring((L), -1))); \
  lua_pop((L), 1); }

typedef struct {
  int index;
} l_stackindex;

L_INLINE l_stackindex
ll_sidx(int i)
{
  return (l_stackindex){i};
}

typedef union {
  int index;
  l_stackindex sidx;
} l_tableindex;

typedef union {
  int index;
  l_stackindex sidx;
} l_funcindex;

#endif /* LNLYLIB_CORE_LAPI_H */

