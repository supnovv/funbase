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
} l_tableindex;

typedef struct {
  int index;
} l_funcindex;

#endif /* LNLYLIB_CORE_LAPI_H */

