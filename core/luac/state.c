#include "core/luac/state.h"
#define L_GLOBAL_TABLE_NAME "lnlylib_global_table"
#define L_MAX_FIELD_NAME_LEN 80

/** lua_setupvalue(L, funcindex, n)
It assigns the stack top value to the function's n-th upvalue.
The function's first upvalue is usually _ENV.
If success, it returns upvalue's name and pops up the top value from stack.
It returns NULl and pops nothing if something goes wrong.
*/

L_EXTERN void /* the env table is on the top [-1, +0, -] */
laac_setENV(lua_State* L, l_funcindex f) {
  if (lua_setupvalue(L, f.index, 1 /* the _ENV */)) return;
  l_loge_s("lua_setupvalue _ENV fail");
  lua_pop(L, 1);
}

L_EXTERN void /* a value is on the top [-1, +0, e] */
laac_setfield(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN lua_State*
luac_newstate() {
  lua_State* L;
  
  if (!(L = luaL_newstate())) {
    l_loge_s("luaL_newstate fail");
    return 0;
  }
  
  luaL_openlibs(L); /* open all standard lua libraries */
  
}
