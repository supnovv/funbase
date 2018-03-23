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
l_state_setFuncEnv(lua_State* L, l_funcindex f) {
  if (lua_setupvalue(L, f.index, 1 /* the _ENV */)) return;
  l_loge_s("lua_setupvalue _ENV fail");
  lua_pop(L, 1);
}

L_EXTERN void /* a value is on the top [-1, +0, e] */
l_state_setField(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

/** lua_newtable(lua_State* L)  [-0, +1, m]
Creates a new empty table and pushes it onto the stack.
It is equivalent to lua_createtable(L, 0, 0).
---
## lua_createtable(lua_State* L, int narr, int nrec)  [-0, +1, m]
Creates a new empty table and pushes it onto the stack.
@narr is a hint for how many elements the table will have as a sequence.
@nrec is a hint for how many other elements the table will have.
Lua may use these hints to preallocate memory for the new table.
This preallocation is useful for performance when you know in advance how many elements the table will have.
Otherwise you can use the function lua_newtable.
*/

L_EXTERN l_tableindex /* push table on top and return the index */
l_state_newTable(lua_State* L) {
  lua_newtable(L);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* push table on top and return the index */
l_state_newTable2(lua_State* L, int nseq, int nrest) {
  lua_createtable(L, nseq, nrest);
  return (l_tableindex){lua_gettop(L))};
}


L_EXTERN lua_State*
l_state_newState() {
  lua_State* L;
  
  if (!(L = luaL_newstate())) {
    l_loge_s("luaL_newstate fail");
    return 0;
  }
  
  luaL_openlibs(L); /* open all standard lua libraries */
  
}

L_EXTERN void
l_state_freeState(lua_State* L) {
  if (!L) return;
  l_luaextra_free(L);
  lua_close(L);
}

#if 0
local lucy = L_LUACONF_GLOBAL_TABLE
local root = lucy.rootdir
assert(root, 'rootdir not exist')
package.path = package.path .. ';' .. root .. '?.lua'
package.cpath = package.cpath .. ';' .. root .. '?.so;' .. root .. '?.dll'
#endif

L_EXTERN void
l_state_setPath(lua_State* L, const void* path) {
}

