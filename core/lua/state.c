#include "core/luac/state.h"
#define L_GLOBAL_TABLE_NAME "lnlylib_global_table"
#define L_MAX_FIELD_NAME_LEN 80

/** lua_setupvalue(L, int funcindex, int n) const char* [-(0|1), +0, -]
It assigns the top stack value as the function's n-th upvalue.
Actually the function that has upvalues is called closure.
The function's first upvalue is _ENV, it refers to the global env _G by default.
If success, it returns upvalue's name and pops up the top value from stack.
It returns NULl and pops nothing if something goes wrong.
Function notation: popup 0 or 1 value, push 0 value, doesn't raise error.
---
## lua_getupvalue(L, int funcindex, int n) const char* [-0, +(0|1), -]
It pushes the function's n-th upvalue onto the stack and return its name.
Returns NULL and pushes nothing if something goes wrong.
For C functions, this function uses the empty string "" for all upvalues.
For Lua functions, upvalues are the external local variables that the function
uses, and that are consequently included in its closure.
*/

L_EXTERN void /* the env table is on the top [-1, +0, -] */
l_state_setFuncEnv(lua_State* L, l_funcindex f) {
  if (lua_setupvalue(L, f.index, 1 /* the _ENV */)) return;
  l_loge_s("lua_setupvalue _ENV");
  lua_pop(L, 1);
}


/** lua_setfield(L, int tableindex, const char* K) [-1, +0, e]
Set the top value to the table's field, i.e. table[K] = v, and pops the value.
As in Lua, this function may trigger a metamethod for the "newindex" event.
---
## lua_setglobal(L, const char* name) [-1, +0, e]
Pops a value from the stack and sets it as the new value of global name.
---
## lua_getfield(L, int tableindex, const char* K) int [-0, +1, e]
Pushes onto the stack the value table[K], returns the type of the pushed value.
As in Lua, this function may trigger a metamethod for the "index" event.
---
## lua_getglobal(L, const char* name) int [-0, +1, e]
Pushes onto the stack the global value, returns the type of the value.
*/

L_EXTERN void /* [-1, +1, e] */
l_state_setField(lua_State* L, l_tableindex t, const void* fieldname, l_stackindex s) {
  lua_pushvalue(L, s.index);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
l_state_setFieldString(lua_State* L, l_tableindex t, const void* fieldname, l_strn strval) {
  lua_pushlstring(L, (const char*)strval.start, (size_t)strval.len);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
l_state_setFieldInteger(lua_State* L, l_tableindex t, const void* fieldname, l_long intval) {
  lua_pushinteger(L, (lua_Integer)intval);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
l_state_setGlobal(lua_State* L, const void* globalname, l_stackindex s) {
  lua_pushvalue(L, s.index);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN void /* [-1, +1, e] */
l_state_setGlobalString(lua_State* L, const void* globalname, l_strn strval) {
  lua_pushlstring(L, (const char*)strval.start, (size_t)strval.len);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN void /* [-1, +1, e] */
l_state_setGlobalInteger(lua_State* L, const void* globalname, l_long intval) {
  lua_pushinteger(L, (lua_Integer)intval);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN l_stackindex /* [-0, +1, e] */
l_state_getGlobal(lua_State* L, const void* globalname) {
  lua_getglobal(L, (const char*)globalname);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
l_state_getGlobal2(lua_State* L, const void* globalname, l_luatypenum* t) {
  int type = lua_getglobal(L, (const char*)globalname);
  if (t) t.n = type;
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
l_state_getTable(lua_State* L, const void* globalname) {
  lua_getglobal(L, (const char*)globalname);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
l_state_getTable2(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_getfield(L, t.index, (const char*)fieldname);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
l_state_getField(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_getfield(L, t.index, (const char*)fieldname);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
l_state_getField2(lua_State* L, l_tableindex t, const void* fieldname, l_luatypenum* t) {
  int type = lua_getfield(L, t.index, (const char*)fieldname);
  if (t) t.n = type;
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN int
l_state_isNoneOrNil(l_luatypenum t) {
  return (t.n == LUA_TNIL || t.n == LUA_TNONE);
}

L_EXTERN l_state_luanil()
l_luatypenil() {
  return (l_luatypenum){LUA_TNIL};
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
---
## lua_istable(lua_State* L, int index) int [-0, +0, -]
Returns 1 if the value at the given index is a table, and o otherwise.
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
#define L_GLOBAL_TABLE_NAME "lnlylib_global_table"
#define L_MAX_FIELD_NAME_LEN 80

local lucy = L_LUACONF_GLOBAL_TABLE
local root = lucy.rootdir
assert(root, 'rootdir not exist')
package.path = package.path .. ';' .. root .. '?.lua'
package.cpath = package.cpath .. ';' .. root .. '?.so;' .. root .. '?.dll'
#endif

luaopen_package()
setpath(L, "path", LUA_PATH_VAR, LUA_PATH_DEFAULT);
setpath(L, "cpath", LUA_CPATH_VAR, LUA_CPATH_DEFAULT);

static void
l_luaconf_init(lua_State* L)
{
  l_tableindex table;
  l_funcindex func;

  table = l_luastate_newtable(L); /* push table */
  lua_pushliteral(L, L_ROOT_DIR); /* push value */
  l_luastate_setfield(L, table, "rootdir"); /* pop value */
  lua_setglobal(L, L_LUACONF_TABLE_NAME); /* pop table */

  if (!l_luastate_execfile(L, l_strn_literal(L_ROOT_DIR "conf/init.lua"), 0)) {
    l_loge_s("execute init.lua failed");
    return;
  }

  func = l_luastate_loadfile(L, l_strn_literal(L_ROOT_DIR "conf/conf.lua")); /* push func if success */
  if (func.index == 0) {
    l_loge_s("load conf.lua failed");
    return;
  }

  lua_getglobal(L, L_LUACONF_TABLE_NAME); /* push table */
  l_luastate_setenv(L, func); /* pop table */

  l_luastate_call(L, func, 0); /* pop func */

#if 0
  lua_getglobal(L, libname); /* push the table */
  if (!lucy_dofile(L, L_ROOT_DIR "core/base.lua", 1)) { /* push one result */
    l_loge_s("open base.lua failed");
    lua_pop(L, 1); /* pop the table */
    return;
  }
  lucy_settablefield(L, "base"); /* pop the reult */
  lua_pop(L, 1); /* pop the table */
#endif
}

/** lua_isinteger(L, int stackindex) int [-0, +0, -]
Returns 1 if the value at the given index is an integer, that is,
the value is a number and is represented as an integer, and 0 otherwise.
---
## lua_isnumber(L, int stackindex) int [-0, +0, -]
Returns 1 if the value at the given index is a number or a string convertible
to a number, and 0 otherwise.
---
## lua_isstring(L, int stackindex) int [-0, +0, -]
Returns 1 if the value at the given index is a string or a number,
and 0 otherwise.
---
## lua_tointegerx(L, int stackindex, int* isnum) lua_Integer [-0, +0, -]
Converts the Lua value at the given index to the signed integer type.
The Lua value must be an integer, or a number or string convertible to an
integer, otherwise lua_tointegerx returns 0.
If isnum is not NULL, it gets a boolean value indicates that whether the
operation succeeded.
---
## lua_tonumberx(L, int stackindex, int* isnum) lua_Number [-0, +0, -]
Converts teh Lua value at the given index to the C type lua_Number.
The Lua value must be a number or a string convertible to a number;
otherwise, lua_tonumberx returns 0.
If isnum is not NULL, it gets a boolean value indicates that whether the
operation succeeded.
---
## lua_tolstring(L, int stackindex, size_t* len) const char* [-0, +0, m]
Converts the Lua value at the given index to a C string.
If len is not NULL, it sets *len with the string length.
The Lua value must be a string or a number; otherwise, it returns NULL.
If the value is a number, then it also **changes the actual value** in the
stack to a string (this change confuses lua_next when lua_tolstring is applied
to keys during a table traversal).
It returns a pointer to a string inside the Lua state.
This string always has a zero after its last character, but can contain other
zeros in its body.
Because Lua has garbage collection, there is no guarantee that the pointer
returned by lua_tolstring will be valid after the corresponding Lua value is
removed from the stack.
*/

L_EXTERN l_long /* if top is an integer return it, otherwise return 0 */
l_state_getInteger(lua_State* L) {
  int topindex = lua_gettop(L);
  if (!lua_isinteger(L, topindex)) return 0;
  return (l_long)lua_tointeger(L, topindex, 0);
}

L_EXTERN l_strn /* if top is an string return it, otherwise return "" */
l_state_getString(lua_State* L) {
  const char* s = 0;
  size_t len = 0;
  int topindex = lua_gettop(L);
  if (!lua_isstring(L, topindex) || !(s = lua_tolstring(L, topindex, &len)) {
    return l_strn_literal("");
  }
  return l_strn_l(s, (l_int)len);
}

#define L_DEFAULT_PATH_GLOBAL_NAME "lnlylib_default_path"
#define L_DEFAULT_C_PATH_GLOBAL_NAME "lnlylib_default_c_path"

L_EXTERN l_stackindex
l_state_newString(lua_State* L, l_strn s1, l_strn s2, l_strn s3) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN void
l_state_setPath(lua_State* L, const void* path) {
  l_luatypenum type;
  l_tableindex package;
  l_stackindex default_path, default_c_path;

  package = l_state_getTable(L, "package");

  default_path = l_state_getGlobal(L, L_DEFAULT_PATH_GLOBAL_NAME, &type);
  if (l_state_isNoneOrNil(type)) {
    default_path = l_state_getField(L, package, "path");
    l_state_setGlobal(L, L_DEFAULT_PATH_GLOBAL_NAME, default_path);
  }

  default_c_path = l_state_getGlobal(L, L_DEFAULT_C_PATH_GLOBAL_NAME, &type);
  if (l_state_isNoneOrNil(type)) {
    default_c_path = l_state_getField(L, package, "cpath");
    l_state_setGlobal(L, L_DEFAULT_C_PATH_GLOBAL_NAME, default_c_path);
  }
}

