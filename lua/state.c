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
lunac_setfuncenv(lua_State* L, l_funcindex f) {
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
lunac_setfield(lua_State* L, l_tableindex t, const void* fieldname, l_stackindex s) {
  lua_pushvalue(L, s.index);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
lunac_setfieldstring(lua_State* L, l_tableindex t, const void* fieldname, l_strn strval) {
  lua_pushlstring(L, (const char*)strval.start, (size_t)strval.len);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
lunac_setfieldinteger(lua_State* L, l_tableindex t, const void* fieldname, l_long intval) {
  lua_pushinteger(L, (lua_Integer)intval);
  lua_setfield(L, t.index, (const char*)fieldname); /* will pop the top or raise error */
}

L_EXTERN void /* [-1, +1, e] */
lunac_setglobal(lua_State* L, const void* globalname, l_stackindex s) {
  lua_pushvalue(L, s.index);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN void /* [-1, +1, e] */
lunac_setglobalstring(lua_State* L, const void* globalname, l_strn strval) {
  lua_pushlstring(L, (const char*)strval.start, (size_t)strval.len);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN void /* [-1, +1, e] */
lunac_setglobalinteger(lua_State* L, const void* globalname, l_long intval) {
  lua_pushinteger(L, (lua_Integer)intval);
  lua_setglobal(L, (const char*)globalname);
}

L_EXTERN l_stackindex /* [-0, +1, e] */
lunac_getglobal(lua_State* L, const void* globalname) {
  lua_getglobal(L, (const char*)globalname);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
lunac_getglobal2(lua_State* L, const void* globalname, l_luatypenum* t) {
  int type = lua_getglobal(L, (const char*)globalname);
  if (t) t.n = type;
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
lunac_gettable(lua_State* L, const void* globalname) {
  lua_getglobal(L, (const char*)globalname);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
lunac_gettable2(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_getfield(L, t.index, (const char*)fieldname);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
lunac_getfield(lua_State* L, l_tableindex t, const void* fieldname) {
  lua_getfield(L, t.index, (const char*)fieldname);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex /* [-0, +1, e] */
lunac_getfield2(lua_State* L, l_tableindex t, const void* fieldname, l_luatypenum* t) {
  int type = lua_getfield(L, t.index, (const char*)fieldname);
  if (t) t.n = type;
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN int
lunac_isnoneornil(l_luatypenum t) {
  return (t.n == LUA_TNIL || t.n == LUA_TNONE);
}

L_EXTERN l_luatypenum
lunac_nilvalue() {
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
lunac_newtable(lua_State* L) {
  lua_newtable(L);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* push table on top and return the index */
lunac_newtable2(lua_State* L, int nseq, int nrest) {
  lua_createtable(L, nseq, nrest);
  return (l_tableindex){lua_gettop(L)};
}


L_EXTERN lua_State*
lunac_newstate() {
  lua_State* L;
  
  if (!(L = luaL_newstate())) {
    l_loge_s("luaL_newstate fail");
    return 0;
  }
  
  luaL_openlibs(L); /* open all standard lua libraries */
  
}

L_EXTERN void
lunac_freestate(lua_State* L) {
  if (!L) return;
  l_luaextra_free(L);
  lua_close(L);
}

/** lua_gettop(L) int [-0, +0, -]
Returns the index of the top element in the stack.
Because indices start at 1, this result is equal to the number of
elements in the stack; in particular, 0 means an empty stack.
---
## lua_pop(L, int n) [-n, +0, -]
Pops n elements from the stack.
---
## lua_settop(L, int index) [-?, +?, -]
Accepts any index, or 0, and sets the stack top to this index.
If the new top is larger than the old one, then the new elements
are filled with nil.
If index is 0, then all stack elements are removed.
*/

L_EXTERN void /* remove stack elements to s, and s is also removed */
lunac_popstackto(lua_State* L, l_stackindex s) {
  int n = lua_gettop(L) - s.index + 1;
  if (n <= 0) return;
  lua_pop(L, n);
}

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
lunac_getinteger(lua_State* L) {
  int topindex = lua_gettop(L);
  if (!lua_isinteger(L, topindex)) return 0;
  return (l_long)lua_tointeger(L, topindex, 0);
}

L_EXTERN l_strn /* if top is an string return it, otherwise return "" */
lunac_getstring(lua_State* L) {
  const char* s = 0;
  size_t len = 0;
  int topindex = lua_gettop(L);
  if (!lua_isstring(L, topindex) || !(s = lua_tolstring(L, topindex, &len))) {
    return l_strn_literal("");
  }
  return l_strn_l(s, (l_int)len);
}

L_EXTERN l_stackindex
lunac_newstring(lua_State* L, l_strn s1, l_strn s2, l_strn s3) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring4(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring5(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  if (s5.len > 0) luaL_addlstring(&B, (const char*)s5.start, (size_t)s5.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring6(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  if (s5.len > 0) luaL_addlstring(&B, (const char*)s5.start, (size_t)s5.len);
  if (s6.len > 0) luaL_addlstring(&B, (const char*)s6.start, (size_t)s6.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring7(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  if (s5.len > 0) luaL_addlstring(&B, (const char*)s5.start, (size_t)s5.len);
  if (s6.len > 0) luaL_addlstring(&B, (const char*)s6.start, (size_t)s6.len);
  if (s7.len > 0) luaL_addlstring(&B, (const char*)s7.start, (size_t)s7.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring8(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  if (s5.len > 0) luaL_addlstring(&B, (const char*)s5.start, (size_t)s5.len);
  if (s6.len > 0) luaL_addlstring(&B, (const char*)s6.start, (size_t)s6.len);
  if (s7.len > 0) luaL_addlstring(&B, (const char*)s7.start, (size_t)s7.len);
  if (s8.len > 0) luaL_addlstring(&B, (const char*)s8.start, (size_t)s8.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_stackindex
lunac_newstring9(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8, l_strn s9) {
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  if (s1.len > 0) luaL_addlstring(&B, (const char*)s1.start, (size_t)s1.len);
  if (s2.len > 0) luaL_addlstring(&B, (const char*)s2.start, (size_t)s2.len);
  if (s3.len > 0) luaL_addlstring(&B, (const char*)s3.start, (size_t)s3.len);
  if (s4.len > 0) luaL_addlstring(&B, (const char*)s4.start, (size_t)s4.len);
  if (s5.len > 0) luaL_addlstring(&B, (const char*)s5.start, (size_t)s5.len);
  if (s6.len > 0) luaL_addlstring(&B, (const char*)s6.start, (size_t)s6.len);
  if (s7.len > 0) luaL_addlstring(&B, (const char*)s7.start, (size_t)s7.len);
  if (s8.len > 0) luaL_addlstring(&B, (const char*)s8.start, (size_t)s8.len);
  if (s9.len > 0) luaL_addlstring(&B, (const char*)s9.start, (size_t)s9.len);
  luaL_pushresult(&B);
  return (l_stackindex){lua_gettop(L)};
}

#define L_DEFAULT_PATH_GLOBAL_NAME "lnlylib_default_path"
#define L_DEFAULT_C_PATH_GLOBAL_NAME "lnlylib_default_c_path"

L_EXTERN void
lunac_setpath(lua_State* L, l_strn path) {
  l_luatypenum type;
  l_tableindex package;
  l_stackindex default_path, default_c_path;
  l_stacsindex path_string;

  package = lunac_gettable(L, "package");

  default_path = lunac_getglobal(L, L_DEFAULT_PATH_GLOBAL_NAME, &type);
  if (lunac_isnoneornil(type)) {
    default_path = lunac_getfield(L, package, "path");
    lunac_setglobal(L, L_DEFAULT_PATH_GLOBAL_NAME, default_path);
  }

  default_c_path = lunac_getglobal(L, L_DEFAULT_C_PATH_GLOBAL_NAME, &type);
  if (lunac_isnoneornil(type)) {
    default_c_path = lunac_getfield(L, package, "cpath");
    lunac_setglobal(L, L_DEFAULT_C_PATH_GLOBAL_NAME, default_c_path);
  }

  path_string = lunac_newstring4(L, lunac_getstring(L, default_path), l_strn_literal(";"), path, l_strn_literal("?.lua"));
  lunac_setfield(L, package, "path", path_string);

  path_string = lunac_newstring6(L, lunac_getstring(L, default_c_path), l_strn_literal(";"),
      path, l_strn_literal("/c/?.so;"), path, l_strn_literal("/c/?.dll");
  lunac_setfield(L, package, "cpath", path_string);

  lunac_popstackto(L, package.stackindex);
}

L_EXTERN void
lunac_addpath(lua_State* L, l_strn path) {
  l_tableindex package;
  l_stackindex old_path, new_path_string;

  package = lunac_gettable(L, "package");

  old_path = lunac_getfield(L, package, "path");
  new_path_string = lunac_newstring4(L, lunac_getstring(L, old_path), l_strn_literal(";"), path, l_strn_literal("?.lua"));
  lunac_setfield(L, package, "path", new_path_string);

  old_path = lunac_getfield(L, package, "cpath");
  new_path_string = lunac_newstring6(L, lunac_getstring(L, old_path), l_strn_literal(";"),
      path, l_strn_literal("/c/?.so;"), path, l_strn_literal("/c/?.dll");
  lunac_setfield(L, package, "cpath", new_path_string);

  lunac_popstackTo(L, package.stackindex);
}

L_EXTERN l_stackindex /* [-0, +2, -] */
lunac_getpath(lua_State* L) {
  l_tableindex package = lunac_gettable(L, "package");
  return lunac_getfield(L, package, "path");
}

L_EXTERN l_stackindex /* [-0, +2, -] */
lunac_getcpath(lua_State* L) {
  l_tableindex package = lunac_gettable(L, "package");
  return lunac_getfield(L, package, "cpath");
}

/** luaL_requiref(L, const char* modulename, lua_CFunction openfunc, int setglobal) [-0, +1, e]
If modulename is not already present in package.loaded, calls openfunc with string
as an argument and sets the call result in package.loaded[modulename], as if that
function has been called through require.
If setglobal is true, also stores the module info global modulename.
Leaves a copy of the module on the stack.
---
## require(modulename)
1. Loads the given module. It first check package.loaded[modulename], if it is not already loaded then
2. Try to find a loader based on package.searchers sequence, the default configuration for searchers is below.
3. If package.preload[modulename] has a value, call this value as a loader; otherwise,
4. require searches for a Lua loader using the path stored in package.path; if it fails,
5. It searches for a C loader using the path stored in package.cpath; if it also fails,
6. It tries an all-in-one loader, see package.searchers.
7. Once a loader is found, require calls it with two arguments, the modulename and an extra value depeneded.
8. If the loader returns non-nil value, require assigns the returned value to package.loaded[modulename].
9. If the loader does not return a non-nil value and no value assigned to package.loaded[modulename], then true is assigned to it.
0. In any case, require returns the final value of package.loaded[modulename].
1. If there is any error loading or running the module, or if it cannot find any loader for the module, an error raised.
---
## package.searchers
A sequence that stroed the searcher functions, when looking for a module, they are called by require in ascending order.
The search function is called with the modulename, and return a loader function plus an extra value that will be passed to that loader.
Or it returns a string explaining why it did not find the module or nil if it has nothing to say.
Lua initializes the packages.searchers with 4 searcher functions.
1. The first one looks for a loader in the package.preloaded table
2. The second one looks for a Lua loader using the path stored in package.path, how the path is searched is described by package.searchpath
3. The third one looks for a C loader using the path stored in package.cpath, how the path is searched is described by package.searchpath
4. The fourth search tries an all-in-one loader. It searches the C path for a library for the root name of the given module.
Special for searching C loader, when the C library is found, the searcher will find a function named luaopen_xxx in the library as the loader.
For instance, if the module name is a.b.c-v2.1 and the library a/b/c.so for example is found, the searcher will try to find luaopen_a_b_c in a/b/c.so
And for all-in-one loader, when to find module a.b.c, it will try to find the library a.so for example, and then look for a loader function luaopen_a_b_c.
With this facility, a package can pack several C submodules into one single library, with each submodule keeping its original open function.
All searchers except the first one (preload) return as the extra value the file name where the module was found, as returned by package.searchpath.
---
## package.searchpath(name, path[, sep[, rep]])
Searches for the given name in the given path.
A path is a string containing a sequence of templates separated by semicolons.
For each template, the function replaces each ? mark (if any) in the template with a copy of name wherein all sep (a dot, by default)
were replaced by rep (the system's directory separator, by default), and then tries to open the resulting file name.
For instance, if the path stirng is "./?.lua;/usr/local/?/init.lua", and the search name is "a.b.c",
searchpath will try to pen the files "./a/b/c.lua", "/usr/local/a/b/c/init.lua" in order.
It returns the resulting name of the first file that it can open in read mode (after closing the file),
or nil plus an error message if none succeeds, this error message lists all file names it tried to open.
*/

static int
l_cfuncforlua_searchandload(lua_State* L) {
  int name_index = lua_gettop(L);
  l_tableindex package = lunac_gettable(L, "package");
  l_stackindex lua_path = lunac_getfield(L, package, "path");
  l_funcindex search_func = lunac_getfieldfunction(L, package, "searchpath");
  lua_pushvalue(L, name_index);
  lua_pushvalue(L, lua_path);
  lunac_call(L, search_func, 1); /* only get 1 result */
  if (lua_isnil(L, lua_gettop(L))) return 1; /* return nil on the top */
  lunac_loadfilefromtop(L);
  return 1;
}

L_EXTERN l_funcindex
lunac_searchandload(lua_State* L, const void* name) {
  luaL_requiref(L, (const char*)name, l_cfuncforlua_searchandload, false);
  return (l_funcindex){lua_gettop(L)};
}

L_EXTERN l_funcindex
lunac_loadfile(lua_State* L, const void* luafile) {

}

L_EXTERN l_funcindex
lunac_loadstring(lua_State* L, l_strn luacode) {

}
