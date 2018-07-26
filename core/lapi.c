#define LNLYLIB_API_IMPL
#include "core/lapi.h"

L_EXTERN void /* pos is not popped */
ll_pop_to(lua_State* L, int pos)
{
  int n = lua_gettop(L) - pos;
  if (n > 0) {
    lua_pop(L, n);
  }
}

L_EXTERN void /* pos is popped out */
ll_pop_beyond(lua_State* L, int pos)
{
  int n = lua_gettop(L) - pos + 1;
  if (n > 0) {
    lua_pop(L, n);
  }
}

L_EXTERN void /* [-1, +1, e] */
ll_set_global(lua_State* L, const void* name, int stackindex)
{
  lua_pushvalue(L, stackindex);
  lua_setglobal(L, (const char*)name);
}

L_EXTERN void /* [-1, +1, e] */
ll_set_field(lua_State* L, l_tableindex t, const void* field, int stackindex)
{
  lua_pushvalue(L, stackindex);
  lua_setfield(L, t.index, (const char*)field);
}

L_EXTERN int /* [-0, +1, e] */
ll_get_global(lua_State* L, const void* name)
{
  lua_getglobal(L, (const char*)name); /* it returns value type */
  return lua_gettop(L);
}

L_EXTERN l_tableindex /* [-0, +1, e] */
ll_get_table(lua_State* L, const void* name)
{
  l_tableindex table;
  table.index = ll_get_global(L, name);
  return table;
}

L_EXTERN int /* [-0, +1, e] */
ll_get_field(lua_State* L, l_tableindex t, const void* field)
{
  lua_getfield(L, t.index, (const char*)field); /* it returns value type */
  return lua_gettop(L);
}

L_EXTERN l_tableindex /* [-0, +1, e] */
ll_get_field_table(lua_State* L, l_tableindex t, const void* field)
{
  l_tableindex table;
  table.index = ll_get_field(L, t, field);
  return table;
}

L_EXTERN l_tableindex
ll_new_table(lua_State* L)
{
  lua_newtable(L);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex
ll_new_table_with_size(lua_State* L, int nseq, int rest)
{
  lua_createtable(L, nseq > 0 ? nseq : 0, rest > 0 ? rest : 0);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN int
ll_type(lua_State* L, int stackindex)
{
  return lua_type(L, stackindex)
}

L_EXTERN l_bool
ll_is_str(lua_State* L, int stackindex)
{
  return ll_type(L, stackindex) == LUA_TSTRING;
}

L_EXTERN l_bool
ll_is_int(lua_State* L, l_statckindex stackindex)
{
  return lua_isinteger(L, stackindex)
}

L_EXTERN l_bool
ll_is_num(lua_State* L, int stackindex)
{
  return ll_type(stackindex) == LUA_TNUMBER;
}

L_EXTERN l_bool
ll_is_invalid(lua_State* L, int stackindex)
{
  int n = ll_type(L, stackindex)
  return (n == LUA_TNIL || n == LUA_TNONE);
}

L_EXTERN l_strn
ll_to_strn(lua_State* L, int stackindex)
{
  if (ll_is_int(L, stackindex) || ll_is_str(L, stackindex)) {
    size_t len = 0;
    const char* s = 0;
    s = lua_tolstring(L, stackindex, &len);
    if (s == 0) {
      return l_empty_strn();
    } else {
      return l_strn_l(s, (l_int)len);
    }
  } else {
    /* TODO: consider other types */
    return l_empty_strn();
  }
}

/** create string in lua **
const char* lua_pushfstring(lua_State* L, const char* fmt, ...);
const char* lua_pushvfstring(lua_State* L, const char* fmt, va_list argp);
---
Pushes onto the stack a formatted string and returns a pointer to this string.
It is similar to the ISO C function sprintf, but has some important differences.
* You don't have to allocate space for the result: the result is a Lua string
  and Lua takes care of memory allocation (and deallocation, through garbage
  collection)
* The conversion specifiers are quite restricted. There are no flags, widths,
  or precisions. The conversion specifiers can only be '%%', '%s' (inserts a
  zero-terminated string), '%f' (inserts a lua_Number), '%I' (inserts a
  lua_Integer), '%p' (inserts a pointer as a hexadecimal numberal), '%d'
  (inserts an int), '%c' (inserts an int as a one-byte character), and '%U'
  (inserts a long int as a UTF-8 byte sequence).
Unlike other push functions, this function checks for the stack space it needs,
including the slot for its result.
---
L_buffinit(lua_State* L, luaL_Buffer* B);
void luaL_addvalue(luaL_Buffer* B); // add the value on top, and pop the value
void luaL_addchar/string(luaL_Buffer* B, char c/const char* s);
void luaL_addlstring(luaL_Buffer* B, const char* s, size_t l);
char* luaL_prepbuffsize(lua_Buffer* B, size_t sz);
char* luaL_buffinitsize(lua_State* L, luaL_Buffer* B, size_t sz);
void luaL_addsize(luaL_Buffer* B, size_t n);
void luaL_pushresult(luaL_Buffer* B);
---
After init luaL_Buffer, you can add values via luaL_addvalue/char/string/lstring,
or after luaL_prepbuffsize/luaL_buffinitsize you can copy data to buffer, and
luaL_addsize with the copied size to increase the buffer size. Finally, you call
luaL_pushresult push the result string to the top of the stack.
**********************************************************************/

static int
ll_new_strv(lua_State* L, int n, ...)
{
  va_list vl;
  l_strn s;
  luaL_Buffer B;
  luaL_buffinit(L, &B);

  va_start(vl, n);
  while (n-- > 0) {
    s = va_arg(vl, l_strn);
    if (s.p && s.n > 0) {
      luaL_addlstring(&B, (const char*)s.p, (size_t)s.n);
    }
  }
  va_end(vl);

  luaL_pushresult(&B);
  return lua_gettop(L);
}

L_EXTERN int
ll_new_str_n(lua_State* L, int n, l_strn* s)
{
  luaL_Buffer B;
  l_strn* pend = 0;

  luaL_buffinit(L, &B);
  pend = s + n;

  while (s < pend) {
    if (s->str && s->len > 0) {
      luaL_addlstring(&B, (const char*)s->str, (size_t)s->len);
    }
    s += 1;
  }

  luaL_pushresult(&B);
  return lua_gettop(L);
}

L_EXTERN int
ll_new_str_1(lua_State* L, l_strn s1)
{
  return ll_new_strv(L, 1, s1);
}

L_EXTERN int
ll_new_str_2(lua_State* L, l_strn s1, l_strn s2)
{
  return ll_new_strv(L, 2, s1, s2);
}

L_EXTERN int
ll_new_str_3(lua_State* L, l_strn s1, l_strn s2, l_strn s3)
{
  return ll_new_strv(L, 3, s1, s2, s3);
}

L_EXTERN int
ll_new_str_4(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4)
{
  return ll_new_strv(L, 4, s1, s2, s3, s4);
}

L_EXTERN int
ll_new_str_5(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5)
{
  return ll_new_strv(L, 5, s1, s2, s3, s4, s5);
}

L_EXTERN int
ll_new_str_6(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6)
{
  return ll_new_strv(L, 6, s1, s2, s3, s4, s5, s6);
}

L_EXTERN int
ll_new_str_7(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7)
{
  return ll_new_strv(L, 7, s1, s2, s3, s4, s5, s6, s7);
}

L_EXTERN int
ll_new_str_8(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8)
{
  return ll_new_strv(L, 8, s1, s2, s3, s4, s5, s6, s7, s8);
}

L_EXTERN int
ll_new_str_9(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8, l_strn s9)
{
  return ll_new_strv(L, 9, s1, s2, s3, s4, s5, s6, s7, s8, s9);
}

L_EXTERN int /* [-0, +2, -] */
ll_get_path(lua_State* L)
{
  l_tableindex t = ll_get_table(L, "package");
  return ll_get_field(L, t, "path");
}

L_EXTERN int /* [-0, +2, -] */
ll_get_cpath(lua_State* L)
{
  l_tableindex t = ll_get_table(L, "package");
  return ll_get_field(L, t, "cpath");
}

L_EXTERN void
ll_add_path(lua_State* L, l_strn path)
{
  l_tableindex package;
  int old_path;
  int new_path;

  if (l_strn_is_empty(&path)) {
    return;
  }

  package = ll_get_table(L, "package");
  old_path = ll_get_field(L, package, "path");

  new_path = ll_new_str_4(L, ll_to_strn(L, old_path), l_const_strn(";"), path, l_const_strn("?.lua"));
  ll_set_field(L, package, "path", new_path);

  ll_pop_beyond(L, package.index);
}

L_EXTERN void
ll_add_cpath(lua_State* L, l_strn path)
{
  l_tableindex package;
  int old_path;
  int new_path;

  if (l_strn_is_empty(&path)) {
    return;
  }

  package = ll_get_table(L, "package");
  old_path = ll_get_field(L, package, "cpath");

  new_path = ll_new_str_6(L, ll_to_strn(L, old_path), l_const_strn(";"), path, l_const_strn("?.so;"), path, l_const_strn("?.dll"));
  ll_set_field(L, package, "cpath", new_path);

  ll_pop_beyond(L, package.index);
}

L_EXTERN l_funcindex /* load the code to a function */
ll_load_expr(lua_State* L, l_strn expr)
{}

L_EXTERN l_funcindex /* load the code to a function */
ll_load_file(lua_State* L, l_strn file)
{}

L_EXTERN l_funcindex /* load the code to a function */
ll_load_clib(lua_State* L, l_strn file, l_strn openfuncname)
{

}

/** search and load moudle **
void luaL_requiref(lua_State* L, const char* modname, lua_CFunction openf, int setglobal);
If modname is not already present in package.loaded, calls openf with
string modname as an argument. openf is a loader, it loaded the code
as a function. luaL_requiref call the returned function to run the code
and sets the call result in package.loaded[modname], as if that function
has been called through require.
If setglobal is true, also stores the module into global modname.
Leaves a copy of the module (func loaded) on the stack.
---
local func = require(modname)
1. Loads the given module, it first check package.loaded[modname],
   if it is not already loaded then,
2. Try to find a loader based on package.searchers sequence, the
   default configure for searchers is below,
3. If package.preloaded[modname] has a value, call this value as
   a loader; otherwise,
4. require searches for a Lua loader using the path stored in
   package.path, if it fails,
5. It searches for a C loader using the path stored in package.cpath,
   if it also fails,
6. It tries an all-in-one loader, see package.searchers
7. Once a loader is found, require calls it with two arguments, the
   modname and an extra value depended.
8. If the loader returns non-nil value, i.e., it returns a function,
   require calls this function and assign the returned value to
   package.loaded[modname], otherwise,
9, if there is no value assigned to package.loaded[modname], true is
   assigned to it.
0, In any case, require returns the final value of
   package.loaded[modname].
1. If there is any error loading or running the module, or if it
   cannot find any loader for the module, an error raised
---
package.searchers
A sequence that stored the searcher functions, when looking for a
module, they are called by require in ascending order.
The search function is called with the modname, and return a
loader function plus an extra value that will be passed to
that loader.
Or it returns a string explaining why it did not find the module
or nil if it has nothing to say.
Lua initializes the packages.searchers with 4 funcitons.
1. The 1st one looks for a loader in the package.preloaded table
2. The 2nd one looks for a Lua loader using the path stored in
   package.path, how the path is searched is described in
   package.searchpath.
3. The 3rd one looks for a C loader using the path stored in
   package.cpath, how the path is searched is described in
   package.searchpath.
   When the C library is found, the searcher will find a function
   named luaopen_xxx in the library as the loader. For instance,
   if the module name is a.b.c-v2.1 and the library a/b/c.so for
   example is found, the search will try to find luaopen_a_b_c in
   a/b/c.so
4. The 4th search tries an all-in-one loader. It searches the C path
   for a library for the root name of the given module.
   For all-in-one loader, when to find module a.b.c, it will try to
   find the library a.so for example, and then look for a loader
   function luaopen_a_b_c. With this facility, a package can pack
   several C submodules into one single library, with each submodule
   keeping its original open function.
All searchers except the 1st one (preload) return as the extra value
the file name where the module was found, as returned by
package.searchpath.
---
package.searchpath(name, path[, sep[, rep]])
Searches for the given name in given path. A path is a string
containing a sequence of templates separated by semicolons (;).
For each template, the function replaces each ? mark (if any) in
the template with a copy of name wherein all sep (default is .)
were replaced by rep (default is directory separator), and then
tries to open the resulting file name.
For instance, if the path string is "./?.lua;/usr/local/?/init.lua",
and the search name is "a.b.c", searchpath will try to open the
files "./a/b/c.lua", "/usr/local/a/b/c/init.lua" in order.
It returns the resulting name of the first file that it can open
in read mode (after closing the file), or nil plus an error
message if none succeeds, this error message lists all file names
it tried to open.
---
package.loadlib(libname, funcname)
Dynamically links the host program within the C library libname.
If funcname is "*", then it only links with the library, making the
symbols exported by the library available to other dynamically linked
libraries.
Otherwise, it looks for a function funcname inside the library and
returns this function as a C function. So, funcname must follow the
lua_CFunction prototype.
This is a low-level function. It completely bypasses the package and
module system. Unlike require, it doesn not perform any path searching
and does not automatically adds extensions. libname must be the
complete file name of the C library, including if necessary a path and
an extension. funcname must be the exact name exported by the C library
(which may depend on the C compiler and linker used).
This funciton is not supported by Standard C. As such, it is only
available on some platform (Windows, Linux, Mac OS X, Solaris, BSD,
plus other Unix systems that support the dlfcn standard).
**********************************************************************/

static l_funcindex
ll_dynlib_load(lua_State* L, l_strn lib, l_strn symbol)
{
  /* can use package.loadlib */
}

static int
ll_search_and_load_func(lua_State* L)
{
  int name_lookup = 0;
  l_strn main_module;
  l_tablindex package;
  int path;
  l_funcindex search_func;

  name_lookup = lua_gettop(L);
  package = ll_get_table(L, "package");
  path = ll_get_field(L, package, "path");
  search_func = ll_get_field(L, package, "searchpath");

  lua_pushvalue(L, name_lookup);
  lua_pushvalue(L, path.index);
  ll_pcall(L, search_func, 1); /* get 1 result: nil or the found file name */

  if (lua_isnil(L, -1)) {
    goto try_to_laod_from_c_library;
  } else {
    ll_load_file(L, ll_to_strn(L, -1)); /* load the lua find */
    return 1; /* return loaded func on the top */
  }

try_to_load_from_c_library:

  ll_pop_to(L, search_func.index);
  path = ll_get_field(L, package, "cpath");

  lua_pushvalue(L, name_lookup);
  lua_pushvalue(L, path.index);
  ll_pcall(L, search_func, 1); /* get 1 result: nil or c library file name found */

  if (lua_isnil(L, -1)) {
    goto try_all_modules_in_one_library_method;
  } else {
    l_filename f = ll_gen_luaopen_func_name(ll_to_strn(L, name_lookup));
    ll_dynlib_load(L, ll_to_strn(L, -1), l_filename_strn(&f));
    return 1; /* return loaded func on the top */
  }

try_all_module_in_one_library_method:

  if (!ll_get_main_module(ll_to_strn(L, name_lookup), &main_module)) { /* name_lookup need has sub module names */
    return 1; /* return nil on the top */
  }

  ll_pop_to(L, path.index);

  ll_push_str(L, main_module);
  lua_pushvalue(L, path.index);
  ll_pcall(L, search_func, 1); /* get 1 result: nil or c library file name found */

  if (lua_isnil(L, -1)) {
    return 1; /* return nil on the top */
  } else {
    l_filename f = ll_gen_luaopen_func_name(ll_to_strn(L, name_lookup));
    ll_dynlib_load(L, ll_to_strn(L, -1), l_filename_strn(&f));
    return 1; /* return loaded func on the top */
  }
}

L_EXTERN l_funcindex
ll_search_and_load(lua_State* L, const void* file)
{
  luaL_requiref(L, (const char*)file, ll_search_and_load_func, false);
  return (l_funcindex){lua_gettop(L)};
}

