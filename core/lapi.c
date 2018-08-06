#define LNLYLIB_API_IMPL
#include <string.h>
#include <stdarg.h>
#include <lualib.h>
#include <lauxlib.h>
#include "core/lapi.h"

/** lua state and coroutine **
lua_State* lua_newstate(lua_Alloc f, void* ud);
lua_State* luaL_newstate();
void lua_close(lua_State* L);
lua_State* lua_newthread(lua_State* L);
---
The lua_newstate create a new thread (or a main coroutine) running in a new,
independent state. Returns NULL if it cannot create the thread. THe arguments
f is the allocator function; Lua does all memory allocation for this state
through this function. The second argument, ud, is an opaque pointer that Lua
passes to the allocator in every call.
luaL_newstate calls lua_newstate with an allocator based on the standard C
realloc function and then sets a panic function (lua_atpanic) that prints an
error message to the standard error output in case of fatal errors. Returns
the new state, or NULL if there is a memory allocation error.
The lua_newthread a new thread (or a new coroutine related to the main
coroutine), pushes it on the stack, and returns a pointer represents the new
coroutine. The returned coroutine shares with the main coroutine its global
environment, but has an independent execution stack. There is no explicit
function to close or to destroy a coroutine. Coroutines are subject to garbage
collection, like any Lua object.
---
int lua_resume(lua_State* co, lua_State* from, int nargs);
int lua_yield(lua_State* L, int nresults);
int lua_yieldk(lua_State* L, int nresults, lua_KContext ctx, lua_KFunction k);
---
The lua_resume function start or resume a coroutine. The parameter from
represents the coroutine that is resuming co. If there is no such coroutine,
this parameter can be NULL.
To start a coroutine, you push onto the stack the main function plus any
arguments; then you call lua_resume, with nargs being the number of arguments.
This call returns when the coroutine suspends or finishes its execution. When
it returns, the stack contains all values passed to lua_yield, or all values
returned by the main function. lua_resume return LUA_YIELD if the coroutine
yields, LUA_OK if the coroutine finishes its execution without errors, or an
error code in case of errors. In case of errors, the stack is not unwound, so
you can use the debug API over it. The error object is on the top of the stack.
To resume a coroutine, you remove any results from the last lua_yield, put on
its stack only the values to be passed as results from yield, and then call
lua_resume.
The lua_yield function is equivalent to lua_yieldk, but it has no continuation.
Therefore, when the thread resumes, it continues the function that called the
function calling lua_yield.
@nresults, number of results on the stack. These results will be returned to
the caller who called the lua_resume to resume this coroutine.
**********************************************************************/

struct lnlylib_env;
extern struct lnlylib_env* l_get_lnlylib_env();

L_EXTERN lua_State*
ll_new_state()
{
  lua_State* L = 0;
  struct lnlylib_env* E = 0;

  E = l_get_lnlylib_env();

  if (E == 0) {
    L = luaL_newstate();
  } else {
    L = lua_newstate(l_alloc_func, E);
  }

  if (L == 0) {
    l_loge_s(E, "create lua state failed");
    return 0;
  }

  return L;
}

L_EXTERN void
ll_close_state(lua_State* L)
{
  if (L) {
    lua_close(L);
  }
}

L_EXTERN lua_State*
ll_new_coro(lua_State* L)
{
  lua_State* co = 0;
  co = lua_newthread(L);
  if (co == 0) {
    l_loge_s(LNUL, "create lua coro failed");
    return 0;
  }
  return co;
}

L_EXTERN void*
ll_set_extra(lua_State* L, void* p)
{
  /** void* lua_getextraspace(lua_State* L) **
  Returns a pointer to a raw memory area associated with the given Lua
  state. The application can use this area for any purpose; Lua does
  not use it for anything. Each new thread has this area initialized
  with a copy of the area of the main thread. By default, this area
  has the size of a pointer to void, but you can recompile Lua with
  a different size for this area. (See LUA_EXTRASPACE in luaconf.h)
  ********************************************************************/
  l_uint* extra = (l_uint*)lua_getextraspace(L);
  l_uint oldval = *extra;
  *extra = (l_uint)p;
  return (void*)oldval;
}

L_EXTERN void*
ll_get_extra(lua_State* L)
{
  l_uint* extra = (l_uint*)lua_getextraspace(L);
  return (void*)*extra;
}

L_EXTERN void /* pos n emements */
ll_pop_n(lua_State* L, int n)
{
  int total = lua_gettop(L);
  if (n < total) {
    lua_pop(L, n);
  } else {
    lua_pop(L, total);
  }
}

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

L_EXTERN l_funcindex /* [-0, +1, e] */
ll_get_field_func(lua_State* L, l_tableindex t, const void* field)
{
  l_funcindex func;
  func.index = ll_get_field(L, t, field);
  return func;
}

static l_bool
ll_table_getn(lua_State* L, l_tableindex t, const void* name)
{
  l_sbuf1k name_buf;
  l_strbuf* key_names = 0;
  l_byte* key = 0;
  l_byte* key_end = 0;
  l_bool loop_exit = 0;
  int tableindex = t.index;
  int oldtop = lua_gettop(L);

  if (name == 0) {
    return false;
  }

  key_names = l_sbuf1k_init_from(&name_buf, l_strn_c(name));
  if (l_strbuf_is_empty(key_names)) {
    l_loge_1(LNUL, "chained name too long or empty %d", ld(strlen((const char*)name)));
    return false;
  }

  key = key_end = l_strbuf_cstr(key_names);

  for (; ;) {
    while (*key_end) {
      if (*key_end == '.') {
        break;
      } else {
        key_end += 1;
      }
    }

    /* key_end is \0 or '.' */

    loop_exit = (*key_end == 0);

    if (*key_end == '.') {
      *key_end = 0;
    }

    if (key == key_end) {
      l_loge_s(LNUL, "empty key name string");
      ll_pop_to(L, oldtop);
      return false;
    }

    if (!ll_is_table(L, tableindex)) {
      l_loge_1(LNUL, "access %s of a not-a-table", ls(key));
      ll_pop_to(L, oldtop);
      return false;
    }

    lua_getfield(L, tableindex, (const char*)key);
    tableindex = -1;

    if (loop_exit) {
      break;
    }

    key_end += 1;
    key = key_end;
  }

  if (lua_gettop(L) == oldtop + 1) {
    return true;
  } else {
    lua_copy(L, -1, oldtop + 1);
    ll_pop_to(L, oldtop + 1);
    return true;
  }
}

static l_bool /* if return true, the value is on the top */
ll_table_getv(lua_State* L, l_tableindex t, int n, va_list vl)
{
  const char* key = 0;
  int tableindex = t.index;
  int oldtop = lua_gettop(L);

  if (n <= 0) {
    return false;
  }

  while (n-- > 0) {
    key = va_arg(vl, const char*);
    if (key == 0) {
      l_loge_s(LNUL, "empty key name string");
      ll_pop_to(L, oldtop);
      return false;
    }
    if (!ll_is_table(L, tableindex)) {
      l_loge_1(LNUL, "access %s of a not-a-table", ls(key));
      ll_pop_to(L, oldtop);
      return false;
    }
    lua_getfield(L, tableindex, (const char*)key);
    tableindex = -1;
  }

  if (lua_gettop(L) == oldtop + 1) {
    return true;
  } else {
    lua_copy(L, -1, oldtop + 1);
    ll_pop_to(L, oldtop + 1);
    return true;
  }
}

L_EXTERN l_int
ll_table_get_int(lua_State* L, l_tableindex t, const void* namechain)
{
  if (ll_table_getn(L, t, namechain)) {
    return ll_to_int(L, -1);
  } else {
    return 0;
  }
}

L_EXTERN l_strn
ll_table_get_str(lua_State* L, l_tableindex t, const void* namechain)
{
  if (ll_table_getn(L, t, namechain)) {
    return ll_to_strn(L, -1);
  } else {
    return L_EMPTY_STR;
  }
}

L_EXTERN double
ll_table_get_num(lua_State* L, l_tableindex t, const void* namechain)
{
  if (ll_table_getn(L, t, namechain)) {
    return ll_to_num(L, -1);
  } else {
    return 0;
  }
}

L_EXTERN l_int
ll_table_get_intv(lua_State* L, l_tableindex t, int n, ...)
{
  l_int a = 0;
  va_list vl;
  va_start(vl, n);
  if (ll_table_getv(L, t, n, vl)) {
    a = ll_to_int(L, -1);
  }
  va_end(vl);
  return a;
}

L_EXTERN l_strn
ll_table_get_strv(lua_State* L, l_tableindex t, int n, ...)
{
  l_strn s = L_EMPTY_STR;
  va_list vl;
  va_start(vl, n);
  if (ll_table_getv(L, t, n, vl)) {
    s = ll_to_strn(L, -1);
  }
  va_end(vl);
  return s;
}

L_EXTERN double
ll_table_get_numv(lua_State* L, l_tableindex t, int n, ...)
{
  double f = 0;
  va_list vl;
  va_start(vl, n);
  if (ll_table_getv(L, t, n, vl)) {
    f = ll_to_num(L, -1);
  }
  va_end(vl);
  return f;
}

L_EXTERN void
ll_set_funcenv(lua_State* L, l_funcindex func, l_tableindex t)
{
  /** const char* lua_setupvalue(lua_State* L, int funcindex, int n); [-(0|1), +0, -]
      const char* lua_getupvalue(lua_State* L, int funcindex, int n); [-0, +(0|1), -]
  Sets the value of a closure's upvalue. It assigns the value at the top
  of the stack to the upvalue and returns its name. It also pops the
  value from the stack. Returns NULL (and pops nothing) when the index n
  is greater than the number of upvalues.
  Gets information about the n-th upvalue of the closure at func index.
  It pushes the upvalue's value onto the stack and returns its name.
  Returns NULL (and pushes nothing) when the index n is greater than the
  number of upvalues. For C functions, this function uses the empty string
  "" as a name for all upvalues. For Lua functions, upvalues are the
  external local variables that the function uses, and that are consequently
  included in its closure. Upvalues have no particular order, as they are
  active through the whole function. They are numbered in an arbitrary order.
  **/
  lua_pushvalue(L, t.index);
  if (lua_setupvalue(L, func.index, 1) == 0) {
    l_loge_s(LNUL, "set func env failed");
    ll_pop_n(L, 1);
  }
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
  return lua_type(L, stackindex);
}

L_EXTERN l_bool
ll_is_str(lua_State* L, int stackindex)
{
  return ll_type(L, stackindex) == LUA_TSTRING;
}

L_EXTERN l_bool
ll_is_int(lua_State* L, int stackindex)
{
  return lua_isinteger(L, stackindex);
}

L_EXTERN l_bool
ll_is_num(lua_State* L, int stackindex)
{
  return ll_type(L, stackindex) == LUA_TNUMBER;
}

L_EXTERN l_bool
ll_is_table(lua_State* L, int value_at)
{
  return ll_type(L, value_at) == LUA_TTABLE;
}

L_EXTERN l_bool
ll_is_func(lua_State* L, int value_at)
{
  return ll_type(L, value_at) == LUA_TFUNCTION;
}

L_EXTERN l_bool
ll_is_udata(lua_State* L, int value_at)
{
  return ll_type(L, value_at) == LUA_TUSERDATA;
}

L_EXTERN l_bool
ll_is_ldata(lua_State* L, int value_at)
{
  return ll_type(L, value_at) == LUA_TLIGHTUSERDATA;
}

L_EXTERN l_bool
ll_is_valid(lua_State* L, int stackindex)
{
  int n = ll_type(L, stackindex);
  return (n != LUA_TNIL && n != LUA_TNONE);
}

L_EXTERN l_bool
ll_nt_valid(lua_State* L, int stackindex)
{
  int n = ll_type(L, stackindex);
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

L_EXTERN l_int
ll_to_int(lua_State* L, int value_at)
{
  return (l_int)lua_tointeger(L, value_at);
}

L_EXTERN double
ll_to_num(lua_State* L, int value_at)
{
  return (double)lua_tonumber(L, value_at);
}

L_EXTERN const l_byte*
ll_to_cstr(lua_State* L, int stackindex)
{
  return ll_to_strn(L, stackindex).p;
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
    if (s->p && s->n > 0) {
      luaL_addlstring(&B, (const char*)s->p, (size_t)s->n);
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

/** dump functions and values **
string.dump(func [, strip]) ; return a string containing a binary representation of the given function
typedef int (*lua_Writer)(lua_State* L, const void* p, size_t sz, void* data);
int lua_dump(lua_State* L, lua_Writer writer, void* data, int strip); # this function pops nothing and also pushes nothing
---
lua_dump dumps a function as a binary chunk. The lua function is on the top of the stack, this lua function is not popped after lua_dump is called.
If loaded again (the compiled chunk), results in a function equivalent to the one dumped.
If strip is true, the binary representation may not include all debug information about the function, to save space.
---
lua_dump uses the writer function to produces the binary chunk out.
Every time it produces another piece of chunk, lua_dump calls the writer, passing along with the buffer and its size, and the data parameter.
The writer returns 0 means no errors, any other value means an error and stops lua_dump from calling the writer again.
The lua_dump return the error code returned from the writer function.
---
Functions with upvalues have only their number of upvalues saved.
When loaded, those upvalues receive fresh instances containing nil.
You can use the debug library to serialize and reload the upvalues of a function in a way adequate to your needs.
**********************************************************************/
static int
ll_string_writer_for_compile(lua_State* L, const void* p, size_t sz, void* data)
{
  luaL_Buffer* B = 0;
  L_UNUSED(L);
  B = (luaL_Buffer*)data;
  luaL_addlstring(B, p, sz);
  return 0;
}

static int
ll_file_writer_for_compile(lua_State* L, const void* p, size_t sz, void* data)
{
  l_int n = 0;
  l_file* out = 0;
  L_UNUSED(L);
  out = (l_file*)data;
  n = l_file_write(out, p, (l_int)sz);
  return n == (l_int)sz;
}

L_EXTERN const l_byte* /* return the stack index of the string of the compiled chunk */
ll_compile_to_str(lua_State* L, l_funcindex func)
{
  int n = 0;
  luaL_Buffer B;
  luaL_buffinit(L, &B);
  lua_pushvalue(L, func.index);
  n = lua_dump(L, ll_string_writer_for_compile, &B, true);
  if (n == 0) {
    luaL_pushresult(&B);
    return ll_to_cstr(L, -1);
  } else {
    return 0;
  }
}

L_EXTERN l_bool
ll_compile_to_file(lua_State* L, l_funcindex func, const void* file)
{
  int n = 0;
  l_file outfile;

  outfile = l_file_open_write(file);
  if (outfile.file == 0) {
    return false;
  }

  lua_pushvalue(L, func.index);
  n = lua_dump(L, ll_file_writer_for_compile, &outfile, true);
  l_file_close(&outfile);

  return n == 0;
}

/** load the code into a function **
load(chunk [, chunkname [, mode [, env]]]) ; chunk is a string or a function
typedef const char* (*lua_Reader)(lua_State* L, void* data, size_t* size);
int lua_load(lua_State* L, lua_Reader reader, void* data, const char* chunkname, const char* mode);
int luaL_loadstring(lua_State* L, const char* s);
int luaL_loadbufferx(lua_State* L, const char* buffer, size_t sz, const char* chunkname, const char* mode); luaL_loadbuffer is same but pass mode as 0
int luaL_loadfilex(lua_State* L, const char* filename, const char* mode); luaL_loadfile is same but pass mode as 0
---
The load function loads a Lua chunk without running it.
If there are no errors, lua_load pushes the compiled chunk as a Lua function on top of the stack,
Otherwise, it pushes an error message, and the return value can be:
LUA_OK (0) - no errors
LUA_ERRSYNTAX - syntax error during precompilation
LUA_ERRMEM - memory allocation (out-of-memory) error
LUA_ERRGCMM - error while running a __gc metamethod, this error has no relation with the chunk being loaded
---
For lua_loadfile, if the filename is NULL, then it loads from the stdin.
And the first line in the file is ignored if it starts with a #.
lua_loadfile returns the same results as lua_load, but it has an extra error code LUA_ERRFILE for file related errors,
e.g., it cannot open or read the file.
---
lua_load uses a user-specified reader function to read the code (the Lua chunk).
lua_load uses the stack internally, so the reader function must always leave the stack unmodified when returning.
The reader must return a pointer to a block of memory with a new piece of the chunk and set size to the block size.
The block must exist until the reader function is called again.
To signal the end of the chunk, the reader must return NULL or set size to 0.
The reader function may return pieces of any size greater than 0.
---
The chunkname gives a name to the chunk, which is used for error messages and in debug information.
The mode controls whether the chunk can be text or binary (a precompiled chunk).
It may be the string "b" (only binary chunks), "t" (only text chunks), or "bt" (both binary and text).
The default is "bt", and pass NULL also equal to the value "bt".
---
The chunk has the environment varialbe _ENV as its 1st upvalue, if there are more upvalues they are initialized with nil.
If you want to change the environment variable for the chunk, you can call lua_setupvalue to change the 1st upvalue.
**********************************************************************/

L_EXTERN l_funcindex /* load lua code into a function */
ll_load_code(lua_State* L, l_strn code)
{
  int n = 0;
  if (l_strn_is_empty(&code)) {
    return (l_funcindex){0};
  }
  n = luaL_loadbufferx(L, (const char*)code.p, (size_t)code.n, 0, 0);
  if (n == LUA_OK) {
    return (l_funcindex){lua_gettop(L)};
  } else {
    ll_pop_error(L);
    return (l_funcindex){0};
  }
}

L_EXTERN l_funcindex
ll_load_compiled_code(lua_State* L, l_strn code)
{
  int n = 0;
  if (l_strn_is_empty(&code)) {
    return (l_funcindex){0};
  }
  n = luaL_loadbufferx(L, (const char*)code.p, (size_t)code.n, 0, "b");
  if (n == LUA_OK) {
    return (l_funcindex){lua_gettop(L)};
  } else {
    ll_pop_error(L);
    return (l_funcindex){0};
  }
}

L_EXTERN l_funcindex /* load lua code from the file into a function */
ll_load_file(lua_State* L, const void* file)
{
  int n = 0;

  if (file == 0) { /* dont load from stdin */
    return (l_funcindex){0};
  }

  n = luaL_loadfilex(L, (const char*)file, 0);
  if (n == LUA_OK) {
    return (l_funcindex){lua_gettop(L)};
  } else {
    ll_pop_error(L);
    return (l_funcindex){0};
  }
}

L_EXTERN l_funcindex
ll_load_compiled_file(lua_State* L, const void* file)
{
  int n = 0;

  if (file == 0) { /* dont load from stdin */
    return (l_funcindex){0};
  }

  n = luaL_loadfilex(L, (const char*)file, "b");
  if (n == LUA_OK) {
    return (l_funcindex){lua_gettop(L)};
  } else {
    ll_pop_error(L);
    return (l_funcindex){0};
  }
}

/** call the lua function (a real lua function or a lua callable c function) **
void lua_call(lua_State* L, int nargs, int nresults);
void lua_callk(lua_State* L, int nargs, int nresults, lua_KContext ctx, lua_KFunction k);
int lua_pcall(lua_State* L, int nargs, int nresults, int msgh);
int lua_pcallk(lua_State* L, int nargs, int nresults, int msgh, lua_KContext ctx, lua_KFunction k);
lua_KContext is a type that can store a pointer in it;
typedef int (*lua_KFunction)(lua_State* L, int status, lua_KContext ctx);
typedef int (*lua_CFunction)(lua_State* L);
---
lua_call - return results on the stack or raise a error, and it is a no return value c function
lua_pcall - return results or a error object on the stack, and it is a c function returns an error code
---
A function is called with following protocol:
1. the function is pushed onto the stack
2. the arguments for the function are pushed in direct order, first argument pushed first
3. then you call lua_call/lua_pcall with the nargs the number of arguments you pushed onto the stack
4. all arguments and the function value are poped from the stack after the function is called
5. and then all the results are on the stack, starting at the original function position
6. the number of results is adjusted to nresults, unless nresults is LUA_MULTRET to get all results
7. lua takes care that the returned values fit into the stack space, but it does not ensure any extra spcae in stack
Notes about the C functions using lua stack
1. in the C function that used by lua code, i.e. a C function with the type lua_CFunction
   you can just gets the arguments from the stack, do your work with the stack, and push the function results,
   after the function is called by lua (via lua_call/lua_pcall or via lua code function call syntax func(a, b) for example,
   the results are moved to the starting position before the function is called, i.e. after called only results left on the stack
2. if a C function is just a normal C function operated the stack, not used for lua code, then
   you better keep the stack unchanged, i.e. before and after the function called, the stack's content is the same
   this kind of code is balanced, it is considered good programming practice
The arguments ctx and k, allowed the called function yield from c, i.e.
1. you can provide a continuation function k for the function call
2. if the coroutine that called the funciton is yield, and the coroutine is resumed again, the function k is called
lua_pcall calls the function in protected mode, i.e. if there is any error during the call,
1. lua_pcall catches it, pushed a single result value the error object on the stack, and return the error code
2. while the lua_call doesn't catch the error, the error just throwed and propagated upwards, and lua_call doesn't return error code (due to it throw the error)
3. if there are no errors during the call, lua_pcall and lua_call behaves exactly the same
4. and msgh the stack index of a message handler can be passed into lua_pcall, when the error happens,
   msgh is called by lua_pcall with the error object, and lua_pcall pushed the value returned by the handler into the stack as the error object
   typically, the message handler is used to add more debug information to the error object, such as a stack traceback.
   such information cannot be gathered after the return of lua_pcall, since by then the stack has unwound.
5. lua_pcall can return following error code:
   LUA_OK (0) - success
   LUA_ERRRUN - a runtime error
   LUA_ERRMEM - memory allocation error, for such errors, lua does not call the message handler
   LUA_ERRERR - error while running the message handler
   LUA_ERRGCMM - error while running a __gc metamethod, for such errors, lua doesn't call the message handler, as this error typically has no relation with the function being called
**********************************************************************/

L_EXTERN void /* the func and the args are already pushed on the stack, after call results are on the stack or raise a error */
ll_call_func(lua_State* L, l_funcindex func, int results)
{
  int args = lua_gettop(L) - func.index;
  lua_call(L, args, results);
}

L_EXTERN l_bool /* the func and the args are already pushed on the stack, after call results or a error object on the stack */
ll_pcall_func(lua_State* L, l_funcindex func, int results)
{
  int args = lua_gettop(L) - func.index;
  int n = lua_pcall(L, args, results, 0); /* [-(nargs+1), +(nresults|1), -] */
  if (n == LUA_OK) {
    return true;
  } else {
    ll_pop_error(L);
    return false;
  }
}

L_EXTERN l_bool
ll_pcall_with_msgh(lua_State* L, l_funcindex func, int results, l_funcindex msgh)
{
  int args = lua_gettop(L) - func.index;
  int n = lua_pcall(L, args, results, msgh.index);
  if (n == LUA_OK) {
    return true;
  } else {
    ll_pop_error(L);
    return false;
  }
}

L_EXTERN int /* load the file and run it, no args passed, return all results or nil */
ll_exec_file(lua_State* L, const void* file)
{
  l_funcindex func;
  func = ll_load_file(L, file);
  if (func.index <= 0) {
    lua_pushnil(L);
  } else {
    l_bool succ = false;
    succ = ll_pcall_func(L, func, LUA_MULTRET);
    if (!succ) {
      lua_pushnil(L);
    }
  }
  return lua_gettop(L);
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

#if 0
static l_funcindex
ll_dynlib_load(lua_State* L, l_strn lib, l_strn symbol)
{
  /* can use package.loadlib */
  L_UNUSED(L);
  L_UNUSED(lib);
  L_UNUSED(symbol);
  return (l_funcindex){0};
}

static int
ll_search_and_load_func(lua_State* L)
{
  int name_lookup = 0;
  l_strn main_module;
  l_tableindex package;
  int path;
  l_funcindex search_func;

  name_lookup = lua_gettop(L);
  package = ll_get_table(L, "package");
  path = ll_get_field(L, package, "path");
  search_func = ll_get_field_func(L, package, "searchpath");

  lua_pushvalue(L, name_lookup);
  lua_pushvalue(L, path);
  ll_call_func(L, search_func, 1); /* get 1 result: nil or the found file name */

  if (lua_isnil(L, -1)) {
    goto try_to_laod_from_c_library;
  } else {
    ll_load_file(L, ll_to_cstr(L, -1)); /* load the lua find */
    return 1; /* return loaded func on the top */
  }

try_to_load_from_c_library:

  ll_pop_to(L, search_func.index);
  path = ll_get_field(L, package, "cpath");

  lua_pushvalue(L, name_lookup);
  lua_pushvalue(L, path);
  ll_call_func(L, search_func, 1); /* get 1 result: nil or c library file name found */

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

  ll_pop_to(L, path);

  ll_push_str(L, main_module);
  lua_pushvalue(L, path);
  ll_call_func(L, search_func, 1); /* get 1 result: nil or c library file name found */

  if (lua_isnil(L, -1)) {
    return 1; /* return nil on the top */
  } else {
    l_filename f = ll_gen_luaopen_func_name(ll_to_strn(L, name_lookup));
    ll_dynlib_load(L, ll_to_strn(L, -1), l_filename_strn(&f));
    return 1; /* return loaded func on the top */
  }
}

L_EXTERN int /* return the stack index of the value */
ll_search_and_exec_file(lua_State* L, const void* file)
{
  luaL_requiref(L, (const char*)file, ll_search_and_load_func, false);
  return lua_gettop(L);
}
#endif
