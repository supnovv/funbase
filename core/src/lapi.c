#define LNLYLIB_API_IMPL
#include "core/lapi.h"

L_EXTERN void /* pos is not popped */
ll_pop_to(lua_State* L, l_stackindex pos)
{
  int n = lua_gettop(L) - pos.index;
  if (n > 0) {
    lua_pop(L, n);
  }
}

L_EXTERN void /* [-1, +1, e] */
ll_set_global(lua_State* L, const void* name, l_stackindex val)
{
  lua_pushvalue(L, val.index);
  lua_setglobal(L, (const char*)name);
}

L_EXTERN void /* [-1, +1, e] */
ll_set_field(lua_State* L, l_tableindex t, const void* field, l_stackindex val)
{
  lua_pushvalue(L, val.index);
  lua_setfield(L, t.index, (const char*)field);
}

L_EXTERN l_stackindex /* [-0, +1, e] */
ll_get_global(lua_State* L, const void* name)
{
  lua_getglobal(L, (const char*)name); /* it returns value type */
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
ll_get_table(lua_State* L, const void* name)
{
  l_tableindex table;
  table.sidx = ll_get_global(L, name);
  return table;
}

L_EXTERN l_stackindex /* [-0, +1, e] */
ll_get_field(lua_State* L, l_tableindex t, const void* field)
{
  lua_getfield(L, t.index, (const char*)field); /* it returns value type */
  return (l_stackindex){lua_gettop(L)};
}

L_EXTERN l_tableindex /* [-0, +1, e] */
ll_get_field_table(lua_State* L, l_tableindex t, const void* field)
{
  l_tableindex table;
  table.sidx = ll_get_field(L, t, field);
  return table;
}

L_EXTERN l_tableindex
ll_new_table(lua_State* L)
{
  lua_newtable(L);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_tableindex
ll_new_table_inited_size(lua_State* L, int nseq, int rest)
{
  lua_createtable(L, nseq > 0 ? nseq : 0, rest > 0 ? rest : 0);
  return (l_tableindex){lua_gettop(L)};
}

L_EXTERN l_funcindex
ll_load_expr(lua_State* L, l_strn expr)
{}

L_EXTERN l_funcindex
ll_load_file(lua_State* L, l_strn file)
{}

L_EXTERN l_funcindex
ll_search_and_load(lua_State* L, l_strn file)
{}

