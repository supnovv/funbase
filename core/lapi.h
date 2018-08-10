#ifndef LNLYLIB_CORE_LAPI_H
#define LNLYLIB_CORE_LAPI_H
#include "core/base.h"

typedef struct lua_State lua_State;

#define ll_pop_error(L) {\
  l_loge_1(LNUL, "lua error %s", ls(ll_to_cstr((L), -1))); \
  ll_pop_n((L), 1); }

typedef struct {
  int index;
} l_tableindex;

typedef struct {
  int index;
} l_funcindex;

L_EXTERN lua_State* ll_new_state();
L_EXTERN void ll_close_state(lua_State* L);
L_EXTERN lua_State* ll_new_coro(lua_State* L);
L_EXTERN void* ll_set_extra(lua_State* L, void* p);
L_EXTERN void* ll_get_extra(lua_State* L);

L_EXTERN void ll_pop_n(lua_State* L, int n);
L_EXTERN void ll_pop_to(lua_State* L, int pos);
L_EXTERN void ll_pop_beyond(lua_State* L, int pos);
L_EXTERN void ll_push_value(lua_State* L, int value_at);

L_EXTERN void ll_set_global(lua_State* L, const void* name, int value_at);
L_EXTERN void ll_set_field(lua_State* L, l_tableindex t, const void* field, int value_at);
L_EXTERN void ll_set_funcenv(lua_State* L, l_funcindex func, l_tableindex t);

L_EXTERN int ll_get_global(lua_State* L, const void* name); /* return the stack index of the value */
L_EXTERN int ll_get_field(lua_State* L, l_tableindex t, const void* field); /* return the stack index of the value */
L_EXTERN l_tableindex ll_get_table(lua_State* L, const void* name); /* get the global table */
L_EXTERN l_tableindex ll_get_field_table(lua_State* L, l_tableindex t, const void* field);
L_EXTERN l_funcindex ll_get_field_func(lua_State* L, l_tableindex t, const void* field);

L_EXTERN l_int ll_table_get_int(lua_State* L, l_tableindex t, const void* namechain); /* [-0, +[0|1], -] */
L_EXTERN double ll_table_get_num(lua_State* L, l_tableindex t, const void* namechain);
L_EXTERN l_strn ll_table_get_str(lua_State* L, l_tableindex t, const void* namechain);
L_EXTERN l_int ll_table_get_intv(lua_State* L, l_tableindex t, int n, ...); /* [-0, +[0|1], -] */
L_EXTERN double ll_table_get_numv(lua_State* L, l_tableindex t, int n, ...);
L_EXTERN l_strn ll_table_get_strv(lua_State* L, l_tableindex t, int n, ...);

L_EXTERN l_tableindex ll_new_table(lua_State* L);
L_EXTERN l_tableindex ll_new_table_with_size(lua_State* L, int nseq, int rest);

L_EXTERN int ll_type(lua_State* L, int value_at); /* return the lua value type */
L_EXTERN l_bool ll_is_str(lua_State* L, int value_at); /* the lua value is a string or not */
L_EXTERN l_bool ll_is_int(lua_State* L, int value_at);
L_EXTERN l_bool ll_is_num(lua_State* L, int value_at);
L_EXTERN l_bool ll_is_table(lua_State* L, int value_at);
L_EXTERN l_bool ll_is_func(lua_State* L, int value_at);
L_EXTERN l_bool ll_is_udata(lua_State* L, int value_at);
L_EXTERN l_bool ll_is_ldata(lua_State* L, int value_at); /* light userdata */
L_EXTERN l_bool ll_is_valid(lua_State* L, int value_at); /* is a valid lua value */
L_EXTERN l_bool ll_nt_valid(lua_State* L, int value_at); /* is not a valid lua value */
L_EXTERN l_int ll_to_int(lua_State* L, int value_at); /* if cannot convert to int, return 0 */

L_EXTERN double ll_to_num(lua_State* L, int value_at); /* if cannot convert to num, reutrn 0 */
L_EXTERN l_strn ll_to_strn(lua_State* L, int value_at); /* convert the lua value to strn */
L_EXTERN const l_byte* ll_to_cstr(lua_State* L, int value_at); /* convert the lua value to cstr */

L_EXTERN int ll_new_str_n(lua_State* L, int n, l_strn* s); /* return the string value stack index */
L_EXTERN int ll_new_str_1(lua_State* L, l_strn s1);
L_EXTERN int ll_new_str_2(lua_State* L, l_strn s1, l_strn s2);
L_EXTERN int ll_new_str_3(lua_State* L, l_strn s1, l_strn s2, l_strn s3);
L_EXTERN int ll_new_str_4(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4);
L_EXTERN int ll_new_str_5(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5);
L_EXTERN int ll_new_str_6(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6);
L_EXTERN int ll_new_str_7(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7);
L_EXTERN int ll_new_str_8(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8);
L_EXTERN int ll_new_str_9(lua_State* L, l_strn s1, l_strn s2, l_strn s3, l_strn s4, l_strn s5, l_strn s6, l_strn s7, l_strn s8, l_strn s9);

L_EXTERN int ll_get_path(lua_State* L); /* return the string value stack index */
L_EXTERN int ll_get_cpath(lua_State* L);
L_EXTERN void ll_add_path(lua_State* L, l_strn path);
L_EXTERN void ll_add_cpath(lua_State* L, l_strn path);

L_EXTERN const l_byte* ll_compile_to_str(lua_State* L, l_funcindex func); /* return the stack index of the string */
L_EXTERN l_bool ll_compile_to_file(lua_State* L, l_funcindex func, const void* file);

L_EXTERN l_funcindex ll_load_code(lua_State* L, l_strn code);
L_EXTERN l_funcindex ll_load_compiled_code(lua_State* L, l_strn code);
L_EXTERN l_funcindex ll_load_file(lua_State* L, const void* file);
L_EXTERN l_funcindex ll_load_compiled_file(lua_State* L, const void* file);

L_EXTERN void ll_call_func(lua_State* L, l_funcindex func, int results);
L_EXTERN l_bool ll_pcall_func(lua_State* L, l_funcindex func, int results);
L_EXTERN l_bool ll_pcall_with_msgh(lua_State* L, l_funcindex func, int results, l_funcindex msgh);

L_EXTERN int ll_exec_file(lua_State* L, const void* file); /* load the file and run it, no args passed, return all results */
L_EXTERN int ll_search_and_exec_file(lua_State* L, const void* file); /* search the file and run it, return the stack index of the value after execute */

#endif /* LNLYLIB_CORE_LAPI_H */

