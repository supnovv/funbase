#ifndef LNLYLIB_CORE_MATCH_H
#define LNLYLIB_CORE_MATCH_H
#include "core/base.h"

typedef struct l_stirng_pattern l_string_pattern;
L_EXTERN l_string_pattern* l_create_string_pattern(const l_strn* strn, l_int num_of_strings, l_bool case_insensitive);
L_EXTERN void l_destroy_string_pattern(l_string_pattern** patt);

L_EXTERN const l_byte* const l_string_too_short;
L_EXTERN const l_byte* l_string_match_x(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int* which_string, l_int* matched_len);
L_EXTERN const l_byte* l_string_match(const l_string_pattern* patt, const l_byte* start, const l_byte* pend);
L_EXTERN const l_byte* l_string_match_times(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int n); /* match exactly n times, return 0 or l_string_too_short for fail */
L_EXTERN const l_byte* l_string_match_repeat(const l_string_pattern* patt, const l_byte* start, const l_byte* pend); /* return start if no matched */
L_EXTERN const l_byte* l_skip_chars_until_match_the_pattern(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_byte** last_match_start); /* return 0 - too short to match, otherwise success */

#endif /* LNLYLIB_CORE_MATCH_H */

