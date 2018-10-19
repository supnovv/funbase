#ifndef LNLYLIB_CORE_MATCH_H
#define LNLYLIB_CORE_MATCH_H
#include "core/base.h"

typedef struct l_string_pattern l_string_pattern;
L_EXTERN l_string_pattern* l_create_string_pattern(const l_strn* strn, l_int num_of_strings); /* case insensitive */
L_EXTERN l_string_pattern* l_create_string_pattern_case_sensitive(const l_strn* strn, l_int num_of_strings);
L_EXTERN void l_destroy_string_pattern(l_string_pattern** patt);

static const l_byte* const l_string_too_short = (const l_byte* const)(l_uint)1;

L_EXTERN const l_byte* l_string_match_x(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int* which_string, l_int* matched_len);

L_EXTERN const l_byte* l_string_match(const l_string_pattern* patt, const l_byte* start, const l_byte* pend);

/* match exactly n times, return 0 or l_string_too_short for fail */
L_EXTERN const l_byte* l_string_match_times(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int n);

/* return start if no matched */
L_EXTERN const l_byte* l_string_match_repeat(const l_string_pattern* patt, const l_byte* start, const l_byte* pend);

/* return 0 - too short to match, otherwise success */
L_EXTERN const l_byte* l_skip_chars_until_match_the_pattern(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, const l_byte** last_match_start);

typedef struct {
  const l_byte* pend;
  l_int string_i;
  l_int match_len;
} l_match_result;

L_EXTERN l_match_result l_skip_space_and_match(const l_string_pattern* space_patt, const l_string_pattern* match_patt, const l_byte* start, const l_byte* pend);

L_EXTERN void l_string_match_test();

#endif /* LNLYLIB_CORE_MATCH_H */

