#define LNLYLIB_API_IMPL
#include <string.h>
#include "core/match.h"

static const l_byte
l_bit_1_count_g[256] = {
           /*0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111*/
  /*0000+0*/ 0,   1,   1,   2,   1,   2,   2,   3,   1,   2,   2,   3,   2,   3,   3,   4,
  /*0001+1*/ 1,   2,   2,   3,   2,   3,   3,   4,   2,   3,   3,   4,   3,   4,   4,   5,
  /*0010+1*/ 1,   2,   2,   3,   2,   3,   3,   4,   2,   3,   3,   4,   3,   4,   4,   5,
  /*0011+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*0100+1*/ 1,   2,   2,   3,   2,   3,   3,   4,   2,   3,   3,   4,   3,   4,   4,   5,
  /*0101+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*0110+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*0111+3*/ 3,   4,   4,   5,   4,   5,   5,   6,   4,   5,   5,   6,   5,   6,   6,   7,
  /*1000+1*/ 1,   2,   2,   3,   2,   3,   3,   4,   2,   3,   3,   4,   3,   4,   4,   5,
  /*1001+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*1010+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*1011+3*/ 3,   4,   4,   5,   4,   5,   5,   6,   4,   5,   5,   6,   5,   6,   6,   7,
  /*1100+2*/ 2,   3,   3,   4,   3,   4,   4,   5,   3,   4,   4,   5,   4,   5,   5,   6,
  /*1101+3*/ 3,   4,   4,   5,   4,   5,   5,   6,   4,   5,   5,   6,   5,   6,   6,   7,
  /*1110+3*/ 3,   4,   4,   5,   4,   5,   5,   6,   4,   5,   5,   6,   5,   6,   6,   7,
  /*1111+4*/ 4,   5,   5,   6,   5,   6,   6,   7,   5,   6,   6,   7,   6,   7,   7,   8
};

L_EXTERN l_byte
l_bit_1_count_of_byte(l_byte n)
{
  return l_bit_1_count_g[n];
}

L_EXTERN l_int /* x should > 0 and be 2^n, return n */
l_bit_pos_of_power_of_two(l_ulong x)
{
  l_int n = 0;
  if (x >> 32) {
    n += 32;
    x >>= 32;
  }
  if (x >> 16) {
    n += 16;
    x >>= 16;
  }
  if (x >> 8) {
    n += 8;
    x >>= 8;
  }
  if (x == 0) {
    return n;
  } else {
    return n + l_bit_1_count_g[x-1];
  }
}

typedef struct {
  l_byte bits[32];
  l_byte bit_1_count[32];
} l_char_match_dict;

typedef struct {
  l_ulong char_256_bits[4];
  l_ulong strs_match_the_range;
} l_match_range; /* 5-ulong 40-byte */

typedef struct l_char_slice {
  l_char_match_dict char_match_dict;
  struct l_char_slice* next;
  l_match_range* range;
  l_long num_ranges;
  l_ulong strs_match_any_char;
  l_ulong strs_end_at_cur_slice;
} l_char_slice; /* 13-ulong 104-byte */

typedef struct {
  l_char_slice head;
  l_byte strs_match_cur_char[8]; /* 1 ~ 8-byte */
} l_8_string_char_slice; /* 112-byte + 8 * 40-byte (max 0.5KB) */

typedef struct {
  l_char_slice head;
  l_ushort strs_match_cur_char[4]; /* 1 ~ 16-ushort, max 32-byte */
} l_16_string_char_slice; /* 136-byte + 16 * 40-byte (max 0.7KB) */

typedef struct {
  l_char_slice head;
  l_umedit strs_match_cur_char[2]; /* 1 ~ 32-umedit, max 128-byte */
} l_32_string_char_slice; /* 232-byte + 32 * 40-byte (max 1.5KB) */

typedef struct {
  l_char_slice head;
  l_ulong strs_match_cur_char[1]; /* 1 ~ 64-ulong, max 512-byte */
} l_64_string_char_slice; /* 616-byte + 64 * 40-byte (max 3KB) */

typedef struct l_string_pattern {
  l_short slice_type;
  l_short case_insensitive;
  l_medit num_slices;
  l_char_slice* slice;
} l_string_pattern;

typedef struct {
  l_smplnode node;
  l_int ch;
  l_ulong strs_match_the_char;
} l_char_node;

typedef struct {
  l_int struct_size;
  l_int range_offset;
  l_int chars;
  l_spriorq charq;
  l_char_node char_nodes[64];
  l_int num_ranges;
  l_match_range range[64];
  l_ulong strs_match_any_char;
  l_ulong strs_end_at_cur_slice;
} l_char_slice_info;

typedef struct {
  const l_byte* s;
  const l_byte* e;
  const l_byte* cur;
} l_string_info;

static l_string_pattern*
l_string_pattern_create_impl(l_char_slice_info* slice_arr, l_int num_slices, l_int slice_type, l_bool case_insensitive)
{
  l_string_pattern* pattern = 0;
  l_int total_size = sizeof(l_string_pattern);
  l_char_slice_info* slice = 0;
  l_match_range* range = 0;
  l_int range_i = 0;
  l_int i = 0;

  l_int char_i = 0;
  l_char_node* char_node = 0;

  l_char_slice* real_slice = 0;
  l_byte bit_1_count = 0;
  l_int byte_i = 0;
  l_int temp_i = 0;

  /* count the ranges and move all ranges together */
  for (slice = slice_arr; slice < slice_arr + num_slices; slice += 1) {
    slice->num_ranges = 0;
    for (range_i = 0; range_i < 64; range_i += 1) {
      range = slice->range + range_i;
      if (range->strs_match_the_range == 0) continue;
      /* check current range is the same range or not comparing to the ranges already have */
      for (temp_i = 0; temp_i < slice->num_ranges; ++temp_i) {
        if (slice->range[temp_i].char_256_bits[0] == range->char_256_bits[0] &&
            slice->range[temp_i].char_256_bits[1] == range->char_256_bits[1] &&
            slice->range[temp_i].char_256_bits[2] == range->char_256_bits[2] &&
            slice->range[temp_i].char_256_bits[3] == range->char_256_bits[3]) {
          slice->range[temp_i].strs_match_the_range |= range->strs_match_the_range;
          break;
        }
      }
      if (temp_i < slice->num_ranges) {
        continue;
      }
      /* this is a different range */
      if (slice->num_ranges != range_i) {
        slice->range[slice->num_ranges] = *range;
      }
      slice->num_ranges += 1;
    }
  }

  /* calculate the structure size */
  switch (slice_type) {
  case 0:
    for (slice = slice_arr; slice < slice_arr + num_slices; slice += 1) {
      slice->range_offset =  sizeof(l_8_string_char_slice);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 1:
    for (slice = slice_arr; slice < slice_arr + num_slices; slice += 1) {
      slice->range_offset = sizeof(l_16_string_char_slice);
      if (slice->chars > 4) slice->range_offset += (l_enlarge_to_times_of_4(slice->chars) - 4) * sizeof(l_ushort);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 2:
    for (slice = slice_arr; slice < slice_arr + num_slices; slice += 1) {
      slice->range_offset = sizeof(l_32_string_char_slice);
      if (slice->chars > 2) slice->range_offset += (l_enlarge_to_times_of_2(slice->chars) - 2) * sizeof(l_umedit);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 3:
    for (slice = slice_arr; slice < slice_arr + num_slices; slice += 1) {
      slice->range_offset =  sizeof(l_32_string_char_slice);
      if (slice->chars > 1) slice->range_offset += (slice->chars - 1) * sizeof(l_ulong);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  default:
    return 0;
  }

  /* alloc and build the structure */

  pattern = (l_string_pattern*)l_malloc(LNUL, total_size);
  l_zero_n(pattern, total_size);

  pattern->slice_type = slice_type;
  pattern->case_insensitive = case_insensitive;
  pattern->num_slices = num_slices;
  pattern->slice = (l_char_slice*)(pattern + 1);
  real_slice = pattern->slice;

  for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
    if (slice->num_ranges > 0) {
      real_slice->range = (l_match_range*)(((l_byte*)real_slice) + slice->range_offset);
      real_slice->num_ranges = slice->num_ranges;
      l_copy_n(real_slice->range, slice->range, sizeof(l_match_range) * slice->num_ranges);
    } else {
      real_slice->range = 0;
      real_slice->num_ranges = 0;
    }

    real_slice->strs_match_any_char = slice->strs_match_any_char;
    real_slice->strs_end_at_cur_slice = slice->strs_end_at_cur_slice;

    for (char_i = 0, char_node = (l_char_node*)l_spriorq_top(&slice->charq); char_i < slice->chars; char_i += 1, char_node = (l_char_node*)char_node->node.next) {
      real_slice->char_match_dict.bits[l_bit_belong_which_byte(char_node->ch)] |= l_bit_mask_of_byte(char_node->ch);
#ifdef LNLYLIB_DEBUG
      l_logv_7(LNUL, "slice %d char_index %d char %c bit_mask %d (%.2x) byte[%d] %.2x", ld(i), ld(char_i), lc(char_node->ch),
          ld(char_node->ch), lx(l_bit_mask_of_byte(char_node->ch)), ld(l_bit_belong_which_byte(char_node->ch)), lx(real_slice->char_match_dict.bits[l_bit_belong_which_byte(char_node->ch)]));
#endif
      switch (slice_type) {
      case 0:
        ((l_8_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_byte)(char_node->strs_match_the_char & 0xff);
        break;
      case 1:
        ((l_16_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_ushort)(char_node->strs_match_the_char & 0xffff);
        break;
      case 2:
        ((l_32_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_umedit)(char_node->strs_match_the_char & 0xffffffff);
        break;
      case 3:
        ((l_64_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= char_node->strs_match_the_char;
        break;
      }
    }

    l_assert(LNUL, char_node == 0);

    bit_1_count = 0;
    real_slice->char_match_dict.bit_1_count[0] = 0;

    for (byte_i = 1; byte_i < 32; ++byte_i) {
      bit_1_count += l_bit_1_count_g[real_slice->char_match_dict.bits[byte_i - 1]];
      real_slice->char_match_dict.bit_1_count[byte_i] = bit_1_count;
#ifdef LNLYLIB_DEBUG
      l_logv_2(LNUL, "slice %d - %d", ld(i), ld(bit_1_count));
#endif
    }

    if (i + 1 < num_slices) {
      real_slice->next = (l_char_slice*)(((l_byte*)real_slice) + slice->struct_size);
      real_slice = real_slice->next;
    } else {
      real_slice->next = 0;
      break;
    }
  }

  return pattern;
}

static l_bool
l_charq_less(l_smplnode* a, l_smplnode* b)
{
  return ((l_char_node*)a)->ch > ((l_char_node*)b)->ch;
}

static void
l_char_slice_info_init(l_char_slice_info* slice)
{
  l_spriorq_init(&slice->charq, l_charq_less);
}

static void
l_add_string_match_the_char(l_char_slice_info* slice, l_int string_i, l_int ch)
{
  l_char_node* node = 0;

  for (node = (l_char_node*)l_spriorq_top(&slice->charq); node; node = (l_char_node*)node->node.next) {
    if (node->ch == ch) {
      node->strs_match_the_char |= l_bit_mask_of_ulong(string_i);
      return;
    }
  }

  node = slice->char_nodes + slice->chars;
  slice->chars += 1;

  node->ch = ch;
  node->strs_match_the_char = l_bit_mask_of_ulong(string_i);
  l_spriorq_push(&slice->charq, &node->node);
}

static void
l_add_string_match_any_char(l_char_slice_info* slice, l_int string_i)
{
  slice->strs_match_any_char |= l_bit_mask_of_ulong(string_i);
}

static void
l_add_string_end_at_cur_slice(l_char_slice_info* slice, l_int string_i)
{
  slice->strs_end_at_cur_slice |= l_bit_mask_of_ulong(string_i);
}

static l_bool
l_add_string_match_the_range(l_char_slice_info* slice, l_int string_i, l_byte s, l_byte e, l_bool case_insensitive)
{
  if (s > e) {
    l_loge_4(LNUL, "invalid char range %c(%d) %c(%d)", lc(s), ld(s), lc(e), ld(e));
    return false;
  } else {
    l_match_range* range = slice->range + string_i;
    l_int i = s;
#ifdef LNLYLIB_DEBUG
    l_logv_4(LNUL, "add match char range %c(%d) %c(%d)", lc(s), ld(s), lc(e), ld(e));
#endif
    for (; i <= e; i += 1) {
      range->char_256_bits[l_bit_belong_which_ulong(i)] |= l_bit_mask_of_ulong(i);
      if (case_insensitive && l_is_letter(i)) {
        range->char_256_bits[l_bit_belong_which_ulong(l_flip_case(i))] |= l_bit_mask_of_ulong(l_flip_case(i));
      }
    }
    range->strs_match_the_range |= l_bit_mask_of_ulong(string_i);
    return true;
  }
}

static l_bool
l_add_string_match_char_class(l_char_slice_info* slice, l_int string_i, l_byte char_class, l_bool ci)
{
  /** character class **
  %{a} %[az[AZ]
  %{A} %|#00(A(Z(a(z#ff|
  %{d} %[09]
  %{D} %|#00(0(9#ff|
  %{x} %[09[AF[af]
  %{X} %|#00(0(9(A(F(a(f#ff|
  %{l} %[az]
  %{u} %[AZ]
  %{L} %|#00(a(z#ff|
  %{U} %|#00(A(Z#ff| */

  switch (char_class) {
  case 'a':
    l_add_string_match_the_range(slice, string_i, 'A', 'Z', ci);
    l_add_string_match_the_range(slice, string_i, 'a', 'z', ci);
    return true;
  case 'A':
    l_add_string_match_the_range(slice, string_i, 0, 'A' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'Z' + 1, 'a' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'z' + 1, 0xff, ci);
    return true;
  case 'd':
    l_add_string_match_the_range(slice, string_i, '0', '9', ci);
    return true;
  case 'D':
    l_add_string_match_the_range(slice, string_i, 0, '0' - 1, ci);
    l_add_string_match_the_range(slice, string_i, '9' + 1, 0xff, ci);
    return true;
  case 'x':
    l_add_string_match_the_range(slice, string_i, '0', '9', ci);
    l_add_string_match_the_range(slice, string_i, 'A', 'F', ci);
    l_add_string_match_the_range(slice, string_i, 'a', 'f', ci);
    return true;
  case 'X':
    l_add_string_match_the_range(slice, string_i, 0, '0' - 1, ci);
    l_add_string_match_the_range(slice, string_i, '9' + 1, 'A' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'F' + 1, 'a' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'f' + 1, 0xff, ci);
    return true;
  case 'l':
    l_add_string_match_the_range(slice, string_i, 'a', 'z', ci);
    return true;
  case 'L':
    l_add_string_match_the_range(slice, string_i, 0, 'a' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'z' + 1, 0xff, ci);
    return true;
  case 'u':
    l_add_string_match_the_range(slice, string_i, 'A', 'Z', ci);
    return true;
  case 'U':
    l_add_string_match_the_range(slice, string_i, 0, 'A' - 1, ci);
    l_add_string_match_the_range(slice, string_i, 'Z' + 1, 0xff, ci);
    return true;
  default:
    l_loge_1(LNUL, "invalid char class %c", lc(char_class));
    return false;
  }
}

L_EXTERN void
l_destroy_string_pattern(l_string_pattern** patt)
{
  if (*patt) {
    l_mfree(LNUL, *patt);
    *patt = 0;
  }
}

L_EXTERN l_string_pattern*
l_create_string_pattern(const l_strn* strn, l_int num_of_strings, l_bool case_insensitive)
{
  if (strn == 0 || num_of_strings <= 0 || num_of_strings > 64) {
    return 0;
  }{

  l_string_info str_info[num_of_strings];
  l_string_info* s = 0;
  l_int longest_string_length = 0;
  l_int i = 0;

  for (i = 0; i < num_of_strings; ++i) {
    str_info[i].s = strn[i].p;
    str_info[i].e = strn[i].p + strn[i].n;
    str_info[i].cur = strn[i].p;
    if (longest_string_length < strn[i].n) {
      longest_string_length = strn[i].n;
    }
  }{

  l_char_slice_info slice_info[longest_string_length];
  l_char_slice_info* slice = 0;
  l_int handled_chars = 0;
  l_int is_range_begin = 0;
  l_int range_start_ch = 0;
  l_int ch, end_ch;

  l_zero_n(slice_info, sizeof(l_char_slice_info) * longest_string_length);
  slice = slice_info;

  for (; ;) {
    handled_chars = 0;
    l_char_slice_info_init(slice);
    for (i = 0; i < num_of_strings; ++i) {
      s = str_info + i;
      if (s->cur >= s->e) {
        continue;
      }

      handled_chars += 1;
      ch = *s->cur;

      /* %<non-%> */
      if (ch != '%') {
        if (case_insensitive) {
          l_add_string_match_the_char(slice, i, l_to_lower(ch));
        } else {
          l_add_string_match_the_char(slice, i, ch);
        }
        s->cur += 1;
        if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
        continue;
      }

      /* %<end> */
      if (s->cur + 1 == s->e) {
        l_add_string_match_the_char(slice, i, '%');
        s->cur += 1;
        l_add_string_end_at_cur_slice(slice, i);
        continue;
      }

      s->cur += 1;
      ch = *s->cur;

      /* %% */
      if (ch == '%') {
        l_add_string_match_the_char(slice, i, '%');
        s->cur += 1;
        if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
        continue;
      }

      /* %. */
      if (ch == '.') {
        l_add_string_match_any_char(slice, i);
        s->cur += 1;
        if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
        continue;
      }

      /* %00 to %ff */
      if (l_is_hex_digit(ch)) {
        if (s->e - s->cur < 2 || l_nt_hex_digit(*(s->cur + 1))) {
          l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur-1));
          return 0;
        }
        ch = l_hex_string_to_int(l_strn_l(s->cur, 2));
        if (case_insensitive) {
          l_add_string_match_the_char(slice, i, l_to_lower(ch));
        } else {
          l_add_string_match_the_char(slice, i, ch);
        }
        s->cur += 2;
        if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
        continue;
      }

      /* %|[a#7f| */
      if (ch == '|') {
        is_range_begin = true;
        s->cur += 1;
  check_range_boundary:
        ch = *s->cur;
        if (s->e - s->cur < 3 || (ch != '[' && ch != '(' && ch != '#' && ch != '{')) {
          l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
          return 0;
        }
        if (ch == '[') {
          end_ch = *(s->cur + 2);
          if (end_ch != '|' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{') {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (is_range_begin) {
            range_start_ch = *(s->cur + 1);
            is_range_begin = false;
          } else {
            if (!l_add_string_match_the_range(slice, i, range_start_ch, *(s->cur + 1), case_insensitive)) {
              return 0;
            }
            is_range_begin = true;
          }
          if (end_ch == '|') {
            s->cur += 3;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 2;
            goto check_range_boundary;
          }
        } else if (ch == '(') {
          end_ch = *(s->cur + 2);
          if (end_ch != '|' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{') {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (is_range_begin) {
            range_start_ch = *(s->cur + 1) + 1;
            is_range_begin = false;
          } else {
            if (!l_add_string_match_the_range(slice, i, range_start_ch, *(s->cur + 1) - 1, case_insensitive)) {
              return 0;
            }
            is_range_begin = true;
          }
          if (end_ch == '|') {
            s->cur += 3;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 2;
            goto check_range_boundary;
          }
        } else if (ch == '{') {
          end_ch = *(s->cur + 2);
          if (end_ch != '|' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{') {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (!l_add_string_match_char_class(slice, i, *(s->cur + 1), case_insensitive)) {
            return 0;
          }
          if (end_ch == '|') {
            s->cur += 3;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 2;
            goto check_range_boundary;
          }
        } else if (ch == '#') {
          end_ch = *(s->cur + 3);
          if (s->e - s->cur < 4 || (end_ch != '|' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{')) {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (is_range_begin) {
            range_start_ch = l_hex_string_to_int(l_strn_l(s->cur + 1, 2));
            is_range_begin = false;
          } else {
            if (!l_add_string_match_the_range(slice, i, range_start_ch, l_hex_string_to_int(l_strn_l(s->cur + 1, 2)), case_insensitive)) {
              return 0;
            }
            is_range_begin = true;
          }
          if (end_ch == '|') {
            s->cur += 4;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 3;
            goto check_range_boundary;
          }
        } else {
          l_loge_s(LNUL, "should not ge to here");
          return 0;
        }
      }

      /* %[az[AZ] */
      if (ch == '[' || ch == '(' || ch == '#' || ch == '{') {
  check_ranges:
        ch = *s->cur;
        if (ch == '[') {
          end_ch = *(s->cur + 3);
          if (s->e - s->cur < 4 || (end_ch != ']' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{')) {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur-1));
            return 0;
          }
          if (!l_add_string_match_the_range(slice, i, *(s->cur + 1), *(s->cur + 2), case_insensitive)) {
            return 0;
          }
          if (end_ch == ']') {
            s->cur += 4;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 3;
            goto check_ranges;
          }
        } else if (ch == '(') {
          end_ch = *(s->cur + 3);
          if (s->e - s->cur < 4 || (end_ch != ')' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{')) {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (!l_add_string_match_the_range(slice, i, *(s->cur + 1) + 1, *(s->cur + 2) - 1, case_insensitive)) {
            return 0;
          }
          if (end_ch == ')') {
            s->cur += 4;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 3;
            goto check_ranges;
          }
        } else if (ch == '#') {
          end_ch = *(s->cur + 5);
          if (s->e - s->cur < 6 || (end_ch != '|' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{')) {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (!l_add_string_match_the_range(slice, i, l_hex_string_to_int(l_strn_l(s->cur + 1, 2)), l_hex_string_to_int(l_strn_l(s->cur + 3, 2)), case_insensitive)) {
            return 0;
          }
          if (end_ch == '|') {
            s->cur += 6;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 5;
            goto check_ranges;
          }
        } else if (ch == '{') {
          end_ch = *(s->cur + 2);
          if (s->e - s->cur < 3 || (end_ch != '}' && end_ch != '[' && end_ch != '(' && end_ch != '#' && end_ch != '{')) {
            l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur));
            return 0;
          }
          if (!l_add_string_match_char_class(slice, i, *(s->cur + 1), case_insensitive)) {
            return 0;
          }
          if (end_ch == '}') {
            s->cur += 3;
            if (s->cur == s->e) l_add_string_end_at_cur_slice(slice, i);
            continue;
          } else {
            s->cur += 2;
            goto check_ranges;
          }
        } else {
          l_loge_s(LNUL, "should not go to here");
          return 0;
        }
      }

      l_loge_1(LNUL, "wrong pattern format %s", ls(s->cur-1));
      return 0;
    }

    if (handled_chars == 0) {
      break;
    }

    slice += 1;
  }{

  l_int num_slices = 0;
  l_int slice_type = 0;
  num_slices = slice - slice_info;
  if (num_of_strings <= 8) {
    slice_type = 0;
  } else if (num_of_strings <= 16) {
    slice_type = 1;
  } else if (num_of_strings <= 32) {
    slice_type = 2;
  } else if (num_of_strings <= 64) {
    slice_type = 3;
  } else {
    l_loge_s(LNUL, "shall not go to here");
    return 0;
  }
  return l_string_pattern_create_impl(slice_info, num_slices, slice_type, case_insensitive);
}}}}

static l_ulong
l_match_current_slice(const l_char_slice* slice, l_int slice_type, l_int ch)
{
  l_int byte_i = l_bit_belong_which_byte(ch);
  const l_char_match_dict* dict = &slice->char_match_dict;
  l_byte moved_belong_byte = dict->bits[byte_i] << l_bit_of_byte_need_moved_bits_to_high(ch); /* shall be l_byte, dont change it */
  l_ulong matched_strings = 0;

  if (moved_belong_byte & 0x80) {
    l_int bit_1_count = dict->bit_1_count[byte_i] + l_bit_1_count_g[moved_belong_byte];
    switch (slice_type) {
    case 0:
      matched_strings = ((l_8_string_char_slice*)slice)->strs_match_cur_char[bit_1_count - 1];
      break;
    case 1:
      matched_strings = ((l_16_string_char_slice*)slice)->strs_match_cur_char[bit_1_count - 1];
      break;
    case 2:
      matched_strings = ((l_32_string_char_slice*)slice)->strs_match_cur_char[bit_1_count - 1];
      break;
    case 3:
      matched_strings = ((l_64_string_char_slice*)slice)->strs_match_cur_char[bit_1_count - 1];
      break;
    default:
      l_loge_1(LNUL, "invalid char slice type %d", ld(slice_type));
      break;
    }
  }

  matched_strings |= slice->strs_match_any_char;

  if (slice->range) {
    l_match_range* range = slice->range;
    for (; range < slice->range + slice->num_ranges; range += 1) {
      if (range->char_256_bits[l_bit_belong_which_ulong(ch)] & l_bit_mask_of_ulong(ch)) {
        matched_strings |= range->strs_match_the_range;
      }
    }
  }

  return matched_strings;
}

L_EXTERN const l_byte*
l_string_match_x(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int* which_string, l_int* matched_len)
{
  l_ulong prevmatch = L_MAX_INT_UL;
  l_ulong curmatch = 0;
  l_ulong first_match = 0;
  l_ulong matches = 0; /* all successfully matched strings currently */

  l_char_slice* slice = 0;
  l_int slice_type = patt->slice_type;
  l_int ci = patt->case_insensitive;
  const l_byte* match_end = 0;

  l_int len_to_match = pend - start;
  const l_byte* cur_char = start;
  l_int ch = 0;
  l_int i = 0;

  if (len_to_match <= 0) {
    return l_string_too_short;
  }

  if (patt->num_slices < len_to_match) {
    len_to_match = patt->num_slices;
  }

  slice = patt->slice;
  for (; i < len_to_match; i += 1) {
    ch = *cur_char++;
    curmatch = prevmatch & l_match_current_slice(slice, slice_type, ci ? l_to_lower(ch) : ch);
    if (curmatch == 0) { /* nobody continously matched success to current char */
      if (match_end) { /* but there are previously matched strings */
        first_match = l_lower_most_bit(matches); /* let the first one match */
        goto match_success;
      } else { /* oops, no one success previously, return 0 indicates failed */
        return 0;
      }
    }

    if (curmatch & slice->strs_end_at_cur_slice) { /* some strings matched success */
      match_end = cur_char;
      matches = curmatch & slice->strs_end_at_cur_slice; /* please keep matches inside this if, dont move it */
      first_match = l_lower_most_bit(curmatch);
      if (first_match & matches) { /* if the first_match matches, let success immediately */
        goto match_success;
      }
    }

    prevmatch = curmatch;
    slice = slice->next;
  }

  /* somebody continously matched to the last char, but the match is not end yet */

  if (match_end) { /* luckily, there are some matched success previously */
    first_match = l_lower_most_bit(matches); /* choose the first one match */
    goto match_success;
  }

  return l_string_too_short; /* oops, no one success, report the matching string is too short */

match_success:
  if (which_string) *which_string = l_bit_pos_of_power_of_two(first_match);
  if (matched_len) *matched_len = match_end - start;
  return match_end;
}

L_EXTERN const l_byte*
l_string_match(const l_string_pattern* patt, const l_byte* start, const l_byte* pend)
{
  return l_string_match_x(patt, start, pend, 0, 0);
}

L_EXTERN const l_byte* /* match exactly n times, return 0 or l_string_too_short for fail */
l_string_match_times(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int n)
{
  l_int i = 0;
  const l_byte* e = start;
  while (i++ < n) {
    if ((e = l_string_match(patt, start, pend)) > l_string_too_short) {
      start = e;
    } else {
      break;
    }
  }
  return e;
}

L_EXTERN const l_byte* /* return start if no matched */
l_string_match_repeat(const l_string_pattern* patt, const l_byte* start, const l_byte* pend)
{
  const l_byte* e = 0;
  while ((e = l_string_match(patt, start, pend)) > l_string_too_short) {
    start = e;
  }
  return start;
}

L_EXTERN const l_byte* /* return 0 - too short to match, otherwise success */
l_skip_chars_until_match_the_pattern(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, const l_byte** last_match_start)
{
  const l_byte* e = 0;
  while ((e = l_string_match(patt, start, pend)) == 0) { /* continue if unmatch */
    ++start;
  }
  if (last_match_start) *last_match_start = start;
  return e == l_string_too_short ? 0 : e;
}

static l_bool
l_char_match_dict_is_bit_set(const l_char_match_dict* dict, l_byte ch)
{
  return (dict->bits[l_bit_belong_which_byte(ch)] & l_bit_mask_of_byte(ch)) != 0;
}

static l_bool
l_match_range_is_bit_set(const l_match_range* range, l_byte ch)
{
  return (range->char_256_bits[l_bit_belong_which_ulong(ch)] & l_bit_mask_of_ulong(ch)) != 0;
}

static l_byte
l_char_match_dict_bit_1_count(const l_char_match_dict* dict, l_byte ch)
{
  l_int byte_i = l_bit_belong_which_byte(ch);
  l_byte moved_belong_byte = dict->bits[byte_i] << l_bit_of_byte_need_moved_bits_to_high(ch); /* shall be l_byte, dont change it */
#ifdef LNLYLIB_DEBUG
  l_logv_7(LNUL, "'%c' %d byte[%d] mask %.2x byte_value %.2x moved_byte %.2x prev_bit_1_count %d",
      lc(ch), ld(ch), ld(byte_i), lx(l_bit_mask_of_byte(ch)), lx(dict->bits[byte_i]), lx(moved_belong_byte), ld(dict->bit_1_count[byte_i]));
#endif
  return dict->bit_1_count[byte_i] + l_bit_1_count_g[moved_belong_byte];
}

L_EXTERN void
l_string_match_test()
{
  { const l_strn http_methods[] = {
        l_literal_strn("GET"),
        l_literal_strn("HEAD"),
        l_literal_strn("POST")
      };
    l_string_pattern* patt = l_create_string_pattern(http_methods, 3, true);

    l_assert(LNUL, patt->slice_type == 0);
    l_assert(LNUL, patt->case_insensitive == true);
    l_assert(LNUL, patt->num_slices == 4);

    { l_8_string_char_slice* slice_1 = (l_8_string_char_slice*)patt->slice;
      l_8_string_char_slice* slice_2 = 0;
      l_8_string_char_slice* slice_3 = 0;
      l_8_string_char_slice* slice_4 = 0;

      l_assert(LNUL, slice_1->head.next != 0);
      l_assert(LNUL, slice_1->head.range == 0);
      l_assert(LNUL, slice_1->head.num_ranges == 0);
      l_assert(LNUL, slice_1->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_1->head.strs_end_at_cur_slice == 0);

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'g') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'h') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'p') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'G') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'H') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'P') == false);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'g') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'h') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'p') == 3);

      l_assert(LNUL, slice_1->strs_match_cur_char[0] == (1 << 0));
      l_assert(LNUL, slice_1->strs_match_cur_char[1] == (1 << 1));
      l_assert(LNUL, slice_1->strs_match_cur_char[2] == (1 << 2));

      slice_2 = (l_8_string_char_slice*)slice_1->head.next;

      l_assert(LNUL, slice_2->head.next != 0);
      l_assert(LNUL, slice_2->head.range == 0);
      l_assert(LNUL, slice_2->head.num_ranges == 0);
      l_assert(LNUL, slice_2->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_2->head.strs_end_at_cur_slice == 0);

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'e') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'o') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'E') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'O') == false);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_2->head.char_match_dict, 'e') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_2->head.char_match_dict, 'o') == 2);

      l_assert(LNUL, slice_2->strs_match_cur_char[0] == ((1 << 0) | (1 << 1)));
      l_assert(LNUL, slice_2->strs_match_cur_char[1] == (1 << 2));

      slice_3 = (l_8_string_char_slice*)slice_2->head.next;

      l_assert(LNUL, slice_3->head.next != 0);
      l_assert(LNUL, slice_3->head.range == 0);
      l_assert(LNUL, slice_3->head.num_ranges == 0);
      l_assert(LNUL, slice_3->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_3->head.strs_end_at_cur_slice == (1 << 0));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'a') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 's') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 't') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'A') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'S') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'T') == false);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_3->head.char_match_dict, 'a') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_3->head.char_match_dict, 's') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_3->head.char_match_dict, 't') == 3);

      l_assert(LNUL, slice_3->strs_match_cur_char[0] == (1 << 1));
      l_assert(LNUL, slice_3->strs_match_cur_char[1] == (1 << 2));
      l_assert(LNUL, slice_3->strs_match_cur_char[2] == (1 << 0));

      slice_4 = (l_8_string_char_slice*)slice_3->head.next;

      l_assert(LNUL, slice_4->head.next == 0);
      l_assert(LNUL, slice_4->head.range == 0);
      l_assert(LNUL, slice_4->head.num_ranges == 0);
      l_assert(LNUL, slice_4->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_4->head.strs_end_at_cur_slice == ((1 << 1) | (1 << 2)));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'd') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 't') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'D') == false);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'T') == false);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 'd') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 't') == 2);

      l_assert(LNUL, slice_4->strs_match_cur_char[0] == (1 << 1));
      l_assert(LNUL, slice_4->strs_match_cur_char[1] == (1 << 2));

      { const l_strp matching_string = l_literal_strp("gEt.HEAd.pOSt.ge.pos");
        const l_byte* pend = 0;
        l_int which_string = 0;
        l_int matched_len = 0;

        pend = l_string_match_x(patt, matching_string.s, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, *pend == '.');
        l_assert(LNUL, pend - matching_string.s == 3);
        l_assert(LNUL, which_string == 0);
        l_assert(LNUL, matched_len == 3);

        pend = l_string_match_x(patt, pend, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, pend == 0);

        pend = l_string_match_x(patt, matching_string.s + 4, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, *pend == '.');
        l_assert(LNUL, which_string == 1);
        l_assert(LNUL, matched_len == 4);

        pend = l_string_match_x(patt, pend, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, pend == 0);

        pend = l_string_match_x(patt, matching_string.s + 9, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, *pend == '.');
        l_assert(LNUL, which_string == 2);
        l_assert(LNUL, matched_len == 4);

        pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, pend == 0);

        pend = l_string_match_x(patt, matching_string.s + 17, matching_string.e, &which_string, &matched_len);
        l_assert(LNUL, pend == l_string_too_short);
      }
    }

    l_destroy_string_pattern(&patt);
  }

  { const l_strn range_match_test[] = {
        l_literal_strn("man"), /* the string mankind can never be matched, because if mankind match, man also match, it is man always match due to it is ahead of mankind. */
        l_literal_strn("mankind"), /* so if you want mankind can be matched, you need move the mankind up. */
        l_literal_strn("hi%.%[az]%[az]%[az]"),
        l_literal_strn("forget"),
        l_literal_strn("for"),
        l_literal_strn("a%.%(04)%[az]%#2140|"),
        l_literal_strn("bird%[09[az[AZ]"),
        l_literal_strn("card"),
        l_literal_strn("dar%[az]%[az]ess"),
        l_literal_strn("earthquakes")
      };

    /* slice_1  slice_2  slice_3  slice_4  slice_5  slice_6  slice_7  slice_8  slice_9  slice_10 slice_11
       m        a        n<e>
       m        a        n        k        i        n        d<e>
       h        i        %.       %[az]    %[az]    %[az]<e>
       f        o        r        g        e        t<e>
       f        o        r<e>
       a        %.       %(04)    %[az]    %#2140<e>
       b        i        r        d        %[09[az[AZ]<e>
       c        a        r        d<e>
       d        a        r        %[az]    %[az]    e        s        s<e>
       e        a        r        t        h        q        u        a        k        e        s */

    l_string_pattern* patt = l_create_string_pattern(range_match_test, 10, false);

    l_assert(LNUL, patt->slice_type == 1);
    l_assert(LNUL, patt->case_insensitive == false);
    l_assert(LNUL, patt->num_slices == 11);

    { l_16_string_char_slice* slice_1 = (l_16_string_char_slice*)patt->slice;
      l_16_string_char_slice* slice_2 = 0;
      l_16_string_char_slice* slice_3 = 0;
      l_16_string_char_slice* slice_4 = 0;
      l_16_string_char_slice* slice_5 = 0;
      l_16_string_char_slice* slice_6 = 0;
      l_16_string_char_slice* slice_7 = 0;
      l_16_string_char_slice* slice_8 = 0;
      l_16_string_char_slice* slice_9 = 0;
      l_16_string_char_slice* slice_10 = 0;
      l_16_string_char_slice* slice_11 = 0;

      /* slice_1 - a(5) b(6) c(7) d(8) e(9) f(3,4) h(2) m(0,1)
         m 0
         m 1
         h 2
         f 3
         f 4
         a 5
         b 6
         c 7
         d 8
         e 9 */

      l_assert(LNUL, slice_1->head.next != 0);
      l_assert(LNUL, slice_1->head.range == 0);
      l_assert(LNUL, slice_1->head.num_ranges == 0);
      l_assert(LNUL, slice_1->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_1->head.strs_end_at_cur_slice == 0);

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'a') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'b') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'c') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'd') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'e') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'f') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'h') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_1->head.char_match_dict, 'm') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'a') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'b') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'c') == 3);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'd') == 4);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'e') == 5);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'f') == 6);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'h') == 7);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_1->head.char_match_dict, 'm') == 8);

      l_assert(LNUL, slice_1->strs_match_cur_char[0] == (1 << 5));
      l_assert(LNUL, slice_1->strs_match_cur_char[1] == (1 << 6));
      l_assert(LNUL, slice_1->strs_match_cur_char[2] == (1 << 7));
      l_assert(LNUL, slice_1->strs_match_cur_char[3] == (1 << 8));
      l_assert(LNUL, slice_1->strs_match_cur_char[4] == (1 << 9));
      l_assert(LNUL, slice_1->strs_match_cur_char[5] == ((1 << 3) | (1 << 4)));
      l_assert(LNUL, slice_1->strs_match_cur_char[6] == (1 << 2));
      l_assert(LNUL, slice_1->strs_match_cur_char[7] == ((1 << 0) | (1 << 1)));

      /* slice_2 - a(0,1,7,8,9) i(2,6) o(3,4)
         a  0
         a  1
         i  2
         o  3
         o  4
         %. 5
         i  6
         a  7
         a  8
         a  9 */

      slice_2 = (l_16_string_char_slice*)slice_1->head.next;

      l_assert(LNUL, slice_2->head.next != 0);
      l_assert(LNUL, slice_2->head.range == 0);
      l_assert(LNUL, slice_2->head.num_ranges == 0);
      l_assert(LNUL, slice_2->head.strs_match_any_char == (1 << 5));
      l_assert(LNUL, slice_2->head.strs_end_at_cur_slice == 0);

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'a') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'i') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_2->head.char_match_dict, 'o') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_2->head.char_match_dict, 'a') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_2->head.char_match_dict, 'i') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_2->head.char_match_dict, 'o') == 3);

      l_assert(LNUL, slice_2->strs_match_cur_char[0] == ((1 << 0) | (1 << 1) | (1 << 7) | (1 << 8) | (1 << 9)));
      l_assert(LNUL, slice_2->strs_match_cur_char[1] == ((1 << 2) | (1 << 6)));
      l_assert(LNUL, slice_2->strs_match_cur_char[2] == ((1 << 3) | (1 << 4)));

      /* slice_3 - n(0,1) r(3,4,6,7,8,9)
         n<e>   0
         n      1
         %.     2
         r      3
         r<e>   4
         %(04)  5
         r      6
         r      7
         r      8
         r      9 */

      slice_3 = (l_16_string_char_slice*)slice_2->head.next;

      l_assert(LNUL, slice_3->head.next != 0);
      l_assert(LNUL, slice_3->head.range != 0);
      l_assert(LNUL, slice_3->head.num_ranges == 1);
      l_assert(LNUL, slice_3->head.strs_match_any_char == (1 << 2));
      l_assert(LNUL, slice_3->head.strs_end_at_cur_slice == ((1 << 0) | (1 << 4)));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'n') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_3->head.char_match_dict, 'r') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_3->head.char_match_dict, 'n') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_3->head.char_match_dict, 'r') == 2);

      l_assert(LNUL, slice_3->strs_match_cur_char[0] == ((1 << 0) | (1 << 1)));
      l_assert(LNUL, slice_3->strs_match_cur_char[1] == ((1 << 3) | (1 << 4) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9)));

      l_assert(LNUL, slice_3->head.range[0].strs_match_the_range == (1 << 5));
      l_assert(LNUL, l_match_range_is_bit_set(slice_3->head.range + 0, '0') == false);
      l_assert(LNUL, l_match_range_is_bit_set(slice_3->head.range + 0, '1') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_3->head.range + 0, '2') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_3->head.range + 0, '3') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_3->head.range + 0, '4') == false);

      /* slice_4 - d(6,7) g(3) k(1) t(9)
               0
         k     1
         %[az] 2
         g     3
               4
         %[az] 5
         d     6
         d<e>  7
         %[az] 8
         t     9 */

      slice_4 = (l_16_string_char_slice*)slice_3->head.next;

      l_assert(LNUL, slice_4->head.next != 0);
      l_assert(LNUL, slice_4->head.range != 0);
      l_assert(LNUL, slice_4->head.num_ranges == 1);
      l_assert(LNUL, slice_4->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_4->head.strs_end_at_cur_slice == (1 << 7));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'd') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'g') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 'k') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_4->head.char_match_dict, 't') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 'd') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 'g') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 'k') == 3);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_4->head.char_match_dict, 't') == 4);

      l_assert(LNUL, slice_4->strs_match_cur_char[0] == ((1 << 6) | (1 << 7)));
      l_assert(LNUL, slice_4->strs_match_cur_char[1] == (1 << 3));
      l_assert(LNUL, slice_4->strs_match_cur_char[2] == (1 << 1));
      l_assert(LNUL, slice_4->strs_match_cur_char[3] == (1 << 9));

      l_assert(LNUL, slice_4->head.range[0].strs_match_the_range == ((1 << 2) | (1 << 5) | (1 << 8)));
      l_assert(LNUL, l_match_range_is_bit_set(slice_4->head.range + 0, 'a') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_4->head.range + 0, 'b') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_4->head.range + 0, 'y') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_4->head.range + 0, 'z') == true);

      /* slice_5 - e(3) h(9) i(1)
                         0
         i               1
         %[az]           2
         e               3
                         4
         %#2140<e>       5
         %[09[az[AZ]<e>  6
                         7
         %[az]           8
         h               9 */

      slice_5 = (l_16_string_char_slice*)slice_4->head.next;

      l_assert(LNUL, slice_5->head.next != 0);
      l_assert(LNUL, slice_5->head.range != 0);
      l_assert(LNUL, slice_5->head.num_ranges == 3);
      l_assert(LNUL, slice_5->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_5->head.strs_end_at_cur_slice == ((1 << 5) | (1 << 6)));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_5->head.char_match_dict, 'e') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_5->head.char_match_dict, 'h') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_5->head.char_match_dict, 'i') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_5->head.char_match_dict, 'e') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_5->head.char_match_dict, 'h') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_5->head.char_match_dict, 'i') == 3);

      l_assert(LNUL, slice_5->strs_match_cur_char[0] == (1 << 3));
      l_assert(LNUL, slice_5->strs_match_cur_char[1] == (1 << 9));
      l_assert(LNUL, slice_5->strs_match_cur_char[2] == (1 << 1));

      l_assert(LNUL, slice_5->head.range[0].strs_match_the_range == ((1 << 2) | (1 << 8)));
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'a') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'b') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'y') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'z') == true);

      l_assert(LNUL, slice_5->head.range[1].strs_match_the_range == (1 << 5));
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 1, '\x21') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 1, 0x22) == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 1, 0x3f) == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 1, '\x40') == true);

      l_assert(LNUL, slice_5->head.range[2].strs_match_the_range == (1 << 6));
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, '0') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, '9') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, 'a') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, 'z') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, 'A') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 2, 'Z') == true);

      /* slice_6 - e(8) n(1) q(9) t(3)
                   0
         n         1
         %[az]<e>  2
         t<e>      3
                   4
                   5
                   6
                   7
         e         8
         q         9 */

      slice_6 = (l_16_string_char_slice*)slice_5->head.next;

      l_assert(LNUL, slice_6->head.next != 0);
      l_assert(LNUL, slice_6->head.range != 0);
      l_assert(LNUL, slice_6->head.num_ranges == 1);
      l_assert(LNUL, slice_6->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_6->head.strs_end_at_cur_slice == ((1 << 2) | (1 << 3)));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_6->head.char_match_dict, 'e') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_6->head.char_match_dict, 'n') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_6->head.char_match_dict, 'q') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_6->head.char_match_dict, 't') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_6->head.char_match_dict, 'e') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_6->head.char_match_dict, 'n') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_6->head.char_match_dict, 'q') == 3);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_6->head.char_match_dict, 't') == 4);

      l_assert(LNUL, slice_6->strs_match_cur_char[0] == (1 << 8));
      l_assert(LNUL, slice_6->strs_match_cur_char[1] == (1 << 1));
      l_assert(LNUL, slice_6->strs_match_cur_char[2] == (1 << 9));
      l_assert(LNUL, slice_6->strs_match_cur_char[3] == (1 << 3));

      l_assert(LNUL, slice_6->head.range[0].strs_match_the_range == (1 << 2));
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'a') == true);
      l_assert(LNUL, l_match_range_is_bit_set(slice_5->head.range + 0, 'z') == true);

      /* slice_7 - d(1) s(8) u(9)
               0
         d<e>  1
               2
               3
               4
               5
               6
               7
         s     8
         u     9 */

      slice_7 = (l_16_string_char_slice*)slice_6->head.next;

      l_assert(LNUL, slice_7->head.next != 0);
      l_assert(LNUL, slice_7->head.range == 0);
      l_assert(LNUL, slice_7->head.num_ranges == 0);
      l_assert(LNUL, slice_7->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_7->head.strs_end_at_cur_slice == (1 << 1));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_7->head.char_match_dict, 'd') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_7->head.char_match_dict, 's') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_7->head.char_match_dict, 'u') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_7->head.char_match_dict, 'd') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_7->head.char_match_dict, 's') == 2);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_7->head.char_match_dict, 'u') == 3);

      l_assert(LNUL, slice_7->strs_match_cur_char[0] == (1 << 1));
      l_assert(LNUL, slice_7->strs_match_cur_char[1] == (1 << 8));
      l_assert(LNUL, slice_7->strs_match_cur_char[2] == (1 << 9));

      /* slice_8 - a(9) s(8)
                0
                1
                2
                3
                4
                5
                6
                7
         s<e>   8
         a      9 */

      slice_8 = (l_16_string_char_slice*)slice_7->head.next;

      l_assert(LNUL, slice_8->head.next != 0);
      l_assert(LNUL, slice_8->head.range == 0);
      l_assert(LNUL, slice_8->head.num_ranges == 0);
      l_assert(LNUL, slice_8->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_8->head.strs_end_at_cur_slice == (1 << 8));

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_8->head.char_match_dict, 'a') == true);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_8->head.char_match_dict, 's') == true);

      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_8->head.char_match_dict, 'a') == 1);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_8->head.char_match_dict, 's') == 2);

      l_assert(LNUL, slice_8->strs_match_cur_char[0] == (1 << 9));
      l_assert(LNUL, slice_8->strs_match_cur_char[1] == (1 << 8));

      /* slice_9 - k(9)
            0
            1
            2
            3
            4
            5
            6
            7
            8
         k  9 */

      slice_9 = (l_16_string_char_slice*)slice_8->head.next;

      l_assert(LNUL, slice_9->head.next != 0);
      l_assert(LNUL, slice_9->head.range == 0);
      l_assert(LNUL, slice_9->head.num_ranges == 0);
      l_assert(LNUL, slice_9->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_9->head.strs_end_at_cur_slice == 0);

      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_9->head.char_match_dict, 'k') == true);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_9->head.char_match_dict, 'k') == 1);
      l_assert(LNUL, slice_9->strs_match_cur_char[0] == (1 << 9));

      slice_10 = (l_16_string_char_slice*)slice_9->head.next;
      l_assert(LNUL, slice_10->head.next != 0);
      l_assert(LNUL, slice_10->head.range == 0);
      l_assert(LNUL, slice_10->head.num_ranges == 0);
      l_assert(LNUL, slice_10->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_10->head.strs_end_at_cur_slice == 0);
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_10->head.char_match_dict, 'e') == true);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_10->head.char_match_dict, 'e') == 1);
      l_assert(LNUL, slice_10->strs_match_cur_char[0] == (1 << 9));

      slice_11 = (l_16_string_char_slice*)slice_10->head.next;
      l_assert(LNUL, slice_11->head.next == 0);
      l_assert(LNUL, slice_11->head.range == 0);
      l_assert(LNUL, slice_11->head.num_ranges == 0);
      l_assert(LNUL, slice_11->head.strs_match_any_char == 0);
      l_assert(LNUL, slice_11->head.strs_end_at_cur_slice == (1 << 9));
      l_assert(LNUL, l_char_match_dict_is_bit_set(&slice_11->head.char_match_dict, 's') == true);
      l_assert(LNUL, l_char_match_dict_bit_1_count(&slice_11->head.char_match_dict, 's') == 1);
      l_assert(LNUL, slice_10->strs_match_cur_char[0] == (1 << 9));
    }

    { const l_strp matching_string = l_literal_strp("mankind.mank.forget.forg.hi_max.ak3x@.bird9.birdz.birdZ.card.darkness.earthquakes.earthquake");
      const l_byte* pend = 0;
      l_int which_string = 0;
      l_int matched_len = 0;

       /* 0 "man"
          1 "mankind" // mankind is never matched due to man is ahead of it
          2 "hi%.%[az]%[az]%[az]"
          3 "forget"
          4 "for"
          5 "a%.%(04)%[az]%#2140|"
          6 "bird%[09[az[AZ]"
          7 "card"
          8 "dar%[az]%[az]ess"
          9 "earthquakes" */

      pend = l_string_match_x(patt, matching_string.s, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == 'k');
      l_assert(LNUL, which_string == 0); /* man */
      l_assert(LNUL, matched_len == 3);

      pend = l_string_match_x(patt, pend + 5, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == 'k');
      l_assert(LNUL, which_string == 0); /* man */
      l_assert(LNUL, matched_len == 3);

      pend = l_string_match_x(patt, pend + 2, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 3); /* forget */
      l_assert(LNUL, matched_len == 6);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == 'g');
      l_assert(LNUL, which_string == 4); /* for */
      l_assert(LNUL, matched_len == 3);

      pend = l_string_match_x(patt, pend + 2, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 2); /* hi%.%[az]%[az]%[az] */
      l_assert(LNUL, matched_len == 6);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 5); /* a%.%(04)%[az]%#2140| */
      l_assert(LNUL, matched_len == 5);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 6); /* bird%[09[az[AZ] */
      l_assert(LNUL, matched_len == 5);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 6); /* bird%[09[az[AZ] */
      l_assert(LNUL, matched_len == 5);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 6); /* bird%[09[az[AZ] */
      l_assert(LNUL, matched_len == 5);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 7); /* card */
      l_assert(LNUL, matched_len == 4);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 8); /* dar%[az]%[az]ess */
      l_assert(LNUL, matched_len == 8);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, *pend == '.');
      l_assert(LNUL, which_string == 9); /* earthquakes */
      l_assert(LNUL, matched_len == 11);

      pend = l_string_match_x(patt, pend + 1, matching_string.e, &which_string, &matched_len);
      l_assert(LNUL, pend == l_string_too_short);
    }

    l_destroy_string_pattern(&patt);
  }
}

