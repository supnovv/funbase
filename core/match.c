
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
  return l_bit_1_count_g(n);
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
    return (n + l_bit_1_count_g[x-1] + 1);
  }
}

typedef struct {
  l_ulong char_256_bits[4];
  l_byte bit_1_count[32];
} l_char_match_dict;

typedef struct {
  l_ulong char_256_bits[8];
  l_ulong strs_match_the_range;
} l_match_range; /* 5-ulong 40-byte */

typedef struct l_char_slice {
  l_char_match_dict char_match_dict;
  struct l_char_slice* next;
  l_match_range* range;
  l_ulong num_ranges;
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

typedef struct {
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
l_string_pattern_create_impl(const l_char_slice_info* slice_arr, l_int num_slices, l_int slice_type, l_bool case_insensitive)
{
  l_string_pattern* pattern = 0;
  l_int total_size = sizeof(l_string_pattern);
  const l_char_slice_info* slice = 0;
  const l_match_range* range = 0;
  l_int range_i = 0;
  l_int i = 0;

  l_char_slice* real_slice = 0;
  l_byte prev_bit_1_count = 0;
  l_byte* bit_1_count_byte = 0;
  l_int ulong_i = 0;

  /* count the ranges and move all ranges together */
  for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
    slice->num_ranges = 0;
    for (range_i = 0, range = slice->range; range_i < 64; range_i += 1, range += 1) {
      if (range->strs_match_the_range == 0) continue;
      /* TODO - check current range is the same range or not comparing to the ranges already have */
      if (slice->num_ranges != range_i) {
        slice->range[s->num_ranges] = slice->range[range_i];
      }
      slice->num_ranges += 1;
    }
  }

  /* calculate the structure size */
  switch (slice_type) {
  case 0:
    for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
      slice->range_offset =  sizeof(l_8_string_char_slice);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 1:
    for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
      slice->range_offset = sizeof(l_16_string_char_slice);
      if (slice->chars > 4) slice->range_offset += (l_enlarge_to_times_of_4(slice->chars) - 4) * sizeof(l_ushort);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 2:
    for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
      slice->range_offset = sizeof(l_32_string_char_slice);
      if (slice->chars > 2) slice->range_offset += (l_enlarge_to_times_of_2(slice->chars) - 2) * sizeof(l_umedit);
      slice->struct_size = slice->range_offset + slice->num_ranges * sizeof(l_match_range);
      total_size += slice->struct_size;
    }
    break;
  case 3:
    for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
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
  pattern->slice_type = slice_type;
  pattern->case_insensitive = case_insensitive;
  pattern->num_slices = num_slices;
  pattern->slice = (l_char_slice*)(pattern + 1);
  real_slice = pattern->slice;

  for (i = 0, slice = slice_arr; i < num_slices; i += 1, slice += 1) {
    real_slice->range = (l_match_range*)(((l_byte*)real_slice) + slice->range_offset);
    real_slice->num_ranges = slice->num_ranges;
    l_copy_n(real_slice->range, slice->range, sizeof(l_match_range) * slice_num_ranges);
    real_slice->strs_match_any_char = slice->strs_match_any_char;
    real_slice->strs_end_at_cur_slice = slice->strs_end_at_cur_slice;

    for (char_i = 0, node = l_spriorq_top(&slice->charq); char_i < slice->chars; char_i += 1, node = node->next) {
      real_slice->char_match_dict.char_256_bits[l_bit_belong_which_ulong(((l_char_node*)node)->ch)] |= l_bit_mask_of_ulong(((l_char_node*)node)->ch);
      switch (slice_type) {
      case 0:
        ((l_8_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_byte)(((l_char_node*)node)->strs_match_the_char & 0xff);
        break;
      case 1:
        ((l_16_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_ushort)(((l_char_node*)node)->strs_match_the_char & 0xffff);
        break;
      case 2:
        ((l_32_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= (l_umedit)(((l_char_node*)node)->strs_match_the_char & 0xffffffff);
        break;
      case 3:
        ((l_64_string_char_slice*)real_slice)->strs_match_cur_char[char_i] |= ((l_char_node*)node)->strs_match_the_char;
        break;
      }
    }

    l_assert(LNUL, node == 0);

    prev_bit_1_count = 0;
    bit_1_count_byte = real_slice->char_match_dict.bit_1_count;

    ulong_i = 0;
    bit_1_count_byte[ulong_i * 8 + 0] = 0;

    for (; ;) {
      cur_ulong = real_slice->char_match_dict.char_256_bits[ulong_i];
      bit_1_count_byte[ulong_i * 8 + 1] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*0) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 2] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*1) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 3] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*2) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 4] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*3) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 5] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*4) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 6] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*5) & 0xff]);
      bit_1_count_byte[ulong_i * 8 + 7] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*6) & 0xff]);
      if (ulong_i == 3) break;
      bit_1_count_byte[ulong_i * 8 + 8] = (prev_count += l_bit_1_count_g[(cur_ulong >> 8*7) & 0xff]);
      ulong_i += 1;
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
  slice->cur_range = slice->range;
}

static void
l_add_string_match_the_char(l_char_slice_info* slice, l_int string_i, l_int ch)
{
  l_smplnode* node = 0;

  for (node = l_spriorq_top(&slice->charq); node; node = node->next) {
    if (((l_char_node*)node)->ch == ch) {
      node->strs_match_the_char |= l_bit_mask_of_ulong(string_i);
      return;
    }
  }

  node = slice->char_nodes + slice->chars;
  slice->chars += 1;

  node->ch = ch;
  node->strs_match_the_char = l_bit_mask_of_ulong(string_i);
  l_spriorq_push(&slice->charq, node);
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
l_add_string_match_the_range(const l_char_slice_info* slice, l_int string_i, l_byte s, l_byte e, l_bool case_insensitive)
{
  if (s > e) {
    l_loge_4(LNUL, "invalid char range %c(%d) %c(%d)", lc(s), ld(s), lc(e), ld(e));
    return false;
  } else {
    l_match_range* range = slice->range + string_i;
    l_int i = s;
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
l_add_string_match_char_class(const l_char_slice_info* slice, l_int string_i, l_byte char_class, l_bool ci)
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

L_EXTERN l_string_pattern*
l_string_pattern_create(const l_strn* strn, l_int num_of_strings, l_bool case_insensitive)
{
  if (strn == 0 || num_of_strings <= 0 || num_of_strings > 64) {
    return 0;
  }{

  l_string_info str_info[num_of_strings];
  const l_string_info* s = 0;
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

  l_zero_n(slices, sizeof(l_char_slice_info) * longest_string_length);
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
        ch = l_hex_string_to_n(s->cur, 2);
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
            if (!l_add_string_match_the_range(slice, i, range_start_ch, *(s->cur + 1))) {
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
            if (!l_add_string_match_the_range(slice, i, range_start_ch, *(s->cur + 1) - 1)) {
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
          if (!l_add_string_match_char_class(slice, i, *(s->cur + 1))) {
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
            range_start_ch = l_hex_string_to_n(s->cur + 1, 2);
            is_range_begin = false;
          } else {
            if (!l_add_string_match_the_range(slice, i, range_start_ch, l_hex_string_to_n(s->cur + 1, 2))) {
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
          if (!l_add_string_match_the_range(slice, i, *(s->cur + 1), *(s->cur + 2))) {
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
          if (!l_add_string_match_the_range(slice, i, *(s->cur + 1) + 1, *(s->cur + 2) - 1)) {
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
          if (!l_add_string_match_the_range(slice, i, l_hex_string_to_n(s->cur + 1, 2), l_hex_string_to_n(s->cur + 3, 2))) {
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
          if (!l_add_string_match_char_class(slice, i, *(s->cur + 1))) {
            return 0;
          }
          if (end_ch == '}') {
            s->cur += 3；
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

    slice += 1;

    if (handled_chars == 0) {
      break;
    }
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

L_EXTERN l_ulong
l_match_current_slice(const l_char_slice* slice, l_int slice_type, l_int ch, l_int case_insensitive)
{
  l_int belong_ulong = 0;
  l_ulong bits_value_of_ulong = 0;
  l_ulong matched_strings = 0;

  if (case_insensitive) {
    ch = l_to_lower(ch);
  }

  belong_ulong = l_bit_belong_which_ulong(ch);
  bits_value_of_ulong = slice->char_match_dict.char_256_bits[belong_ulong] << l_bit_of_ulong_need_moved_bits_to_high(ch);

  if (bits_value_of_ulong & (0x80000000 << 32)) {
    l_int bit_1_count = slice->char_match_dict.bit_1_count[l_bit_belong_which_byte(ch)] + l_bit_1_count_g[bits_value_of_ulong >> 8*7];
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
    l_int i = 0;
    l_match_range* range = slice->range;
    for (; i < slice->num_ranges; ++i, range += 1) {
      if (range->char_256_bits[belong_ulong] & l_bit_mask_of_ulong(ch)) {
        matched_strings |= range->strs_match_the_range;
      }
    }
  }

  return matched_strings;
}

L_EXTERN const l_byte* const
l_string_too_short = (const l_byte* const)(l_uint)1;

L_EXTERN const l_byte*
l_string_match_x(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_int* which_string, l_int* matched_len)
{
  l_ulong prevmatch = L_MAX_INT_UL;
  l_ulong curmatch = 0;
  l_ulong matches = 0;
  l_ulong first_match = 0;

  l_char_slice* slice = 0;
  l_int slice_type = patt->slice_type;
  const l_byte* match_end = 0;
  const l_byte* cur_char = start;

  l_int len_to_match = pend - start;
  l_int i = 0;
  l_int ch = 0;

  if (len_to_match <= 0) {
    return l_string_too_short;
  }

  if (patt->longest_string_length < len_to_match) {
    len_to_match = patt->longest_string_length;
  }

  slice = patt->slice;
  while (i++ < len_to_match) {
    ch = *cur_char++;
    curmatch = prevmatch & l_match_current_slice(slice, slice_type, ch, patt->case_insensitive);
    if (curmatch == 0) {
      if (match_end) {
        first_match = l_lower_most_bit(matches);
        goto match_success;
      } else {
        return 0;
      }
    }

    matches = curmatch & slice->strs_end_at_cur_slice;
    if (matches) {
      match_end = cur_char;
      first_match = l_lower_most_bit(curmatch);
      if (matches & first_match) {
        goto match_success;
      }
    }

    prevmatch = curmatch;
    slice = slice->next;
  }

  if (match_end) {
    first_match = l_lower_most_bit(matches);
    goto match_sucess;
  }

  return l_string_too_short;

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
  while ((e = l_string_match(patt, start)) > l_string_too_short) {
    start = e;
  }
  return start;
}

L_EXTERN const l_byte* /* return 0 - too short to match, otherwise success */
l_string_match_to(const l_string_pattern* patt, const l_byte* start, const l_byte* pend, l_byte** last_match_start)
{
  const l_byte* e = 0;
  while ((e = l_string_match(patt, start, pend)) == 0) { /* continue if unmatch */
    ++start;
  }
  if (last_match_start) *last_match_start = start;
  return e == l_string_too_short ? 0 : e;
}

