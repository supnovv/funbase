#include <cstdio>
#include "lnlylib.h"

class l_datafrag {
  const static l_byte codec[] = {
    "0123456789+abcde"
    "fghijklmnopqrstu"
    "vwxyz/ABCDEFGHIJ"
    "KLMNOPQRSTUVWXYZ"};
  l_byte frag[48];
  mutable l_byte dest[64];
public:
  l_datafrag() {
    l_zero_n(this, sizeof(l_datafrag));
  }

  l_datafrag(l_strn data) {
    set_data(data);
  }

  void set_data(l_strn data) {
    if (data.p && data.n > 0) {
      l_int i = 0;
      l_int len = 48;
      if (data.n < len) {
        len = data.n;
      }
      while (i < len) {
        frag[i] = data.p[i];
        i += 1;
      }
      while (i < 48) {
        frag[i] = 0;
        i += 1;
      }
    } else {
      l_zero_n(this, sizeof(l_datafrag));
    }
  }

  l_strn get_codec() const {
    l_ulong* n = (l_ulong*)frag;
    int dest_i = 0;
    int num_i = 0;
    int codec_i = 0;
    int power_n = 2;
    int mask = 0;
    int shift = 0;
    for (; dest_i < 64; ++dest_i) {
      mask = power_n - 1;
      codec_i = 0;
      codec_i |= ((n[0] & mask) >> shift) << 0;
      codec_i |= ((n[1] & mask) >> shift) << 1;
      codec_i |= ((n[2] & mask) >> shift) << 2;
      codec_i |= ((n[3] & mask) >> shift) << 3;
      codec_i |= ((n[4] & mask) >> shift) << 4;
      codec_i |= ((n[5] & mask) >> shift) << 5;
      dest[dest_i] = codec[codec_i];
      power_n <<= 1;
      shift += 1;
    }
    return l_strn_l(dest, 64);
  }
}

L_EXTERN l_bool
l_read_entire_file(l_string* out, const void* filename)
{
}

class l_datablock {
  l_ulong size;
  l_string buffer;
public:
  l_datablock()
    size(0), buffer()
    {}

  l_bool set_plain(const void* filename) {
    l_bool succ = l_read_entire_file(&buffer, filename);
    if (succ) {
    } else {
    }
    return succ;
  }

  l_bool set_image(const void* filename) {
  }
};

#define L_MAX_DATA_BLOCK 100
#define L_MAX_FILENAME_LEN 128

int main(int argc, char** argv) {
  char key[100] = {0};
  int num_blocks = 0;
  l_datablock block[L_MAX_DATA_BLOCK+1];
  l_datafrag fragment;
  l_file file;
  int i = 0;

  char file_type[8] = {0};
  char file_name[L_MAX_FILENAME_LEN+1];
  char cmd[16] = {0};
  if (argc != 2 || argv == 0) {
    return 0;
  }

  file = l_file_open_read_write(argv[1]);
  if (l_file_nt_open(&file)) {
    printf("Cannot open file '%s'.\n", argv[1]);
    return 0;
  }

  printf("datamix %s > ", argv[1]);
  if (scanf(" %40s", key) != 1 || strlen(key) != 40) {
    return 0;
  }
  for (i = 0; i < 40; ++i) {
    if ((key[i] >= '0' && key[i] <= '9') || (key[i] >= 'a' && key[i] <= 'z')) {
      continue;
    }
    return 0;
  }

  for (; ;) {
    printf("\ndatamix %s!> ", argv[1]);
    if ((scanf(" %8s", cmd) != 1) {
      continue;
    }
    if (strcmp(cmd, "add") == 0) {
      if (num_blocks == L_MAX_DATA_BLOCKS) {
        printf("No buffers available anymore.\n");
        continue;
      }
      if (scanf(" %5s %80s", file_type, file_name) != 2) {
        continue;
      }
      if (strcmp(file_type, "plain") == 0) {
      } else if (strcmp(file_type, "image") == 0) {
      } else {
        continue;
      }
    } else if (strcmp(cmd, "commit") == 0) {
    } else if (strcmp(cmd, "del") == 0) {
      if (num_blocks > 0) {
        num_blocks -= 1;
      }
      if (num_blocks == 0) {
        continue;
      }
    } else if (strcmp(cmd, "exit") == 0) {
      if (num_blocks > 0) {
        printf("Please commit the content first.\n");
        continue;
      } else {
        break;
      }
    }
    l_print_current_blocks_status(block, num_blocks);
  }

  return 0;
}
