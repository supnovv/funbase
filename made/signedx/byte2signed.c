#include <stdio.h>

int main(int argc, char* argv[]) {
  int n = 0;
  if (argc != 2 || argv == 0) {
    printf("Usage: byte2signed fe\n");
    return 0;
  }
  if (sscanf(argv[1], " %2x", &n) != 1) {
    printf("invalid input.\n");
    return 0;
  } else {
    unsigned char b = (unsigned char)(n & 0xff);
    printf("0x%02x -> %d\n", (unsigned)b, (int)(signed char)b);
    return 0;
  }
}
