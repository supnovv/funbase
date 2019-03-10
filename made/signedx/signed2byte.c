#include <stdio.h>

int main(int argc, char* argv[]) {
  int n = 0;
  if (argc != 2 || argv == 0) {
    printf("Usage: signed2byte -10\n");
    return 0;
  }
  if (sscanf(argv[1], " %d", &n) != 1) {
    printf("invalid input.\n");
    return 0;
  } else {
    unsigned char b = (unsigned char)(n & 0xff);
    printf("%d -> 0x%02x\n", (int)(signed char)b, (unsigned)b);
    return 0;
  }
}
