#include <stdio.h>

int main(int argc, char** argv)
{
  int i = 0;

  for (; i < argc; i += 1) {
    printf("arg[%02d] -> [%s]\n", i, argv[i]);
  }

  return 0;
}

