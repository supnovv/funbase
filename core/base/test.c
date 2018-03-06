#include "core/base.h"
{
  /* sizeof array test */
  char s[] = "abc";
  l_medit a[] = {1, 2, 3, 4};
  l_assert(sizeof(s) == 4);
  l_assert(sizeof(a) == 4*4);
  /* strlen test */
  l_assert(strlen(NULL) == 0);
}

