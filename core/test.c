#include "core/base.h"
#include "core/lapi.h"
#include "core/beat.h"
#include "osi/base.h"

{ /* array test */
  char s[] = "abc";
  int a[] = {1, 2, 3, 4};
  int b[3] = {1};

  /* string literal is a char array, the end \0 character is inclued in the array */
  l_assert(sizeof(s) == 4);

  /* the size of normal array */
  l_assert(sizeof(a) == 4*sizeof(int));

  /* array initialize test */
  l_assert(b[0] == 1);
  l_assert(b[1] == 0);
  l_assert(b[2] == 0);
}

{ /* string test */
  l_assert(strlen(NULL) == 0);
}

{ /** very large memory space allocation **/
}

{ /** compare performance between loop assign and memory copy **/
}

{ /** compare performance between memory copy and struct assign **/
}

{ /** the difference between lua code "require(string)" and C api "luaopen_string(L)" **/
}

{ /** lua table test, use empty key "" to store a vlue **/
}

