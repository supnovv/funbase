#include <stdio.h>
#include <string.h>
#include "lnlylib.h"

int start(lnlylib_env* E)
{
#include "core/test.c"

#if defined(L_PLAT_LINUX)
#include "osi/lnxtest.c"
#else
/* other platform test */
#endif

  l_string_match_test();
  return 0;
}

int main(int argc, char** argv)
{
  l_set_log_level(5);
  return lnlylib_main(start, argc, argv);
}

