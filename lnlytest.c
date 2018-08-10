#include <string.h>
#include "lnlylib.h"

int start(lnlylib_env* E)
{
#include "core/test.c"
#if defined(L_PLAT_LINUX)
#include "osi/lnxtest.c"
#else
#endif
  return 0;
}

int main(int argc, char** argv)
{
  return lnlylib_main(start, argc, argv);
}

