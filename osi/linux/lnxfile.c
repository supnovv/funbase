
typedef struct {
  void* impl;
} l_stdfile;

typedef struct {
  void* impl;
} l_filedesc;


/** access, faccessat - check user's permission for a file
#include <fcntl.h>
#include <unistd.h>
int access(const char* pathname, int mode);
int faccessat(int dirfd, const char* pathname, int mode, int flags);
---


*/

L_EXTERN int
l_file_isexist(const void* name) {
  if (!faccessat(AT_FDCWD, (const char*)name, F_OK, AT_SYMLINK_NOFOLLOW)) return true;
  l_loge1("faccessat %s", lserror(errno));
  return false;
}
