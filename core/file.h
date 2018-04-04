#ifndef LNLYLIB_CORE_FILE_H
#define LNLYLIB_CORE_FILE_H
#include "core/base.h"

typedef struct {
  void* stream;
} l_dirstream;

typedef union {
  int fd;
  void* pvfd;
  double fd8b;
} l_filedesc;

typedef struct {
  l_long size;
  l_long ctime;
  l_long atime;
  l_long mtime;
  l_byte isfile;
  l_byte isdir;
  l_byte islink;
} l_fileattr;

L_EXTERN int l_file_isexist(const void* name);
L_EXTERN int l_file_isexistat(l_filedesc* dirfd, const void* name);
L_EXTERN int l_file_getattr(l_fileaddr* fa, const void* name);
L_EXTERN int l_file_folderexist(const void* foldername);
L_EXTERN int l_file_opendir(l_filedesc* self, const void* name);
L_EXTERN void l_file_closefd(l_filedesc* self);
L_EXTERN int l_dirstream_opendir(l_dirstream* self, const void* name);
L_EXTERN void l_dirstream_close(l_dirstream* self);
L_EXTERN const l_byte* l_dirstream_read(l_dirstream* self);
L_EXTERN const l_byte* l_dirstream_read2(l_dirstream* self, int* isdir);

#endif /* LNLYLIB_CORE_FILE_H */

