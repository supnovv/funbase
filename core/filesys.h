#ifndef LNLYLIB_CORE_FILESYS_H
#define LNLYLIB_CORE_FILESYS_H
#include "core/base.h"

typedef struct {
  void* file;
} l_stanfile;

L_EXTERN l_stanfile
l_stanfile_open_read(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_read_nobuf(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_write(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_write_nobuf(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_append(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_append_nobuf(const void* name);

L_EXTERN l_stanfile
l_stanfile_open_read_write(const void* name);

L_EXTERN void
l_stanfile_close(l_stanfile* s);

L_EXTERN void
l_stanfile_clearerr(l_stanfile* s);

L_EXTERN l_bool
l_stanfile_flush(l_stanfile* s);

L_EXTERN l_bool
l_stanfile_rewind(l_stanfile* s);

L_EXTERN l_bool
l_stanfile_seekto(l_stanfile* s, l_int pos);

L_EXTERN l_bool
l_stanfile_forward(l_stanfile* s, l_int offset);

L_EXTERN l_bool
l_stanfile_backward(l_stanfile* s, l_int offset);

L_EXTERN l_int
l_stanfile_read(l_stanfile* s, void* out, l_int size);

L_EXTERN l_int
l_stanfile_write(l_stanfile* s, const void* p, l_int len);

L_EXTERN l_int
l_stanfile_put(l_stanfile* s, l_byte ch);

L_EXTERN l_int
l_stanfile_get(l_stanfile* s, l_byte* ch);

L_EXTERN l_bool
l_stanfile_remove(const void* name);

L_EXTERN l_bool
l_stanfile_rename(const void* from, const void* to);

L_EXTERN void
l_stanfile_redirect_stdout(const void* name);

L_EXTERN void
l_stanfile_redirect_stderr(const void* name);

L_EXTERN void
l_stanfile_redirect_stdin(const void* name);

typedef union {
  l_int fd;
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

typedef struct {
  void* ds;
} l_dirstm;

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

#endif /* LNLYLIB_CORE_FILESYS_H */

