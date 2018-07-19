#ifndef LNLYLIB_CORE_FILESYS_H
#define LNLYLIB_CORE_FILESYS_H
#include "core/base.h"

typedef struct {
  void* file;
} l_stdfile;

L_EXTERN l_stdfile l_stdfile_open_read(const void* name);
L_EXTERN l_stdfile l_stdfile_open_read_nobuf(const void* name);
L_EXTERN l_stdfile l_stdfile_open_write(const void* name);
L_EXTERN l_stdfile l_stdfile_open_write_nobuf(const void* name);
L_EXTERN l_stdfile l_stdfile_open_append(const void* name);
L_EXTERN l_stdfile l_stdfile_open_append_nobuf(const void* name);
L_EXTERN l_stdfile l_stdfile_open_read_write(const void* name);
L_EXTERN void l_stdfile_close(l_stdfile* s);
L_EXTERN void l_stdfile_clearerr(l_stdfile* s);
L_EXTERN l_bool l_stdfile_flush(l_stdfile* s);
L_EXTERN l_bool l_stdfile_rewind(l_stdfile* s);
L_EXTERN l_bool l_stdfile_seekto(l_stdfile* s, l_int pos);
L_EXTERN l_bool l_stdfile_forward(l_stdfile* s, l_int offset);
L_EXTERN l_bool l_stdfile_backward(l_stdfile* s, l_int offset);
L_EXTERN l_int l_stdfile_read(l_stdfile* s, void* out, l_int size);
L_EXTERN l_int l_stdfile_write(l_stdfile* s, const void* p, l_int len);
L_EXTERN l_int l_stdfile_write_strn(l_stdfile* out, l_strn s);
L_EXTERN l_int l_stdfile_put(l_stdfile* s, l_byte ch);
L_EXTERN l_int l_stdfile_get(l_stdfile* s, l_byte* ch);
L_EXTERN l_bool l_stdfile_remove(const void* name);
L_EXTERN l_bool l_stdfile_rename(const void* from, const void* to);
L_EXTERN void l_stdfile_redirect_stdout(const void* name);
L_EXTERN void l_stdfile_redirect_stderr(const void* name);
L_EXTERN void l_stdfile_redirect_stdin(const void* name);

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

