#define LNLYLIB_API_IMPL
#include "core/fsys.h"

static l_stanfile
ll_stanfile_open(const void* name, const char* mode)
{
  l_stanfile s = {0};
  if (name && mode) {
    s.file = fopen((const char*)name, mode);
    if (s.file == 0) {
      l_loge_1("fopen %s", lserror(errno));
    }
  } else {
    l_loge_s("invalid parameter");
  }
  return s;
}

static l_stanfile
ll_stanfile_open_nobuf(const void* name, const char* mode)
{
  l_stanfile s = ll_stanfile_open(name, "wb");
  if (s.file) {
    setbuf((FILE*)s.file, 0);
  }
  return s;
}

static void
ll_stanfile_reopen(FILE* file, const void* name, const char* mode)
{
  if (name && mode) {
    if (freopen((const char*)name, mode, file) == 0) {
      l_loge_1("freopen %s", lserror(errno));
    }
  } else {
    l_loge_s("invalid parameter");
  }
}

L_EXTERN l_stanfile
l_stanfile_open_read(const void* name)
{
  return ll_stanfile_open(name, "rb");
}

L_EXTERN l_stanfile
l_stanfile_open_read_nobuf(const void* name)
{
  return ll_stanfile_open_nobuf(name, "rb");
}

L_EXTERN l_stanfile
l_stanfile_open_write(const void* name)
{
  return ll_stanfile_open(name, "wb");
}

L_EXTERN l_stanfile
l_stanfile_open_write_nobuf(const void* name)
{
  return ll_stanfile_open_nobuf(name, "wb");
}

L_EXTERN l_stanfile
l_stanfile_open_append(const void* name)
{
  return ll_stanfile_open(name, "ab");
}

L_EXTERN l_stanfile
l_stanfile_open_append_nobuf(const void* name)
{
  return ll_stanfile_open_nobuf(name, "ab");
}

L_EXTERN l_stanfile
l_stanfile_open_read_write(const void* name)
{
  return ll_stanfile_open(name, "rb+");
}

L_EXTERN void
l_stanfile_close(l_stanfile* s)
{
  if (s->file == 0) {
    return;
  }
  if (fclose((FILE*)s->file) != 0) {
    l_loge_1("fclose %s", lserror(errno));
  }
  s->file = 0;
}

L_EXTERN void
l_stanfile_clearerr(l_stanfile* s)
{
  clearerr((FILE*)s->file);
}

L_EXTERN l_bool
l_stanfile_flush(l_stanfile* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fflush((FILE*)s->file) == 0) {
    return true;
  } else {
    l_loge_1("fflush %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stanfile_rewind(l_stanfile* s)
{
  if (s->file == 0) {
    return false;
  }
  if (fseek((FILE*)s->file, 0, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_1("fseek SET %s", lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stanfile_seekto(l_stanfile* s, l_int pos)
{
  if (s->file == 0 || pos < 0 || pos > L_MAX_INT_IO) {
    l_loge_1("invalid parameter %d", ld(pos));
    return false;
  }
  if (fseek((FILE*)s->file, pos, SEEK_SET) == 0) {
    return true;
  } else {
    l_loge_1("fseek SET %d %s", ld(pos), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stanfile_forword(l_stanfile* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1("invalid parameter %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_1("fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_bool
l_stanfile_backward(l_stanfile* s, l_int offset)
{
  if (s->file == 0 || offset < 0 || offset > L_MAX_INT_IO) {
    l_loge_1("invalid parameter %d", ld(offset));
    return false;
  }
  if (fseek((FILE*)s->file, -offset, SEEK_CUR) == 0) {
    return true;
  } else {
    l_loge_1("fseek CUR %d %s", ld(offset), lserror(errno));
    return false;
  }
}

L_EXTERN l_int
l_stanfile_read(l_stanfile* s, void* out, l_int size)
{
  if (s->file == 0 || out == 0 || size < 0 || size > L_MAX_INT_IO) {
    l_loge_1("invalid parameter %d", ld(size));
    return 0;
  }
  if (size == 0) {
    return 0;
  }
  { l_int n = 0;
    n = (l_int)fread(out, 1, (size_t)size, (FILE*)s->file);
    if (n == size) {
      return n;
    }
    if (!feof((FILE*)s->file)) {
      l_loge_1("fread %s", lserror(errno));
    }
    if (n < 0) {
      return 0;
    }
    return n;
  }
}

L_EXTERN l_int
l_stanfile_write(l_stanfile* s, const void* p, l_int len)
{
  if (s->file == 0 || p == 0 || len < 0 || len > L_MAX_INT_IO) {
    l_loge_1("invalid parameter %d", ld(len));
    return 0;
  }
  if (len == 0) {
    return 0;
  }
  { l_int n = (l_int)fwrite(p, 1, (size_t)len, (FILE*)s->file);
    if (n == len) {
      return n;
    }
    l_loge_1("fwrite %s", lserror(errno));
    if (n < 0) {
      return 0;
    }
    return n;
  }
}

L_EXTERN l_int
l_stanfile_write_strn(l_stanfile* out, l_strn s) {
  return l_stanfile_write(out, s.start, s.len);
}

L_EXTERN l_int
l_stanfile_put(l_stanfile* s, l_byte ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fwrite(&ch, 1, 1, (FILE*)s->file) != 1) {
    l_loge_1("fwrite %s", lserror(errno));
    return 0;
  }
  return 1;
}

L_EXTERN l_int
l_stanfile_get(l_stanfile* s, l_byte* ch)
{
  if (s->file == 0) {
    return 0;
  }
  if (fread(ch, 1, 1, (FILE*)s->file) != 1) {
    if (!feof((FILE*)s->file)) {
      l_loge_1("fread %s", lserror(errno));
    }
    return 0;
  }
  return 1;
}

L_EXTERN l_bool
l_stanfile_remove(const void* name)
{
  if (name) {
    if (remove((const char*)name) == 0) {
      return true;
    } else {
      l_loge_1("remove %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s("invalid parameter");
    return false;
  }
}

L_EXTERN l_bool
l_stanfile_rename(const void* from, const void* to)
{
  /* int rename(const char* oldname, const char* newname);
   * Changes the name of the file or directory specified by oldname
   * to newname. This is an operation performed directly on a file;
   * No streams are involved in the operation. If oldname and newname
   * specify different paths and this is supported by the system, the
   * file is moved to the new location. If newname names an existing
   * file, the function may either fail or override the existing file,
   * depending on the specific system and library implementation.
   */
  if (from && to) {
    if (rename((const char*)from, (const char*)to) == 0) {
      return true;
    } else {
      l_loge_1("rename %s", lserror(errno));
      return false;
    }
  } else {
    l_loge_s("invalid parameter");
    return false;
  }
}

L_EXTERN void
l_stanfile_redirect_stdout(const void* name)
{
  ll_stanfile_reopen(stdout, name, "wb");
}

L_EXTERN void
l_stanfile_redirect_stderr(const void* name)
{
  ll_stanfile_reopen(stderr, name, "wb");
}

L_EXTERN void
l_stanfile_redirect_stdin(const void* name)
{
  ll_stanfile_reopen(stdin, name, "rb");
}

