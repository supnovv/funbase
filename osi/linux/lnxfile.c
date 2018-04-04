
/** access, faccessat - check user's permission for a file
#include <fcntl.h>
#include <unistd.h>
int access(const char* pathname, int mode);
int faccessat(int dirfd, const char* pathname, int mode, int flags);
---
The mode specifies the accessibility checks to be performed,
and is either the value F_OK,
or a mask consisting of the bitwise OR of one or more of R_OK, W_OK, and X_OK.
F_OK tests for the existence of the file.
R_OK, W_OK, and X_OK test whether the file exists and grants read, write,
and execute permission, resectively.
The faccessat() system call operates in exactly the same way as access(),
except for the differences described here.
If the patchname given is relative, then it is interpreted relateve to the
directory referred to by the file descriptor dirfd.
If the pathname is related, and dirfd is the special value AT_FDCWD,
then pathname is related to the current working directory like access().
If pathname is absolute, then dirfd is ignored.
The flags is constructed by OR togother zero of more following values:
AT_EACCESS - perform access checks using the effective user and group IDs,
by default, faccessat() uses the calling process's real user and group IDs
like access(). In other words, AT_EACCESS answers the "can I read/write/exe
this file?". Without AT_EACCESS, it answers a slightly different question:
"(assuming I'm a setuid binary) can the user who invoked me read/write/exe this
file?", which gives set-user-ID programs the possibility to prevent malicious
users from causing them to read files which users shouldn't be able to read.
AT_SYMLINK_NOFOLLOW - if pathname is a symbolic link, do not dereference it;
instead return information about the link itself.
On success, zero is returned; on error, -1 is returned and errno is set.
*/

typedef struct {
  int fd;
} l_lnxfiledesc;

L_EXTERN int
l_file_isexist(const void* name) {
  return (name && faccessat(AT_FDCWD, (const char*)name, F_OK, AT_SYMLINK_NOFOLLOW) == 0);
}

L_EXTERN int
l_file_isexistat(l_filedesc* dirfd, const void* name) {
  l_lnxfiledesc* pdir = (l_lnxfiledesc*)dirfd);
  if (pdir->fd == -1 || !name) return false;
  return (faccessat(pdir->fd, (const char*)name, F_OK, AT_SYMLINK_NOFOLLOW) == 0);
}

L_EXTERN int
l_file_getattr(l_fileaddr* fa, const void* name) {
  struct stat st;
  if (lstat((const char*)name, &st) != 0) return false;
  fa->size = (l_long)st.st_size;
  fa->ctime = (l_long)st.st_ctime;
  fa->atime = (l_long)st.st_atime;
  fa->mtime = (l_long)st.st_mtime;
  fa->isfile = (l_byte)(S_ISREG(st.st_mode) != 0);
  fa->isdir = (l_byte)(S_ISDIR(st.st_mode) != 0);
  fa->islink = (l_byte)(S_ISLNK(st.st_mode) != 0);
  return true;
}

L_EXTERN int
l_file_folderexist(const void* foldername) {
  struct stat st;
  return (lstat((const char*)name, &st) == 0 && S_ISDIR(st.st_mode));
}

L_EXTERN int
l_file_opendir(l_filedesc* self, const void* name) {
  l_lnxfiledesc* p = (l_lnxfiledesc*)self;
  p->fd = open((const char*)name, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOATIME);
  return (p->fd != -1);
}

L_EXTERN void
l_file_closefd(l_filedesc* self) {
  l_lnxfiledesc* p = (l_lnxfiledesc*)self;
  if (p->fd == -1) return;
  if (close(p->fd) != 0) {
    l_loge_1("close %d", lserror(errno));
  }
  p->fd = -1;
}

/** directory stream
#include <sys/types.h>
#include <dirent.h>
DIR* opendir(const char* name);
It opens a directory stream corresponding to the name,
and returns a pointer to the directory stream.
The stream is positioned at the first entry in the directory.
Filename entries can be read from a directory stream using readdir(3).
---
## struct dirent* readdir(DIR* d);
It returns a pointer to a dirent structure representing the next
directory entry in the directory stream pointed by d.
It returns NULL on reaching the end of the directory or if an error occurred.
    struct dirent {
      ino_t d_ino; // Inode number
      off_t d_off; // current location, should treat as an opaque value
      unsigned short d_reclen; // length of this record
      unsigned char d_type; // type of file, not supported in all filesystem
      char d_name[256]; // null-terminated filename
    };
The only fields in the dirent structure that are managed by POSIX.1 is d_name and d_ino.
The other fields are unstandardized, and no present on all systems.
The d_name can be at most NAME_MAX characters preceding the terminating null byte.
It is recommanded that application use readdir(3) instead of readdir_r().
Furthermore, since version 2.24, glibc deprecates readdir_r().
The reasons are: on systems where NAME_MAX is undefined, calling readdir_r() may
be unsafe because the interface doesn't allow the caller to specify the length
of the buffer used for the returned directory entry;
On some systems, readdir_r() cann't read directory entries with very long names.
When the glibc implementation encounters such a name, readdir_r() fails with
the error ENAMETOOLONG after the final directory entry has been read.
On some other systems, readdir_r() may return a success status,
but the returned d_name field may not be null terminated or may be truncated;
In the current POSIX.1 specification (POSIX.1-2008), readdir(3) is not required to be thread-safe.
However, in modern implementation (including glibc), concurrent calls to readdir(3)
that specify different directory streams are thread-safe.
Therefore, the use of readdir_r() is generally unneccessary in multithreaded programs.
In cases where multiple threads must read from the same stream, using readdir(3) with
external sychronization is still preferable to the use of readdir_r().
*/

typedef struct {
  DIR* stream;
} l_lnxdirstream;

L_EXTERN int
l_dirstream_opendir(l_dirstream* self, const void* name) {
  l_lnxdirstream* d = (l_lnxdirstream*)self;
  if ((d->stream = opendir((const char*)name)) == 0) {
    l_loge_1("opendir %s", lserror(errno));
  }
  return (d->stream != 0);
}

L_EXTERN void
l_dirstream_close(l_dirstream* self) {
  l_lnxdirstream* d = (l_lnxdirstream*)self;
  if (d->stream == 0) return;
  if (closedir(d->stream) != 0) {
    l_loge_1("closedir %s", lserror(errno));
  }
  d->stream = 0;
}

L_EXTERN const l_byte*
l_dirstream_read(l_dirstream* self) {
  l_lnxdirstream* d = (l_lnxdirstream*)self;
  struct dirent* entry = 0;
  errno = 0;
  if ((entry = readdir(d->stream)) == 0) {
    if (errno != 0) l_loge_1("readdir %s", lserror(errno));
    return 0;
  }
  return l_strz(entry->d_name);
}

L_EXTERN const l_byte*
l_dirstream_read2(l_dirstream* self, int* isdir) {
  l_lnxdirstream* d = (l_lnxdirstream*)self;
  struct dirent* entry = 0;
  if (isdir) *isdir = 0;
  errno = 0;
  if ((entry = readdir(d->stream)) == 0) {
    if (errno != 0) l_loge_1("readdir %s", lserror(errno));
    return 0;
  }
  if (isdir) {
#if 0
    *isdir = (entry->d_type == DT_DIR);
#else
    struct stat st;
    if (lstat(entry->d_name, &st) != 0) {
      l_loge_1("lstat %s", lserror(errno));
      return 0;
    }
    isdir = S_ISDIR(st.st_mode);
#endif
  }
  return l_strz(entry->d_name);
}


