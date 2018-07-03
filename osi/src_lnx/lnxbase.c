#define LNLYLIB_API_IMPL
#include "osi/src_lnx/lnxpref.h"
#include "osi/base.h"

/** Open shared object and resolve symbols **
#include <dlfcn.h> // link with -ldl
void* dlopen(const char* filename, int flags);
int dlclose(void* handle);
char* dlerror(void); // return error info and clear the error
void* dlsym(void* handle, const char* symbol);
---
The function dlopen() loads the dynamic shared object file named by
the null-terminated string filename and returns an opaque "handle"
for the loaded object. This handle is employed with other functions,
such as dlsym(3), dladdr(3), dlinfo(3), and dlclose(3).
If filename is NULL, then the returned handle is for the main program.
If filename contains a slash ("/"), then it is interpreted as a
relative or absolute pathname. Otherwise, then dynamic linker searches
for the object as follows (see ld.so(8) for further details).
* (ELF only) If the executable file for the calling prgram contains a
DT_RPATH tag, and does not contain a DT_RUNPATH tag, then the
directories listed in the DT_RPATH tag are searched.
* If, at the time that the program was started, the environment
variable LD_LIBRARY_PATH was defined to contain a colon-separated list
of directories, then these are searched. (As a security measure, this
variable is ignored for set-user-ID and set-group-ID programs.)
* (ELF only) If the executable file for the calling program contains
a DT_RUNPATH tag, then the directories listed in that tag are searched.
* The cache file /etc/ld.so.cache (maintianed by ldconfig(8)) is
checked to see whether it contains an entry for filename.
* The directories /lib and /usr/lib are searched (in that order).
---
If the object specified by filename has dependencies on other shared
objects, then these are also automatically loaded by the dynamic linker
using the same rules. (This process may occur recursively, if those
objects in turn have dependencies, and so on.)
One of the following two values must be included in flags:
* RTLD_LAZY: Perform lazy binding, Resolve symbols only as the code
that references them is executed. If the symbol is never referenced,
then it is never resolved. (Lazy binding is performed only for function
references; references to variables are always immediately bound when
the shared object is loaded.) Since glibc 2.1.1, this flag is
overridden by the effect of the LD_BIND_NOW environment variable.
* RTLD_NOW: If this value is sepcified, or the environment variable
LD_BIND_NOW is set to a nonempty string, all undefined symbols in the
shared object are resolved before dlopen() returns. If this cannot be
done, an error is returned.
---
Zero of more of the following values may also be ORed in flags:
* RTLD_GLOBAL: The symbols defined by this shared object will be made
available for symbol resolution of subsequently loaded shared objects.
* RTLD_LOCAL: This is the converse of RTLD_GLOBAL, and the default if
neither flag is specified. Symbols defined in this shared object are
not made available to resolve references in subsequently loaded shared
objects.
* RTLD_NODELETE (since glibc 2.2): Do not unload the shared object
during dlclose(). Consequently, the object's static variables are not
reinitialized if the object is reloaded with dlopen() at a later time.
* RTLD_NOLOAD (since glibc 2.2): Don't load the shared object. This can
be used to test if the object is already resident (dlopen() returns
NULL if it is not, or the object's handle if it is resident). This flag
can also be used to promote the flags on a shared object that is
already loaded. For example, a shared object that was previously loaded
with RTLD_LOCAL can be reopened with RTLD_NOLOAD|RTLD_GLOBAL.
* RTLD_DEEPBIND (since glibc 2.3.4): Place the lookup scope of the
symbols in this shared object ahead of the global scope. This means
that a self-contained object will use its own symbols in preference to
global symbols with the same name contained in objects that have
already been loaded.
---
If filename is NUL, then the returned handle is for the main program.
When given to dlsym(), this handle causes a search for a symbol in the
main program, followed by all shared objects loaded at program startup,
and then all shared objects loaded by dlopen() with the flag
RTLD_GLOBAL.
External references in the shared object are resolved using the shared
objects in that object's dependency list and any other objects
previously opened with the RTLD_GLOBAL flag. If the executable was
linked with the flag "-rdynamic" (or, synonymously, "--export-dynamic"),
then the global symbols in the executable will also be used to resolve
references in a dynamically loaded shared object.
If the same shared object is loaded again with dlopen(), the same object
handle is returned. The dynamic linker maintains reference counts for
object handles, so a dynamically loaded shared object is not deallocated
until dlclose() has been called on it as many times as dlopen() has
succeeded on it. Any initialization returns are called just once.
However, a subsequent dlopen() call that loads the same shared object
with RTLD_NOW may force symbol resolution for a shared object earlier
loaded with RTLD_LAZY.
If dlopen() fails for any reason, it returns NULL.
---
The function dlsym() takes a handle returned by dlopen(3) along with
a null-terminated symbol name, and returns the address where that
symbol is loaded into memory. If the symbol is not found, in the
specified object or any of the shared objects that were automatically
loaded by dlopen(3) when that object was loaded, dlsym() returns NULL.
Since the value of the synbol could actually be NULL, so that a NULL
return from dlsym() need not indicate an error. The correct way to
test for an error is to call dlerror(3) to clear any error conditions,
then call dlsym(), and then call dlerror(3) again, and check whether
dlerror() returned value is not NULL.
**********************************************************************/

#define l_dynlib_logerr(tag) { \
  char* errmsg = 0; \
  if ((errmsg = dlerror())) { \
    l_loge_1(tag " %s", ls(errmsg)); \
  } else { \
    l_loge_s(tag " fail"); \
  }}

L_EXTERN l_dynlib_handle
l_dynlib_open(l_strn path, l_strn name)
{
  l_dynlib_handle h = {0};
  l_filename fn;
  l_filename_init(&fn);

  /** void* dlopen(const char *filename, int flags) **
  If filename is NULL, then the returned handle is for the main
  program. If filename contains a slash ("/"), then it is interpreted
  as a (relative or absolute) pathname. Otherwise, the dynamic linker
  searches for the object. The fn.s is not NULL, and l_filename_addpath
  add the path separator "/" automatically, therefore dlopen here only
  lookup in the folder specified. **/

  if (l_filename_addpath(&fn, path) && l_filename_append(&fn, name)) {
    h.impl = dlopen(fn.s, RTLD_NOW);
    if (h.impl == 0) {
      l_dynlib_logerr("dlopen");
    }
  }

  return h;
}

L_EXTERN void
l_dynlib_close(l_dynlib_handle* handle)
{
  if (handle->impl) {
    if (dlclose(handle->impl) != 0) {
      l_dynlib_logerr("dlclose");
    }
    handle->impl = 0;
  }
}

L_EXTERN void*
l_dynlib_symbol(l_dynlib_handle* handle, l_strn symbol_name)
{
  void* symbol = 0;
  if (symbol_name.str && symbol_name.len > 0) {
    char* errstr = 0;
    dlerror(); /* clear old error */
    symbol = dlsym(handle->impl, symbol_name.str);
    if (symbol == 0 && (errstr = dlerror())) {
      l_loge_1("dlsym %s", ld(errstr));
    }
  }
  return symbol;
}

L_EXTERN int
l_thrhdl_create(l_thrhdl* thrhdl, void* (*start)(void*), void* para) {
  int n = pthread_create((pthread_t*)thrhdl, 0, start, para);
  if (n == 0) return true;
  l_loge_1("pthread_create %s", lserror(n));
  return false;
}

L_EXTERN int
l_thrhdl_cancel(l_thrhdl* thrhdl) {
  /* pthread_cancel - send a cancellation request to a thread
  #include <pthread.h>
  int pthread_cancel(pthread_t thread);
  On success, it returns 0; on error, it returns a nonzero
  error number */
  int n = pthread_cancel(*((pthread_t*)thrhdl));
  if (n == 0) return true;
  l_loge_1("pthread_cancel %s", lserror(errno));
  return false;
}

L_EXTERN l_thrhdl
l_thrhdl_self() {
  l_thrhdl thrhdl;
  *((pthread_t*)&thrhdl) = pthread_self();
  return thrhdl;
}

L_EXTERN int
l_thrhdl_join(l_thrhdl* thrhdl) {
  int n = 0;
  void* exitcode = 0;
  /* wait thread terminate, if already terminated then return
  immediately. the thread needs to be joinable. join a alreay
  joined thread will results in undefined behavior. */
  if ((n = pthread_join(*((pthread_t*)thrhdl), &exitcode)) != 0) {
    l_loge_1("pthread_join %s", lserror(n));
  }
  return (int)(l_int)exitcode;
}

L_EXTERN void
l_thrhdl_sleep(l_long us) {
  struct timespec req;
  req.tv_sec = (time_t)(us / 1000000);
  req.tv_nsec = (long)(us % 1000000 * 1000);
  if (nanosleep(&req, 0) != 0) {
    if (errno != EINTR) l_loge_1("nanosleep %s", lserror(errno));
  }
}

L_EXTERN void
l_thrhdl_exit() {
  pthread_exit((void*)1);
}

L_EXTERN void
l_thrkey_init(l_thrkey* self) {
  /** pthread_key_create - thread-specific data key creation **
  #include <pthread.h>
  int pthread_key_create(pthread_key_t* key, void (*destructor)(void*));
  If successful, it shall store the newly created key value at *key and
  shall return zero. Otherwise, an error number shall be returned to
  indicate the error. */
  int n = pthread_key_create((pthread_key_t*)self, 0);
  if (n != 0) l_loge_1("pthread_key_create %s", lserror(n));
}

L_EXTERN void
l_thrkey_free(l_thrkey* self) {
  /** pthread_key_delete - thread-specific data key deletion **
  #include <pthread.h>
  int pthread_key_delete(pthread_key_t key); */
  int n = pthread_key_delete(*(pthread_key_t*)self);
  if (n != 0) l_loge_1("pthread_key_delete %s", lserror(n));
}

L_EXTERN void
l_thrkey_setData(l_thrkey* self, const void* data) {
  /* different threads may bind different values to the same key, the value
  is typically a pointer to blocks of dynamically allocated memory that have
  been reserved for use by the calling thread. */
  int n = pthread_setspecific(*(pthread_key_t*)self, data);
  if (n != 0) l_loge_1("pthread_setspecific %s", lserror(n));
}

L_EXTERN void*
l_thrkey_getData(l_thrkey* self) {
  /** thread-specific data management **
  #include <pthread.h>
  void* pthread_getspecific(pthread_key_t key);
  int pthread_setspecific(pthread_key_t key, const void* value);
  No errors are returned from pthread_getspecific(), pthread_setspecific() may fail:
  ENOMEM - insufficient memory exists to associate the value with the key.
  EINVAL - the key value is invalid. */
  return pthread_getspecific(*(pthread_key_t*)self);
}

L_EXTERN void
l_mutex_init(l_mutex* self) {
  int n = pthread_mutex_init((pthread_mutex_t*)self, 0);
  if (n != 0) l_loge_1("pthread_mutex_init %s", lserror(n));
}

L_EXTERN void
l_mutex_free(l_mutex* self) {
  int n = pthread_mutex_destroy((pthread_mutex_t*)self);
  if (n != 0) l_loge_1("pthread_mutex_destroy %s", lserror(n));
}

L_EXTERN int
l_mutex_lock(l_mutex* self) {
  int n = pthread_mutex_lock((pthread_mutex_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_mutex_lock %s", lserror(n));
  return false;
}

L_EXTERN int
l_mutex_unlock(l_mutex* self) {
  int n = pthread_mutex_unlock((pthread_mutex_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_mutex_unlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_mutex_tryLock(l_mutex* self) {
  int n = pthread_mutex_trylock((pthread_mutex_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1("pthread_mutex_trylock %s", lserror(n));
  return false;
}

L_EXTERN void
l_rwlock_init(l_rwlock* self) {
  int n = pthread_rwlock_init((pthread_rwlock_t*)self, 0);
  if (n != 0) l_loge_1("pthread_rwlock_init %s", lserror(n));
}

L_EXTERN void
l_rwlock_free(l_rwlock* self) {
  int n = pthread_rwlock_destroy((pthread_rwlock_t*)self);
  if (n != 0) l_loge_1("pthread_rwlock_destroy %s", lserror(n));
}

L_EXTERN int
l_rwlock_rdlock(l_rwlock* self) {
  int n = pthread_rwlock_rdlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_rwlock_rdlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_wrlock(l_rwlock* self) {
  int n = pthread_rwlock_wrlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_rwlock_wrlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_unlock(l_rwlock* self) {
  int n = pthread_rwlock_unlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_rwlock_unlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_tryRead(l_rwlock* self) {
  int n = pthread_rwlock_tryrdlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1("pthread_rwlock_tryrdlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_tryWrite(l_rwlock* self) {
  int n = pthread_rwlock_trywrlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1("pthread_rwlock_trywrlock %s", lserror(n));
  return false;
}

L_EXTERN void
l_condv_init(l_condv* self) {
  int n = pthread_cond_init((pthread_cond_t*)self, 0);
  if (n != 0) l_loge_1("pthread_cond_init %s", lserror(n));
}

L_EXTERN void
l_condv_free(l_condv* self) {
  int n = pthread_cond_destroy((pthread_cond_t*)self);
  if (n != 0) l_loge_1("pthread_cond_destroy %s", lserror(n));
}

L_EXTERN int
l_condv_wait(l_condv* self, l_mutex* mutex) {
  int n = pthread_cond_wait((pthread_cond_t*)self, (pthread_mutex_t*)mutex);
  if (n == 0) return true;
  l_loge_1("pthread_cond_wait %s", lserror(n));
  return false;
}

L_EXTERN int
l_condv_timedWait(l_condv* self, l_mutex* mutex, l_long ns) {
  l_time curtime = l_system_time();
  struct timespec tm;
  int n = 0;

  /* caculate the absolute time */
  ns += curtime.nsec + curtime.sec *  L_NSEC_PERSEC;
  if (ns < 0) { ns = 0; l_loge_s("l_condv_timedWait invalid timeout value"); }
  tm.tv_sec = (time_t)(ns / L_NSEC_PERSEC);
  tm.tv_nsec = (long)(ns - tm.tv_sec * L_NSEC_PERSEC);

  /* int pthread_cond_timedwait(pthread_cond_t* cond,
  pthread_mutex_t* mutex, const struct timespec* abstime); */
  n = pthread_cond_timedwait((pthread_cond_t*)self, (pthread_mutex_t*)mutex, &tm);
  if (n == 0 || n == ETIMEDOUT) return true;
  l_loge_1("pthread_cond_timedwait %d", lserror(n));
  return false;
}

L_EXTERN int
l_condv_signal(l_condv* self) {
  /* pthread_cond_signal() shall unblock at least one of the threads
  that are blocked on the specified condition variable cond (if any
  threads are blocked on cond).
  pthread_cond_broadcast() and pthread_cond_signal() functions shall
  have no effect if there are no threads currently blocked on cond.
  It is not safe to use the pthread_cond_signal() function in a signal
  handler that is invoked asynchronously. Even if it were safe, there
  would still be a race between the test of the Boolean pthread_cond_wait()
  that could not be efficiently eliminated.
  Mutexes and condition variables are thus not suitable for releasing
  a waiting thread by signaling from code running in a signal handler. */
  int n = pthread_cond_signal((pthread_cond_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_cond_signal %s", lserror(n));
  return false;
}

L_EXTERN int
l_condv_broadcast(l_condv* self) {
  int n = pthread_cond_broadcast((pthread_cond_t*)self);
  if (n == 0) return true;
  l_loge_1("pthread_cond_broadcast %s", lserror(n));
  return false;
}

/** access, faccessat - check user's permission for a file **
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
**********************************************************************/

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

