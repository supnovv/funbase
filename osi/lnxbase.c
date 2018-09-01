#define LNLYLIB_API_IMPL
#include "osi/lnxdefs.h"
#include "osi/base.h"

/** The software clock, HZ, and jiffies **
The accuracy of various time related system calls is limited
by the resolution of the software clock, a clock maintained
by the kernel which measures time in jiffies. The size of a
jiffy is determined by the value of the kernel constant HZ.
The value of HZ varies across kernel versions and hardware
platforms. On i386 the situation is as follows: on kernels up
to and including 2.4.x, HZ was 100, giving a jiffy value of
0.01 seconds; starting with 2.6.0, HZ was raised to 1000,
giving a jiffy of 0.001 seconds. Since kernel 2.6.13, the HZ
value is a kernel configuration parameter and can be 100, 250
(the default) or 1000. Since kernel 2.6.20, a further frequency
is available: 300, a number that divides evently for the common
video frame rates (PAL, 25HZ; NTSC, 30HZ).
 ** High-resolution timers **
Before Linux 2.6.21, the accuraccy of timer and sleep system
calls was limited by the size of the jiffy.
Since Linux 2.6.21, Linux supports high-resolution timers (HRTs),
optionally configuration via CONFIG_HIGH_RES_TIMERS. On a system
that supports HRTs, the accuracy of sleep and timer system calls
is *no longer* constrained by the jiffy, but instead can be as
accurate as the hardware allows (microsecond accuracy is typical
of modern hardware). You can determine whether high-resolution
timers are supported by checking the resolution returned by a call
to clock_getres() or looking at the "resolution" entries in
/proc/timer_list.
HRTs are not supported on all hardware architectures. (Support is
provided on x86, arm, and powerpc, among others.)
 ** The Epoch and calendar time **
UNIX systems represent time in seconds since the Epoch, 1970/01/01
00:00:00 +0000 (UTC). A program can determine the calendar time
using gettimeofday(), which returns time (in seconds and microseconds)
that have elapsed since the Epoch; time() provides similar information,
but only with accuracy to the nearest second.
// Epoch in microseconds since 1970/01/01 00:00:00.
static const uint64_t EPOCH_DELTA = 0x00dcddb30f2f8000ULL;
在32位Linux系统中，time_t是一个有符号整数，可以表示的日期范围从
1901年12月13日20时45分52秒至2038年1月19日03:14:07（SUSv3未定义
time_t值为负值的含义）。因此当前许多32位UNIX系统都面临一个2038
年的理论问题，如果执行的计算涉及未来日期，那么问题会在2038年之前
就会遭遇。事实上，到了2038年可能所有UNIX系统都早已升级为64位或更
高位系统，这一问题也随之缓解。但32为嵌入式系统，由于其寿命较之台
式机硬件更长，故而仍然会受此问题困扰。此外对于依然以32位time_t格
式保存时间的历史数据和应用程序，仍然是一个问题。
 ** Sleeping, timer and timer slack **
Various system calls and functions (nanosleep, clock_nanosleep, sleep)
allow a program to sleep for a specified period of time. And various
system calls (alarm, getitimer, timerfd_create, timer_create) allow a
process to set a timer that expires at some point in the future, and
optionally at repeated intervals.
Since Linux 2.6.28, it is possible to control the "timer slack" value
for a thread. The timer slack is the length of time by which the kernel
may delay the wake-up of certain system calls that block with a timeout.
Permitting this delay allows the kernel to coalesce wake-up events, thus
possibly reducing the number of system wake-ups and saving power. For
more description of PR_SET_TIMERSLACK in prctl().
 ** Clock function on MAC OSX **
The function clock_gettime() is not available on OSX. The mach kernel
provides three clocks: SYSTEM_CLOCK returns the time since boot time,
CALENDAR_CLOCK returns the UTC time since 1970/01/01, REALTIME_CLOCK is
deprecated and is the same as SYSTEM_CLOCK in its current implementation.
The documentation for clock_get_time() on OSX says the clocks are
monotonically incrementing unless someone calls clock_set_time(). Calls
to clock_set_time() are discouraged as it could break the monotonic
property of the clocks, and in fact, the current implementation returns
KERN_FAILURE without doing anything.
stackoverflow.com/questions/11680461/monotonic-clock-on-osx
opensource.apple.com/source/xnu/xnu-344.2/osfmk/mach/clock_types.h
#include <mach/clock.h>
#include <mach/mach.h>
clock_serv_t cclock;
mach_timespec_t mts;
host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
clock_get_time(cclock, &mts);
mach_port_deallocate(mach_task_self(), cclock);
---
struct timespec {
  time_t tv_sec;  // seconds
  long tv_nsec; // nanoseconds
}; **/

static l_time
l_impl_get_time(clockid_t id)
{
  l_time time = {0};
  struct timespec spec = {0};

  if (clock_gettime(id, &spec) != 0) {
    l_loge_2(LNUL, "clock_gettime %d %s", ld(id), lserror(errno));
    return time;
  }

  time.sec = (l_long)spec.tv_sec;
  time.nsec = (l_umedit)spec.tv_nsec;
  time.zsec = 0;

  return time;
}

static l_long
l_impl_get_time_ms(clockid_t id)
{
  struct timespec spec;
  if (clock_gettime(id, &spec) != 0) {
    return 0;
  } else {
    return (l_long)(spec.tv_sec * 1000 + spec.tv_nsec / 1000000);
  }
}

L_EXTERN l_time
l_system_time()
{
  return l_impl_get_time(CLOCK_REALTIME);
}

L_EXTERN l_long
l_system_time_ms()
{
  return l_impl_get_time_ms(CLOCK_REALTIME);
}

L_EXTERN l_time
l_mono_time()
{
  /** clock_gettime **
  #include <sys/time.h>
  int clock_gettime(clockid_t id, struct timespec* out);
  @CLOCK_REALTIME时clock_gettime提供了与time函数相同的功能，不
  过当系统支持高精度时间时，clock_gettime返回精度更高的实时系统时间；
  System-wide clock that measures real (i.e., wall-clock) time.
  Setting this clock requires appropriate privilege. This clock is
  affected by discontinuous jumps in the system time (e.g., if the
  system adaministrator manually changes the clock), and by the
  incremental adjustments performed by adjtime and NTP.
  @CLOCK_MONOTONIC cannot be set and represents monotonic time
  since some unspecified starting point. This clock is not
  affected by discontinuous jumps in the system time, but is
  affected by the incremental adjustments performed by adjtime
  and NTP.
  @CLOCK_BOOTTIME (since Linux 2.6.39, Linux-specific) is
  identical to CLOCK_MONOTONIC, except it also includes any time
  that the system is suspended. This allows applications to get
  a suspend-aware monotonic clock without having to deal with
  the complications of CLOCK_REALTIME, which may have discontinuities
  if the time is changed using settimeofday or similar.
  @@OSX doesn't support this function */

#if defined(CLOCK_BOOTTIME)
  return l_impl_get_time(CLOCK_BOOTTIME);
#else
  return l_impl_get_time(CLOCK_MONOTONIC);
#endif
}

L_EXTERN l_long
l_mono_time_ms()
{
#if defined(CLOCK_BOOTTIME)
  return l_impl_get_time_ms(CLOCK_BOOTTIME);
#else
  return l_impl_get_time_ms(CLOCK_MONOTONIC);
#endif
}

L_EXTERN l_date
l_system_date()
{
  return l_date_from_time(l_system_time());
}

L_EXTERN l_date
l_date_from_msec(l_long utcmsec)
{
  l_long secs = utcmsec / 1000;
  l_date date = l_date_from_secs(secs);
  date.nsec = (utcmsec - secs * 1000) * 1000000;
  return date;
}

L_EXTERN l_date
l_date_from_secs(l_long utcsecs)
{
  struct tm st;
  l_date date = {0};
  time_t secs = utcsecs;
  l_zero_n(&st, sizeof(struct tm));

  /* struct tm* gmtime_r(const time_t* secs, struct tm* out);
  The function converts the time secs to broken-down time representation,
  expressed in Coordinated Universal Time (UTC), i.e. since Epoch.
  EPOCH_DELTA = 0x00dcddb30f2f8000; ms since 0000/01/01 to 1970-01-01 00:00:00 +0000
  struct tm {
    int tm_sec;    // Seconds (0-60)
    int tm_min;    // Minutes (0-59)
    int tm_hour;   // Hours (0-23)
    int tm_mday;   // Day of the month (1-31)
    int tm_mon;    // Month (0-11)
    int tm_year;   // Year - 1900
    int tm_wday;   // Day of the week (0-6, Sunday = 0)
    int tm_yday;   // Day in the year (0-365, 1 Jan = 0)
    int tm_isdst;  // Daylight saving time
  }; */

  if (gmtime_r(&secs, &st) != &st) { /* gmtime_r needs _POSIX_C_SOURCE >= 1 */
    l_loge_1(LNUL, "gmtime_r %s", lserror(errno));
    return date;
  }

  /* Single UNIX Specification 的以前版本允许双闰秒，于是tm_sec值的有效范围是
  0到61。但是UTC的正式定义不允许双闰秒，所以现在tm_sec值的有效范围是0到60。*/

  date.yhms = ((l_long)(st.tm_year + 1900) << 17); /* tm_year - number of years since 1900 */
  date.yhms |= ((((l_long)st.tm_hour) & 0x1f) << 12); /* tm_hour - from 0 to 23 */
  date.yhms |= ((((l_long)st.tm_min) & 0x3f) << 6); /* tm_min - from 0 to 59 */
  date.yhms |= (((l_long)st.tm_sec) & 0x3f); /* tm_sec - from 0 to 60, 60 can be leap second */

  /* in many implementations, including glibc, a 0 in tm_mday is
  interpreted as meaning the last day of the preceding month.
  这里的意思是，在将分解结构tm转换成time_t时（例如mktime），如果将
  tm_mday设为0则表示当前天数是前一个月的最后一天 */

  date.rest = ((((l_long)(0)) & 0x1f) << 21); /* timezone */
  date.rest |= ((((l_long)(st.tm_yday + 1)) & 0x1ff) << 12); /* tm_yday - from 0 to 365 */
  date.rest |= ((((l_long)(st.tm_wday)) & 0x07) << 9); /* tm_wday - from 0 to 6, 0 is Sunday */
  date.rest |= ((((l_long)(st.tm_mon + 1)) & 0x0f) << 5); /* tm_mon - from 0 to 11 */
  date.rest |= (((l_long)(st.tm_mday)) & 0x1f); /* tm_mday - from 1 to 31 */

  if (st.tm_isdst != 0) {
    /* daylight saving time is in effect. gmtime_r将时间转换成协调统一时间的分解
    结构，不会像localtime_r那样考虑本地时区和夏令时，该值应该在此处无效.
    A flag that indicates whether daylight saving time is in
    effect at the time described.  The value is positive if
    daylight saving time is in effect, zero if it is not, and
    negative if the information is not available. */
    l_logw_s(LNUL, "gmtime_r invalid tm_isdst");
  }

  return date;
}

L_EXTERN l_bool
l_timestamp_from_msec(l_long utcmsec, l_value* v)
{
  struct tm st;
  time_t secs = utcmsec / 1000;

  if (gmtime_r(&secs, &st) != &st) {
    return false;
  }

  v[0].d = st.tm_year + 1900;
  v[1].d = st.tm_mon + 1;
  v[2].d = st.tm_mday;
  v[3].d = st.tm_hour;
  v[4].d = st.tm_min;
  v[5].d = st.tm_sec;
  v[6].d = utcmsec - secs * 1000;
  return true;
}

#if 0
static l_strn l_weekday_abbr[] = {
  l_literal_strn("Sun"), l_literal_strn("Mon"), l_literal_strn("Tue"), l_literal_strn("Wed"),
  l_literal_strn("Thu"), l_literal_strn("Fri"), l_literal_strn("Sat"), l_literal_strn("Sun")
};

static l_strn l_month_abbr[] = {
  l_literal_strn("Nul"), l_literal_strn("Jan"), l_literal_strn("Feb"), l_literal_strn("Mar"),
  l_literal_strn("Apr"), l_literal_strn("May"), l_literal_strn("Jun"), l_literal_strn("Jul"),
  l_literal_strn("Aug"), l_literal_strn("Sep"), l_literal_strn("Oct"), l_literal_strn("Nov"), l_literal_strn("Dec")
};
#endif

L_EXTERN l_medit
l_utcsec_offset()
{
  /** struct tm* localtime_r(const time_t* timep, struct tm* result); **
  Take utc time seconds, convert it to local time structure **/

#if defined(ANDROID) || defined(L_PLAT_BSD)
  time_t t = time(0);
  struct tm local;

  if (localtime_r(&t, &local) != &local) {
    l_loge_1(LNUL, "localtime_r %s", lserror(errno));
    return 0;
  }

  if (st.tm_isdst != 0) {
    /* daylight saving time is in effect. gmtime_r将时间转换成协调统一时间的分解
    结构，不会像localtime_r那样考虑本地时区和夏令时，该值应该在此处无效.
    A flag that indicates whether daylight saving time is in
    effect at the time described.  The value is positive if
    daylight saving time is in effect, zero if it is not, and
    negative if the information is not available. */
    l_logw_s(LNUL, "localtime_r invalid tm_isdst");
  }

  /* long tm_gmtoff; // offset from UTC in seconds
  The field tm_gmtoff is the offset (in seconds) of the time represented
  from UTC, with positive values indicating locations east of the Prime
  Meridian. */

  return (l_medit)local.tm_gmtoff;
#else
  l_medit offset = 0;
  time_t utcsecs = 24 * 3600; /* 1970-01-02 00:00:00 +0000 */
  struct tm local;

  if (localtime_r(&utcsecs, &local) != &local) {
    l_loge_1(LNUL, "localtime_r %s", lserror(errno));
    return 0;
  }

  if (local.tm_isdst != 0) {
    /* daylight saving time is in effect. gmtime_r将时间转换成协调统一时间的分解
    结构，不会像localtime_r那样考虑本地时区和夏令时，该值应该在此处无效.
    A flag that indicates whether daylight saving time is in
    effect at the time described.  The value is positive if
    daylight saving time is in effect, zero if it is not, and
    negative if the information is not available. */
    l_logw_s(LNUL, "localtime_r invalid tm_isdst");
  }

  offset = (l_medit)(local.tm_hour * 3600 + local.tm_min * 60 + local.tm_sec);
  if (local.tm_mday < 2) {
    offset -= 24 * 3600;
  }

  return offset;
#endif
}

L_EXTERN l_date
l_date_from_time(l_time utctime)
{
  l_date date = l_date_from_secs(utctime.sec);
  date.nsec = utctime.nsec;
  return date;
}

/** file operations **/

L_EXTERN l_bool
l_file_exist(const void* name)
{
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
  On success, zero is returned; on error, -1 is returned and errno is set. **/

  return (name && faccessat(AT_FDCWD, (const char*)name, F_OK, AT_SYMLINK_NOFOLLOW) == 0);
}

L_EXTERN l_bool
l_file_exist_in(l_filehdl* dir, const void* name)
{
  if (l_filehdl_is_empty(dir) || name == 0) {
    return false;
  } else {
    return (faccessat(dir->fd, (const char*)name, F_OK, AT_SYMLINK_NOFOLLOW) == 0);
  }
}

L_EXTERN l_bool
l_file_attr(l_fileattr* attr, const void* name)
{
  struct stat st;
  if (lstat((const char*)name, &st) != 0) return false;
  attr->size = (l_long)st.st_size;
  attr->ctime = (l_long)st.st_ctime;
  attr->atime = (l_long)st.st_atime;
  attr->mtime = (l_long)st.st_mtime;
  attr->isfile = (l_byte)(S_ISREG(st.st_mode) != 0);
  attr->isdir = (l_byte)(S_ISDIR(st.st_mode) != 0);
  attr->islink = (l_byte)(S_ISLNK(st.st_mode) != 0);
  return true;
}

L_EXTERN l_bool
l_file_dir_exist(const void* dirname)
{
  if (dirname == 0) {
    return false;
  } else {
    struct stat st;
    return (lstat((const char*)dirname, &st) == 0 && S_ISDIR(st.st_mode));
  }
}

L_EXTERN l_bool
l_file_is_dir(const void* name)
{
  if (name == 0) {
    return false;
  } else {
    struct stat st;
    if (lstat((const char*)name, &st) != 0) {
      l_loge_1(LNUL, "lstat %s", lserror(errno));
      return false;
    } else {
      return (S_ISDIR(st.st_mode) != 0);
    }
  }
}

L_EXTERN l_filehdl
l_file_open_dir(const void* name)
{
  l_filehdl dir;
  dir.fd = open((const char*)name, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOATIME);
  return dir;
}

L_EXTERN l_dirstm
l_dirstm_open(const void* name)
{
  /** directory stream **
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
  external sychronization is still preferable to the use of readdir_r(). **/

  l_dirstm stm;
  if (name == 0) {
    stm.impl = 0;
  } else {
    stm.impl = opendir((const char*)name);
  }
  return stm;
}

L_EXTERN l_bool
l_dirstm_is_empty(const l_dirstm* stm)
{
  return stm->impl == 0;
}

L_EXTERN l_bool
l_dirstm_nt_empty(const l_dirstm* stm)
{
  return stm->impl != 0;
}

L_EXTERN void
l_dirstm_close(l_dirstm* stm)
{
  if (stm->impl == 0) {
    return;
  } else {
    if (closedir((DIR*)stm->impl) != 0) {
      l_loge_1(LNUL, "closedir %s", lserror(errno));
    }
    stm->impl = 0;
    return;
  }
}

L_EXTERN const l_byte*
l_dirstm_read(l_dirstm* stm)
{
  struct dirent* entry = 0;
  errno = 0;
  if ((entry = readdir((DIR*)stm->impl)) == 0) {
    if (errno != 0) l_loge_1(LNUL, "readdir %s", lserror(errno));
    return 0;
  }
  return l_strc(entry->d_name);
}

L_EXTERN const l_byte*
l_dirstm_read_x(l_dirstm* stm, int* isdir)
{
  struct dirent* entry = 0;
  if (isdir) *isdir = 0;
  errno = 0;
  if ((entry = readdir((DIR*)stm->impl)) == 0) {
    if (errno != 0) l_loge_1(LNUL, "readdir %s", lserror(errno));
    return 0;
  }
  if (isdir) {
#if defined(DT_DIR) && defined(DT_UNKNOWN)
    *isdir = (entry->d_type == DT_DIR);
    if (entry->d_type != DT_UNKNOWN) {
      *isdir = (entry->d_type == DT_DIR);
    } else {
      *isdir = l_file_is_dir(entry->d_name);
    }
#else
    *isdir = l_file_is_dir(entry->d_name);
#endif
  }
  return l_strc(entry->d_name);
}

/** dynamic library loading **
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
dlerror() returned value is not NULL. **/

#define l_dynhdl_logerr(tag) { \
  char* errmsg = 0; \
  if ((errmsg = dlerror())) { \
    l_loge_1(LNUL, tag " %s", ls(errmsg)); \
  } else { \
    l_loge_s(LNUL, tag " fail"); \
  }}

L_EXTERN l_bool
l_dynhdl_is_empty(l_dynhdl* hdl)
{
  return (hdl->impl == 0);
}

L_EXTERN l_bool
l_dynhdl_nt_empty(l_dynhdl* hdl)
{
  return (hdl->impl != 0);
}

L_EXTERN l_dynhdl
l_empty_dynhdl()
{
  l_dynhdl h;
  h.impl = 0;
  return h;
}

static l_dynhdl
l_impl_dynhdl_open(l_strn fullname)
{
  if (fullname.p && fullname.n > 0) {
    l_dynhdl hdl;
    char* errmsg = 0;
    dlerror(); /* clear old error */
    hdl.impl = dlopen((const char*)fullname.p, RTLD_NOW);
    if (hdl.impl == 0 && (errmsg = dlerror())) { /* if the library file is not exist, it is no error just return 0 */
      l_loge_1(LNUL, "dlopen %s", ls(errmsg));
    }
    return hdl;
  } else {
    return l_empty_dynhdl();
  }
}

L_EXTERN l_dynhdl
l_dynhdl_open(l_strn name)
{
  l_sbuf4k name_buf;
  l_strbuf* name_str = 0;

  name_str = l_sbuf4k_init_from(&name_buf, name);

  if (l_strbuf_write(name_str, l_literal_strn(".so")) > 0) {
    return l_impl_dynhdl_open(l_strbuf_strn(name_str));
  } else {
    return l_empty_dynhdl();
  }
}

L_EXTERN l_dynhdl
l_dynhdl_open_from(l_strn path, l_strn lib_name)
{
  l_sbuf4k buffer;
  l_strbuf* name = 0;

  /** void* dlopen(const char *filename, int flags) **
  If filename is NULL, then the returned handle is for the main
  program. If filename contains a slash ("/"), then it is interpreted
  as a (relative or absolute) pathname. Otherwise, the dynamic linker
  searches for the object. The fn.s is not NULL, and l_strbuf_add_path
  add the path separator "/" automatically, therefore dlopen here only
  lookup in the folder specified. **/

  name = l_sbuf4k_init(&buffer);

  if (l_strbuf_add_path(name, path) > 0 && l_strbuf_end_path_x(name, lib_name, l_literal_strn(".so")) > 0) {
    return l_impl_dynhdl_open(l_strbuf_strn(name));
  } else {
    return l_empty_dynhdl();
  }
}

L_EXTERN void*
l_dynhdl_load(l_dynhdl* hdl, l_strn sym_name)
{
  void* sym = 0;
  if (sym_name.p && sym_name.n > 0) {
    char* errstr = 0;
    dlerror(); /* clear old error */
    sym = dlsym(hdl->impl, (const char*)sym_name.p);
    if (sym == 0 && (errstr = dlerror())) {
      l_loge_1(LNUL, "dlsym %s", ls(errstr));
    }
  }
  return sym;
}

L_EXTERN void
l_dynhdl_close(l_dynhdl* hdl)
{
  if (hdl->impl) {
    if (dlclose(hdl->impl) != 0) {
      l_dynhdl_logerr("dlclose");
    }
    hdl->impl = 0;
  }
}

/** concurrency **/

L_EXTERN int
l_thrhdl_create(l_thrhdl* thrhdl, void* (*start)(void*), void* para)
{
  int n = pthread_create((pthread_t*)thrhdl, 0, start, para);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_create %s", lserror(n));
  return false;
}

L_EXTERN int
l_thrhdl_cancel(l_thrhdl* thrhdl)
{
  /* pthread_cancel - send a cancellation request to a thread
  #include <pthread.h>
  int pthread_cancel(pthread_t thread);
  On success, it returns 0; on error, it returns a nonzero
  error number */
  int n = pthread_cancel(*((pthread_t*)thrhdl));
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_cancel %s", lserror(errno));
  return false;
}

L_EXTERN l_thrhdl
l_thrhdl_self()
{
  l_thrhdl thrhdl;
  *((pthread_t*)&thrhdl) = pthread_self();
  return thrhdl;
}

L_EXTERN int
l_thrhdl_join(l_thrhdl* thrhdl)
{
  int n = 0;
  void* exitcode = 0;
  /* wait thread terminate, if already terminated then return
  immediately. the thread needs to be joinable. join a alreay
  joined thread will results in undefined behavior. */
  if ((n = pthread_join(*((pthread_t*)thrhdl), &exitcode)) != 0) {
    l_loge_1(LNUL, "pthread_join %s", lserror(n));
  }
  return (int)(l_int)exitcode;
}

L_EXTERN void
l_thread_sleep_us(l_long us)
{
  struct timespec req;
  req.tv_sec = (time_t)(us / 1000000);
  req.tv_nsec = (long)(us % 1000000 * 1000);
  if (nanosleep(&req, 0) != 0) {
    if (errno != EINTR) l_loge_1(LNUL, "nanosleep %s", lserror(errno));
  }
}

L_EXTERN void
l_thread_sleep_ms(l_long ms)
{
  l_thread_sleep_us(ms * 1000);
}

L_EXTERN void
l_thread_exit()
{
  pthread_exit((void*)1);
}

L_EXTERN void
l_thrkey_init(l_thrkey* self)
{
  /** pthread_key_create - thread-specific data key creation **
  #include <pthread.h>
  int pthread_key_create(pthread_key_t* key, void (*destructor)(void*));
  If successful, it shall store the newly created key value at *key and
  shall return zero. Otherwise, an error number shall be returned to
  indicate the error. */
  int n = pthread_key_create((pthread_key_t*)self, 0);
  if (n != 0) l_loge_1(LNUL, "pthread_key_create %s", lserror(n));
}

L_EXTERN void
l_thrkey_free(l_thrkey* self)
{
  /** pthread_key_delete - thread-specific data key deletion **
  #include <pthread.h>
  int pthread_key_delete(pthread_key_t key); */
  int n = pthread_key_delete(*(pthread_key_t*)self);
  if (n != 0) l_loge_1(LNUL, "pthread_key_delete %s", lserror(n));
}

L_EXTERN void
l_thrkey_set_data(l_thrkey* self, const void* data)
{
  /* different threads may bind different values to the same key, the value
  is typically a pointer to blocks of dynamically allocated memory that have
  been reserved for use by the calling thread. */
  int n = pthread_setspecific(*(pthread_key_t*)self, data);
  if (n != 0) l_loge_1(LNUL, "pthread_setspecific %s", lserror(n));
}

L_EXTERN void*
l_thrkey_get_data(l_thrkey* self)
{
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
l_mutex_init(l_mutex* self)
{
  int n = pthread_mutex_init((pthread_mutex_t*)self, 0);
  if (n != 0) l_loge_1(LNUL, "pthread_mutex_init %s", lserror(n));
}

L_EXTERN void
l_mutex_free(l_mutex* self)
{
  int n = pthread_mutex_destroy((pthread_mutex_t*)self);
  if (n != 0) l_loge_1(LNUL, "pthread_mutex_destroy %s", lserror(n));
}

L_EXTERN int
l_mutex_lock(l_mutex* self)
{
  int n = pthread_mutex_lock((pthread_mutex_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_mutex_lock %s", lserror(n));
  return false;
}

L_EXTERN int
l_mutex_unlock(l_mutex* self)
{
  int n = pthread_mutex_unlock((pthread_mutex_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_mutex_unlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_mutex_try_lock(l_mutex* self)
{
  int n = pthread_mutex_trylock((pthread_mutex_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1(LNUL, "pthread_mutex_trylock %s", lserror(n));
  return false;
}

L_EXTERN void
l_rwlock_init(l_rwlock* self)
{
  int n = pthread_rwlock_init((pthread_rwlock_t*)self, 0);
  if (n != 0) l_loge_1(LNUL, "pthread_rwlock_init %s", lserror(n));
}

L_EXTERN void
l_rwlock_free(l_rwlock* self)
{
  int n = pthread_rwlock_destroy((pthread_rwlock_t*)self);
  if (n != 0) l_loge_1(LNUL, "pthread_rwlock_destroy %s", lserror(n));
}

L_EXTERN int
l_rwlock_rdlock(l_rwlock* self)
{
  int n = pthread_rwlock_rdlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_rwlock_rdlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_wrlock(l_rwlock* self)
{
  int n = pthread_rwlock_wrlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_rwlock_wrlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_unlock(l_rwlock* self)
{
  int n = pthread_rwlock_unlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_rwlock_unlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_try_read(l_rwlock* self)
{
  int n = pthread_rwlock_tryrdlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1(LNUL, "pthread_rwlock_tryrdlock %s", lserror(n));
  return false;
}

L_EXTERN int
l_rwlock_try_write(l_rwlock* self)
{
  int n = pthread_rwlock_trywrlock((pthread_rwlock_t*)self);
  if (n == 0) return true;
  if (n != EBUSY) l_loge_1(LNUL, "pthread_rwlock_trywrlock %s", lserror(n));
  return false;
}

L_EXTERN void
l_condv_init(l_condv* self)
{
  int n = pthread_cond_init((pthread_cond_t*)self, 0);
  if (n != 0) l_loge_1(LNUL, "pthread_cond_init %s", lserror(n));
}

L_EXTERN void
l_condv_free(l_condv* self)
{
  int n = pthread_cond_destroy((pthread_cond_t*)self);
  if (n != 0) l_loge_1(LNUL, "pthread_cond_destroy %s", lserror(n));
}

L_EXTERN int
l_condv_wait(l_condv* self, l_mutex* mutex)
{
  int n = pthread_cond_wait((pthread_cond_t*)self, (pthread_mutex_t*)mutex);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_cond_wait %s", lserror(n));
  return false;
}

L_EXTERN int
l_condv_timed_wait(l_condv* self, l_mutex* mutex, l_long ns)
{
  l_time curtime = l_system_time();
  struct timespec tm;
  int n = 0;

  /* caculate the absolute time */
  ns += curtime.nsec + curtime.sec *  L_NSEC_PERSEC;
  if (ns < 0) { ns = 0; l_loge_1(LNUL, "EINVAL %d", ld(ns)); }
  tm.tv_sec = (time_t)(ns / L_NSEC_PERSEC);
  tm.tv_nsec = (long)(ns - tm.tv_sec * L_NSEC_PERSEC);

  /* int pthread_cond_timedwait(pthread_cond_t* cond,
  pthread_mutex_t* mutex, const struct timespec* abstime); */
  n = pthread_cond_timedwait((pthread_cond_t*)self, (pthread_mutex_t*)mutex, &tm);
  if (n == 0 || n == ETIMEDOUT) return true;
  l_loge_1(LNUL, "pthread_cond_timedwait %d", lserror(n));
  return false;
}

L_EXTERN int
l_condv_signal(l_condv* self)
{
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
  l_loge_1(LNUL, "pthread_cond_signal %s", lserror(n));
  return false;
}

L_EXTERN int
l_condv_broadcast(l_condv* self)
{
  int n = pthread_cond_broadcast((pthread_cond_t*)self);
  if (n == 0) return true;
  l_loge_1(LNUL, "pthread_cond_broadcast %s", lserror(n));
  return false;
}

/** socket **/

L_EXTERN l_bool
l_sockaddr_init(l_sockaddr* sockaddr, l_strn ip, l_ushort port)
{
  /** inet_pton htons/l ntohs/l **
  #include <arpa/inet.h> // some systems require <netinet/in.h> instead of <arpa/inet.h>
  int inet_pton(int family, const char* str, void* dest); // convert ipv4 and ipv6 from text to binary
  uint32_t htonl(uint32_t hostlong);  // host to net (32-bit)
  uint16_t htons(uint16_t hostshort); // host to net (16-bit)
  uint32_t ntohl(uint32_t netlong);   // net to host (32-bit)
  uint16_t ntohs(uint16_t netshort);  // net to host (16-bit)
  AF_INET - src contains an ipv4 network address in dotted-decimal format,
  "ddd.ddd.ddd.ddd", where ddd is a decimal number of up to three digits in
  the range 0 to 255. The address is convered to a struct in_addr and copied
  to dest, which must be sizeof(struct in_addr) long.
  AF_INET6 - src contains an ipv6 network address. The address is cnverted to
  and struct in6_addr and copied to dest, which must be sizeof(struct in6_addr)
  long. The allowed formats for ipv6 addresses are: x:x:x:x:x:x:x:x, each x can
  be up to 4 hex digits; A series of contiguous zero values in the preferred
  format can be abbreviated to ::, only one instance of :: can occur in an
  address. For example, the loopback address 0:0:0:0:0:0:0:1 can be abbreviated
  as ::1, the wildcard address, consisting of all zeros, can be written as ::;
  An alternate format is useful for expressing ipv4-mapped ipv6 address. This
  form is written as x:x:x:x:x:x:d.d.d.d, an example of such an address is
  ::FFFF:204.152.189.116.
  inet_pton() returns 1 on success, 0 is returned if src does not contain a
  character string representing a valid network in the specified address family.
  If family does not contain a valid address family, -1 is returned and errno is
  set to EAFNOSUPPORT. ***********************************************/
  if (l_strn_is_empty(&ip)) {
    return false;
  } else {
    int retn = 0;
    l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)sockaddr;
    l_zero_n(sa, sizeof(l_impl_lnxsaddr));
    if (l_strn_has(&ip, ':')) {
      retn = inet_pton(AF_INET6, (const char*)ip.p, &(sa->addr.sa6.sin6_addr));
      if (retn == 1) {
        sa->len = sizeof(struct sockaddr_in6);
        sa->addr.sa6.sin6_family = AF_INET6;
        sa->addr.sa6.sin6_port = htons(port);
        return true;
      }
    } else {
      retn = inet_pton(AF_INET, (const char*)ip.p, &(sa->addr.sa4.sin_addr));
      if (retn == 1) {
        sa->len = sizeof(struct sockaddr_in);
        sa->addr.sa4.sin_family = AF_INET;
        sa->addr.sa4.sin_port = htons(port);
        return true;
      }
    }
    if (retn == 0) {
      l_loge_1(LNUL, "inet_pton EINVAL ip %strn", lstrn(&ip));
    } else {
      l_loge_1(LNUL, "inet_pton %s", lserror(errno));
    }
    return false;
  }
}

L_EXTERN l_bool
l_sockaddr_init_from(l_sockaddr* sockaddr, const l_bin_ip* addr)
{
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)sockaddr;
  l_zero_n(sa, sizeof(l_impl_lnxsaddr));
  switch (addr->type) {
  case L_IPV4_ADDR:
    sa->addr.sa4.sin_addr.s_addr = ((((l_umedit)addr->a[0]) << 24) | (((l_umedit)addr->a[1]) << 16) | (((l_umedit)addr->a[2]) << 8) | addr->a[3]);
    sa->len = sizeof(struct sockaddr_in);
    sa->addr.sa4.sin_port = htons(addr->port);
    sa->addr.sa4.sin_family = AF_INET;
    return true;
  case L_IPV6_ADDR:
    l_copy_n(sa->addr.sa6.sin6_addr.s6_addr, addr->a, 16);
    sa->len = sizeof(struct sockaddr_in6);
    sa->addr.sa6.sin6_port = htons(addr->port);
    sa->addr.sa6.sin6_family = AF_INET6;
    return true;
  default:
    l_loge_1(LNUL, "create sockaddr from invalid family %d", ld(addr->type));
    break;
  }
  return false;
}

L_EXTERN l_sockaddr
l_sockaddr_local(l_socket sock)
{
  /** getsockname **
  #include <sys/socket.h>
  int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
  returns the current address the sock is bound. the addrlen should be
  initialized to indicate the amount of space in bytes pointed to by addr.
  on return it contains the actual size of the socket address.
  the returned address is truncated if the buffer provided is too small; in
  this case, addrlen will return a value greater than was supplied to the call. */
  l_sockaddr sockaddr;
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)&sockaddr;
  socklen_t provided_len = sizeof(struct sockaddr_in6);
  sa->len = provided_len;
  if (getsockname(sock.fd, &(sa->addr.sa), &(sa->len)) != 0) {
    l_loge_1(LNUL, "getsockname %s", lserror(errno));
    sa->len = 0;
    sa->addr.sa.sa_family = 0;
  } else {
    if (sa->len > provided_len) {
      l_loge_s(LNUL, "getsockname address truncated");
      sa->len = provided_len;
    }
  }
  return sockaddr;
}

static l_ushort
l_sockaddr_family(const l_sockaddr* self)
{
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)self;
  return sa->addr.sa.sa_family;
}

L_EXTERN l_ushort
l_get_sock_port(const l_sockaddr* addr)
{
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)addr;
  return ntohs(sa->addr.sa4.sin_port);
}

L_EXTERN l_bin_ip
l_get_binary_ip(const l_sockaddr* addr)
{
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)addr;
  l_ushort family = sa->addr.sa.sa_family;
  l_bin_ip ip;
  ip.port = l_get_sock_port(addr);
  if (family == AF_INET) {
    l_byte* p = (l_byte*)(&sa->addr.sa4.sin_addr.s_addr); /* network byte-order */
    ip.type = L_IPV4_ADDR;
    ip.a[0] = p[0]; /* high byte at lower address */
    ip.a[1] = p[1];
    ip.a[2] = p[2];
    ip.a[3] = p[3];
  } else if (family == AF_INET6) {
    l_byte* p = sa->addr.sa6.sin6_addr.s6_addr;
    ip.type = L_IPV6_ADDR;
    l_copy_n(ip.a, p, 16); /* network byte-order */
  } else {
    ip.type = 0;
    ip.a[0] = 0;
  }
  return ip;
}

L_EXTERN l_bool
l_bin_ip_init(l_bin_ip* addr, const void* ip, l_ushort port)
{
  l_sockaddr sa;
  if (l_sockaddr_init(&sa, l_strn_c(ip), port)) {
    *addr = l_get_binary_ip(&sa);
    return true;
  } else {
    return false;
  }
}

L_EXTERN l_str_ip
l_get_string_ip(const l_sockaddr* addr)
{
  /** inet_ntop - convert ipv4 and ipv6 addresses from binary to text form **
  #include <arpa/inet.h>
  const char* inet_ntop(int family, const void* src, char* dest, socklen_t size);
  This function converts the network address src in the family into a character
  string. The resulting is copied to the buffer pointed to by dest, which must
  be a non-null pointer. The caller specifies the number of bytes available in
  this buffer in size.
  AF_INET - src points to a struct in_addr (in network byte order) which is
  converted to an ipv4 network address in the dotted-decimal format, "ddd.ddd.ddd.ddd".
  The buffer dest must be at least INET_ADDRSTRLEN bytes long.
  AF_INET6 - src points to a struct in6_addr (in network byte order) which is
  converted to a representation of this address in the most appropriate ipv6
  network address format for this address. The buffer dst must be at least
  INET6_ADDRSTRLEN bytes long.
  On success, inet_ntop() returns a non-null pointer to dst. NULL is returned if
  there was an error, with errno set to indicate the error. */

  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)addr;
  l_str_ip buffer;
  l_byte* out = buffer.s;

  l_assert(LNUL, sizeof(l_str_ip) >= INET6_ADDRSTRLEN);

  if (sa->addr.sa.sa_family == AF_INET) {
      if (inet_ntop(AF_INET, &(sa->addr.sa4.sin_addr), (char*)out, INET_ADDRSTRLEN) != 0) {
        return buffer;
      } else {
        l_loge_1(LNUL, "inet_ntop 4 %s", lserror(errno));
      }
  } else if (sa->addr.sa.sa_family == AF_INET6) {
      if (inet_ntop(AF_INET6, &(sa->addr.sa6.sin6_addr), (char*)out, INET6_ADDRSTRLEN) != 0) {
        return buffer;
      } else {
        l_loge_1(LNUL, "inet_ntop 6 %s", lserror(errno));
      }
  } else {
    l_loge_s(LNUL, "inet_ntop invalid family");
  }

  return buffer;
}

L_EXTERN l_str_ip
l_get_string_ip_from(const l_bin_ip* ip)
{
  l_str_ip s;
  l_stropt b = l_get_stropt(s.s, sizeof(s), 0);
  l_ostream os = l_stropt_ostream(&b);
  switch (ip->type) {
  case L_IPV4_ADDR:
    l_ostream_format_4(&os, "%d.%d.%d.%d", ld(ip->a[0]), ld(ip->a[1]), ld(ip->a[2]), ld(ip->a[3]));
    break;
  case L_IPV6_ADDR:
    l_ostream_format_8(&os, "%.4zx:%.4zx:%.4zx:%.4zx:%.4zx:%.4zx:%.4zx:%.4zx",
        lx((((l_ushort)ip->a[0]) << 8) | ip->a[1]), lx((((l_ushort)ip->a[2]) << 8) | ip->a[3]),
        lx((((l_ushort)ip->a[4]) << 8) | ip->a[5]), lx((((l_ushort)ip->a[6]) << 8) | ip->a[7]),
        lx((((l_ushort)ip->a[8]) << 8) | ip->a[9]), lx((((l_ushort)ip->a[10]) << 8) | ip->a[11]),
        lx((((l_ushort)ip->a[12]) << 8) | ip->a[13]), lx((((l_ushort)ip->a[14]) << 8) | ip->a[15]));
    break;
  default:
    s.s[0] = 0;
    break;
  }
  return s;
}

static l_bool
l_set_non_block(int fd)
{
  /** fcntl - manipulate file descriptor **
  #include <unistd.h>
  #include <fcntl.h>
  int fcntl(int fd, int cmd, ...);
  On error, this function returns -1, and errno is set appropriately.
  File status flags: each open fd has certain associated status flags,
  initialized by open(2) and possibly modified by fcntl(). duplicated
  fds refer to the same open fd, and thus share the same file status
  flags. The file status flags and their semantics are described in open(2).
  F_GETFL (void) - return (as the function result) the file access mode
  and the file status flags, arg is ignored.
  F_SETFL (int) - set the file status flags to the value specified by arg.
  File access mode (O_RDONLY, O_WRONLY, O_RDWR) and file creation flags (i.e.,
  O_CREATE,O_EXCL, O_NOCTTY, O_TRUNC) in arg are ignored. On linux, this
  command can change only the O_APPEND, O_ASYNC, O_DIRECT, O_NOATIME, and
  O_NONBLOCK flags. It is not possible to change the O_DSYNC and O_SYNC flags. */
  int flag = 0;
  if ((flag = fcntl(fd, F_GETFL)) == -1) {
    l_loge_1(LNUL, "fcntl getfl %s", lserror(errno));
    return false;
  } else {
    if (fcntl(fd, F_SETFL, flag | O_NONBLOCK) == -1) {
      l_loge_1(LNUL, "fcntl setfl %s", lserror(errno));
      return false;
    } else {
      return true;
    }
  }
}

static l_socket
l_socket_create(int domain, int type, int protocol)
{
  /** socket - create an endpoint for communication **
  #include <sys/types.h>
  #include <sys/socket.h>
  int socket(int domain, int type, int protocol);
  It returns a file descriptor that refers to the communication endpoint.
  The domain specifies a communication domain, it selects the protocol
  family which will be used for communication.
  AF_UNIX, AF_LOCAL - local communication - man unix(7)
  AF_INET - ipv4 internet protocols - man ip(7)
  AF_INET6 - ipv6 internet protocols - man ipv6(7)
  AF_NETLINK - kernel user interface device - man netlink(7)
  AF_APPLETALK - AppleTalk - man ddp(7), ...
  ---
  The socket 'type' specifies the communication semantics.
  SOCK_STREAM - provides sequenced, reliable, two-way, connection-based
  byte streams. an out-of-band data transmission mechanism may be supported.
  SOCK_DGRAM - supports datagrams (connectionless, unreliable messages
  of a fixed maximum length).
  SOCK_SEQPACKET - provides a sequenced, reliable, two-way connection-based
  data transmission path for datagrams of fixed maximum length; a consumer
  is required to read an entire packet with each input system call.
  SOCK_RAW - provides raw network protocol access.
  SOCK_RDM - provides a reliable datagram layer that does not guarantee
  ordering.
  SOCK_PACKET - obsolete and should not be used. see packet(7).
  Since Linux 2.6.27, the 'type' argument serves a second purpose: in
  addition to specifying a socket type, it may include the bitwise OR
  of any of the following values, to modify the behavior of socket().
  SOCK_NONBLOCK - set the O_NONBLOCK file status flag on the new open fd.
  using this flag saves extra calls to fcntl(2) to achieve the same result.
  SOCK_CLOEXEC - set the close-on-exec (FD_CLOEXEC) flag on the new fd.
  see the description of the O_CLOEXEC flag in open(2) for reasons.
  The SOCK_NONBLOCK and SOCK_CLOEXEC flags are Linux-specific.
  ---
  The 'protocol' specifies a particular protocol to be used with the socket.
  Normally only a single protocol exists to support a particular socket type
  within a given protocol family, in which case protocol can be specified
  as 0. However, it is possible that many protocols may exist, in which case
  a particular protocol must be specified in this manner. protocol number is
  defined in http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml,
  and also can be seen in '/etc/protocols'.
  ---
  The communications protocols which implement a SOCK_STREAM ensure that data
  is not lost and duplicated. If a piece of data for which the peer protocol
  has buffer space cannot be successfully thransmitted within a reasonable length
  of time, then the connection is considered to be dead. When SO_KEEPALIVE is
  enabled on the socket the protocol checks in a protocol-specific manner if the
  other end is still alive. A SIGPIPE signal is raised if a process sends or
  receives on a broken stream; this causes naive processes, which do not handle
  the signal, to exit. SOCK_SEQPACKET sockets employ the same system calls as
  SOCK_STREAM sockets. The only difference is that read(2) calls will return
  only the amount of data requested, and any data remaining in the arriving packet
  will be discarded. Also all message boundaries in incoming datagrams are preserved.
  SOCK_DGRAM and SOCK_RAW sockets allow sending of datagrams to correspondents
  named in sendto(2) calls. Datagrams are generally received with recvfrom(2), which
  returns the next datagram along with the address of its sender.
  ---
  An fcntl(2) F_SETOWN operation can be used to specify a process or process
  group to receive a SIGURG signal when the out-of-band data arrives or SIGPIPE
  signal when a SOCK_STREAM connection breaks unexpectedly. This operation may
  also be used to set the process or process group that receives the I/O and
  asynchronous notification of I/O events via SIGIO. Using F_SETOWN is equivalent
  to an ioctl(2) call with the FIOSETOWN or SIOCSPGRP argument.
  When the network signals an error condition to the protocol module (e.g., using
  an ICMP message for IP) the pending error flag is set for the socket. The next
  operation on this socket will return the error code of the pending error. For
  some protocols it is possible to enable a per-socket error queue to retrieve
  detailed information about the error; see IP_RECVERR in ip(7).
  ---
  On success, a fd for the new socket is returned. On error, -1 is returned,
  and errno is set appropriately.
  EACCES - permission to create a socket of the specified type/protocol is denied
  EAFNOSUPPORT - doesn't support the specified address family.
  EINVAL - unknown protocol, or protocol family not available, or invalid falgs in type.
  EMFILE - the per-process limit on the number of open fds has been reached.
  ENFILE - the system-wide limit on the total number of open files has been reached.
  ENOBUFS or ENOMEM - insufficient memory is available.
  EPROTONOSUPPORT - the protocol type or the specified protocol is not supported.
  Other errors may be generated by the underlying protocol modules. */
  l_socket sock;
  if ((sock.fd = socket(domain, type, protocol)) == -1) {
    l_loge_1(LNUL, "socket %s", lserror(errno));
  } else {
    l_set_non_block(sock.fd);
  }
  return sock;
}

L_EXTERN void
l_filehdl_close(l_filehdl* hdl)
{
  /** close - close a file descriptor **
  #include <unistd.h>
  int close (int fd);
  It closes a fd, so that it no longer refers to any file and may be reused.
  Any record locks held on the file it was associated with, and owned by the
  process, are removed (regardless of the fd that was used to obtain the lock).
  If fd is the last fd referring to the underlying open fd, the resources
  associated with the open fd are freed.
  It returns zero on success. On error, -1 is returned, and errno is set.
  EBADF - fd isn't a valid open fd.
  EINTR - the close() call was interrupted by a signal.
  EIO - an I/O error occurred.
  close() should not be retried after an error. Retrying the close() after a
  failure return is the wrong thing to do, since this may cause a reused fd
  from another thread to be closed. This can occur because the Linux kernel
  always releases the fd early in the close operation, freeing it for resue.
  关闭一个TCP套接字的默认行为是把该套接字标记为已关闭，然后立即返回到调用进程。
  该套接字不能再由调用进程使用，也就是说不能再作为read或write的第一个参数。然而
  TCP将尝试发送已排队等待发送到对端的任何数据，发送完毕后发生正常的TCP断连操作。
  我们将在7.5节介绍的SO_LINGER套接字选项可以用来改变TCP套接字的这种默认行为。我
  们将在那里介绍TCP应用进程必须怎么做才能确信对端应用进程已收到所有排队数据。
  关闭套接字只是导致相应描述符的引用计数减1，如果引用计数仍大于0，这个close调用
  并不会引发TCP断连流程。如果我们确实想在某个TCP连接上发送一个FIN，可以调用
  shutdown函数以代替close()，我们将在6.5节阐述这么做的动机。*/
  if (hdl->fd != -1) {
    if (close(hdl->fd) != 0) {
      l_loge_1(LNUL, "close filehdl %s", lserror(errno));
    }
    hdl->fd = -1;
  }
}

L_EXTERN void
l_socket_close(l_socket* sock)
{
  l_filehdl_close(sock);
}

L_EXTERN void
l_socket_shutdown(l_socket* sock, l_byte r_w_a)
{
  /** shutdown - shut down part of a full-duplex connection **
  #include <sys/socket.h>
  int shutdown(int sockfd, int how);
  This call causes all or part of a full-duplex connection on the socket
  associated with sockfd to be shut down. If how is SHUT_RD, further
  receptions will be disallowed. If how is SHUT_WR, further transmissions
  will be disallowed. If how is SHUT_RDWR, further receptions and
  transmissions will be disallowed.
  On success, zero is returned. On error, -1 is returned, and errno is set.
  EBADF - sockfd is not a valid fd.
  EINVAL - an invalid value was specified in how.
  ENOTCONN - the specified socket is not connected.
  ENOTSOCK - the sockfd is not refer to a socket. */
  int flag = 0;
  switch (r_w_a) {
  case 'r': case 'R': flag = SHUT_RD; break;
  case 'w': case 'W': flag = SHUT_WR; break;
  case 'a': case 'A': flag = SHUT_RDWR; break;
  default: l_loge_s(LNUL, "shutdone EINVAL"); break;
  }
  if (shutdown(sock->fd, flag) != 0) {
    l_loge_1(LNUL, "shutdown %s", lserror(errno));
  }
}

static l_bool
l_impl_socket_bind(int sock, const l_sockaddr* addr)
{
  /** bind - bind a address to a socket **
  #include <sys/types.h>
  #include <sys/socket.h>
  int bind(int sockfd, const struct sockaddr* addr, socklen_t addrlen);
  It is normally necessary to assign a local address using bind() before
  SOCK_STREAM socket may receive connections. The actual structure passed
  to the addr argument will depend on the address family. The only purpose
  of struct sockaddr is to do the cast.
  On success, zero is returned. On error, -1 is returned and errno is set.
  EACCES - the address is portected, and the user is not the superuser.
  EADDRINUSE - the given address is already in use.
  EBADF - sockfd is not a valid fd.
  EINVAL - the socket is already bound to an address, or addrlen is wrong
  addr is not a valid address for this socket's domain.
  ENOTSOCK - the sockfd is not refer to a socket.
  And there are other errors specific to UNIX domain (AF_UNIX) sockets.
  如果一个TCP客户或服务器未曾调用bind绑定一个端口，当使用connect或listen
  时，内核会为相应的套接字选择一个临时端口。当然这对于TCP服务器是罕见的，
  服务器通常会绑定一个众所周知的端口。一个例外是RPC服务器，它们通常由内核
  为它们的监听套接字选择一个临时端口，然后该端口会通过RPC端口映射器进行注
  册。客户在connect这些服务器之前，必须与端口映射器联系以获取它们的临时
  端口。如果由内核选择临时端口，需要调用函数getsockname()来获取。
  进程可以把一个特定的IP地址绑定到它的套接字上，这个IP地址必须属于其所在
  主机的网络接口之一。对于TCP客户，这相当于给在该套接字上发送的IP数据报
  指派了源IP地址；但客户端通常不绑定IP地址，当连接套接字时，内核根据所用
  外出网络接口来选择源IP地址，而所用外出接口则取决于到达服务器所需路径（见
  TCPv2第737页）。对于TCP服务器，这就限定该套接字只接收那些目的地址为这个
  IP地址的客户连接；如果服务器不绑定IP地址，内核就把客户发送的SYN的目的IP
  地址作为服务器的源IP地址（TCPv2第943页）。
  我们必须区分分组的的到达地址和该分组的目的IP地址，一般大多数实现都使用弱
  端系统模型，这意味着一个分组只要其目的IP地址能够匹配目的主机的某个网络接
  口就行，这个目的地址不一定是它的到达地址。绑定非通配IP地址可以根据数据包
  使用的目的IP地址来确定接下来发送数据包的源IP地址，而对到达接口不做限制，
  除非主机采用强端系统模型。
  如果绑定的IP地址是通配地址，就是告诉系统要是系统是多缩主机（有多个网络接口），
  我们将接受目的地址为任何本地接口的连接。如果绑定的端口为0则系统会自动分配
  一个临时端口使用。如果自行设置一个临时端口使用，它应该比1023大（不应该是保留
  端口），比5000大（以免与许多源自Berkeley的实现分配临时端口的范围冲突），比
  49152小（以免与临时端口号的“正确”范围冲突）。
  从bind返回的一个常见错误是EADDRINUSE，到7.5节讨论SO_REUSEADDR和SO_REUSEPORT
  这两个套接字选项时在详细讨论。*/
  l_impl_lnxsaddr* sa = (l_impl_lnxsaddr*)addr;
  if (sa->len == 0 || sock == -1) {
    l_loge_2(LNUL, "bind EINVAL %d %d", ld(sa->len), ld(sock));
    return false;
  }
  if (bind(sock, &(sa->addr.sa), sa->len) != 0) {
    l_loge_1(LNUL, "bind %s", lserror(errno));
    return false;
  } else {
    return true;
  }
}

static l_bool
l_impl_socket_listen(int sock, int backlog)
{
  /** listen - listen for connections on a socket **
  #include <sys/types.h>
  #include <sys/socket.h>
  int listen(int sockfd, int backlog);
  It marks the socket referred to by sockfd as a passive socket,
  that is, as a socket that will be used to accept incoming
  connection requests using accept. The sockfd is a fd that refers
  to a socket of type SOCK_STREAM or SOCK_SEQPACKET.
  The backlog defines the maximum length to which the queue of
  pending connections for sockfd may grow. If a connection request
  arrives when the queue is full, the client may receive an error
  with an indication of ECONNREFUSED or, if the underlying protocol
  supports retransmission, the request may be ignored so that a
  later reattempt at connection succeeds.
  The behavior of the backlog on TCP sockets changed with Linux 2.2.
  Now it specifies the queue length for completely established sockets
  waiting to be accepted, instead of the number of incomplete connection
  requests. The maximum length of the queue for incomplete sockets can
  be set using /proc/sys/net/ipv4/tcp_max_syn_backlog. When syncookies
  are enabled there is no logical maximum length and this setting is
  ignored. See tcp(7) for more information.
  If the backlog is greater than the value in /proc/sys/net/core/somaxconn,
  then it is silently truncated to that value; the default value in this
  file is 128. In kernels before 2.4.25, this limit was a hard coded value,
  SOMAXCONN, with the value 128.
  EADDRINUSE - Another socket is already listening on the same port.
  EBADF - the sockfd is not a valid fd.
  ENOTSOCK - the sockfd is not refer to a socket.
  EOPNOTSUPP - the socket is not of a type that supports the listen() op.
  当socket函数创建一个套接字时，它被假设为一个主动套接字，即调用connect发起
  连接的客户套接字。listen函数把一个为连接的套接字转换成一个被动套接字，指示
  内核应该接受指向该套接字的连接请求。根据TCP状态图，调用listen后导致套接字
  从CLOSED状态转换成LISTEN状态。
  内核为任何给定的监听套接字维护了两个队列。一个是未完成连接队列(incomplete
  connection queue），该队列里的套接字处于SYN_RCVD状态，表示已经收到了客户发
  的SYN但是TCP三次握手还没有完成；一个是已完成队列（completed connection
  queue），其中的套接字都处于ESTABLISHED状态，TCP三次握手的连接过程已成功完成。
  当来自客户SYN到达时，TCP在未完成连接队列中创建一个新项，然后发送SYN和ACK到
  客户端。该新建项会保持在队列中直到客户端ACK到来或等待超时（源自Berkeley的
  实现这个超时值为75s）。如果成功收到客户的ACK，该项会从未完成队列移到已完成
  队列的队尾。当进程调用accept时，已完成队列中的对头项将返回给进程，或者如果
  该队列为空，那么进程将被投入睡眠直到TCP在该队列放入一项才唤醒。
  backlog的含义从未有过正式的定义，4.2BSD宣称它的定义是由未处理连接构成的队列
  可能增长到的最大长度。该定义并未解释未处理连接是处于SYNC_RCVD状态的连接，还
  是尚未由进程接受的处于ESTABLISHED状态的连接，亦或两者皆可。源自Berkeley的实现
  给backlog增设了一个模糊因子，它乘以1.5得到未处理队列最大长度。不要把backlog
  设为0，因为不同的实现对此有不同的解释，如果你不想让任何客户连接到你的监听套
  接字上，那就关闭该监听套接字。指定一个比内核能够支持的值大的backlog是可以接受
  的，内核会把所指定的偏大值截断成自己支持的最大值而不返回错误。
  当一个客户SYN到达时，或这些队列是满的，TCP就忽略该SYN，不应该发送RST。因为
  这种情况是暂时的，客户TCP将重发SYN。要是服务器TCP立即响应一个RST，客户的
  connect就会立即返回一个错误，强制客户应用程序处理这种状况而不是让TCP的重传机制
  来处理。另外，客户无法区分响应SYN的RST究竟意味着“该端口没有服务器在监听”，还是
  “该端口有服务器在监听，不过它的队列已经满了”。
  在三次握手之后，但在服务器accept之前到达的数据应该有服务器TCP排队，最大数据量
  为相应已连接套接字的接收缓冲区大小。*/
  if (listen(sock, backlog) != 0) {
    l_loge_1(LNUL, "listen %s", lserror(errno));
    return false;
  } else {
    return true;
  }
}

L_EXTERN l_socket
l_socket_tcp_listen(const l_sockaddr* addr, int backlog)
{
  l_socket sock;
  const l_impl_lnxsaddr* sa = 0;
  int domain = 0;

  sa = (const l_impl_lnxsaddr*)addr;
  domain = (sa == 0 ? AF_INET : sa->addr.sa.sa_family);

  if (domain != AF_INET && domain != AF_INET6) {
    l_loge_s(LNUL, "listen wrong address family");
    return l_empty_socket();
  }

  sock = l_socket_create(domain, SOCK_STREAM, IPPROTO_TCP);
  if (l_socket_is_empty(&sock)) {
    return sock;
  }

  if (addr) {
    if (!l_impl_socket_bind(sock.fd, addr)) {
      l_socket_close(&sock);
      return l_empty_socket();
    }
  }

  /* 如果一个TCP客户或服务器未曾调用bind绑定一个端口，当使用connect或
  listen 时，内核会为相应的套接字选择一个临时端口 */

  if (l_impl_socket_listen(sock.fd, (backlog <= 0 ? L_SOCKET_BACKLOG : backlog))) {
    return sock;
  } else {
    l_socket_close(&sock);
    return l_empty_socket();
  }
}

typedef void (*l_sigfunc)(int);

static l_sigfunc
l_impl_sigact(int sig, l_sigfunc func)
{
  /** POSIX signal interrupt process's execution **
  信号（signal）是告知某个进程发生了某个事件的通知，也称谓软中断
  （software interrupt）。信号通常是异步发生的，也就是说进程预先
  不知道信号的准确发生时机。信号可以有一个进程发送给另一个进程或
  进程自身，或者由内核发给某个进程。例如SIGCHLD信号是由内核在任
  何子进程终止时发给它的父进程的一个信号。
  每个信号都有一个与之关联的处置（disposition），也称为行为
  （action），我们通过调用sigaction函数来设定一个信号的处置，并有
  三种选择。其一，可以提供一个函数，只要有特定信号发生时它就会调用。
  这样的函数称为信号处理函数（signal handler），这种行为称为捕获
  （catching）信号。有两个信号不能被捕获，它们是SIGKILL和SIGSTOP。
  信号处理函数的原型为 void (*handle)(int signo) 。对于大多数信号
  来说，调用sigaction函数并指定信号发生时所调用的函数就是捕获信号
  所需要的全部工作。不过稍后可以看到，SIGIO、SIGPOLL和SIGURG这些
  信号还要求捕获它们的进程做些额外工作。
  其二，可以把某个信号的处置设定为SIG_IGN来忽略它，SIGKILL和SIGSTOP
  这两个信号不能被忽略。其三，可以把某个信号的处置设定为SIG_DFL启用
  默认处置。默认处置通常是在收到信号后终止进程，某些信号还在当前工作
  目录产生一个core dump。另有个别信号的默认处置是忽略，SIGCHLD和
  SIGURG（带外数据到达时发送）是默认处置为忽略的两个信号。
  POSIX允许我们指定一组信号，它们在信号处理函数被调用时阻塞，任何阻塞
  的信号都不能递交给进程。如果将sa_mask成员设为空集，意味着在该信号处
  理期间，不阻塞额外的信号。POSIX会保证被捕获的信号在其信号处理期间总
  是阻塞的。因此在信号处理函数执行期间，被捕获的信号和sa_mask中的信号
  是被阻塞的。如果一个信号在被阻塞期间产生了一次或多次，那么该信号被
  解阻塞之后通常只递交一次，即Unix信号默认是不排队的。利用sigprocmask
  函数选择性的阻塞或解阻塞一组信号是可能的。这使得我们可以做到在一段
  临界区代码执行期间，防止捕获某些信号，以保护这段代码。
  警告：在信号处理函数中调用诸如printf这样的标准I/O函数是不合适的，其原
  因将在11.18中讨论。在System V和Unix 98标准下，如果一个进程把SIGCHLD
  设为SIG_IGN，它的子进程就不会变成僵死进程。不幸的是，这种做法仅仅适用
  于System V和Unix 98，而POSIX明确表示没有这样的规定。处理僵死进程的可
  移植方法是捕获SIGCHLD信号，并调用wait或waitpid。
  从子进程终止SIGCHLD信号递交时，父进程阻塞于accept调用，然后SIGCHLD的
  处理函数会执行，其wait调用取到子进程的PID和终止状态，最后返回。既然
  该信号实在父进程阻塞于慢系统调用（accept）时由父进程捕获的，内核会使
  accept返回一个EINTR错误（被中断的系统调用）。
  我们用术语慢系统调用（slow system call）描述accept函数，该术语也适用
  于那些可能永远阻塞的系统调用。举例来说，如果没有客户连接到服务器上，
  那么服务器的accept就没有返回的机会。类似的如果客户没有发送数据给服务
  器，那么服务器的read调用也将永不返回。适用于慢系统调用的基本规则是：
  当阻塞于某个慢系统调用的一个进程捕获某个信号且相应相应信号处理函数返回
  时，该系统调用可能返回一个EINTR错误。有些内核自动重启某些被中断的系统
  调用。不过为了便于移植，当我们编写捕获信号的程序时，必须对慢系统调用
  返回EINTR有所准备。移植性问题是由早期使用的修饰词“可能”、“有些”和对
  POSIX的SA_RESTART标志的支持是可选的这一事实造成的。即使某个实现支持
  SA_RESTART标志，也并非所有被中断的系统调用都可以自动重启。例如大多数
  源自Berkeley的实现从不自动重启select，其中有些实现从不重启accept和
  recvfrom。
  为了处理被中断的accept，需要判断错误是否是EINTR，如果是则重新调用accept
  函数，而不是直接错误返回。对于accept以及诸如read、write、select和open
  之类的函数来说，这是合适的。不过有一个函数我们不能重启：connect。如果该函数
  返回EINTR，我们就不能再次调用它，否则立即返回一个错误。当connect被一个捕获
  的信号中断而且不自动重启时（TCPv2第466页），我们必须调用select来等待连接
  完成。如16.3节所述。
   ** 服务器进程终止 **
  当服务器进程终止时会关闭所有打开的文件描述符，这回导致向客户端发送一个FIN，
  而客户端TCP响应一个ACK，这就是TCP连接终止工作的前半部分。如果此时客户不理会
  读取数据时返回的错误，反而写入更多的数据到服务器上会发生什么呢？这种情况是
  可能发生的，例如客户可能在读回任何数据之前执行两次针对服务器的写操作，而第
  一次写引发了RST。适用于此的规则是：当一个进程向某个已收到RST的套接字执行写
  操作时，内核会向该进程发送一个SIGPIPE信号。该信号的默认行为是终止进程，因此
  进程必须捕获以免不情愿的被终止。不论该进程是捕获该信号并从其信号处理函数返回，
  还是简单地忽略该信号，写操作都将返回EPIPE错误。
  一个在Usenet上经常问及的问题是如何在第一次写操作时而不是在第二次写操作时捕获
  该信号。这是不可能的，第一次写操作引发RST，第二次写引发SIGPIPE信号。因为写一个
  已接收了FIN的套接字不成问题，但写一个已接收了RST的套接字是一个错误。
  处理SIGPIPE的建议方法取决于它发生时应用进程想做什么。如果没有特殊的事情要做，
  那么将信号处理方法直接设置为SIG_IGN，并假设后续的输出操作将检查EPIPE错误并终止。
  如果信号出现时需采取特殊措施（可能需要在日志文件中记录），那么就必须捕获该信号，
  以便在信号处理函数中执行所有期望的动作。但是必须意识到，如果使用了多个套接字，
  该信号的递交无法告诉我们是哪个套接字出的错。如果我们确实需要知道是哪个write
  出错，那么必须不理会该信号，那么从信号处理函数返回后再处理EPIPE错误.
  ---
  ** sigaction - examine and change a signal action **
  #include <signal.h>
  int sigaction(int signum, const struct sigaction* act, struct sigaction* oldact);
  It is used to change the action taken by a process on receipt of a specific signal.
  signum specifies the signal and can be any valid signal except SIGKILL and SIGSTOP.
  If act is non-null, the new action for signum is installed from act. If oldact is
  non-null, the previous action is saved in oldact. The sigaction structure:
  struct sigaction {
    void (*sa_handler)(int);
    void (*sa_sigaction)(int, siginfo_t*, void*);
    sigset_t sa_mask;
    int sa_flags;
    void (*sa_restorer)(void);
  };
  On some architectures a union is involved, don't assign to both sa_handler and
  sa_sigaction. The sa_restorer is not intended for application use (POSIX does
  not specify a sa_restorer field).
  sa_handler specifies the action to be associated with signum and may be SIG_DFL
  for the default action, SIG_IGN to ignore this signal, or pointer to a signal
  handling function. This function receives the signal number as its only argument.
  sa_mask specifies a mask of signals which should be blocked during execution of
  the signal handler. In addition, the signal which triggered the handler will be
  blocked, unless the SA_NODEFER flag is used.
  If SA_SIGINFO is specified in sa_flags, then sa_sigaction (instead of sa_handler)
  specifies the signal-handling function for signum. Other flags can be:
  SA_NOCLDSTOP - If signum is SIGCHLD, don't receive notification when child process
  stop (i.e., when they receive one of SIGSTOP, SIGTSTP, SIGTTIN, or SIGTTOU) or
  resume (i.e., they receive SIGCONT). This flag is only for SIGCHLD.
  SA_NOCLDWAIT (since Linux 2.6) - If signum is SIGHLD, do not transform children
  into zombies when they terminate. This flag is menaingful only when establishing a
  handler for SIGCHLD, or when setting that signal's disposition to SIG_DFL.
  SA_NODEFER - Don't prevent the signal from being received from within its own
  signal handler. This flag is meaningful only when establishing a single handler.
  SA_NOMASK is an obsolete, nonstandard synonym for this flag.
  SA_ONSTACK - Call the signal handler on an alternate signal stack provided by
  sigaltstack(2). If an alternate stack is not available, the default stack will be
  used. This flag is meaningful only when establishing a signal handler.
  SA_RESETHAND - Restore the signal action to the default upon entry to the signal
  handler. This flag is meaningful only when establishing a signal handler. SA_ONESHOT
  is an obsolete, nonstandard synonym for this flag.
  SA_RESTART - Provide behavior compatible with BSD signal semantics by making certain
  ssystem calls restartable across signals. This flag is meaningful only when establishing
  a signal handler. See signal(7) for a discussion of system call restarting.
  SA_RESTORER - Not intended for application use. This flag is used by C libraries to
  indicate that the sa_restorer field contains the address of a "signal trampoline".
  SA_SIGINFO (since Linux 2.2) - The signal handler takes three arguments, not one. In
  this case, sa_sigaction should be set instead of sa_handler. This flag is meaningful
  only when establishing a signal handler.
  It is returns 0 on success, on error, -1 is returned, and errno is set.
   ** POSIX signal set operations **
  #include <signal.h>
  int sigemptyset(sigset_t* set); // set empty
  int sigfillset(sigset_t* set); // set full, including all signals
  int sigaddset(sigset_t* set, int sig);
  int sigdelset(sigset_t* set, int sig);
  int sigismember(const sigset_t* set, int sig);
  Return 0 on success and -1 on error, and sigismember returns 1 if is a member, returns
  0 if not a member, and -1 on error. On error, the errno is set.
  When creating a filled signal set, the glibc sigfillset() function doesn't include
  the two real-time signals used internally by the NPTL threading implementation. See
  nptl(7) for details. **/

  struct sigaction act, oldact;
  act.sa_handler = func;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;

  /* SA_RESTART标志可可选的。如果设置，由相应信号中断的系统调用将由内核自动重启。
  如果被捕获的信号不是SIGALRM且SA_RESTART有定义，我们就设置标志。对SIGALRM进行特殊
  处理的原因在于，产生该信号的目的正如14.2节讨论的那样，通常是为I/O操作设置超时，
  这种情况下我们希望受阻塞的系统调用被该信号中端掉从内核返回到用户进程。一些较早的
  系统（如SunOS 4.x）默认设置为自动重启被中断的系统调用（内核自动重启系统调用不会返
  回用户进程），并定义了与SA_RESTART互补的SA_INTERRUPT标志。如果定义了该标志，我们
  就在被捕获的信号是SIGALRM时设置它。*/

  if (sig == SIGALRM) {
  #ifdef SA_INTERRUPT
    act.sa_flags |= SA_INTERRUPT; /* SunOS 4.x */
  #endif
  } else {
  #ifdef SA_RESTART
    act.sa_flags |= SA_RESTART; /* SVR4, 4.4BSD */
  #endif
  }

  if (sigaction(sig, &act, &oldact) != 0) {
    l_loge_1(LNUL, "sigaction %s", lserror(errno));
    return SIG_ERR;
  }

  return oldact.sa_handler;
}

L_EXTERN void
l_sigign(int sig)
{
  l_impl_sigact(sig, SIG_IGN);
}

L_EXTERN void
l_socket_prepare()
{
  l_sigign(SIGPIPE);
}

L_EXTERN void
l_socket_accept(l_socket skt, void (*cb)(void*, l_socketconn*), void* ud)
{
  /** accept - accept a connection on a socket **
  #include <sys/types.h>
  #include <sys/socket.h>
  int accept(int sockfd, struct sockaddr* addr, socklen_t* addrlen);
  It is used with connection-based socket types (SOCK_STREAM,
  SOCK_SEQPACKET). It extracts the first connection request on the
  queue of pending connections for the listening sockfd, and creates
  a new connected socket, and returns a new fd referring to it. The
  newly created socket is not in the listening state. The original
  sockfd is unaffected by this call.
  The addr is a pointer to a sockaddr structure. This structure is
  filled in with the address of the peer socket, as known to the
  communications layer. The exact format of the address returned is
  determined by the socket's address family. When addr is NULL, nothing
  is filled in; in this case, addrlen is not used, and should also
  be NULL. The addrlen argument is a valud-result argument: the caller
  must initialize it to contain the size in bytes of the structure
  pointed to by addr; on return it will contain the acutal size of
  the peer address. The returned address is truncated if the buffer
  provided is too small; in this case, addrlen will return a value
  greater than was supplied to the call.
  If no pending connections are present on the queue, and the socket
  is not marked as nonblocking, accept() blocks the caller until a
  connection is present. If the socket is marked nonblocking and no
  pending connections are present on the queue, accept() fails with
  the error EAGIN or EWOULDBLOCK.
  on success, it returns a nonnegative interger that is a fd for the accepted
  socket. on error, -1 is returned, and errno is set appropriately.
  Error handling - linux accept passes already-pending network errors on the
  new socket as an error code from accept(). this behavior differs from other
  BSD socket implementations. for reliable operation the application should
  detect the network errors defined for the protocol after accept() and treat
  them like EAGAIN by retrying. in the case of TCP/IP, these are ENETDOWN,
  EPROTO, ENOPROTOOPT, EHOSTDOWN, ENONET, EHOSTUNREACH, EOPNOTSUPP, and
  ENETUNREACH.
  当有一个已完成的连接准备好被accept时，如果使用select会将其作为可读描述符
  返回该连接的监听套接字。因此如果我们使用select在某个监听套接字上等待一个
  外来连接，那就没必要把该监听套接字设置为非阻塞的，这是因为如果select告诉我
  们该套接字上已有连接就绪，那么随后的accept调用不应该阻塞。不幸的是，这里存在
  一个可能让我们掉入陷阱的定时问题(Gierth 1996）。为了查看这个问题，我们使TCP
  客户端建立连接后发送一个RST到服务器。然后在服务器端当select返回监听套接字
  可读后睡一段时间，然后才调用accept来模拟一个繁忙的服务器。此时客户在服务器调
  用accept之前就中止了连接时，源自Berkeley的实现不把这个中止的连接返回给服务器
  进程，而其他实现应该返回ECONNABORTED错误，却往往代之以返回EPROTO错误。考虑
  源自Berkeley的实现，在TCP收到RST后会将已完成的连接移除出队列，假设队列中没有
  其他已完成的连接，此时调用accept时会被阻塞，直到其他某个客户建立一个连接为止。
  但是在此期间服务器单纯阻塞在accept调用上，无法处理任何其他以就绪的描述符。本
  问题的解决方法是：当使用select获悉某个监听套接字上有已完成的连接准备好时，总
  是把这个监听套接字设为非阻塞；在后续的accept调用中忽略以下错误继续执行直到队
  列为空返回EAGIN或EWOULDBLOCK为止：ECONNABORTED（POSIX实现，客户中止了连接）、
  EPROTO(SVR4实现，客户中止了连接）和EINTR（有信号被捕获）。
  以上在accept之前收到RST的情况，不同实现可能以不同的方式处理这种中止的连接。
  源自Berkeley的实现完全在内核中处理中止的连接，服务器进程根本看不到。然而大多数
  SVR4实现返回一个错误给服务其进程作为accept的返回结果，不过错误本身取决于实现。
  这些SVR4实现返回一个EPROTO，而POSIX指出返回的必须是ECONNABORTED（software
  caused connection abort）。POSIX作出修正的理由在于，流子系统（streams subsystem）
  中发生某些致命的协议相关事件时，也会返回EPROTO。要是对于客户引起的一个已建立连接
  的非致命中止也返回同样的错误，那么服务器就不知道该再次调用accept还是不该。换成
  是ECONNABORTED错误，服务器可以忽略它，再次调用accept即可。源自Berkeley的内核从
  不把该错误传递给进程的做法所涉及的步骤在TCPv2中得到阐述，引发该错误的RST在第964
  页到达处理，导致tcp_close被调用。**/

  int n = 0, sock = 0;
  l_socketconn conn;
  l_impl_lnxsaddr* sa = 0;
  socklen_t provided_len = 0;

  sa = (l_impl_lnxsaddr*)&(conn.remote);
  provided_len = sizeof(struct sockaddr_in6);

  for (; ;) {
    sa->len = provided_len;
    sock = accept(skt.fd, &(sa->addr.sa), &(sa->len));
    if (sock != -1) {
      if (sa->len > provided_len) {
        l_loge_s(LNUL, "accept address truncated");
        sa->len = provided_len;
      }
      l_set_non_block(sock);
      conn.sock.fd = sock;
      cb(ud, &conn);
      continue;
    }

    n = errno;
    if (n == EAGAIN || n == EWOULDBLOCK) {
      /* no more pending completed connections in the kernel */
      return;
    }

    switch (n) {
    case EINTR: /* system call was interrupted by a signal */
    case ECONNABORTED: /* a connection has been aborted */
    case EPROTO: /* protocol error */
      /* current connection is interrupted, aborted or has protocol error,
      so skip this connection and continue to accept next connections
      in the kernel queue until it is empty */
      l_logw_1(LNUL, "accept %s and continue", lserror(n));
      break; /* continue the for loop */
    case EBADF: /* sockfd is not an open fd */
    case EFAULT: /* the addr is not in a writable part of the user address space */
    case EINVAL: /* sockfd is not listening for connections, or addrlen is invalid */
    case EMFILE: /* per-process limit on the number of open fds reached */
    case ENFILE: /* system-wide limit on the total number of open files reached */
    /* no enough free memory. this often means that the memory allocation is limited */
    case ENOMEM: case ENOBUFS: /* by the socket buffer limits, not by the system memroy */
    case ENOTSOCK: /* the sockfd does not refer to a socket */
    case EOPNOTSUPP: /* the referenced socket is not of type SOCK_STREAM */
    case EPERM: /* firewall rules forbid connection */
      l_loge_1(LNUL, "accept %s", lserror(n));
      return; /* unrecoverable error, return */
    default:
      /* in addition, network errors for the new socket and as defined for the protocol
      may be returned. various linux kernels can return other errors such as ENOSR,
      ESOCKTNOSUPPORT, EPROTONOSUPPORT, ETIMEOUT. the value ERESTARTSYS may be seen
      during a trace. */
      l_loge_1(LNUL, "accept other error: %s", lserror(n));
      return;
    }
  }
}

/** TCP连接的各种可能错误 **
服务器主机崩溃 - 为了模拟这种情形必须在不同的主机上运行客户端和服务器。我们先启动
服务器再启动客户，接着在客户上键入一行文本以确认连接工作正常，然后从网络上断开服务
器主机，并在客户上键入另一行文本。这样同时也模拟了当客户发送数据时服务器不可达的情
形（例如建立连接后某些中间路由器不工作）。当服务器崩溃时，已有的网络连接上不会发出
任何东西。这里我们假设的是主机崩溃，而不是执行关机命令。在客户键入文本后，客户TCP
会持续重传数据，试图从服务器上接收到一个ACK。TCPv2的25.11节给出了TCP重传的一个经典
模式：源自Berkeley的实现重传该数据包12次，共等待约9分钟才放弃。当客户TCP最终放弃时
（假设在这段时间内，服务器主机没有重新启动或假设主机仍然不可达），客户进程会返回一个
错误。如果服务器主机已崩溃从而对客户数据没有响应，则返回的错误将是ETIMEOUT。然而如果
某个中间路由器判定服务器主机已不可达，从而响应一个"destination unreachable"的ICMP
消息，那么所返回的错误是EHOSTUNRACH或ENETUNREACH。
尽管我们的客户最终还是会发现对端主机已崩溃或不可达，不过有时候我们需要比不得不等待
9分钟更快的检测出这种情况。所用的方法就是对读操作设置一个超时，我们将在14.2中讨论
这一点。这里讨论的情形只有在我们向服务器发送数据时才能检测到它已经崩溃。如果我们
不主动给它发送数据也想检测服务器主机的崩溃，那么需要另外一个技术，也就是我们将在
7.5节讨论的SO_KEEPALIVE套接字选项，或某些客户/服务器心搏函数。
如果服务器崩溃后又重启，重新连接到网络中。此时假设没有使用SO_KEEPALIVE选项，那么所
发生的事情是：服务器崩溃后客户发送的数据会在重传，当服务器重新启动并连网后，它的
TCP丢失了崩溃前的所有连接信息，因此服务器的TCP会响应一个RST；当客户收到该RST时，客
户正阻塞于读取系统调用，导致该调用返回ECONNRESET错误。
服务器主机关机 - Unix系统关机时，init进程通常先给所有进程发送SIGTERM信号（该信号可
被捕获），等待一段固定的时间（往往在5到20s之间），然后给所有仍在运行的进程发送
SIGKILL信号（该信号不能被捕获）。这么做留给所有运行进程一小段时间来清除和终止。如果
我们忽略SIGTERM信号（如果默认处置SIGTERM也会终止进程），我们的服务器将由SIGKILL信号
终止。进程终止会使所有打开这的描述符都被关闭，然后服务器会发生正常的TCP断连过程。正
如5.12节所讨论的一样，我们必须在客户中使用select或poll函数，以防TCP断连时客户阻塞在
其他的函数中而不能快速知道TCP已经断连了。*/

static int
l_impl_socket_connect(int sock, const l_impl_lnxsaddr* sa)
{
  /** connect - initiate a conneciton on a socket **
  #include <sys/types.h>
  #include <sys/socket.h>
  int connect(int sockfd, const struct sockaddr* addr, socklen_t addrlen);
  It connects the socket referred to sockfd to the address specified by addr.
  The addrlen specifies the size of addr. The format of the address in addr
  is determined by the address space of the socket sockfd.
  If the sockfd is of type SOCK_DGRAM, then addr is the address to which
  datagrams are sent by default, and the only address from which datagrams
  are received. If the socket is of type SOCK_STREAM or SOCK_SEQPACKET, this
  call attempts to make a connection to the socket that is bound to the
  address specified by addr.
  Generally, connection-based protocol sockets may successfully connect()
  only once; connectionless protocol sockets may use connect() multiple
  times to change their association. Connectionless sockets may dissolve
  the association by connecting to an address with the sa_family member of
  sockaddr set to AF_UNSPEC (supported on Linux since kernel 2.2).
  ---
  If the connection or binding succeeds, zero is returned. On error, -1
  is returned, and errno is set. If connect() fails, consider the state of
  the socket as unspecified. Protable appliations should close the socket
  and create a new one for reconnecting.
  EACCES, EPERM - The user tried to connect to a broadcast address without
  having the socket broadcast flag enbaled or the conneciton request failed
  because of a local firewall rule.
  EADDRINUSE - local address is already in use.
  EADDRNOTAVAIL - (Internet domain sockets) The socket referred to by sockfd
  had not previously been bound to and address and, upon attempting to bind
  it to an ephemeral port, it was determined that all port numbers in the
  ephemeral port range are currently in use. See the discussion of
  /proc/sys/net/ipv4/ip_local_port_range in ip(7).
  EAFNOSUPPORT - the passed address didn't have the correct address family.
  EAGAIN - No more free local ports or insufficient entries in the routing
  cache. For AF_INET see the description of /proc/sys/net/ipv4/
  ip_local_port_range ip(7) for information on how to increase the number
  of local ports.
  EALREADY - the socket is nonblocking and a previous connection attempt
  has not yet been completed.
  EBADF - sockfd is not a valid open fd.
  ECONNREFUSHED - no one listening on the remote address.
  EFAULT - the socket structure address is outside the user's address space.
  EINPROGRESS - the socket is nonblocking and the connection cannot be
  completed immediately. It is possible to select or poll for completion
  by selecting the socket for writing. After select indicates writability,
  use getsockopt to read the SO_ERROR option at level SO_SOCKET to
  determine whether connect() completed successfully (SO_ERROR is zero) or
  unsuccessfully.
  EINTR - the system call was interrupted by a signal that was caught.
  EISCONN - the socket is already connected.
  ENETUNREACH - network is unreachable.
  ENOTSOCK - the sockfd does not refer to a socket.
  EPROTOTYPE - the socket type does not support the requested communications
  protocol. This error can occur, for example, on an attempt to conect a
  UNIX domain datagram socket to a stream socket.
  ETIMEDOUT - timeout while attempting connection. the server may be too busy
  to accept new connections. Note that for ip sockets the timeout may be very
  long when syncookies are enabled on the server.
  如果是TCP套接字，调用connect函数会激发TCP三次握手过程，而且仅在连接建立成功
  或出错时才返回，其中错误返回可能有以下几种情况：
  如果TCP客户没有收到SYN的回应，则返回ETIMEOUT错误。举例来说，调用connect()时，
  4.4BSD内核发送一个SYN，若无响应则等待6s后再发送一个，若仍然无响应则等待24s
  后再发送一个（TCPv2第828页）。若总共等了75s后仍未收到响应则返回本错误。
  若对客户的SYN响应的是RST，则表明该服务器主机在我们指定的端口上没有进程在等待
  与之连接（如服务器进程也许没有在运行）。这是一种硬错误（hard error），客户一
  接收到RST就马上返回ECONNREFUSED错误。RST是TCP发生错误时发送的一种TCP数据包。
  产生RST的三个条件是：目的地为某端口的SYN到达，然而该端口上没有正在监听的服务
  器；TCP想取消一个已有连接；TCP接收到一个根本不存在的连接上的数据包。
  若客户发出的SYN在中间某个路由器上引发一个destination unreachable（目的地不可
  达）ICMP错误，则认为是一种软错误（soft error）。客户主机内核保存该消息，并按
  第一种情况中所述的时间间隔重新发送SYN。若在某个规定的时间后仍未收到响应，则把
  保存的消息作为EHOSTUNREADCH或ENETUNREACH错误返回给进程。以下两种情况也是可能
  的：一是按照本地系统的转发表，根本没有到达远程系统的路径；二是connect调用根本
  不等待就返回。许多早期系统比如4.2BSD在收到“目的地不可达”ICMP错误时会不正确地
  放弃建立连接的尝试。注意，网路不可达的错误被认为已过时，应用进程应该把
  ENETUNREACH和EHOSTUNREACH作为相同的错误对待。
  按照TCP状态转换图，connect函数导致当前套接字从CLOSED状态转移到SYN_SENT状态，
  若成功再转移到ESTABLISHED状态，如connect失败则该套接字不再可用，必须关闭。
  我们不能对这样的套接字再次调用connect函数
  ---
  当在一个非阻塞的TCP套接字上调用connect时会立即返回一个EINPROGRESS，已经发起的
　　TCP三次握手继续进行，然后可以使用select检测这个连接是否建立成功。使用非阻塞连接:
  (1) 可以把三次握手叠加在其他处理上，完成一个connect要一个RTT时间，而RTT波动范围
　很大，从局域网上的几个毫秒到几百个毫秒到广域网的甚至几秒，这段时间内可能还有其他事情
　需要处理。
　　(2) 可以使用这个技术同时建立多个连接，这个用途随着Web浏览器变得流行起来。
  (3) 既然使用select等待连接建立，就可以给select指定时间限制，使得能缩短连接超时。
　许多实现有75s到数分钟的connect超时时间，应用程序有时想要一个更短的超时。实现方法
　之一就是使用非阻塞connect，另外14.2节也介绍过设置套接字操作超时的方法。
  尽管套接字是非阻塞的，如果连接的服务器在同一个主机上，调用connect通常连接会立即成功。
　源自Berkeley的实现（和POSIX）关于select和非阻塞connect有以下两个规则：
　　(1) 当连接成功建立时，描述符变成可写（TCPv2第531页）；
　 (2) 当连接建立遇到错误时，描述符变得可读又可写（TCPv2第530页）；
　关于select的这两个规则出自6.3节中关于描述符就绪条件的相关规则，一个TCP套接字变为可写
　的条件是，其发送缓冲区中有可用空间（对于连接建立中的套接字而言本子条件总为真，因为尚未
　往其写出任何数据）并且套接字已成功建立（本子条件为真发生在三次握手成功之后）。一个TCP
　套接字发生发生某个错误时，这个待处理错误总是导致套接字变为可读又可写。
  调用select之前可能连接已经建立并有来自对端的数据到达，这种情况下即使套接字上不发生错误，
　套接字也是可读又可写的；阻塞套接字连接在TCP三次握手完成之前被中断了会发生什么？假设被中
　断的套接字没有被内核自动重启，那么它会返回EINTR，此时不能再次调用connect等待未完成的连接，
　这样做将导致返回EADDRINUSE错误，我们只能调用select，就像对于非阻塞connect所做的那样，
　连接建立成功时返回套接字可写条件，连接建立失败时select返回套接字既可读也可写条件。*/
  int status = 0;

  for (; ;) {
    if (connect(sock, &(sa->addr.sa), sa->len) == 0) {
      return 0;
    }

    status = errno;
    l_logd_1(LNUL, "connect %s", lserror(status));

    switch (status) {
    case EINTR: /* the system call was interrupted by a singal that was caught */
      break; /* continue to for loop */
    case EISCONN: /* the socket is already connected */
      return 0;
    case EALREADY: /* the socket is nonblocking and a previous connection attempt has not yet been completed */
    case EINPROGRESS: /* the socket is nonblocking and the connection cannot be completed immediately */
      return -1; /* not compeleted yet */
    default:
      l_loge_1(LNUL, "connect %s", lserror(status));
      return -2;
    }
  }
}

L_EXTERN l_socket
l_socket_tcp_connect(const l_sockaddr* addr, l_bool* done)
{
  /** If connect() fails, consider the state of the socket as unspecified.
  Protable appliations should close the socket and create a new one for
  reconnecting. **/

  l_socket sock;
  const l_impl_lnxsaddr* sa = 0;
  int domain = 0;
  int status = 0;

  sa = (const l_impl_lnxsaddr*)addr;
  domain = sa->addr.sa.sa_family;

  if (domain != AF_INET && domain != AF_INET6) {
    l_loge_s(LNUL, "listen wrong address family");
    return l_empty_socket();
  }

  sock = l_socket_create(domain, SOCK_STREAM, IPPROTO_TCP);
  if (l_socket_is_empty(&sock)) {
    return sock;
  }

  status = l_impl_socket_connect(sock.fd, sa);
  if (status == 0) {
    if (done) *done = true;
    return sock;
  } else if (status == -1) {
    if (done) *done = false;
    return sock;
  } else {
    if (done) *done = false;
    l_socket_close(&sock);
    return l_empty_socket();
  }
}

L_EXTERN l_bool
l_socket_cmpl_connect(l_socket sock)
{
  /** It is possible to select(2) or poll(2)
  for completion by selecting the socket for writing.  After
  select(2) indicates writability, use getsockopt(2) to read the
  SO_ERROR option at level SOL_SOCKET to determine whether
  connect() completed successfully (SO_ERROR is zero) or
  unsuccessfully (SO_ERROR is one of the usual error codes
  listed here, explaining the reason for the failure). **/

  L_UNUSED(sock);
  return false;
}

static l_int
l_impl_read(int fd, void* out, l_int size)
{
  /** read - read from a file descriptor **
  #include <unistd.h>
  ssize_t read(int fd, void *buf, size_t count);
  read up to count bytes from fd into out buffer. if count is 0, read() may detect
  the errors described below. in the absence of any errors, or if read() does not
  check for errors, it returns 0 and has no other effects. according to POSIX.1, if
  count is greater than SSIZE_MAX, the result is implementation-defined; on linux,
  read() and similar system calls will transfer at most 0x7ffff000 (2,147,479,552)
  bytes, returning the number of bytes actually transferred. (this is true on both
  32-bit and 64-bit systems).
  On success, the number of bytes read is returned, and the file position is
  advanced by this number. It is not an error if this number is smaller than the
  number of bytes requested; this may happen for example because fewer bytes are
  actually available right now (maybe because we were close to end-of-file, or
  reading from a pipe, or from a terminal), or because read() was interrupted by
  a signal. On error, -1 is returned, and errno is set appropriately. In this case,
  it is left unspecified whether the file position changes.
  EAGAIN - the fd refers to a file other than a socket and has been marked nonblocking,
  and the read would block.
  EAGAIN or EWOULDBLOCK - the fd refers to a socket and has been marked nonblocking,
  and the read would block. POSIX.1-2001 allows either error to be returned for this
  case, and does not require these constants to have the same value, so a portable
  application should check for both possibilities.
  EBADF - fd is not a valid fd or is not open for reading
  EFAULT - buf is outside your accessible address space
  EINTR - the call was interrupted by a signal before any data was read
  EINVAL - fd is attached to an object which is unsuitable for reading; or the file was
  opened with the O_DIRECT flag, and either the address specified in buf, the value
  specified in count, or the file offset is not suitably aligned
  EINVAL - fd was created via a call to timerfd_create(2) and the wrong size buffer was
  given to read(); see timerfd_create(2) for further information
  EIO - I/O error. this will happen for example when the process is in a background
  process group, tries to read from this controlling terminal, and either it is ignoring
  or blocking SIGTTIN or its process group is orphaned. It may also occur when there is
  a low-level I/O error while reading from a disk or tape.
  EISDIR - fd refers to a directory.
  Other errors may occurs, depending on the object connected to fd. */
  ssize_t n = 0;
  int status = 0;

  for (; ;) {
    if ((n = read(fd, out, (size_t)size)) >= 0) {
      /* note that one case about read bytes n < count is:
      at least one byte is read and then interrupted by a
      signal, the call is returned success in this case. */
      return (l_int)n;
    }

    status = errno;

    if (status == EAGAIN || status == EWOULDBLOCK) {
      return -1; /* data is not available currently */
    } else if (status == EINTR) {
      /* interrupted by a signal before read any bytes,
      try to read again. */
      continue; /* continue the for loop */
    } else {
      l_loge_1(LNUL, "read %s", lserror(status));
      return -2; /* error occurred */
    }
  }
}

static l_int
l_impl_write(int fd, const void* data, l_int size)
{
  /** write - write to a file descriptor **
  #include <unistd.h>
  ssize_t write(int fd, const void *buf, size_t count);
  Writes up to count bytes from the buffer to the file referred to by the fd.
  The number of bytes written may be less than count if, for example, there
  is insufficient space on the underlying physical medium, or the RLIMIT_FSIZE
  resource limit is encountered (see setrlimit(2)), or the call was interrupted
  by a signal handler after having written less then bytes. (see also pipe(7).)
  POSIX requires that a read(2) that can be proved to occur after a write()
  has returned will return the new data. Note that not all filesystems are
  POSIX conforming.
  According to POSIX.1, if count is greater than SSIZE_MAX, the result is
  implementation-defined; on linux, write() and similar system calls will
  transfer at most ***0x7ffff000 (2,147,479,522)*** bytes, returning the number of
  bytes actually transferred. (this is true on both 32-bit and 64-bit systems).
  A successful return from write() does not make any guarantee that data has
  been committed to disk. In fact, on some buggy implementations, it does not
  even guarantee that space has successfully been reserved for the data. The
  only way is to call fsync(2) after you are done writing all your data.
  If a write() is interrupted by a signal handler before any bytes are written,
  then the call fails with the error EINTR; if it is interrupted after at
  least one byte has been writtn, the call succeeds, and returns the number
  of bytes written.
  ---
  On success, the number of bytes written is returned. It is not an error if
  this number is smaller than the number of bytes requested; this may happen
  for example because the disk device was filled, or interrupted by a signal.
  On error, -1 is returned, and errno is set appropriately.
  If count is zero and fd refers to a regular file, then write() may return a
  failure status if one of the errors below is detected. If no errors are
  detected, or error detection is not performed, 0 will be returned without
  causing any other effect. If count is zero and fd refers to a file other than
  a regular file, the results are not specified.
  EAGAIN - the fd refers to a file other than a socket and has been marked
  nonblocking, and the write would block.
  EAGAIN or EWOULDBLOCK - the fd refers to a socket and has been marked
  nonblocking, and the write would block. POSIX.1-2001 allows either error to
  be returned for this case, and does not require these constants to have the
  same value, so a protable application should check for both possibilities.
  EBADF - fd is not a valid fd or is not open for writing.
  EDESTADDRREQ - fd refers to a datagram socket for which a peer address has
  not been set using connect(2).
  EDQUOT - the usr's quota of disk blocks on the filesystem containing the file
  referred to by fd has been exhausted.
  EFAULT - buf is outside your accessible address space.
  EFBIG - an attempt was made to write a file that exceeds the implementation-
  defined maximum file size or the process's file size limit, or to write
  at position past the maximum allowed offset.
  EINTR - the call was interrupted by a signal before any data was written.
  EINVAL - fd is attached to an object which is unsuitable for writing; or the
  file was opened with the O_DIRECT flag, and either the address specified in
  buf, the value specified in count, or the file offset is not suitably aligned.
  EIO - a low-level I/O error occurred while modifying the inode.
  ENOSPC - the device containing the file referred to by fd has no room for
  the data.
  EPERM - the operation was prevented by a file seal; see fcntl(2).
  EPIPE - fd is connected to a pipe or socket whose reading end is closed.
  When this happens the writing process will also receive a SIGPIPE signal.
  (Thus, the write return value is seen only if the program catches, blocks
  or ignores this signal.)
  Other errors may occur, depending on the object connected to fd. */
  ssize_t n = 0;
  int status = 0;

  for (; ;) {
    if ((n = write(fd, data, (size_t)size)) >= 0) {
      /* note that one case about written bytes n < count is:
      at least one byte is written and then interrupted by a
      signal, the call is returned success in this case. */
      return (l_int)n;
    }

    status = errno;

    if (status == EAGAIN || status == EWOULDBLOCK) {
      return -1; /* cannot write currently */
    } else if (status == EINTR) {
      /* interrupted by a signal before written any bytes,
      try to read again. */
      continue; /* continue the for loop */
    } else {
      l_loge_1(LNUL, "write %s", lserror(status));
      return -2; /* error occurred */
    }
  }
}

L_EXTERN l_int
l_data_read(l_filehdl hdl, void* out, l_int size)
{
  l_int n = 0;
  l_int done = 0;
  l_int left = 0;
  l_byte* buff = 0;

continue_to_read:

  buff = l_strc(out) + done;
  left = size - done;

  if (left <= 0) {
    return done;
  }

  n = l_impl_read(hdl.fd, buff, left);

  if (n > 0) {
    done += n;
    goto continue_to_read;
  } else {
    /* 0, -1 - cannot read right now, < -1 - error happened */
    return done;
  }
}

L_EXTERN l_int
l_data_write(l_filehdl hdl, const void* from, l_int size)
{
  l_int n = 0;
  l_int done = 0;
  l_int left = 0;
  const l_byte* data = 0;

continue_to_write:

  data = l_strc(from) + done;
  left = size - done;

  if (left <= 0) {
    return done;
  }

  n = l_impl_write(hdl.fd, data, left);

  if (n > 0) {
    done += n;
    goto continue_to_write;
  } else {
    /* 0, -1 - cannot write right now, < -1 - error happened */
    return done;
  }
}

/** io events **/

#define l_filehdl_from(fd) (l_filehdl){fd}

static int
l_eventfd_create()
{
  /** eventfd - create a file descriptor for event notification **
  #include <sys/eventfd.h>
  int eventfd(unsigned int initval, int flags);
  Creates an "eventfd object" that can be used as an event wait/notify
  mechanism by user-space applications, and by the kernel to notify
  user-space applications of events. The object contains an unsigned
  64-bit integer (uint64_t) counter that is maintained by the kernel.
  This counter is initialized with the value specified in the
  argument initval.
  eventfd() is available on Linux since kernel 2.6.22. Working support
  is provided in glibc since version 2.8. The eventfd2() system call
  is available on Linux since kernel 2.6.27. Since version 2.9, the
  glibc eventfd() wrapper will employ the eventfd2() system call,
  if it is supported by the kernel. eventfd() and eventfd2() are
  Linux-specific.
  The following values may be bitwise ORed in flags to change the
  behavior of eventfd():
  EFD_CLOEXEC (since Linux 2.6.27) - set the close-on-exec (FD_CLOEXEC)
  flag on the new file descriptor.
  EFD_NONBLOCK (since Linux 2.6.27) - set the O_NONBLOCK file status
  flag on the new open fd. Using this flag saves extra calls to fcntl(2)
  to achieve the same result.
  EFD_SEMAPHORE (since Linux 2.6.30) - provided semaphore-like semantics
  for reads from the new fd.
  In Linux up to version 2.6.26, the flags arugment is unused, and must
  be specified as zero.
  ---
  It returns a new fd that can be used to refer to the eventfd object. The
  following operations can be perfromed on the file descriptor.
  * read(2) - each successful read(2) returns an 8-byte integer. A read(2)
  will fail with the error EINVAL if the size of the supplied buffer is less
  than 8 bytes. The value returned by read(2) is host type order, i.e., the
  native byte order for integers on the host machine.
  The semantics of read(2) depend on whether the evenfd counter currently
  has a nonzero value and whether the EFD_SEMAPHORE flag was specified when
  creating the evenfd file descriptor:
  If EFD_SEMAPHORE was not specified and the eventfd counter has a nonzero
  value, than a read(2) returns 8 bytes containing that value, and the
  counter's value is reset to zero.
  If EFD_SEMAPHORE was specified and eventfd counter has a nonzero value,
  then a read(2) returns 8 bytes containing the value 1, and the counter's
  value is decremented by 1.
  If the eventfd counter is zero at the time of the call to read(2), then
  the call either blocks until the counter becomes nonzero (at which time,
  the read(2) proceeds as described above) or fails with the error EAGAIN
  if the file descriptor has been made nonblocking.
  * write(2) - a write(2) call adds the 8-byte integer value supplied in
  its buffer to the counter. The maximum value that may be stored in the
  counter is the largest unsigned 64-bit value minux 1 (i.e., 0xfffffffe).
  If the addition would cause the counter's value to exceed the maximum,
  then the write(2) either blocks until a read(2) is performed on the fd.
  or fails with the error EAGAIN if the fd has been made nonblocking.
  A write(2) will fail with the error EINVAL if the size of the supplied
  buffer is less than 8 bytes, or if an attempt is made to write the value
  0xffffffff.
  * poll(2), select(2) and similar - the returned fd supports poll(2),
  epoll(7) and select(2), as follows:
  The fd is readable if the counter has a value greater than 0. The fd is
  writable if it is possible to write a value of at least "1" without blocking.
  If an overflow of the counter value was detected, then select(2) indicates
  the fd as being both readable and writable, and poll(2) returns a POLLERR
  event. As noted above, write(2) can never overflow the counter. However an
  overflow can occur if 2^64 evenfd "signal posts" were performed by the KAIO
  subsystem (theoretically possible, but practically unlikely). If an overflow
  has occured, then read(2) will return that maximum uint64_t value (i.e,
  0xffffffff). The eventfd fd also supports the other fd multiplexing APIs:
  pselect(2) and ppoll(2).
  * close(2) - when the fd is no longer required it shoud be closed. When all
  fd associated with the same eventfd object have been closed, the resources
  for object are freed by the kernel.
  ---
  On success, eventfd() returns a new eventfd. On error, -1 is returned and
  errno is set to indicate the error.
  EINVAL - an unsupported value was specified in flags.
  EMFILE - the per-process limit on open fd has been reached.
  ENFILE - the system-wide limit on the total number of open files has been reached.
  ENODEV - could not mount (internal) anonymous inode device.
  ENOMEM - there was insufficent memory to create a new eventfd file descriptor.
  Application can use an eventfd file descriptor instead of a pipe in all cases where
  a pipe is used simply to signal events. The kernel overhead of an eventfd is
  much lower than that of a pipe, and only one fd is required (versus the two required
  for pipe). When used in the kernel, an eventfd descriptor can provide a bridge
  from kernel to user space, allowing, for example, functionalities like (KAIO, kernel
  AIO) to signal to a file descriptor that some operation is complete.
  A key point about an eventfd is that it can be monitored just like any other fd
  using select(2), poll(2), or epoll(7). This means that an application can simultaneously
  monitor the readiness of "traditonal" files and the readiness of other kernel
  mechanisms that support the eventfd interface. (Without the eventfd() interface, these
  mechanisms could not be multiplexed via select(2), poll(2), or epoll(7)). */
  int hdl = eventfd(0, EFD_NONBLOCK);
  if (hdl == -1) {
    l_loge_1(LNUL, "eventfd %s", lserror(errno));
  }
  return hdl;
}

static void
l_eventfd_close(int* hdl)
{
  if (*hdl == -1) {
    return;
  } else {
    if (close(*hdl) != 0) {
      l_loge_1(LNUL, "eventfd close %s", lserror(errno));
    }
    *hdl = -1;
  }
}

static l_bool
l_eventfd_write(int hdl)
{
  l_ulong count = 1;
  int n = l_impl_write(hdl, &count, sizeof(l_ulong));
  if (n == sizeof(l_ulong)) {
    return true;
  } else {
    l_loge_1(LNUL, "eventfd write fail %s", lserror(errno));
    return false;
  }
}

static l_bool
l_eventfd_read(int hdl)
{
  l_ulong count = 0;
  int n = l_impl_read(hdl, &count, sizeof(l_ulong));
  if (n == sizeof(l_ulong)) {
    return true;
  } else {
    l_loge_1(LNUL, "eventfd read fail %s", lserror(errno));
    return false;
  }
}

/** epoll - I/O event notification facility **
The epoll API performs a similar task to poll(2): monitoring multiple file
description to see if I/O is possible on any of them. The epoll API can be
used either as an edge-triggered or a level-triggered and scales well to
large numbers of watched file descriptions.
The difference between the ET and LT can be described as follows. Suppose
that a fd received 2KB data, a call to epoll_wait is done that will return
fd as a ready file descriptor. And then 1KB data is read from fd, than a call
to epoll_wait again. If the fd configured using ET then the call to epoll_wait
will be blocked despite the available data still present in the buffer; meanwhile
the remote peer might be expecting a response based on the data it alrady sent.
If it is, the call to epoll_wait will blcok indefinitely due to ET mode only
triggered when changes occur on the monitored file descritor.
An application that employs the EF should use nonblocking fd to avoid having a
blocking read or write starve a task that is handling multiple fds. The suggested
way to use ET epoll with nonblocking fds and by waiting for an event only after
read or write return EAGAIN.
By contracst, when used as LT, epoll is simply a faster poll. Since even with
ET epoll, multiple events can be generated upon receipt of multiple chunks of data,
the caller has the option to specify EPOLLONESHOT flag, to tell epoll to disable
the fd after the receipt of an event. It is the caller's responsibility to rearm
the fd using epoll_ctl with EPOLL_CTL_MOD.
If the system is in autosleep mode via /sys/power/autosleep and an event happens
which wakes the device from sleep, the device driver will keep the device awake
only until that event is queued. It is necessary to use the epoll_ctl EPOLLWAKEUP
flag to keep the device awake until the event has been processed. In specifically,
the system will be kept awake from the moment the event is queued, through the
epoll_wait is returned and until the subsequence epoll_wait call. If the event
should keep the system awake beyond that time, then a separate wake_lock should
be taken before the second epoll_wait call.
/proc/sys/fs/epoll/max_user_watchers (since Linux 2.6.28) specifies a limit on the
total number of fd that a user can register across all epoll instances on the
system. The limit is per real user ID. Each registered fd costs roughly 90 bytes
on a 32-bit kernel, and roughtly 160 bytes on a 64-bit kernel. Currently, the
default value for max_user_watchers is 1/25 (4%) of the available low memory,
divided by the registration cost in bytes.
The edge-triggered usage requires more clarification to avoid stalls in the
application event loop.
What happens if you register the same fd on an epoll instance twice? You will
probably get EEXIST. However, it is possible to add a duplicate fd (dup(2),
dup2(2), fcntl(2) F_DUPFD). This can be a useful technique for filtering events,
if the duplicate fd are registered with different events masks.
Can two epoll instances wait for the same fd? If so, are events reported to both
epoll fd? Yes, and events would be reported to both. However, careful programming
may be needed to do this correctly.
Is the epoll file descriptor itself poll/epoll/selectable? Yes, If an epoll fd
has events waiting, then it will indicate as being readable. What happens if one
attempts to put an epoll fd into its own fd set? The epoll_ctl call will return
EINVAL. However, you can add an epoll fd inside another epoll fd set.
Will closing a fd cause it to be removed from all epoll sets automatically? Yes,
but be aware of the following point. A fd is a reference to an open fd. Whether
a fd is duplcated via dup(2), dup2(2), fcntl(2) F_DUPFD, or fork(2), a new fd
refering to the same open fd is created. An open fd continues to exist until all
fd refering to it have been closed. A fd is removed from an epoll set only after
all the fds referring to the underlying open fd have been closed. This means that
even after a fd that is part of an epoll set has been closed, events may be reported
for that fd if other fds referring to the same underlying fd remain open.
If more the one event occurs between epoll_wait calls, are they combined? They will
be combined.
Receiving an event from epoll_wait should suggest to you that such fd is ready for
the requested I/O operation. You must consider it ready until the next (nonblocking)
read/write yields EAGAIN. When and how you will use the fd is entirely up to you.
For packet/token-orientied files (e.g., datagram socket, terminal in canonical mode),
the only way to detect the end of the read/write I/O space is to continue to read/write
until EAGAIN. For stream-oriented files (e.g., pipe, FIFO, stream socket), the
condition that the read/write I/O sapce is exhausted can also be detected by checking
the amount of data from/written to the target fd. For example, if you call read by asking
to read a certain amount of data and read returns a lower number of bytes, you can be
sure of having exhausted the read I/O space for the fd. The same is true when using write.
If there is a large amount of I/O space, it is possible that by trying to drain it the
other files will not get processed causing starvation. The solution is to maintain a
ready list and mark the fd as ready in its associated data structure, thereby allowing
the application to remember which files need to be processed but still round robin
amongst all the ready files. This also supports ignoring subsequent events you receive
for fds that are already ready.
If you use an event cache or store all the fds returned from epoll_wait, then make sure
to provide a way to mark its closure dynamically. Suppose you receive 100 events from
epoll_wait, and in event #47 a condition causes event #13 to be closed. If you remove
the structure and close the fd for event #13, then your event cached might still say
there are events waiting for that fd causing confusion.
On solution for this is to call, during the processing of event 47, epoll_ctl to delete
fd 13 and close, then mark its associated data structure as removed and link it to a
cleanup list. If you find another event for fd 13 in your batch processing, you will
discover the fd had been previously removed and there will be no confustion.
The epoll api is Linux-specific. Some other systems provided similar mechanisms, for
example, FreeBSD has kqueue, and Solaris has /dev/poll.
The set of fds that is being monitored via an epoll fd can be viewed the entry for the
epoll fd in the process's /proc/[pid]/fdinfo directory. */

static int
l_impl_epoll_create()
{
  /** epoll_create **
  #include <sys/epoll.h>
  int epoll_create(int size);
  int epoll_create1(int flags);
  Since Linux 2.6.8, the size argument is ignored, but must be greater than zero.
  The kernel used this size hint information to initially allocate internal data
  structures describing events. Nowadays, this hint is no longer required (the
  kernel dynamically sizes the required data structures without needing the hing),
  The positive value of size is in order to ensure backward compatibility when
  new applications run on older kernels.
  epoll_create1() was added to the kernel in version 2.6.27 (epoll_create() is 2.6).
  If flags is 0 then it is the same as epoll_create() with obsolete size argument
  dropped. EPOLL_CLOEXEC can be included in flags to indicate closing fd when exec()
  a new program. 默认情况下，程序中打开的所有文件描述符在exec()执行新程序的过程中保持
  打开并有效。这通常很实用，因为文件描述符在新程序中自动有效，让新程序无需再去了解文件
  名或重新打开。但一些情况下在执行exec()前确保关闭某些特定的文件描述符。尤其是在特权
  进程中调用exec()来启动一个未知程序时，或启动程序并不需要这些已打开的文件描述符时，
  从安全编程的角度出发，应当在加载新程序之前关闭那些不必要的文件描述符。为此，内核为
  每个文件描述符提供了执行时关闭标志，如果设置了这一标志，如果调用exec()成功会自动关闭
  该文件描述符，如果调用exec()失败则文件描述符会继续保持打开。
  系统调用fork()允许一进程（父进程）创建一新进程（子进程），子进程几乎是父进程的翻版
  （可认为父进程一分为二了），它获得了父进程的栈、数据段、堆和执行文本段的拷贝。执行
  fork()时，子进程会获得父进程所有文件描述符的副本，这些副本的创建方式类似于dup()，
  这也意味着父子进程中对应的描述符均指向相同的打开文件句柄。文件句柄包含了当前文件
  偏移量以及文件状态标志，因此这些属性是在父子进程间共享的，例如子进程如果更新了文件
  偏移量，那么这种改变也会影响到父进程中相应的描述符。
  系统调用exec()用于执行一个新程序，它将新程序加载到当前进程中，浙江丢弃现存的程序段，
  并为新程序重新创建栈、数据段以及堆。
  EMFILE - The per-user limit on the number of epoll instances imposed by
  /proc/sys/fs/epoll/max_user_instances was encountered.
  ENFILE - The system-wide limit on the total number of open files has been reached.
  ENOMEM - There was insufficient memory to create the kernel object. */
  int epfd = epoll_create1(0);
  if (epfd == -1) {
    l_loge_1(LNUL, "epoll_create1 %s", lserror(errno));
  }
  return epfd;
}

static void
l_impl_epoll_close(int* hdl)
{
  if (*hdl != -1) {
    if (close(*hdl) != 0) {
      l_loge_1(LNUL, "close epoll %s", lserror(errno));
    }
    *hdl = -1;
  }
}

static l_bool
l_impl_epoll_ctl(int hdl, int op, int fd, struct epoll_event* ev)
{
  if (hdl == -1 || fd == -1 || hdl == fd) {
    l_loge_s(LNUL, "l_impl_epoll_ctl EINVAL");
  }

  /** epoll_event **
  struct epoll_event {
    uint32_t events; // epoll events
    epoll_data_t data; // user data variable
  };
  typedef union epoll_data {
    void* ptr;
    int fd;
    uint32_t u32;
    uint64_t u64;
  } epoll_data_t; */

  ev->events |= EPOLLET; /* edge-triggered */

  /** epoll_ctl **
  #include <sys/epoll.h>
  int epoll_ctl(int epfd, int op, int fd, struct epoll_event* event);
  The epoll interface supports all fds that support poll(2).
  On success, it returns zero. On error, it returns -1 and errno is set.
  EBADF - epfd or fd is not a valid fd.
  EEXIST - op was EPOLL_CTL_ADD, and the supplied fd is already registered.
  EINVAL - epfd is not an epoll fd, or fd is the same as epfd, or the
  requested operation op is not supported by this interface, or an invalid
  event type was specified along with EPOLLEXCLUSIVE, or op was EPOLL_CTL_MOD
  and events include EPOLLEXCLUSIVE, or op was EPOLL_CTL_MOD and the
  EPOLLEXCLUSIVE has previously been applied to this epfd, fd pair; or
  EPOLLEXCLUSIVE was specified in event and fd refers to an epoll instance.
  ELOOP - fd refers to an epoll instance and this EPOLL_CTL_ADD would result
  in a circular loop of epoll instances monitor one another.
  ENOENT - op was EPOLL_CTL_MOD or EPOLL_CTL_DEL, and fd is not registered
  registered with this epoll instance.
  ENOMEM - there was insufficient memory to handle the requested op.
  ENOSPC - the limit imposed by /proc/sys/fs/epoll/max_user_watches was
  encountered while trying to register a new fd on an epoll instance.
  EPERM - the target file fd does not support epoll. this can occur if fd
  refers to, for example, a regular file or a directory. */

  return epoll_ctl(hdl, op, fd, ev) == 0;
}

static l_bool
l_impl_epoll_add(int hdl, int fd, struct epoll_event* ev)
{
  if (l_impl_epoll_ctl(hdl, EPOLL_CTL_ADD, fd, ev)) {
    return true;
  } else {
    if (errno == EEXIST && l_impl_epoll_ctl(hdl, EPOLL_CTL_MOD, fd, ev)) {
      return true;
    } else {
      l_loge_1(LNUL, "l_impl_epoll_add fail %s", lserror(errno));
      return false;
    }
  }
}

static l_bool
l_impl_epoll_mod(int hdl, int fd, struct epoll_event* ev)
{
  if (l_impl_epoll_ctl(hdl, EPOLL_CTL_MOD, fd, ev)) {
    return true;
  } else {
    l_loge_1(LNUL, "l_impl_epoll_mod fail %s", lserror(errno));
    return false;
  }
}

static l_bool
l_impl_epoll_del(int hdl, int fd)
{
  /* In kernel versions before 2.6.9, the EPOLL_CTL_DEL
  operation required a non-null pointer in event, even
  though this argument is ignored. Since Linux 2.6.9, event
  can be specified as NULL when using EPOLL_CTL_DEL.
  Applications that need to be portable to kernels before
  2.6.9 should specify a non-null pointer in event. */
  struct epoll_event ev;
  l_zero_n(&ev, sizeof(struct epoll_event));
  if (l_impl_epoll_ctl(hdl, EPOLL_CTL_DEL, fd, &ev)) {
    return true;
  } else {
    l_loge_1(LNUL, "l_impl_epoll_del fail %s", lserror(errno));
    return false;
  }
}

static void
l_impl_epollmgr_wait(l_impl_epollmgr* mgr, int ms)
{
  /** epoll_wait **
  #include <sys/epoll.h>
  int epoll_wait(int epfd, struct epoll_event* events, int maxevents, int timeout);
  The timeout argument specifies the number of milliseconds that epoll_wait will block.
  Time is measured against the CLOCK_MONOTONIC clock. The call will block until either:
  * a fd delivers an event;
  * the call is interrupted by a signal handler; or
  * the timeout expires.
  Note that the timeout interval will be rounded up to the system clock granularity,
  and kernel scheduling delays mean that the blocking interval may overrun by a small
  amount. Specifying a timeout of -1 causes epoll_wait() to block indefinitely, while
  specifying a timeout of 0 causes epoll_wait() to return immediately, even if no events
  are available.
  In kernel before 2.6.37, a timeout larger than approximately LONG_MAX/HZ ms is treated
  as -1 (i.e., infinity). Thus, for example, on a system where the sizeof(long) is 4 and
  the kernel HZ value is 1000, this means that timeouts greater than 35.79 minutes are
  treated as infinity.
  ---
  On success, it returns the number of fds ready for the requested I/O, or 0 if no fd became
  ready during the requested timeout milliseconds. When an error occurs, epoll_wait() returns
  -1 and errno is set appropriately.
  EBADF - epfd is not a valid fd
  EFAULT - the memory data pointed by events is not accessible with write permission.
  EINTR - the call was interrupted by a signal handler before either any of the requested
  events occured or the timeout expired.
  EINVAL - epfd is not an epoll fd, or maxevents is less than or equal to 0.
  ---
  While one thread is blocked in a call to epoll_wait(), it is possible for another thread
  to add a fd to the waited-upon epoll instance. If the new fd becomes ready, it will cause
  the epoll_wait() call to unblock.
  For a discussion of what may happen if a fd in an epoll instance being monitored by epoll_wait()
  is closed in another thread, see select(2).
  If a fd being monitered by select() is closed in another thread, the result is unspecified.
  On some UNIX systems, select() unblocks and returns, with an indication that the fd is ready
  (a subsequent I/O operation will likely fail with an error, unless another the fd reopened
  between the time select() returned and the I/O operations was performed). On Linux (and some
  other systems), closing the fd in another thread has no effect on select(). In summary, any
  application that relies on a particular behavior in this scenario must be considered buggy.
  Under Linux, select() may report a socket fd as "ready for reading", while nevertheless a
  subsequent read blocks. This could for example happen when data has arrived but upon
  examination has wrong checksum and is discarded. There may be other circumstances in which
  a fd is spuriously reported as ready. Thus it may be safer to use O_NONBLOCK on sockets that
  should not block. */
  int n = epoll_wait(mgr->epfd, mgr->ready, L_MAX_IO_EVENTS, ms);
  if (n == -1) {
    if (errno == EINTR) { /* the call was interrupted by a signal handler */
      l_impl_epollmgr_wait(mgr, 0);
    } else {
      l_loge_1(LNUL, "epoll_wait %s", lserror(errno));
    }
    mgr->nready = 0;
  } else {
    mgr->nready = (n > 0 ? n : 0);
  }
}

static uint32_t l_epoll_mask[] = {
  /* 0x00 */ 0,
  /* 0x01 */ EPOLLIN,    /* L_IO_EVENT_READ */
  /* 0x02 */ EPOLLOUT,   /* L_IO_EVENT_WRITE */
  /* 0x03 */ 0,
  /* 0x04 */ EPOLLPRI,   /* L_IO_EVENT_PRI */
  /* 0x05 */ 0,
  /* 0x06 */ 0,
  /* 0x07 */ 0,
  /* 0x08 */ EPOLLRDHUP, /* L_IO_EVENT_RDH */
  /* 0x09 */ 0
};

static l_ushort l_ioev_rd[] = {
  0, L_IO_EVENT_READ
};

static l_ushort l_ioev_wr[] = {
  0, L_IO_EVENT_WRITE
};

static l_ushort l_ioev_pri[] = {
  0, L_IO_EVENT_PRI
};

static l_ushort l_ioev_rdh[] = {
  0, L_IO_EVENT_RDH
};

static l_ushort l_ioev_hup[] = {
  0, L_IO_EVENT_HUP
};

static l_ushort l_ioev_err[] = {
  0, L_IO_EVENT_ERR
};

static uint32_t
l_gen_epoll_masks(l_umedit masks)
{
  return (l_epoll_mask[masks & L_IO_EVENT_READ] |
          l_epoll_mask[masks & L_IO_EVENT_WRITE] |
          l_epoll_mask[masks & L_IO_EVENT_PRI] |
          l_epoll_mask[masks & L_IO_EVENT_RDH] |
          EPOLLHUP | EPOLLERR);
}

static l_ushort
l_get_ioev_masks(struct epoll_event* ev)
{
  uint32_t masks = ev->events;
  return (l_ioev_rd[(masks & EPOLLIN) != 0] |
          l_ioev_wr[(masks & EPOLLOUT) != 0] |
          l_ioev_pri[(masks & EPOLLPRI) != 0] |
          l_ioev_rdh[(masks & EPOLLRDHUP) != 0] |
          l_ioev_hup[(masks & EPOLLHUP) != 0] |
          l_ioev_err[(masks & EPOLLERR) != 0]);
}

L_EXTERN void
l_ioevmgr_init(l_ioevmgr* self)
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  l_zero_n(mgr, sizeof(l_impl_epollmgr));
  l_mutex_init((l_mutex*)&mgr->wake_lock);
  mgr->epfd = l_impl_epoll_create();
  mgr->wakefd = l_eventfd_create();
}

L_EXTERN void
l_ioevmgr_free(l_ioevmgr* self)
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  mgr->wakefd_added = false;
  mgr->wakeup_count = 0;
  mgr->nready = 0;
  l_mutex_free((l_mutex*)&mgr->wake_lock);
  l_impl_epoll_close(&mgr->epfd);
  l_eventfd_close(&mgr->wakefd);
}

L_EXTERN l_bool
l_ioevmgr_wakeup(l_ioevmgr* self)
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;

  /* if we use a flag like "wait_is_called" to indicate master called epoll_wait() or not,
  and then write eventfd to signal master wakeup only "wait_is_called" is true, then master
  may not be triggered to wakeup. because if this kind of check is performed just before
  master call epoll_wait(), wakeup is not signaled but master will enter into wait state
  next. so dont use this trick. */

  /* here is the another trick to count the wakeup times.
  this function can be called from any thread, the counter
  need to be protected by a lock.*/
  l_mutex_lock((l_mutex*)&mgr->wake_lock);
  if (mgr->wakeup_count) {
    l_mutex_unlock((l_mutex*)&mgr->wake_lock);
    return true; /* already signaled to wakeup */
  }
  mgr->wakeup_count = 1;
  l_mutex_unlock((l_mutex*)&mgr->wake_lock);

  return l_eventfd_write(mgr->wakefd);
}

L_EXTERN l_bool
l_ioevmgr_add(l_ioevmgr* self, l_filehdl hdl, l_ulong ud, l_ushort masks)
{
  /** event masks **
  The bit masks can be composed using the following event types:
  EPOLLIN - The associated file is available for read operations.
  EPOLLOUT - The associated file is available for write operations.
  EPOLLRDHUP (since Linux 2.6.17) - Stream socket peer closed
  connection, or shut down writing half of connection. (This flag
  is especially useful for wirting simple code to detect peer
  shutdown when using Edge Triggered monitoring)
  EPOLLPRI - There is urgent data available for read operations.
  EPOLLERR - Error condition happened on the associated fd.
  epoll_wait will always wait for this event; it is not necessary
  to set it in events.
  EPOLLHUP - Hang up happened on the associated fd. epoll_wait will
  always wait for this event; it is not necessary to set it in
  events. Not that when reading from a channel such as a pipe or a
  stream socket, this event merely indicates that the peer closed
  its end of the channel. Subsequent reads from the channel will
  return 0 (end of file) only after all outstanding data in the
  channel has been consumed.
  EPOLLET - Sets the Edge Triggered behavior for the associated fd.
  The default behavior for epoll is Level Triggered.
  EPOLLONESHOT (since Linux 2.6.2) - Sets the one-shot behavior
  for the associated fd. This means that after an event is pulled
  out with epoll_wait the associated fd is internally disabled and
  no other events will be reported by the epoll interface. The
  user must call epoll_ctrl with EPOLL_CTL_MOD to rearm the fd
  with a new event mask.
  EPOLLWAKEUP (since Linux 3.5) - If EPOLLONESHOT and EPOLLET are
  clear and the process has the CAP_BLOCK_SUSPEND capability, ensure
  that the system does not enter "suspend" or "hibernate" while
  this event is pending or being processed. The event is considered
  as being "processed" from the time when it is returned by a call
  to epoll_wait until the next call to epoll_wait on the same
  epoll fd, the closure of that fd, the removal of the event fd
  with EPOLL_CTL_DEL, or the clearing of EPOLLWAKEUP for the event
  fd with EPOLL_CTL_MOD. If EPOLLWAKEUP is specified, but the caller
  does not have the CAP_BLOCK_SUSPEND capability, then this flag is
  silently ignored. A robust application should double check it has
  the CAP_BLOCK_SUSPEND capability if attempting to use this flag.
  EPOLLEXCLUSIVE (since Linux 4.5) - Sets an exclusive wakeup mode
  for the epoll fd that is being attached to the target fd. When a
  wakeup event occurs and multiple epoll fd are attached to the same
  fd using EPOLLEXCLUSIVE, one or more of the epoll fds will receive
  an event with epoll_wait. The default in this scenario (when
  EPOLLEXCLUSIVE is not set) is for all epoll fds to receive an
  event. EPOLLEXCLUSIVE is thus usefull for avoiding thundering
  herd problems in certain scenarios.
  If the same fd is in multiple epoll instances, some with the
  EPOLLEXCLUSIVE, and others without, then events will be provided
  to all epoll instances that did not specify EPOLLEXCLUSIVE, and
  at least one of the the epoll instances that did specify
  EPOLLEXCLUSIVE.
  The following values may be specified in conjunction with
  EPOLLEXCLUSIVE: EPOLLIN, EPOLLOUT, EPOLLWAKEUP, and EPOLLET.
  EPOLLHUP and EPOLLERR can also be specified, but this is not
  required: as usual, these events are always reported if they
  cocur. Attempts to specify other values yield an error.
  EPOLLEXCLUSIVE may be used only in an EPOLL_CTL_ADD operation;
  attempts to employ it with EPOLL_CTL_MOD yield an error. A call
  to epoll_ctl that specifies EPOLLEXCLUSIVE and specifies the target
  fd as an epoll instance will likewise fail. The error in all of
  these cases is EINVAL. */
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  struct epoll_event ev;
  ev.events = l_gen_epoll_masks(masks);
  ev.data.u64 = ud;
  return l_impl_epoll_add(mgr->epfd, hdl.fd, &ev);
}

L_EXTERN l_bool
l_ioevmgr_mod(l_ioevmgr* self, l_filehdl hdl, l_ulong ud, l_ushort masks)
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  struct epoll_event ev;
  ev.events = l_gen_epoll_masks(masks);
  ev.data.u64 = ud;
  return l_impl_epoll_mod(mgr->epfd, hdl.fd, &ev);
}

L_EXTERN l_bool
l_ioevmgr_del(l_ioevmgr* self, l_filehdl hdl)
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  return l_impl_epoll_del(mgr->epfd, hdl.fd);
}

L_EXTERN int
l_ioevmgr_timed_wait(l_ioevmgr* self, int ms, void (*cb)(l_ulong, l_ushort))
{
  l_impl_epollmgr* mgr = (l_impl_epollmgr*)self;
  struct epoll_event* pcur = 0;
  struct epoll_event* pend = 0;

  if (ms < 0 && ms != -1) { /* timeout cannot be negative except infinity (-1) */
    ms = 0;
  }

  if (ms > 30 * 60 * 1000) { /* Linux may treat the timeout greater than 35.79 minutes as infinity */
    ms = 30 * 60 * 1000; /* 30min */
  }

  if (!mgr->wakefd_added) {
    struct epoll_event e;
    mgr->wakefd_added = true;
    e.events = EPOLLERR | EPOLLIN;
    e.data.u64 = mgr->wakefd;
    if (!l_impl_epoll_add(mgr->epfd, mgr->wakefd, &e)) {
      l_loge_s(LNUL, "epoll_wait add wake fd fail");
    }
  }

  l_impl_epollmgr_wait(mgr, ms);
  if (mgr->nready <= 0) {
    return 0;
  }

  pcur = mgr->ready;
  pend = pcur + mgr->nready;

  for (; pcur < pend; ++pcur) {
    /* l_assert(sizeof(l_filehdl) <= 4) && srvc_id high 32 bit cannot be 0 */
    if (pcur->data.u64 == (l_ulong)mgr->wakefd) {
      l_eventfd_read(mgr->wakefd); /* return > 0 success, -1 block, -2 error */
      l_mutex_lock((l_mutex*)&mgr->wake_lock);
      mgr->wakeup_count = 0;
      l_mutex_unlock((l_mutex*)&mgr->wake_lock);
    } else {
      cb(pcur->data.u64, l_get_ioev_masks(pcur));
    }
  }

  return mgr->nready;
}

L_EXTERN int
l_ioevmgr_wait(l_ioevmgr* self, void (*cb)(l_ulong, l_ushort))
{
  return l_ioevmgr_timed_wait(self, -1, cb);
}

L_EXTERN int
l_ioevmgr_try_wait(l_ioevmgr* self, void (*cb)(l_ulong, l_ushort))
{
  return l_ioevmgr_timed_wait(self, 0, cb);
}
