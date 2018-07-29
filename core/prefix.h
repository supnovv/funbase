#ifndef L_PLAT_PREFIX_H
#define L_PLAT_PREFIX_H

#undef L_PLAT_APPLE
#undef L_PLAT_IOS
#undef L_PLAT_BSD

#if defined(__linux__) /* Linux (Centos, Debian, Fedora, OpenSUSE, RedHat, Ubuntu) */
  #ifndef L_PLAT_LINUX
  #error "L_PLAT_LINUX is not defined"
  #endif
#elif defined(__APPLE__) && defined(__MACH__)
#define L_PLAT_APPLE /* Apple OSX and iOS (Darwin) */
#include <TargetConditionals.h>
/*                      Mac OSX  iOS	  iOS Simulator
TARGET_OS_EMBEDDED      0        1        0
TARGET_OS_IPHONE        0        1        1
TARGET_OS_MAC           1        1        1
TARGET_IPHONE_SIMULATOR 0        0        1 */
#if TARGET_OS_EMBEDDED == 1 || TARGET_OS_IPHONE == 1
#define L_PLAT_IOS /* iPhone, iPad, simulator, etc. */
#elif TARGET_OS_MAC == 1 /* MACOSX */
  #ifndef L_PLAT_MACOSX
  #error "L_PLAT_MACOSX is not defined"
  #endif
#endif
#elif defined(__unix__)
#include <sys/param.h>
#if defined(BSD)
#define L_PLAT_BSD /* DragonFly BSD, FreeBSD, OpenBSD, NetBSD */
#endif
#elif defined(_MSC_VER) && defined(_WIN32) /* Microsoft Windows */
  #ifndef L_PLAT_WINDOWS
  #error "L_PLAT_WINDOWS is not defined"
  #endif
#endif

#undef L_CMPL_CLANG
#undef L_CMPL_ICC
#undef L_CMPL_GCC
#undef L_CMPL_MSC

#if defined(__clang__)
#define L_CMPL_CLANG /* Clang/LLVM, also defines __GNUC__ or __GNUG__ */
#elif defined(__ICC) || defined(__INTEL_COMPILER)
#define L_CMPL_ICC /* Intel ICC/ICPC, also defines __GNUC__ or __GNUG__ */
#elif defined(__GNUC__) || defined(__GNUG__)
#define L_CMPL_GCC /* GNU GCC/G++ */
#elif defined(_MSC_VER)
#define L_CMPL_MSC /* Microsoft Visual Studio */
#endif

#undef L_NEWLINE
#undef L_NL_SIZE
#undef L_PATH_SEP

#if defined(L_PLAT_WINDOWS)
#define L_NEWLINE "\r\n"
#define L_NL_SIZE 2
#define L_PATH_SEP "\\"
#else
#define L_NEWLINE "\n"
#define L_NL_SIZE 1
#define L_PATH_SEP "/"
#endif

#undef L_INLINE
#undef L_EXTERN
#undef L_THREAD_LOCAL

#define L_INLINE static

#if defined(L_BUILD_SHARED)
#if defined(__GNUC__)
#define L_EXTERN extern
#else
#if defined(LNLYLIB_API_IMPL)
#define L_EXTERN __declspec(dllexport)
#else
#define L_EXTERN __declspec(dllimport)
#endif
#endif
#else
#define L_EXTERN extern
#endif

/** Thread-Local Storage **
https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Thread-Local.html#Thread-Local
https://www.akkadia.org/drepper/tls.pdf
---
Thread-local storage (TLS) model in GCC requires significant support
from the linker (ld), dynamic linker (ld.so), and system libraries
(libc.so and libpthread.so), so it is not available everywhere.
The TLS keyword __thread can be used like this:
    __thread int i;
    extern __thread struct state s;
    static __thread char *p;
The __thread specifier may be applied to any global, file-scoped
static, funciton-scoped static, or static data member of a class.
It may not be applied to block-scoped automatic or non-static data
member.
When the address-of operator is applied to a thread-local variable,
it is evaluated at run time and returns the address of the current
thread's instance of that variable. An address so obtained may be
used by any thread. When a thread terminates, any pointers to
thread-local variables in that thread become invalid.
No static initialization may refer to the address of a thread-local
variable. In C++, if an initializer is present for a thread-local
variable, it must be a constant-expression.
**********************************************************************/

#if defined(L_CMPL_GCC)
#define L_THREAD_LOCAL __thread 
#elif defined(L_CMPL_MSC)
#define L_THREAD_LOCAL __declspec(thread) 
#endif

typedef union {
  double d;
  char a[8];
} l_eightbyte;

#undef L_DEFINE_STRUCT_OF_SIZE
#define L_DEFINE_STRUCT_OF_SIZE(name, size) \
  typedef struct {\
    l_eightbyte impl[((size) - 1) / sizeof(l_eightbyte) + 1];\
  } name

#endif /* L_PLAT_PREFIX_H */

