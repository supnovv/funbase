#ifndef L_PLAT_PREFIX_H
#define L_PLAT_PREFIX_H

#undef L_PLAT_LINUX
#undef L_PLAT_APPLE
#undef L_PLAT_WINDOWS
#undef L_PLAT_OSX
#undef L_PLAT_IOS
#undef L_PLAT_BSD

#if defined(__linux__)
#define L_PLAT_LINUX /* Linux (Centos, Debian, Fedora, OpenSUSE, RedHat, Ubuntu) */
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
#elif TARGET_OS_MAC == 1
#define L_PLAT_OSX /* MACOSX */
#endif
#elif defined(__unix__)
#include <sys/param.h>
#if defined(BSD)
#define L_PLAT_BSD /* DragonFly BSD, FreeBSD, OpenBSD, NetBSD */
#endif
#elif defined(_MSC_VER) && defined(_WIN32)
#define L_PLAT_WINDOWS /* Microsoft Windows */
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
#undef L_THREADLOCAL

#define L_INLINE static

#if defined(L_BUILD_SHARED)
#if defined(__GNUC__)
#define L_EXTERN extern
#else
#if defined(L_API_IMPL)
#define L_EXTERN __declspec(dllexport)
#else
#define L_EXTERN __declspec(dllimport)
#endif
#endif
#else
#define L_EXTERN extern
#endif

#if defined(L_CMPL_GCC)
#define L_THREAD_LOCAL __thread 
#elif defined(L_CMPL_MSC)
#define L_THREAD_LOCAL __declspec(thread) 
#endif

#endif /* L_PLAT_PREFIX_H */

