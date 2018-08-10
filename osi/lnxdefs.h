#ifndef LNLYLIB_OSI_LNXDEFS_H
#define LNLYLIB_OSI_LNXDEFS_H
#define _POSIX_C_SOURCE 200809L
#define _GNU_SOURCE
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dlfcn.h> /* link with -ldl */
#include <pthread.h>
#include <sys/eventfd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <errno.h>

/** all defines here must based on the os own definitions **/

#if defined(LNLYLIB_AUTOCONF_CODEGEN)
#define L_CODEGEN_FILEHDL_TYPE_SIZE sizeof(int)
#define L_CODEGEN_FILEHDL_IS_SIGNED (1)
#define L_CODEGEN_FILEHDL_EMPTY_VAL "-1"
#define L_CODEGEN_THRHDL_TYPE_SIZE sizeof(pthread_t)
#define L_CODEGEN_THRKEY_TYPE_SIZE sizeof(pthread_key_t)
#define L_CODEGEN_MUTEX_TYPE_SIZE sizeof(pthread_mutex_t)
#define L_CODEGEN_RWLOCK_TYPE_SIZE sizeof(pthread_rwlock_t)
#define L_CODEGEN_CONDV_TYPE_SIZE sizeof(pthread_cond_t)
#define L_CODEGEN_SOCKADDR_TYPE_SIZE sizeof(l_impl_lnxsaddr)
#define L_CODEGEN_IOEVMGR_TYPE_SIZE sizeof(l_impl_epollmgr)
#endif

/** socket address structures **
struct sockaddr {
  sa_family_t sa_family;
  char        sa_data[14];
};
struct sockaddr_in {         // 'in' is for internet
  sa_family_t    sin_family; // address family: AF_INET
  in_port_t      sin_port;   // port in network byte-order
  struct in_addr sin_addr;   // internet address in network byte-order: struct in_addr { uint32_t s_addr; }
};
struct sockaddr_in6 {
    sa_family_t     sin6_family;   // AF_INET6
    in_port_t       sin6_port;     // port number
    uint32_t        sin6_flowinfo; // IPv6 flow information
    struct in6_addr sin6_addr;     // IPv6 address: struct in6_addr { unsigned char s6_addr[16]; }
    uint32_t        sin6_scope_id; // Scope ID (new in 2.4)
};
**********************************************************************/

typedef struct {
  socklen_t len;
  union {
  struct sockaddr sa;
  struct sockaddr_in sa4;
  struct sockaddr_in6 sa6;
  } addr;
} l_impl_lnxsaddr;

#define L_SOCKET_BACKLOG (128)
#define L_MAX_IO_EVENTS (128)

typedef struct {
  int epfd;
  int wakefd;
  int wakefd_added;
  int wakeup_count;
  int nready;
  pthread_mutex_t wake_lock; /* can use spin lock instead */
  struct epoll_event ready[L_MAX_IO_EVENTS+1];
} l_impl_epollmgr;

#endif /* LNLYLIB_OSI_LNXDEFS_H */

