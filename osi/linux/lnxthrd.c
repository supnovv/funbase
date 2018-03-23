#include "plat/lnx/lnxpref.h"

#define L_API_IMPL
#include "plat/lnx/lnxthrd.h"

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


L_EXTERN l_thrhdl
l_thrhdl_self() {
  l_thrhdl thrhdl;
  *((pthread_t*)&thrhdl) = pthread_self();
  return thrhdl;
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

