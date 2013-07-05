#ifndef UTILS_H
#define UTILS_H
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>

static inline
unsigned int now() {
  return (unsigned int) (100 * clock() / CLOCKS_PER_SEC);
}

#define OUT(msg, ...) fprintf(stderr, msg "\n", ## __VA_ARGS__ )

#define OUT_(msg, ...) fprintf(stderr, msg, ## __VA_ARGS__ )

#define STR(x) # x

#define TOSTRING(x) STR(x)

#define LOG(expr)						\
  ({OUT("[ %09u ] %s/%s[%d]: "STR(expr), now(), __FILE__,	\
	__FUNCTION__,__LINE__);expr;})

#define LOGTIME(expr)						      \
  ({int time=now();expr;time=now()-time;			      \
    OUT("[ %09u ] %s/%s,%d: "STR(expr)" (%d ticks)", now(), __FILE__, \
	__FUNCTION__,__LINE__,time);})

#define FATAL(msg, ...)							\
  do { fprintf(stderr, "FATAL ERROR: " msg "\n", ## __VA_ARGS__ );	\
    exit(0); } while(0)

#define NELEMS(a) ((int)(sizeof(a)/sizeof(a[0])))

#define WARN(msg, ...)							\
  (void) fprintf(stderr, "[ %09u ] %s/%s: " msg "\n", now(), __FILE__,	\
		 __FUNCTION__, ## __VA_ARGS__ )

#ifdef __cplusplus
#define WARN_UPTO(n, msg, ...)						\
  ({static int __warn_counter_##n;					\
  if(__warn_counter_##n++ < n) {					\
    OUT("[ %09u ] %s/%s: " msg " (warning %i of %i)",			\
	now(), __FILE__, __FUNCTION__, ## __VA_ARGS__,			\
	__warn_counter_##n, n);						\
  }})
#else
#define WARN_UPTO(n, msg, ...)						\
  ({ void __fn__(const char *f) {					\
      static int c=0;							\
      if(c++ < n) OUT("[ %09u ] %s/%s: " msg " (warning %i of %i)",	\
		      now(), __FILE__, f, ## __VA_ARGS__, c, n);	\
    } __fn__(__FUNCTION__); })
#endif

#define WARN_ONCE(msg, ...) WARN_UPTO(1, msg, ## __VA_ARGS__ )

#define TRY(f)								\
  if((f) == -1) {							\
    perror(__FILE__ ", " TOSTRING(__LINE__)  " [" #f "]");		\
  }

#define DEF_MINMAX(type, suffix)		\
  static inline					\
  type max##suffix(type a, type b) {		\
    return (a >= b) ? a : b;			\
  }						\
  static inline					\
  type min##suffix(type a, type b) {		\
    return (a < b) ? a : b;			\
  }

DEF_MINMAX(int, i);
DEF_MINMAX(float, f);

#undef DEF_MINMAX


//do { } while(0)


#endif /* UTILS_H */
