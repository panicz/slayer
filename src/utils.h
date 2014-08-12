#ifndef UTILS_H
#define UTILS_H
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <limits.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __MINGW32__
#include <windows.h>
#undef OUT
#undef near
#undef far
#endif

#define EXPECT __builtin_expect
#define UNLIKELY(x) EXPECT((x), 0)
#define LIKELY(x) EXPECT((x), 1)

// these are convinience macros that are meant to remind about the meaning
// of arguments that are given immediate values
#define NO(arg_name) false
#define DO(arg_name) true
#define AS(arg_name, value) value


#if !HAVE_ASPRINTF
int asprintf(char **strp, const char *fmt, ...);
#endif

#if !HAVE_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap);
#endif

typedef void (*generic_function_pointer_t)();

typedef void *pointer;

struct list {
  void *data;
  struct list *next;
};

static inline struct list *
cons(void *data, struct list *next) {
  struct list *cell = (struct list *) malloc(sizeof(struct list));
  cell->data = data;
  cell->next = next;
  return cell;
}

static inline struct list *
list_ref(struct list* l, bool (*condition)(struct list *)) {
  for( ; l; l = l->next) {
    if(condition(l)) {
      return l;
    }
  }
  return NULL;
}

#define LIST_PUSH(list, item) list = cons(item, list)

static inline
unsigned int now() {
  return (unsigned int) (100 * clock() / CLOCKS_PER_SEC);
}

#define OUT(msg, ...) (void) fprintf(stderr, msg "\n", ## __VA_ARGS__ )

#define OUT_(msg, ...) (void) fprintf(stderr, msg, ## __VA_ARGS__ )

#define _TOSTRING(x) # x

#define TOSTRING(x) _TOSTRING(x)

#define LOG(expr)						\
  ({OUT("[ %09u ] %s/%s[%d]: "_TOSTRING(expr), now(), __FILE__,	\
	__FUNCTION__,__LINE__);expr;})

#define LOGTIME(expr)							\
  ({int time=now();expr;time=now()-time;				\
    OUT("[ %09u ] %s/%s,%d: "_TOSTRING(expr)" (%d ticks)", now(),	\
	__FILE__, __FUNCTION__,__LINE__,time);})

#define FATAL(msg, ...)							\
  do { fprintf(stderr, "%s/%s[%d]: FATAL ERROR: " msg "\n",		\
	       __FILE__, __FUNCTION__, __LINE__, ## __VA_ARGS__ );	\
    exit(0); } while(0)

#define NELEMS(a) ((int)(sizeof(a)/sizeof(a[0])))

#define WARN(msg, ...)							\
  (void) fprintf(stderr, "[ %09u ] %s/%s: " msg "\n", now(), __FILE__,	\
		 __FUNCTION__, ## __VA_ARGS__ )

#define TODO(arg, ...)

#define NOTE(msg, ...) WARN_ONCE("NOTE: " msg, ## __VA_ARGS__ )

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

#define DEPRECATED(msg) WARN_ONCE("This function is deprecated. " msg)

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
