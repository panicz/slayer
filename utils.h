#ifndef UTILS_H
#define UTILS_H
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <SDL.h>

#define now() SDL_GetTicks()
#define OUT(msg, ...) fprintf(stderr, msg "\n", ## __VA_ARGS__ )
#define OUT_(msg, ...) fprintf(stderr, msg, ## __VA_ARGS__ )
#define STR(x) # x
#define LOG(expr) ({OUT("[ %09d ] %s/%s[%d]: "STR(expr),now(),__FILE__,__FUNCTION__,__LINE__);expr;})
#define LOGTIME(expr) \
  ({int time=now();expr;time=now()-time;\
    OUT("[ %09d ] %s/%s,%d: "STR(expr)" (%d ticks)",now(),__FILE__,__FUNCTION__,__LINE__,time);})

#define FATAL(msg, ...) do { fprintf(stderr, "FATAL ERROR: " msg "\n", ## __VA_ARGS__ ); exit(0); } while(0)
#define NELEMS(array) (sizeof(array)/sizeof array[0])
#define WARN(msg, ...) fprintf(stderr, "[ %09d ] %s/%s: " msg "\n", now(), __FILE__, __FUNCTION__, ## __VA_ARGS__ )
#define WARN_UPTO(n, msg, ...) \
  ({ void __fn__(const char *f) { \
      static int c=0; \
      if(c++ < n) OUT("[ %09d ] %s/%s: " msg " (warning %i of %i)", now(), __FILE__, f, ## __VA_ARGS__, c, n); \
    } __fn__(__FUNCTION__); })
#define WARN_ONCE(msg, ...) WARN_UPTO(1, msg, ## __VA_ARGS__ )

//do { } while(0)



#endif /* UTILS_H */
