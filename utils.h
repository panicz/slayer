#ifndef UTILS_H
#define UTILS_H
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#define TOSTRING(x) # x
#define LOG(expr) fprintf(stderr, "%s/%s(%d): " TOSTRING(expr) "\n", __FILE__, __FUNCTION__, __LINE__); expr
#define OUT(msg, ...) fprintf(stderr, msg "\n", ## __VA_ARGS__ )
#define FATAL(msg, ...) do { fprintf(stderr, "FATAL ERROR: " msg "\n", ## __VA_ARGS__ ); exit(0); } while(0)
#define NELEMS(array) (sizeof(array)/sizeof array[0])
#define WARN(msg, ...) fprintf(stderr, "%s/%s: " msg "\n", __FILE__, __FUNCTION__, ## __VA_ARGS__ )
#define WARN_UPTO(n, msg, ...) \
  ({ void __fn__(const char *f) { \
      static int c=0; \
      if(c++ < n) OUT("%s/%s: " msg " (warning %i of %i)", __FILE__, f, ## __VA_ARGS__, c, n); \
    } __fn__(__FUNCTION__); })
#define WARN_ONCE(msg, ...) WARN_UPTO(1, msg, ## __VA_ARGS__ )

//do { } while(0)



#endif /* UTILS_H */
