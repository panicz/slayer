#ifndef VMX_HH
#error "vmx/math.hh should be included from vmx.hh"
#endif

template<typename T>
static inline T sgn (T x) // "signum function"
{ 
  if(x == 0)
    return 0;
  return (x > 0) ? 1 : -1;
}

template<typename T>
static inline int signum(T x)
{
  return (x == 0) ? 0 : (x > 0) ? 1 : -1;
}

template<typename T>
static inline T zpsgn (T x) // "zero-positive signum"
{
  return (x >= 0) ? 1 : -1;
}

template<typename T>
static inline T znsgn (T x) // "zero-negative signum"
{
  return (x > 0) ? 1 : -1;
}

template<typename T>
static inline T heavyside (T x)
{
  return (x >= 0) ? 1 : 0;
}

template<typename T>
static inline T max(T a, T b)
{
  return (a > b) ? a : b;
}

template<typename T>
static inline T min(T a, T b)
{
  return (a < b) ? a : b;
}

template<typename T>
static inline T sqr(T x)
{
  return x*x;
}

template<typename T>
static inline T deg2rad(T x)
{
  return M_PI/180.0*(x);
}

template<typename T>
static inline T rad2deg(T x) 
{
  return 180.0/M_PI*(x);
}

template<typename T>
static inline T sindeg(T x)
{
  return sin(M_PI/180*(x));
}

template<typename T>
static inline T cosdeg(T x) 
{
  return cos(M_PI/180*(x));
}
