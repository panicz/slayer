#ifndef VMX_HH
#error "vmx/v2.hh should be included from vmx.hh"
#endif

template<typename T>
class v2 {
public:
  T x, y;
  v2(void);
  v2(T xx, T yy);
  T &operator[](unsigned int i);
  v2<T>& operator=(v2 u);
  v2<T>& operator+=(v2<T> u);
  v2<T>& operator-=(v2<T> u);
  v2<T>& operator*=(T s);
  v2<T>& operator/=(T s);
  v2<T> operator-(void) const;
  bool operator==(v2<T> u) const;
  bool operator!=(v2<T> u) const;
  bool operator%=(v2<T> u) const; // is equal within tolerance
  T sqr(void) const;
  T norm(void) const;
  v2<T> proj(v2<T> u) const;
};

template<typename T>
inline v2<T>::v2(void)
{
  x = y = 0;
}

template<typename T>
inline v2<T>::v2(T xx, T yy)
{
  x = xx;
  y = yy;
}

template<typename T>
inline T& v2<T>::operator[](unsigned int i)
{
  assert(i<2);
  return *(&x+i);
}

template<typename T>
inline v2<T>& v2<T>::operator=(v2<T> u)
{
  x = u.x;
  y = u.y;
  return *this;
}

template<typename T>
inline v2<T>& v2<T>::operator+=(v2<T> u)
{
  x += u.x;
  y += u.y;
  return *this;
}

template<typename T>
inline v2<T>& v2<T>::operator-=(v2<T> u)
{
  x -= u.x;
  y -= u.y;
  return *this;
}

template<typename T>
inline v2<T>& v2<T>::operator*=(T s)
{
  x *= s;
  y *= s;
  return *this;
}

template<typename T>
inline v2<T>& v2<T>::operator/=(T s)
{
  assert(s != 0);
  s = 1/s;
  x *= s;
  y *= s;
  return *this;
}

template<typename T>
inline v2<T> v2<T>::operator-(void) const
{
  return v2<T>(-x, -y);
}

template<typename T>
inline bool v2<T>::operator==(v2<T> u) const
{
  return (x==u.x) && (y==u.y);
}

template<typename T>
inline bool v2<T>::operator!=(v2<T> u) const
{
  return (x!=u.x) || (y!=u.y);
}

template<typename T>
inline bool v2<T>::operator%=(v2<T> u) const
{
  return 
    (fabs(x - u.x) <= tolerance) &&
    (fabs(y - u.y) <= tolerance);
}

template<typename T>
inline T v2<T>::sqr(void) const
{
  return x*x + y*y;
}

template<typename T>
inline T v2<T>::norm(void) const
{
  return sqrt(x*x + y*y);
}

template<typename T> 
static inline v2<T> operator-(v2<T> u, v2<T> v)
{
  return v2<T>(u.x - v.x, u.y - v.y);
}

template<typename T> 
static inline v2<T> operator+(v2<T> u, v2<T> v) 
{
  return v2<T>(u.x + v.x, u.y + v.y);
}

template<typename T> static 
inline T operator^(v2<T> u, v2<T> v) 
{
  return (u.x*v.y - u.y*v.x);
}

template<typename T, typename S> 
static inline T operator*(v2<T> u, v2<S> v)
{
  return (u.x*v.x + u.y*v.y);
}

template<typename T, typename S> 
static inline v2<T> operator*(v2<T> u, S s) 
{
  return v2<T>(s*u.x, s*u.y);
}

template<typename T, typename S> 
static inline v2<T> operator*(S s, v2<T> u) 
{
  return v2<T>(s*u.x, s*u.y);
}

template<typename T, typename S> 
static inline v2<T> operator/(v2<T> u, S s) 
{
  assert(s!=0);
  s = 1/s;
  return v2<T>(s*u.x, s*u.y);
}

template<typename T> 
static inline T norm(v2<T> v) 
{
  return sqrt(v.x*v.x + v.y*v.y);
}

template<typename T> 
static inline T sqr(v2<T> v) 
{
  return v.x*v.x + v.y*v.y;
}

template<typename T>
inline v2<T> v2<T>::proj(v2<T> u) const // project this onto u
{
  T u_sqr = u.sqr();
  if(u_sqr < tolerance) // (an almost) zero vector
    return v2<T>(0,0);
  return (u*(*this))*u/u_sqr;
}

#ifndef NDEBUG
template<typename T> 
static inline std::ostream& operator<<(std::ostream& os, v2<T> v)
{
  return os << "[" << v.x << " " << v.y << "]";
}
#endif
