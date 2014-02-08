#ifndef VMX_HH
#error "vmx/v4.hh should be included from vmx.hh"
#endif

template<typename T>
class v4 {
public:
  T x, y, z, w;
  v4<T>(void);
  v4<T>(T xx, T yy, T zz, T ww);
  v4<T>(v3<T> u, T ww);
  v4<T>(qt<T> q);
  T& operator[](unsigned int i);
  v4<T>& operator=(v4<T> u4);
  v4<T>& operator+=(v4<T> u4);
  v4<T>& operator-=(v4<T> u4);
  v4<T>& operator*=(T s);
  v4<T>& operator/=(T s);
  v4<T> operator-(void) const;
};

template<typename T>
inline v4<T>::v4(void)
{
  x = y = z = w = 0;
}

template<typename T>
inline v4<T>::v4(T xx, T yy, T zz, T ww)
{
  x = xx;
  y = yy;
  z = zz;
  w = ww;
}

template<typename T>
inline v4<T>::v4(v3<T> u, T ww)
{
  x = u.x;
  y = u.y;
  z = u.z;
  w = ww;
}

template<typename T>
inline v4<T>::v4(qt<T> q)
{
  v4<T>(q.v, q.s);
}

template<typename T>
inline T& v4<T>::operator[](unsigned int i)
{
  assert(i<4);
  return *(&x+i); 
}

template<typename T>
inline v4<T>& v4<T>::operator=(v4<T> u4)
{
  x = u4.x;
  y = u4.y;
  z = u4.z;
  w = u4.w;
  return *this;
}

template<typename T>
inline v4<T>& v4<T>::operator+=(v4<T> u4)
{
  x += u4.x;
  y += u4.y;
  z += u4.z;
  w += u4.w;
  return *this;
}

template<typename T>
inline v4<T>& v4<T>::operator-=(v4<T> u4)
{
  x -= u4.x;
  y -= u4.y;
  z -= u4.z;
  w -= u4.w;
  return *this;
}

template<typename T>
inline v4<T>& v4<T>::operator*=(T s)
{
  x *= s;
  y *= s;
  z *= s;
  w *= s;
  return *this;
}

template<typename T>
inline v4<T>& v4<T>::operator/=(T s)
{
  assert(s!=0);
  s = 1/s;
  x *= s;
  y *= s;
  z *= s;
  w *= s;
  return *this;
}

template<typename T>
inline v4<T> v4<T>::operator-(void) const
{
  return v4<T>(-x, -y, -z, -w);
}

template<typename T, typename S> 
static inline T operator*(v4<T> u, v4<S> v)
{
  return (u.x*v.x + u.y*v.y + u.z*v.z + u.w*v.w);
}

template<typename T, typename S> 
static inline v4<T> operator*(v4<T> u4, S s) 
{
  return v4<T>(s*u4.x, s*u4.y, s*u4.z, s*u4.w);
}

template<typename T, typename S> 
static inline v4<T> operator*(S s, v4<T> u4) 
{
  return v4<T>(s*u4.x, s*u4.y, s*u4.z, s*u4.w);
}

template<typename T, typename S> 
static inline v4<T> operator/(v4<T> u4, S s) 
{
  assert(s!=0);
  s = 1/s;
  return v4<T>(s*u4.x, s*u4.y, s*u4.z, s*u4.w);
}



template<typename T, typename S> 
static inline v4<T> operator-(v4<T> u, v4<S> v) 
{
  return v4<T>(u.x - v.x, u.y - v.y, u.z - v.z, u.w - v.w);
}

template<typename T, typename S> 
static inline v4<T> operator+(v4<T> u, v4<S> v) 
{
  return v4<T>(u.x + v.x, u.y + v.y, u.z + v.z, u.w + v.w);
}


#ifndef NDEBUG
template<typename T> 
static inline std::ostream& operator<<(std::ostream& os, v4<T> v)
{
  return os << "[" << v.x << " " << v.y << " " << v.z << " " << v.w << "]";
}
#endif
