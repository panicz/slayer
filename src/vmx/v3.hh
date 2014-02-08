#ifndef VMX_HH
#error "vmx/qt.hh should be included from vmx.hh"
#endif

template<typename T>
class v3 {
public:
  T x, y, z;
  v3<T>(void);
  v3<T>(T xx, T yy, T zz);
  v3<T>(v2<T> u);
  v3<T>(qt<T> q);
  T& operator[](unsigned int i);
  v3<T>& operator=(v3<T> u);
  v3<T>& operator+=(v3<T> u);
  v3<T>& operator-=(v3<T> u);
  v3<T>& operator*=(T s);
  v3<T>& operator/=(T s);
  v3<T> operator-(void) const;
  bool operator==(v3<T> u) const;
  bool operator!=(v3<T> u) const;
  bool operator%=(v3<T> u) const; // is equal with tolerance
  T sqr(void) const;
  T norm(void) const;
  v3<T> proj(v3<T> u) const; // projection of this onto u
  static v3<T> randv3();
};

template<typename T>
inline v3<T>::v3(void) 
{
  x = y = z = 0;
}

template<typename T>
inline v3<T>::v3(T xx, T yy, T zz)
{
  x = xx;
  y = yy;
  z = zz;
}

template<typename T>
inline v3<T>::v3(v2<T> u)
{
  x = u.x;
  y = u.y;
  z = 0;
}

template<typename T>
inline v3<T>::v3(qt<T> q)
{
  x = q.v.x;
  y = q.v.y;
  z = q.v.z;
}

template<typename T>
inline T& v3<T>::operator[](unsigned int i)
{
  assert(i<3);
  return *(&x+i);
}

template<typename T>
inline v3<T>& v3<T>::operator=(v3<T> u)
{
  x = u.x;
  y = u.y;
  z = u.z;
  return *this;
}

template<typename T>
inline v3<T>& v3<T>::operator+=(v3<T> u)
{
  x += u.x;
  y += u.y;
  z += u.z;
  return *this;
}

template<typename T>
inline v3<T>& v3<T>::operator-=(v3<T> u)
{
  x -= u.x;
  y -= u.y;
  z -= u.z;
  return *this;
}

template<typename T>
inline v3<T>& v3<T>::operator*=(T s)
{
  x *= s;
  y *= s;
  z *= s;
  return *this;
}

template<typename T>
inline v3<T>& v3<T>::operator/=(T s)
{
  assert(s!=0);
  s = 1/s;
  x *= s;
  y *= s;
  z *= s;
  return *this;
}

template<typename T>
inline v3<T> v3<T>::operator-(void) const
{
  return v3<T>(-x, -y, -z);
}

template<typename T>
inline bool v3<T>::operator==(v3<T> u) const
{
  return (x==u.x) && (y==u.y) && (z==u.z);
}

template<typename T>
inline bool v3<T>::operator!=(v3<T> u) const
{
  return (x!=u.x) || (y!=u.y) || (z!=u.z);
}

template<typename T>
inline bool v3<T>::operator%=(v3<T> u) const
{
  return 
    (fabs(x - u.x) <= tolerance) &&
    (fabs(y - u.y) <= tolerance) &&
    (fabs(z - u.z) <= tolerance);
}

template<typename T>
inline T v3<T>::sqr(void) const
{
  return x*x + y*y + z*z;
}

template<typename T>
inline T v3<T>::norm(void) const
{
  return sqrt(x*x + y*y + z*z);
}

template<typename T, typename S> 
static inline v3<T> operator-(v3<T> u, v3<S> v)
{
  return v3<T>(u.x - v.x, u.y - v.y, u.z - v.z);
}

template<typename T, typename S> 
static inline v3<T> operator+(v3<T> u, v3<S> v) 
{
  return v3<T>(u.x + v.x, u.y + v.y, u.z + v.z);
}

template<typename T, typename S> 
static inline v3<T> operator^(v3<T> u, v3<S> v) 
{
  return v3<T>(u.y*v.z - u.z*v.y,
	     u.z*v.x - u.x*v.z,
	     u.x*v.y - u.y*v.x);
}

template<typename T, typename S> 
static inline T operator*(v3<T> u, v3<S> v)
{
  return (u.x*v.x + u.y*v.y + u.z*v.z);
}

template<typename T, typename S> 
static inline v3<T> operator*(v3<T> u, S s) 
{
  return v3<T>(s*u.x, s*u.y, s*u.z);
}

template<typename T, typename S> 
static inline v3<T> operator*(S s, v3<T> u) 
{
  return v3<T>(s*u.x, s*u.y, s*u.z);
}

template<typename T, typename S> 
static inline v3<T> operator/(v3<T> u, S s) 
{
  assert(s!=0);
  s = 1/s;
  return v3<T>(s*u.x, s*u.y, s*u.z);
}

template<typename T> 
static inline T norm(v3<T> v) {
  return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
}

template<typename T> static inline T sqr(v3<T> v) {
  return v.x*v.x + v.y*v.y + v.z*v.z;
}


template<typename T>
inline v3<T> v3<T>::proj(v3<T> u) const // project this onto u
{
  T u_sqr = u.sqr();
  if(u_sqr < tolerance) // (an almost) zero vector
    return v3<T>(0,0,0);
  return (u*(*this))*u/u_sqr;
}


template<typename T> 
static inline m3x3<T> operator%(v3<T> v, v3<T> u) 
{
  // return matrix dyadic product: v*u^T
  return m3x3<T>(v.x*u.x, v.x*u.y, v.x*u.z,
		 v.y*u.x, v.y*u.y, v.y*u.z,
		 v.z*u.x, v.z*u.y, v.z*u.z);
}

#ifndef NDEBUG
template<typename T> 
static inline std::ostream& operator<<(std::ostream& os, v3<T> v)
{
  return os << "[" << v.x << " " << v.y << " " << v.z << "]";
}
#endif

template<typename T>
inline v3<T> v3<T>::randv3()
  // return unit vector with random direction
{
  v3<T> v;
  do {
    v.x = (rand() - 0.5*RAND_MAX);
    v.y = (rand() - 0.5*RAND_MAX);
    v.z = (rand() - 0.5*RAND_MAX);
  } while(v.sqr() < tolerance);
  return v / v.norm();
}

