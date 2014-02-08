#ifndef VMX_HH
#error "vmx/qt.hh should be included from vmx.hh"
#endif

/* You can remember this name as an acronym derived from
   "QuaTernion<T>". */

template<typename T>
class qt {
public:
  T s;
  v3<T> v;
  qt<T>(void);
  qt<T>(T ss, v3<T> vv);
  qt<T>(T ss, T xx, T yy, T zz);
  qt<T>(v3<T> vv);  // constructs raw quaternion
  qt<T>(v4<T> v4); // see at the bottom
  qt<T>(v3<T> u, v3<T> w); // constructs a quaternion representing 
  // rotation from u to w, upon Stan Melax's article from "Game Programming 
  // Gems vol.1", Charles River Media 2000, chapter 2.10

  qt<T>& operator=(qt<T> q);
  qt<T>& operator/=(T ss);
  qt<T>& operator+=(qt<T> q);
  qt<T>& operator-=(qt<T> q);
  qt<T> operator~(void) const; // conjugation
  T sqr(void) const; // squared magnitude of quaternion
  T norm(void) const;  // magnitude
  v3<T> rotate(v3<T> u) const; // returns q*u*~q, but is faster
};

template<typename T>
inline qt<T>::qt(void) 
{
  s = 1;
  v = v3<T>(0,0,0);
}

template<typename T>
inline qt<T>::qt(T ss, v3<T> vv) 
{
  s = ss;
  v = vv;
}

template<typename T>
inline qt<T>::qt(T ss, T xx, T yy, T zz) 
{
  s = ss;
  v = v3<T>(xx, yy, zz);
}

template<typename T>
inline qt<T>::qt(v3<T> vv)  // constructs raw quaternion
{ 
  s = 0;
  v = vv;
}

template<typename T>
inline qt<T>::qt(v4<T> v4) 
{
    s = v4.w;
    v = v3<T>(v4.x, v4.y, v4.z);
}

template<typename T>
inline qt<T>::qt(v3<T> u, v3<T> w) // rotation from u to w
{ // This is borrowed from Game Programming Gems vol.1 chpt 2.10 by Stan Melax

  u /= u.norm();
  w /= w.norm();
  v = u^w; // v is now perpendicular to u and w and its magnitude = sin(<u,w)
  T f = sqrt(2.0*(u*w + 1.0));
  
  // if f < tolerance the solution becomes numerically unstable!!!!
  if(f > tolerance) {
    s = 0.5*f;
    v /= f;
    return;
  } 

  if(u*v > 0) {
    assert(fabs(1 - u*v) < tolerance);
    s = 1;
    v = v3<T>(0,0,0);
    return;
  }

  do { v = v3<T>::randv3(); } while( (v^u).sqr() < tolerance);
  v -= v.proj(u);
  v /= v.norm();
  assert(abs(v*u) < tolerance);
  s = 0;
}

template<typename T>
inline qt<T>& qt<T>::operator=(qt<T> q) 
{ 
  s = q.s;
  v = q.v; 
  return *this; 
}

template<typename T>
inline qt<T>& qt<T>::operator/=(T ss) 
{
  assert(ss!=0);
  ss = 1/ss;
  s *= ss;
  v *= ss;
  return *this;
}

template<typename T>
inline qt<T>& qt<T>::operator+=(qt<T> q) 
{
  s += q.s;
  v += q.v;
  return *this;
}


template<typename T>
inline qt<T>& qt<T>::operator-=(qt<T> q)
{
  s -= q.s;
  v -= q.v;
  return *this;
}

template<typename T>
inline qt<T> qt<T>::operator~(void) const // returns conjugation
{ 
  return qt<T>(s, -v);
}

template<typename T>
inline T qt<T>::sqr(void) const // returns squared magnitude of quaternion
{ 
  return s*s + v.x*v.x + v.y*v.y + v.z*v.z;
}

template<typename T>
inline T qt<T>::norm(void) const // returns magnitude
{
  return sqrt(s*s + v.x*v.x + v.y*v.y + v.z*v.z);
}

template<typename T, typename S> 
static inline qt<T> operator*(qt<T> q, qt<S> p)
{
  return qt<T>(q.s*p.s-q.v*p.v, (q.s*p.v)+(p.s*q.v)+(q.v^p.v));
}

template<typename T, typename S> 
static inline qt<T> operator/(qt<T> q, S s)
{
  return qt<T>(q.s/s, q.v/s);
}

template<typename T, typename S> 
static inline qt<T> operator+(qt<T> p, qt<S> q)
{
  return qt<T>(p.s+q.s, p.v+q.v);
}

template<typename T, typename S> 
static inline qt<T> operator-(qt<T> p, qt<S> q)
{
  return qt<T>(p.s-q.s, p.v-q.v);
}

template<typename T, typename S> 
static inline qt<T> operator*(S s, qt<T> q)
{
  return qt<T>(s*q.s, s*q.v);
}

template<typename T, typename S> 
static inline qt<T> operator*(qt<T> q, S s)
{
  return qt<T>(s*q.s, s*q.v);
}

template<typename T>
inline v3<T> qt<T>::rotate(v3<T> u) const
{
  /* 
     This function is optimized equivalent of operation 

       v = q*[0,v]*~q

     We know that the multiplication of two arbitrary
     quaternions is

       q*p = [q.s*p.s-q.v*p.v, (q.s*p.v)+(p.s*q.v)+(q.v^p.v)]

     Therefore, if we substitute to p.s zero, we obtain

       q*[0,v] = [-q.v*v, q.s*v + q.v^v]

     Now, we multiply this quaternion by ~q = [q.s, -q.v]

       [-q.v*v, q.s*v + q.v^v]*[q.s, -q.v] = 

         = [(-q.v*v)*q.s - (q.s*v + q.v^v)*(-q.v),
	    (-q.v*v)*(-q.v)+q.s*(q.s*v + q.v^v)+(q.s*v+q.v^v)^(-q.v)]
	  77 +-* ops
     
	 = [q.s*q.v*v - q.s*v*q.v + v*(q.v^q.v),
	    q.v*(q.v*v) + q.s*q.s*v + q.s*q.v^v - q.s*(v^q.v) + q.v^(q.v^v)]

         = [0, q.v*(q.v*v) + q.s*q.s*v + 2*q.s*q.v^v + 
	                                      + q.v*(q.v*v) - v*(q.v*q.v)] =

	 = [0, 2*((q.v*v)*q.v + q.s*q.v^v) + v*(q.s*q.s - (q.v*q.v))]

          37 +-* ops. m3x3*v3 takes 15 operations, but m3x3(qt) uses 45.
	  Matrix multiplication also uses 45. Quaternion multiplication
	  27. This means that quaternion operations should be used to combine
	  the rotations, and to rotate more than two vectors it's better to
	  compute the matrix.

 */
  return (2*((v*u)*v + (s*v^u)) + u*(s*s - v*v));
}

#ifndef NDEBUG
template<typename T> 
static inline std::ostream& operator<<(std::ostream& os, qt<T> q)
{
  return os << "(" << q.s << ", " << q.v << ")";
}
#endif
