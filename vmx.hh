/* vmx.hh: this library contains a set of common operations on three- 
   and four-dimensional vectors, square matrices and quaternions

   Although multiplying 4x4 matrices is implemented, it's generally a
   good idea to avoid it and work on 3x3's as much as possible.
   Inv function wasn't implemented for 4x4's (this isn't matlab)

   The main disadvantage of this library is matrix indexing: we reach
   an element by giving [col][row] (which conforms to  OpenGL 
   convention) and the indices are from 0 to 2 for 3-element 
   matrices and vectors, and respectively 0 to 3 for 4-element ones,
   unlike in linear algebra.

*/
#ifndef VMX_HH
#define VMX_HH

#include <assert.h>
#include <math.h>
#include <stdlib.h> /* for rand */

#ifndef NDEBUG
#  include <iostream>
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

const float tolerance = 0.0001f; //used in %= comparison and elsewhere

template<typename T> class v2;
template<typename T> class v3;
template<typename T> class qt;
template<typename S> class m3x3;
template<typename T> class v4;
template<typename S> class m4x4;

/* All operators declared on vectors are predictable:
   addition, substraction, dot product (*), cross product (^),
   scalar division and multiplication (/, *), dyadic product (%) */

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

/* You can remember this name as an acronym derived from
   "QuaTernion<T>". */

template<typename T>
class qt {
public:
  v3<T> v;
  T s;
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

template<typename S>
class m3x3 {
public:
  v3<S> c[3];
  /* Think of c as columns, ie. read c[1][2] as "column one, row two".
     This conforms to the OpenGL convention, but differs from common
     customs in linear algebra, where row is given first */

  m3x3<S>(void); // initialize identity
  m3x3<S>(S r0c0, S r0c1, S r0c2,
	  S r1c0, S r1c1, S r1c2,
	  S r2c0, S r2c1, S r2c2);
  m3x3<S>(v3<S> c0, v3<S> c1, v3<S> c2); 
  // note that vectors are inserted as columns

  m3x3<S>(qt<S> q); // if q is unit quaternion, initialize rotation matrix
  // corresponding to the rotation represented by q
  m3x3<S>(v3<S> u, v3<S> v); // matrix vector (dyadic) product: u * v^T

  v3<S>& operator[](unsigned int i);
  m3x3<S>& operator=(m3x3<S> A);
  m3x3<S>& operator+=(m3x3<S> A);
  m3x3<S>& operator-=(m3x3<S> A);
  m3x3<S>& operator*=(S s);
  m3x3<S>& operator/=(S s);
  m3x3<S> operator-(void) const;
  m3x3<S> T(void); // returns transpose
  m3x3<S> inv(void); // returns inverse
};

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

template<typename S>
class m4x4 {
public:
  v4<S> c4[4];
  m4x4<S>(void); // initialize identity
  m4x4<S>(m3x3<S> A); // extend A with 0's 
  m4x4<S>(m3x3<S> A, v4<S> u4); // extend A with u4
  m4x4<S>(S r0c0, S r0c1, S r0c2, S r0c3,
	  S r1c0, S r1c1, S r1c2, S r1c3,
	  S r2c0, S r2c1, S r2c2, S r2c3,
	  S r3c0, S r3c1, S r3c2, S r3c3);
  m4x4<S>(v4<S> c40, v4<S> c41, v4<S> c42, v4<S> c43);
  v4<S>& operator[](unsigned int i);
  m4x4<S>& operator=(m4x4<S> A4);
  m4x4<S>& operator+=(m4x4<S> A4);
  m4x4<S>& operator-=(m4x4<S> A4);
  m4x4<S>& operator*=(S s);
  m4x4<S>& operator/=(S s);
  m4x4<S> T(void);
};


/*****************************************************************
 ************************ CLASS v2f ******************************
 *****************************************************************/
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


/*****************************************************************
 ************************ CLASS v3<T> ******************************
 *****************************************************************/

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




/*****************************************************************
 ************************ CLASS qtf ******************************
 *****************************************************************/


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


/*****************************************************************
 ************************ CLASS m3x3<T> ****************************
 *****************************************************************/


template<typename S>
inline m3x3<S>::m3x3(void) // initialize identity
{
  c[0] = v3<S>(1,0,0);
  c[1] = v3<S>(0,1,0);
  c[2] = v3<S>(0,0,1);
}

template<typename S>
inline m3x3<S>::m3x3(S r0c0, S r0c1, S r0c2,
		     S r1c0, S r1c1, S r1c2,
		     S r2c0, S r2c1, S r2c2)
{
  c[0] = v3<S>(r0c0, r1c0, r2c0);
  c[1] = v3<S>(r0c1, r1c1, r2c1);
  c[2] = v3<S>(r0c2, r1c2, r2c2);
}

template<typename S>
inline m3x3<S>::m3x3(v3<S> c0, v3<S> c1, v3<S> c2)
{          // note that vectors are inserted as columns
  c[0] = c0;  
  c[1] = c1;
  c[2] = c2;
}


template<typename S>
inline m3x3<S>::m3x3(v3<S> u, v3<S> v) // initialize u * v^S
  // (this is the equivalent of m3x3<S> operator&(v3<S>, v3<S>) )
{ 
  c[0] = v.x * u;
  c[1] = v.y * u;
  c[2] = v.z * u;
}

template<typename S>
inline m3x3<S>::m3x3(qt<S> q)
  /* if q is unit quaternion, initialize rotation matrix
     corresponding to the rotation represented by q */
{
  c[0][0] = 1 - 2*(q.v.y*q.v.y + q.v.z*q.v.z);
  c[1][0] = 2*(q.v.x*q.v.y - q.s*q.v.z);
  c[2][0] = 2*(q.v.x*q.v.z + q.s*q.v.y);
  
  c[0][1] = 2*(q.v.x*q.v.y + q.s*q.v.z);
  c[1][1] = 1 - 2*(q.v.x*q.v.x + q.v.z*q.v.z);
  c[2][1] = 2*(q.v.y*q.v.z - q.s*q.v.x);

  c[0][2] = 2*(q.v.x*q.v.z - q.s*q.v.y);
  c[1][2] = 2*(q.v.y*q.v.z + q.s*q.v.x);
  c[2][2] = 1 - 2*(q.v.x*q.v.x + q.v.y*q.v.y);
}


template<typename S>
inline v3<S>& m3x3<S>::operator[](unsigned int i)
{
  assert(i<3);
  return c[i];
}

template<typename S>
inline m3x3<S>& m3x3<S>::operator=(m3x3<S> A)
{
  c[0] = A[0];
  c[1] = A[1];
  c[2] = A[2];
  return *this;
}

template<typename S>
inline m3x3<S>& m3x3<S>::operator+=(m3x3<S> A)
{
  c[0] += A[0];
  c[1] += A[1];
  c[2] += A[2];
  return *this;
}

template<typename S>
inline m3x3<S>& m3x3<S>::operator-=(m3x3<S> A)
{
  c[0] -= A[0];
  c[1] -= A[1];
  c[2] -= A[2];
  return *this;
}

template<typename S>
inline m3x3<S>& m3x3<S>::operator*=(S s)
{
  c[0] *= s;
  c[1] *= s;
  c[2] *= s;
  return *this;
}

template<typename S>
inline m3x3<S>& m3x3<S>::operator/=(S s)
{
  assert(s!=0);
  s = 1/s; 
  c[0] *= s; 
  c[1] *= s; 
  c[2] *= s;
  return *this;
}

template<typename S>
inline m3x3<S> m3x3<S>::operator-(void) const
{
  return m3x3<S>(-c[0], -c[1], -c[2]);
}

template<typename S>
inline m3x3<S> m3x3<S>::T(void)
{
  return m3x3<S>(c[0].x, c[0].y, c[0].z,
		 c[1].x, c[1].y, c[1].z,
		 c[2].x, c[2].y, c[2].z);
}

template<typename S> 
static inline S det(m3x3<S> A)
{
  return (A[1][1]*(A[0][0]*A[2][2] - A[2][0]*A[0][2]) +
	  A[2][1]*(A[1][0]*A[0][2] - A[0][0]*A[1][2]) +
	  A[0][1]*(A[2][0]*A[1][2] - A[1][0]*A[2][2]));
}

template<typename S>
inline m3x3<S> m3x3<S>::inv(void)
{
  S d = det(*this);
  assert(d!=0&&"inversion called for irreversible matrix");
  d = 1/d;
  return m3x3<S>((c[1][1]*c[2][2] - c[2][1]*c[1][2])*d,
		 (c[2][0]*c[1][2] - c[1][0]*c[2][2])*d,
		 (c[1][0]*c[2][1] - c[2][0]*c[1][1])*d,
		 (c[2][1]*c[0][2] - c[0][1]*c[2][2])*d,
		 (c[0][0]*c[2][2] - c[2][0]*c[0][2])*d,
		 (c[2][0]*c[0][1] - c[0][0]*c[2][1])*d,
		 (c[0][1]*c[1][2] - c[1][1]*c[0][2])*d,
		 (c[1][0]*c[0][2] - c[0][0]*c[1][2])*d,
		 (c[0][0]*c[1][1] - c[1][0]*c[0][1])*d);
}

template<typename S, typename T> 
static inline m3x3<S> operator+(m3x3<S> A, m3x3<T> B)
{
  return m3x3<S>(A[0] + B[0],
		 A[1] + B[1],
		 A[2] + B[2]);
}

template<typename S, typename T> 
static inline m3x3<S> operator-(m3x3<S> A, m3x3<T> B)
{
  return m3x3<S>(A[0] - B[0],
		 A[1] - B[1],
		 A[2] - B[2]);
}

template<typename S, typename T>
static inline m3x3<S> operator*(m3x3<S> A, T s)
{
  return m3x3<S>(s*A[0], s*A[1], s*A[2]);
}

template<typename S, typename T> 
static inline m3x3<S> operator*(T s, m3x3<S> A)
{
  return m3x3<S>(s*A[0], s*A[1], s*A[2]);
}

template<typename S, typename T> 
static inline m3x3<S> operator/(m3x3<S> A, T s)
{
  assert(s!=0);
  s = 1/s;
  return m3x3<S>(s*A[0], s*A[1], s*A[2]);
}

template<typename S, typename T> 
static inline v3<S> operator*(m3x3<S> A, v3<T> u)
{
  return v3<S>(A[0][0]*u.x + A[1][0]*u.y + A[2][0]*u.z,
	       A[0][1]*u.x + A[1][1]*u.y + A[2][1]*u.z,
	       A[0][2]*u.x + A[1][2]*u.y + A[2][2]*u.z);
}

template<typename S, typename T> 
static inline v3<S> operator*(v3<S> u, m3x3<T> A)
{
  return v3<S>(u*A[0], u*A[1], u*A[2]);
}

// return rotation matrix of ang degs about axis ax

template<typename T, typename S> 
static inline m3x3<T> rotm3x3(T ang, v3<S> ax)
{
  T m = ax.norm();
  if(m == 0)
    return m3x3<T>(); // identity
  ax /= m;
  m3x3<T> M = ax % ax; // ax*ax^T
  return M+cosdeg(ang)*(m3x3<T>()-M)+sindeg(ang)*m3x3<T>(0, -ax.z, ax.y,
							 ax.z, 0, -ax.x,
							 -ax.y, ax.x, 0);
}

template<typename S, typename T> 
static inline m3x3<S> rotm3x3(S ang, T x, T y, T z)
{
  return rotm3x3<S>(ang, v3<S>(x, y, z));
}

template<typename S> 
static inline m3x3<S> crossm3x3(v3<S> v) {
  return m3x3<S>(0, -v.z, v.y,
		 v.z, 0, -v.x,
		 -v.y, v.x, 0);
}

#ifdef USE_IOSSREAM
template<typename S> 
static inline std::ostream& operator<<(std::ostream& os, m3x3<S> M)
{
  os.setf(std::ios::fixed);
  return os <<"/\t" << M[0][0] <<"\t"<< M[1][0] <<"\t"<< M[2][0] << "\t\\\n"
	    <<"|\t" << M[0][1] <<"\t"<< M[1][1] <<"\t"<< M[2][1] << "\t|\n"
	    <<"\\\t"<< M[0][2] <<"\t"<< M[1][2] <<"\t"<< M[2][2] << "\t/";
}
#endif

/*****************************************************************
 ************************ CLASS v4<T> ******************************
 *****************************************************************/


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

/*****************************************************************
 ************************ CLASS m4x4<T> ****************************
 *****************************************************************/
  
template<typename S>
inline m4x4<S>::m4x4(void) // initialize identity
{
  c4[0] = v4<S>(1,0,0,0);
  c4[1] = v4<S>(0,1,0,0);
  c4[2] = v4<S>(0,0,1,0);
  c4[3] = v4<S>(0,0,0,1);
}

template<typename S>
inline m4x4<S>::m4x4(m3x3<S> A) // extend A as below :)
{
  c4[0] = v4<S>(A[0], 0);
  c4[1] = v4<S>(A[1], 0);
  c4[2] = v4<S>(A[2], 0);
  c4[3] = v4<S>(0,0,0,1);
}

template<typename S>
inline m4x4<S>::m4x4(m3x3<S> A, v4<S> u4) // extend A with u4
{
  c4[0] = v4<S>(A[0], 0);
  c4[1] = v4<S>(A[1], 0);
  c4[2] = v4<S>(A[2], 0);
  c4[3] = u4;
}

template<typename S>
inline m4x4<S>::m4x4(S r0c0, S r0c1, S r0c2, S r0c3,
		     S r1c0, S r1c1, S r1c2, S r1c3,
		     S r2c0, S r2c1, S r2c2, S r2c3,
		     S r3c0, S r3c1, S r3c2, S r3c3)
{
  c4[0] = v4<S>(r0c0, r1c0, r2c0, r3c0);
  c4[1] = v4<S>(r0c1, r1c1, r2c1, r3c1);
  c4[2] = v4<S>(r0c2, r1c2, r2c2, r3c2);
  c4[3] = v4<S>(r0c3, r1c3, r2c3, r3c3);
}

template<typename S>
inline m4x4<S>::m4x4(v4<S> c40, v4<S> c41, v4<S> c42, v4<S> c43)
{
  c4[0] = c40;
  c4[1] = c41;
  c4[2] = c42;
  c4[3] = c43;
}

template<typename S>
inline v4<S>& m4x4<S>::operator[](unsigned int i)
{
  assert(i<4);
  return c4[i];
}

template<typename S>
inline m4x4<S>& m4x4<S>::operator=(m4x4<S> A4)
{
  c4[0] = A4[0];
  c4[1] = A4[1];
  c4[2] = A4[2];
  c4[3] = A4[3];
  return *this;
}

template<typename S>
inline m4x4<S>& m4x4<S>::operator+=(m4x4<S> A4)
{
  c4[0] += A4[0];
  c4[1] += A4[1];
  c4[2] += A4[2];
  c4[3] += A4[3];
  return *this;
}

template<typename S>
inline m4x4<S>& m4x4<S>::operator-=(m4x4<S> A4)
{
  c4[0] -= A4[0];
  c4[1] -= A4[1];
  c4[2] -= A4[2];
  c4[3] -= A4[3];
  return *this;
}

template<typename S>
inline m4x4<S>& m4x4<S>::operator*=(S s)
{
  c4[0] *= s;
  c4[1] *= s;
  c4[2] *= s;
  c4[3] *= s;
  return *this;
}

template<typename S>
inline m4x4<S>& m4x4<S>::operator/=(S s)
{
  assert(s != 0);
  s = 1/s;
  c4[0] *= s;
  c4[1] *= s;
  c4[2] *= s;
  c4[3] *= s;
  return *this;
}

template<typename S>
inline m4x4<S> m4x4<S>::T(void)
{
  return m4x4<S>(c4[0].x, c4[0].y, c4[0].z, c4[0].w,
		 c4[1].x, c4[1].y, c4[1].z, c4[1].w,
		 c4[2].x, c4[2].y, c4[2].z, c4[2].w,
		 c4[3].x, c4[3].y, c4[3].z, c4[3].w);
}

template<typename S, typename T> 
static inline m4x4<S> operator+(m4x4<S> A4, m4x4<T> B4)
{
  return m4x4<S>(A4[0] + B4[0],
		 A4[1] + B4[1],
		 A4[2] + B4[2],
		 A4[3] + B4[3]);
}

template<typename S, typename T> 
static inline m4x4<S> operator-(m4x4<S> A4, m4x4<T> B4)
{
  return m4x4<S>(A4[0] - B4[0],
		 A4[1] - B4[1],
		 A4[2] - B4[2],
		 A4[3] - B4[3]);
}

template<typename S, typename T> 
static inline m4x4<S> operator*(m4x4<S> A4, T s)
{
  return m4x4<S>(s*A4[0], s*A4[1], s*A4[2], s*A4[3]);
}

template<typename S, typename T> 
static inline m4x4<S> operator*(T s, m4x4<S> A4)
{
  return m4x4<S>(s*A4[0], s*A4[1], s*A4[2], s*A4[3]);
}

template<typename S, typename T> 
static inline m4x4<S> operator/(m4x4<S> A4, T s)
{
  assert(s!=0);
  s = 1/s;
  return m4x4<S>(s*A4[0], s*A4[1], s*A4[2], s*A4[3]);
}

template<typename S, typename T> 
static inline v4<S> operator*(m4x4<T> A4, v4<S> u4)
{
  return v4<S>(A4[0][0]*u4.x + A4[1][0]*u4.y + A4[2][0]*u4.z + A4[3][0]*u4.w,
	       A4[0][1]*u4.x + A4[1][1]*u4.y + A4[2][1]*u4.z + A4[3][1]*u4.w,
	       A4[0][2]*u4.x + A4[1][2]*u4.y + A4[2][2]*u4.z + A4[3][2]*u4.w,
	       A4[0][3]*u4.x + A4[1][3]*u4.y + A4[2][3]*u4.z + A4[3][3]*u4.w);
}

template<typename S, typename T> 
static inline v4<S> operator*(v4<S> u4, m4x4<T> A4)
{
  return v4<S>(u4*A4[0], u4*A4[1], u4*A4[2], u4*A4[3]);
}

template<typename S, typename T> 
static inline m3x3<S> operator*(m3x3<S> A, m3x3<T> B)
{
  return
    m3x3<S>(A[0][0]*B[0][0] + A[1][0]*B[0][1] + A[2][0]*B[0][2],
	    A[0][0]*B[1][0] + A[1][0]*B[1][1] + A[2][0]*B[1][2],
	    A[0][0]*B[2][0] + A[1][0]*B[2][1] + A[2][0]*B[2][2],
	    
	    A[0][1]*B[0][0] + A[1][1]*B[0][1] + A[2][1]*B[0][2],
	    A[0][1]*B[1][0] + A[1][1]*B[1][1] + A[2][1]*B[1][2],
	    A[0][1]*B[2][0] + A[1][1]*B[2][1] + A[2][1]*B[2][2],
	    
	    A[0][2]*B[0][0] + A[1][2]*B[0][1] + A[2][2]*B[0][2],
	    A[0][2]*B[1][0] + A[1][2]*B[1][1] + A[2][2]*B[1][2],
	    A[0][2]*B[2][0] + A[1][2]*B[2][1] + A[2][2]*B[2][2]);
}

template<typename S, typename T> 
static inline m4x4<S> operator*(m4x4<S> A, m4x4<T> B)
{
  return 
    m4x4<S>
    (A[0][0]*B[0][0] + A[1][0]*B[0][1] + A[2][0]*B[0][2] + A[3][0]*B[0][3],
     A[0][0]*B[1][0] + A[1][0]*B[1][1] + A[2][0]*B[1][2] + A[3][0]*B[1][3],
     A[0][0]*B[2][0] + A[1][0]*B[2][1] + A[2][0]*B[2][2] + A[3][0]*B[2][3],
     A[0][0]*B[3][0] + A[1][0]*B[3][1] + A[2][0]*B[3][2] + A[3][0]*B[3][3],
 
     A[0][1]*B[0][0] + A[1][1]*B[0][1] + A[2][1]*B[0][2] + A[3][1]*B[0][3],
     A[0][1]*B[1][0] + A[1][1]*B[1][1] + A[2][1]*B[1][2] + A[3][1]*B[1][3],
     A[0][1]*B[2][0] + A[1][1]*B[2][1] + A[2][1]*B[2][2] + A[3][1]*B[2][3],
     A[0][1]*B[3][0] + A[1][1]*B[3][1] + A[2][1]*B[3][2] + A[3][1]*B[3][3],

     A[0][2]*B[0][0] + A[1][2]*B[0][1] + A[2][2]*B[0][2] + A[3][2]*B[0][3],
     A[0][2]*B[1][0] + A[1][2]*B[1][1] + A[2][2]*B[1][2] + A[3][2]*B[1][3],
     A[0][2]*B[2][0] + A[1][2]*B[2][1] + A[2][2]*B[2][2] + A[3][2]*B[2][3],
     A[0][2]*B[3][0] + A[1][2]*B[3][1] + A[2][2]*B[3][2] + A[3][2]*B[3][3],

     A[0][3]*B[0][0] + A[1][3]*B[0][1] + A[2][3]*B[0][2] + A[3][3]*B[0][3],
     A[0][3]*B[1][0] + A[1][3]*B[1][1] + A[2][3]*B[1][2] + A[3][3]*B[1][3],
     A[0][3]*B[2][0] + A[1][3]*B[2][1] + A[2][3]*B[2][2] + A[3][3]*B[2][3],
     A[0][3]*B[3][0] + A[1][3]*B[3][1] + A[2][3]*B[3][2] + A[3][3]*B[3][3]);
  /* I did enjoy writing this routine */
}

template<typename S> 
static inline S det(m4x4<S> M)
{
  return 
    M[0][0]*det(m3x3<S>(M[1][1], M[2][1], M[3][1],
			M[1][2], M[2][2], M[3][2],
			M[1][3], M[2][3], M[3][3]))
    -
    M[0][1]*det(m3x3<S>(M[1][0], M[2][0], M[3][0],
			M[1][2], M[2][2], M[3][2],
			M[1][3], M[2][3], M[3][3]))
    +
    M[0][2]*det(m3x3<S>(M[1][0], M[2][0], M[3][0],
			M[1][1], M[2][1], M[3][1],
			M[1][3], M[2][3], M[3][3]))
    -
    M[0][3]*det(m3x3<S>(M[1][0], M[2][0], M[3][0],
			M[1][1], M[2][1], M[3][1],
			M[1][2], M[2][2], M[3][2]));
}

#ifdef USE_IOSSREAM
template<typename S> 
static inline std::ostream& operator<<(std::ostream& os, m4x4<S> M)
{
  return os 
    << "/"<<M[0][0]<<" "<<M[1][0]<<" "<<M[2][0]<<" "<<M[3][0]<<"\\\n"
    << "|"<<M[0][1]<<" "<<M[1][1]<<" "<<M[2][1]<<" "<<M[3][1]<< "|\n"
    << "|"<<M[0][2]<<" "<<M[1][2]<<" "<<M[2][2]<<" "<<M[3][2]<< "|\n"
    <<"\\"<<M[0][3]<<" "<<M[1][3]<<" "<<M[2][3]<<" "<<M[3][3]<< "/\n";
}
#endif

typedef v2<float> v2f;
typedef v2<double> v2d;
typedef v2<int> v2i;
typedef v3<float> v3f;
typedef v3<double> v3d;
typedef v3<int> v3i;
typedef qt<float> qtf;
typedef qt<double> qtd;
typedef qt<int> qti;
typedef m3x3<float> m3x3f;
typedef m3x3<double> m3x3d;
typedef m3x3<int> m3x3i;
typedef v4<float> v4f;
typedef v4<double> v4d;
typedef v4<int> v4i;
typedef m4x4<float> m4x4f;
typedef m4x4<double> m4x4d;
typedef m4x4<int> m4x4i;


#endif /* VMX_HH */
