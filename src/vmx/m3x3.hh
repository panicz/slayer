#ifndef VMX_HH
#error "vmx/m3x3.hh should be included from vmx.hh"
#endif

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
  return d*m3x3<S>((c[1][1]*c[2][2] - c[2][1]*c[1][2]),
		   (c[2][0]*c[1][2] - c[1][0]*c[2][2]),
		   (c[1][0]*c[2][1] - c[2][0]*c[1][1]),
		   (c[2][1]*c[0][2] - c[0][1]*c[2][2]),
		   (c[0][0]*c[2][2] - c[2][0]*c[0][2]),
		   (c[2][0]*c[0][1] - c[0][0]*c[2][1]),
		   (c[0][1]*c[1][2] - c[1][1]*c[0][2]),
		   (c[1][0]*c[0][2] - c[0][0]*c[1][2]),
		   (c[0][0]*c[1][1] - c[1][0]*c[0][1]));
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
