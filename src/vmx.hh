/* vmx.hh: this library contains a set of common operations on three- 
   and four-dimensional vectors, square matrices and quaternions

   Although multiplying 4x4 matrices is implemented, it's generally a
   good idea to avoid it and work on 3x3's as much as possible.

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

const float tolerance = 0.0001f; //used in %= comparison and elsewhere

#include "vmx/math.hh"

template<typename T> class v2;
template<typename T> class v3;
template<typename T> class qt;
template<typename S> class m3x3;
template<typename T> class v4;
template<typename S> class m4x4;

/* All operators declared on vectors are predictable:
   addition, substraction, dot product (*), cross product (^),
   scalar division and multiplication (/, *), dyadic product (%) */

#include "vmx/v2.hh"
#include "vmx/v3.hh"
#include "vmx/qt.hh"
#include "vmx/m3x3.hh"
#include "vmx/v4.hh"
#include "vmx/m4x4.hh"

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
