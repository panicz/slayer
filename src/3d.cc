#include "slayer.h"
#include "extend.h"
#include "utils.h"
#include "video.h"
#include <limits.h>
#include "vmx.hh"
#include "symbols.h"

#if !HAVE_GL_WINDOW_POS2I
// code borrowed from 
// https://code.google.com/p/lasermission/source/browse/trunk/source/Emulation/glWindowPos.cpp
// (modified)
// courtesy of Mike MacFerrin
void 
glWindowPos2i(int x, int y)
{
  glPushAttrib(GL_TRANSFORM_BIT | GL_VIEWPORT_BIT);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  glViewport(x-1, y-1, 2, 2);
  glRasterPos3f(0, 0, 0);
  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glPopAttrib();
}
#endif // !HAVE_GL_WINDOW_POS2I


#define DEF_SCM_TOFROM_V(n,t,ype,sym)					\
  static inline v##n##t							\
  scm_to_v##n##t(SCM vector) {						\
    int i; v##n##t v;							\
    if (scm_is_uniform_vector(vector))					\
      for (i = 0; i < n; ++i)						\
	v[i] =								\
	  scm_to_##t##ype(scm_uniform_vector_ref(vector,		\
						 scm_from_int(i)));	\
    else if (scm_is_vector(vector))					\
      for (i = 0; i < n; ++i)						\
	v[i] = scm_to_##t##ype(scm_c_vector_ref(vector, i));		\
    else								\
      WARN("function called on a non-vector");				\
    return v;								\
  }									\
									\
  static inline SCM							\
  scm_from_v##n##t(v##n##t v) {						\
    SCM result = scm_make_typed_array(s_##sym, SCM_UNSPECIFIED,		\
				      scm_list_1(scm_from_int(n)));	\
    scm_t_array_handle h;						\
    scm_array_get_handle(result, &h);					\
    t##ype *data = scm_array_handle_##sym##_writable_elements(&h);	\
    									\
    *((v##n##t *) data) = v;						\
    scm_array_handle_release(&h);					\
    return result;							\
  }


DEF_SCM_TOFROM_V(3,f,loat,f32)
DEF_SCM_TOFROM_V(3,d,ouble,f64)
DEF_SCM_TOFROM_V(4,f,loat,f32)
DEF_SCM_TOFROM_V(4,d,ouble,f64)

#undef DEF_SCM_TOFROM_V

#define DEF_SCM_TOFROM_M(n,t,ype,sym)					\
  static inline m##n##x##n##t						\
  scm_to_m##n##x##n##t(SCM array) {					\
    m##n##x##n##t result;						\
    t##ype *data;							\
    if(!scm_is_typed_array(array, s_##sym)) {				\
      WARN("argument should be a uniform " # sym " array");		\
      return result;							\
    }									\
									\
    scm_t_array_handle h;						\
    scm_array_get_handle(array, &h);					\
    if(scm_array_handle_nelems(&h) < n*n) {				\
      WARN("array should contain at least %i elements", n*n);		\
      goto end;								\
    }									\
									\
    data = scm_array_handle_##sym##_writable_elements(&h);		\
    result = *((m##n##x##n##t *) data);					\
  end:									\
    scm_array_handle_release(&h);					\
    return result;							\
  }									\
									\
  static inline SCM							\
  scm_from_m##n##x##n##t(m##n##x##n##t M) {				\
    SCM result = scm_make_typed_array(s_##sym, SCM_UNSPECIFIED,		\
				      scm_list_2(scm_from_int(n),	\
						 scm_from_int(4)));	\
    scm_t_array_handle h;						\
    scm_array_get_handle(result, &h);					\
    t##ype *data = scm_array_handle_##sym##_writable_elements(&h);	\
    									\
    *((m##n##x##n##t *) data) = M;					\
    scm_array_handle_release(&h);					\
    return result;							\
  }

DEF_SCM_TOFROM_M(3,f,loat,f32)
DEF_SCM_TOFROM_M(3,d,ouble,f64)

DEF_SCM_TOFROM_M(4,f,loat,f32)
DEF_SCM_TOFROM_M(4,d,ouble,f64)

#undef DEF_SCM_TOFROM_M

static inline qtf
scm_to_qtf(SCM pair) {
  qtf q;
  q.v = scm_to_v3f(SCM_CDR(pair));
  q.s = scm_to_float(SCM_CAR(pair));
  return q;
}

static inline qtd
scm_to_qtd(SCM pair) {
  qtd q;
  q.v = scm_to_v3d(SCM_CDR(pair));
  q.s = scm_to_double(SCM_CAR(pair));
  return q;
}

static SCM
push_matrix_x() {
  glPushMatrix();
  return SCM_UNSPECIFIED;
}

static SCM
pop_matrix_x() {
  glPopMatrix();
  return SCM_UNSPECIFIED;
}

static SCM
translate_view_x(SCM vector) {
  v3f v = scm_to_v3f(vector);
  glTranslatef(v.x, v.y, v.z);
  return SCM_UNSPECIFIED;
}

static SCM
load_identity_x() {
  glLoadIdentity();
  return SCM_UNSPECIFIED;
}

static SCM
multiply_matrix_x(SCM M) {
  scm_t_array_handle h;
  if (scm_is_array(M)) {
    scm_array_get_handle(M, &h);
    size_t size = scm_array_handle_nelems(&h);
    if (size < 16) {
      WARN("Trying to multiply matrix by too few elements: %d", (int) size);
      goto release;
    }
    if (scm_is_typed_array(M, s_f32)) {
      float const *elements
	= scm_array_handle_f32_elements(&h);
      glMultMatrixf(elements); 
    }
    else if(scm_is_typed_array(M, s_f64)) {
      double const *elements
	= scm_array_handle_f64_elements(&h);
      glMultMatrixd(elements);
    }
    else {
      SCM *elements = (SCM *) scm_array_handle_elements(&h);
      m4x4d matrix;
      for(int i = 0; i < 4; ++i) {
	for(int j = 0; j < 4; ++j) {
	  matrix[j][i] = scm_to_double(elements[4*i+j]);
	}
      }
      glMultMatrixd((double *) &matrix);
    }
  release:
    scm_array_handle_release(&h);
  }

  return SCM_UNSPECIFIED;
}

static SCM
rotate_view_x(SCM quaternion) {
  qtf q = scm_to_qtf(quaternion);
  glRotatef(rad2deg(2*acos(q.s)), q.v.x, q.v.y, q.v.z);
  return SCM_UNSPECIFIED;
}

static SCM
scale_view_x(SCM X, SCM Y, SCM Z) {
  float x, y, z;
  if (!GIVEN(Y)) {
    x = y = z = (float) scm_to_double(X);
  }
  else {
    x = (float) scm_to_double(X);
    y = (float) scm_to_double(Y);
    z = GIVEN(Z) ? (float) scm_to_double(Z) : 1.0;
  }
  glScalef(x, y, z);
  return SCM_UNSPECIFIED;
}

static SCM
set_viewport_x(SCM X, SCM Y, SCM W, SCM H) {
  GLsizei w = scm_to_int(W);
  GLsizei h = scm_to_int(H);

  GLint x = scm_to_int(X);
  GLint y = screen->h - scm_to_int(Y) - h;

  glViewport(x, y, w, h);
  return SCM_UNSPECIFIED;
}

static SCM
current_viewport() {
  struct { GLint x, y, w, h; } s;
  glGetIntegerv(GL_VIEWPORT, (GLint *) &s);

  return scm_list_4(scm_from_int(s.x), 
		    scm_from_int(screen->h - s.y - s.h),
		    scm_from_int(s.w), 
		    scm_from_int(s.h));
}

static inline double 
_z_index(int x, int y) {
  float z;
  glReadPixels(x, screen->h - y - 1, 1, 1, 
	       GL_DEPTH_COMPONENT, 
	       GL_FLOAT, 
	       (GLvoid *) &z);
  return (double) z;
}

static SCM
z_index(SCM X, SCM Y) {
  return scm_from_double(_z_index(scm_to_int(X), scm_to_int(Y)));
}

static inline unsigned int
_display_index(int x, int y) {
  GLuint di;
  glReadPixels(x, screen->h - y - 1, 1, 1,
	       GL_STENCIL_INDEX,
	       GL_UNSIGNED_INT,
	       (GLvoid *) &di);
  return di;
}

static SCM
display_index(SCM X, SCM Y) {
  unsigned int index = _display_index(scm_to_uint(X), scm_to_uint(Y));
  return index ? scm_from_uint(index - 1) : SCM_BOOL_F;
}

static SCM
set_display_index_x(SCM index) {
  if(isnt(index)) {
    glStencilFunc(GL_ALWAYS, 0, 0xff);
    return SCM_UNSPECIFIED;
  }
  unsigned char i = 1 + scm_to_uchar(index);
  if (i == 0) {
    WARN("Display index was set to 0");
    ++i;
  }
  glStencilFunc(GL_ALWAYS, i, 0xff);
  return SCM_UNSPECIFIED;
}

static SCM
max_display_index() {
  return scm_from_uchar(254);
}

static SCM
quaternion_to_matrix(SCM quaternion) {
  qtd q = scm_to_qtd(quaternion);
  return scm_from_m4x4d(m4x4d(m3x3d(q), v4d(0,0,0,1)));
}

static SCM
translation_matrix(SCM vector) {
  v3d v = scm_to_v3d(vector);
  return scm_from_m4x4d(m4x4d(m3x3d(), v4d(v, 1)));
}

static SCM
unproject(SCM _x, SCM _y, SCM _z,
	  SCM _modelview_matrix,
	  SCM _projection_matrix,
	  SCM _viewport_list) {
  NOTE("this function is still experimental and needs to be tested thoroughly");
  v3d m;
  m.x = scm_to_double(_x);
  m.y = scm_to_double(_y);
  m.z = isnt(_z) ?_z_index((int) m.x, (int) m.y) : scm_to_double(_z);

  m4x4d modelview_matrix = scm_to_m4x4d(_modelview_matrix);
  m4x4d projection_matrix = scm_to_m4x4d(_projection_matrix);
  struct { GLint x, y, w, h; } s;
  if(!GIVEN(_viewport_list) 
     || isnt(scm_list_p(_viewport_list))
     || (scm_to_int(scm_length(_viewport_list)) < 4)
     ) {
    glGetIntegerv(GL_VIEWPORT, (GLint *) &s);
  }
  else {
    SCM_LIST_TO_C_ARRAY(int, &s, 4, scm_to_int, _viewport_list);
    s.y = screen->h - s.y - s.h;
  }

  v3d v;
  gluUnProject(m.x, m.y, m.z, 
	       (GLdouble *) &modelview_matrix,
	       (GLdouble *) &projection_matrix,
	       (GLint *) &s,
	       &v.x, &v.y, &v.z);
  v.z *= -1;
  return scm_from_v3d(v);
}

// by convention, each function that ever
// alters a matrix mode, should leave GL_MODELVIEW
// as the current matrix mode.

static inline double
_get_aspect_from_scm(SCM aspect) {
  if ((   aspect == SCM_UNDEFINED) 
      || (aspect == SCM_UNSPECIFIED)
      || (aspect == SCM_BOOL_F)) {
    struct { GLint x, y, w, h; } s;
    glGetIntegerv(GL_VIEWPORT, (GLint *) &s);
    return (double) s.w / (double) s.h; 
  }
  return scm_to_double(aspect);
}

// this code is a travesty of gluPerspective from the GLU library
static SCM
perspective_projection(SCM _fovy, SCM _aspect, SCM _near, SCM _far) {
  double fovy = scm_to_double(_fovy);
  double aspect = _get_aspect_from_scm(_aspect);
  double radians = deg2rad(0.5*fovy);
  double near = GIVEN(_near) ? scm_to_double(_near) : 0.1;
  double far = GIVEN(_far) ? scm_to_double(_far) : 1000.0;
  double d = far-near;
  double sine = sin(radians);
  if((d == 0) || (sine == 0) || (aspect == 0)) {
    WARN("Invalid argument");
    return SCM_UNSPECIFIED;
  }
  d = 1/d;
  double cotangent = cos(radians)/sine;
  return scm_from_m4x4d
    (m4x4d(cotangent/aspect, 0,                0,                0,
	   0,                cotangent,        0,                0,
	   0,                0,                -(far+near)*d,   -2*near*far*d,
	   0,                0,                -1,               0));
}

static SCM
current_projection() {
  m4x4d M;
  glGetDoublev(GL_PROJECTION_MATRIX, (GLdouble *) &M);
  return scm_from_m4x4d(M);
}

static SCM
current_matrix() {
  m4x4d M;
  glGetDoublev(GL_MODELVIEW_MATRIX, (GLdouble *) &M);
  return scm_from_m4x4d(M);
}

static SCM
set_perspective_projection_x(SCM _fovy, SCM _aspect, SCM _near, SCM _far) {
  GLdouble fovy = scm_to_float(_fovy);

  GLdouble aspect = _get_aspect_from_scm(_aspect);

  GLdouble near = (_near == SCM_UNDEFINED)
    ? 0.1 : (GLdouble) scm_to_double(_near);
  GLdouble far = (_far == SCM_UNDEFINED)
    ? 1000.0 : (GLdouble) scm_to_double(_far);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(fovy, aspect, near, far);

  glMatrixMode(GL_MODELVIEW);

  return SCM_UNSPECIFIED;
}

static SCM
set_orthographic_projection_x(SCM left, SCM right, 
			      SCM bottom, SCM top,
			      SCM near, SCM far) {
  
  struct { GLint x, y, w, h; } s;
  glGetIntegerv(GL_VIEWPORT, (GLint *) &s);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(GIVEN(left) ? scm_to_double(left) : (GLdouble) s.x, 
	  GIVEN(right) ? scm_to_double(right) : (GLdouble) (s.x + s.w),
	  GIVEN(bottom) ? scm_to_double(bottom) : (GLdouble) s.y, 
	  GIVEN(top) ? scm_to_double(top) : (GLdouble) (s.y + s.h),
	  GIVEN(near) ? scm_to_double(near) : -1.0,
	  GIVEN(far) ? scm_to_double(far) : 1.0);

  glMatrixMode(GL_MODELVIEW);

  return SCM_UNSPECIFIED;
}

static GLenum GLtypes[SCM_ARRAY_ELEMENT_TYPE_LAST+1];
static inline void
init_GLtypes() {

#define NOT_SUPPORTED -1
  unsigned int i;
  for(i = 0; i < NELEMS(GLtypes); ++i)
    GLtypes[i] = NOT_SUPPORTED;

  /* this is rather unlikely, but it never hurts to check */
#if  (GL_FLOAT == NOT_SUPPORTED)		\
  or (GL_DOUBLE == NOT_SUPPORTED)		\
  or (GL_INT == NOT_SUPPORTED)			\
  or (GL_BYTE == NOT_SUPPORTED)			\
  or (GL_UNSIGNED_BYTE == NOT_SUPPORTED)	\
  or (GL_UNSIGNED_INT == NOT_SUPPORTED)		\
  or (GL_SHORT_INT == NOT_SUPPORTED)	
#error "Invalid special value (the ``NOT_SUPPORTED'' macro constant)"
#endif

#define SET_TYPE(type, GLtype) \
  GLtypes[SCM_ARRAY_ELEMENT_TYPE_##type] = GLtype

  SET_TYPE(S8, GL_BYTE);
  SET_TYPE(U8, GL_UNSIGNED_BYTE);

  SET_TYPE(S16, GL_SHORT);
  SET_TYPE(U16, GL_UNSIGNED_SHORT);
  
  SET_TYPE(S32, GL_INT);
  SET_TYPE(U32, GL_UNSIGNED_INT);

  SET_TYPE(F32, GL_FLOAT);
  SET_TYPE(F64, GL_DOUBLE);

#undef SET_TYPE
}

#define DEF_SET_GL_ARRAY(objects, proc, cls, dflt)	\
  static SCM objects##_gc_guard = SCM_UNDEFINED;	\
  static SCM						\
  set_##objects##_array_x(SCM array) {			\
    glEnableClientState(cls);				\
    if (!scm_is_array(array)) {				\
      WARN("argument is not an array");			\
      return SCM_UNSPECIFIED;				\
    }							\
    scm_t_array_handle handle;				\
    scm_t_array_dim *dims;				\
    const void *data;					\
    GLint size;						\
    scm_array_get_handle(array, &handle);		\
    int type = GLtypes[handle.element_type];		\
    if (type == NOT_SUPPORTED) {			\
      WARN("unsupported array type");			\
      goto end;						\
    }							\
    if (scm_array_handle_rank(&handle) != 2) {		\
      WARN("invalid array dimension, setting  "		\
	   "coordinate size to default (%i)", dflt);	\
      size = dflt;					\
      goto assign;					\
    }							\
    dims = scm_array_handle_dims(&handle);		\
    size = abs(dims[1].ubnd - dims[1].lbnd) + 1;	\
  assign:						\
    data = scm_array_handle_uniform_elements(&handle);	\
    proc(size, (GLenum) type, 0, data);			\
    ASSIGN_SCM(objects##_gc_guard, array);		\
  end:							\
    scm_array_handle_release(&handle);			\
    return SCM_UNSPECIFIED;				\
  }


DEF_SET_GL_ARRAY(vertex, glVertexPointer, GL_VERTEX_ARRAY, 3)
DEF_SET_GL_ARRAY(color, glColorPointer, GL_COLOR_ARRAY, 3)
DEF_SET_GL_ARRAY(texture_coord, glTexCoordPointer, GL_TEXTURE_COORD_ARRAY, 3)

static inline void
_glNormalPointer(GLint size,GLenum type,GLsizei stride,const GLvoid *pointer) {
  return glNormalPointer(type, stride, pointer);
}

DEF_SET_GL_ARRAY(normal, _glNormalPointer, GL_NORMAL_ARRAY, 3)

#undef DEF_SET_GL_ARRAY
#undef NOT_SUPPORTED

static SCM GLnames;
static inline void
init_GLnames() {
  GLnames = gc_protected(scm_c_make_hash_table(64));

#define SET_VALUE(name, value) \
  scm_hash_set_x(GLnames, symbol(name), scm_from_int(value))

  SET_VALUE("points", GL_POINTS);
  SET_VALUE("lines", GL_LINES);
  SET_VALUE("line-strip", GL_LINE_STRIP);
  SET_VALUE("line-loop", GL_LINE_LOOP);
  SET_VALUE("triangles", GL_TRIANGLES);
  SET_VALUE("triangle-strip", GL_TRIANGLE_STRIP);
  SET_VALUE("triangle-fan", GL_TRIANGLE_FAN);
  SET_VALUE("quads", GL_QUADS);
  SET_VALUE("quad-strip", GL_QUAD_STRIP);
  SET_VALUE("polygon", GL_QUAD_STRIP);

  SET_VALUE("vertex-array", GL_VERTEX_ARRAY);
  SET_VALUE("color-array", GL_COLOR_ARRAY);
  SET_VALUE("texture-coord-array", GL_TEXTURE_COORD_ARRAY);
  SET_VALUE("normal-array", GL_NORMAL_ARRAY);
  
#undef SET_VALUE
}

static SCM
forget_array_x(SCM type) {
  ASSERT_SCM_TYPE(symbol, type, 1);
  int type_id = scm_to_int(scm_hash_ref(GLnames, type, SCM_BOOL_F));
  glDisableClientState(type_id);
  return SCM_UNSPECIFIED;
}

static void dont_set_color(const void *unused) { 
  WARN("Unsupported color setter called");
}

static void (*color_setters[SCM_ARRAY_ELEMENT_TYPE_LAST+1][2])(const void *);

static inline void
init_color_setters() {
  int i, dim;
  for(dim = 0; dim <= 1; dim++) {
    for(i = 0; i < NELEMS(color_setters); ++i) {
      color_setters[i][dim] = dont_set_color;
    }
  }
#define SET_COLOR_SETTER(type, dim, gls)		\
  color_setters[SCM_ARRAY_ELEMENT_TYPE_##type][dim-3]	\
    = (void (*)(const void *)) glColor##dim##gls##v
  
#define SET_COLOR_SETTERS(type, gls)		\
  SET_COLOR_SETTER(type, 3, gls);		\
  SET_COLOR_SETTER(type, 4, gls)

  SET_COLOR_SETTERS(S8, b);
  SET_COLOR_SETTERS(S16, s);
  SET_COLOR_SETTERS(S32, i);
  SET_COLOR_SETTERS(F32, f);
  SET_COLOR_SETTERS(F64, d);

  SET_COLOR_SETTERS(U8, ub);
  SET_COLOR_SETTERS(U16, us);
  SET_COLOR_SETTERS(U32, ui);

#undef SET_COLOR_SETTER
}

static inline SCM
_set_color_from_scm_vector(SCM value) {
#define V(n) scm_c_vector_ref(value, n)
#define UINT(x) scm_to_uint(x)
#define DBL(x) scm_to_double(x)
  int i, exact = 1;
  size_t size = mini(scm_c_vector_length(value), 4);
  if(size < 3) {
    WARN("Invalid vector size: %d", (int) size);
    return SCM_BOOL_F;
  }
  for(i = 0; i < (int) size; ++i) {
    if(scm_is_inexact(V(i))) {
      exact = 0;
      break;
    }
  }

  switch(size) {
  case 3:
    if(exact) {
      glColor3ui(UINT(V(0)), UINT(V(1)), UINT(V(2)));
    }
    else {
      glColor3d(DBL(V(0)), DBL(V(1)), DBL(V(2)));
    }
    break;
  case 4:
    if(exact) {
      glColor4ui(UINT(V(0)), UINT(V(1)), UINT(V(2)), UINT(V(3)));
    }
    else {
      glColor4d(DBL(V(0)), DBL(V(1)), DBL(V(2)), DBL(V(3)));
    }
    break;
  }
#undef DBL
#undef UINT
#undef V
  return SCM_UNSPECIFIED;
}

static SCM
set_color_x(SCM value) {
  if(scm_is_unsigned_integer(value, 0, UINT_MAX)) {
    SDL_Color c = sdl_color(scm_to_uint(value));
    glColor4ub(c.r, c.g, c.b, c.unused);
  }
  else if(scm_is_uniform_vector(value)) {
    size_t size = mini(scm_c_uniform_vector_length(value), 4);
    if(size < 2) {
      WARN("Invalid uniform vector size: %d", (int) size);
      return SCM_BOOL_F;
    }
    scm_t_array_handle h;
    scm_array_get_handle(value, &h);
    (color_setters[h.element_type][size-3])
      (scm_array_handle_uniform_elements(&h));
    scm_array_handle_release(&h);
  }
  else if(scm_is_vector(value)) {
    return _set_color_from_scm_vector(value);
  }
  else {
    WARN("Unsupported argument type");
    return SCM_BOOL_F;
  }
  return SCM_UNSPECIFIED;
}

static SCM
draw_faces_x(SCM type, SCM array) {
#define GET_VALUE(name)					\
  scm_to_int(scm_hash_ref(GLnames, name, SCM_BOOL_F))

  if (!scm_is_array(array)) {
    WARN("argument is not an array");
    return SCM_UNSPECIFIED;
  }

  scm_t_array_handle handle;
  scm_array_get_handle(array, &handle);

  size_t nelems = scm_array_handle_nelems(&handle);
  
  const void *data 
    = scm_array_handle_uniform_writable_elements(&handle);

  glDrawElements(GET_VALUE(type), nelems, 
		 GLtypes[handle.element_type], data);

  scm_array_handle_release(&handle);
  return SCM_UNSPECIFIED;
#undef GET_VALUE
}

enum light_properties_enum {
  LIGHT_PROPERTY_UNSUPPORTED = 0,
  LIGHT_PROPERTY_AMBIENT = 1,
  LIGHT_PROPERTY_DIFFUSE = 2,
  LIGHT_PROPERTY_SPECULAR = 3,
  LIGHT_PROPERTY_POSITION = 4,
  LIGHT_PROPERTY_DIRECTION = 5,
  LIGHT_PROPERTY_EXPONENT = 6,
  LIGHT_PROPERTY_CUTOFF = 7,
  LIGHT_PROPERTY_CONSTANT_ATTENUATION = 8,
  LIGHT_PROPERTY_LINEAR_ATTENUATION = 9,
  LIGHT_PROPERTY_QUADRATIC_ATTENUATION = 10,
  NUM_LIGHT_PROPERTIES = 11
};

// up to GL_MAX_LIGHTS can be allocated
static SCM removed_lights = SCM_EOL;
static int next_light = 0;
static SCM light_properties; // hash from symbols to light_properties_enum
// initialzed in init_lights()
static void(*light_property_setters[NUM_LIGHT_PROPERTIES])(int, SCM);
static SCM (*light_property_getters[NUM_LIGHT_PROPERTIES])(int);

static void
light_UNSUPPORTED_setter(int light, SCM value) {
  WARN("unsupported setter called for light %i", light);
}

static SCM
light_UNSUPPORTED_getter(int light) {
  WARN("unsupported getter called for light %i", light);
  return SCM_BOOL_F;
}

#define DEF_LIGHT_SETTER(prop, glprop, type)		\
  static void						\
  light_##prop##_setter(int light, SCM value) {		\
    type newval = scm_to_##type(value);			\
    glLightfv(light, glprop, (GLfloat *) &newval);	\
  }

#define DEF_LIGHT_GETTER(prop, glprop, type)			\
  static SCM							\
  light_##prop##_getter(int light) {				\
    type value;							\
    glGetLightfv(light, glprop, (GLfloat *) &value);		\
    return scm_from_##type(value);				\
  }

#define DEF_LIGHT_ACCESSORS(prop, glprop, type)	\
  DEF_LIGHT_GETTER(prop, glprop, type)		\
  DEF_LIGHT_SETTER(prop, glprop, type)

#define DEF_GL_LIGHT_ACCESSORS(prop, type)	\
  DEF_LIGHT_ACCESSORS(prop, GL_##prop, type)

DEF_GL_LIGHT_ACCESSORS(AMBIENT, v4f)
DEF_GL_LIGHT_ACCESSORS(DIFFUSE, v4f)
DEF_GL_LIGHT_ACCESSORS(SPECULAR, v4f)

/* note that original OpenGL names GL_SPOT_~ are renamed to ~, because 
   the POSITION and DIRECTION properties are straightened out */

static void
light_DIRECTION_setter(int light, SCM value) {
  v4f direction(scm_to_v3f(value), 0);
  v4f position;
  glGetLightfv(light, GL_POSITION, (GLfloat *) &position);
  if(position.w == 0) { // directional light (OpenGL interprets 
    // the GL_POSITION parameter as direction)
    glLightfv(light, GL_POSITION, (GLfloat *) &direction);
  }
  // if the light is directional, OpenGL ignores GL_SPOT_DIRECTION,
  // but it's good to set in case the user wants to switch to positional light
  glLightfv(light, GL_SPOT_DIRECTION, (GLfloat *) &direction);
}

DEF_LIGHT_GETTER(DIRECTION, GL_SPOT_DIRECTION, v3f)

static void
light_POSITION_setter(int light, SCM value) {
  v4f position(0,0,0,0);
  if (scm_is_array(value)) { // positional light
    position = v4f(scm_to_v3f(value), 1.0);
  }
  glLightfv(light, GL_POSITION, (GLfloat *) &position);
}

static SCM
light_POSITION_getter(int light) {
  v4f position;
  glGetLightfv(light, GL_POSITION, (GLfloat *) &position);
  if(position.w) {
    return scm_from_v3f(v3f(position.x, position.y, position.z));
  }
  return SCM_BOOL_F;
}

DEF_LIGHT_ACCESSORS(EXPONENT, GL_SPOT_EXPONENT, float)
DEF_LIGHT_ACCESSORS(CUTOFF, GL_SPOT_CUTOFF, float)

DEF_GL_LIGHT_ACCESSORS(CONSTANT_ATTENUATION, float)
DEF_GL_LIGHT_ACCESSORS(LINEAR_ATTENUATION, float)
DEF_GL_LIGHT_ACCESSORS(QUADRATIC_ATTENUATION, float)

#undef DEF_GL_LIGHT_ACCESSORS
#undef DEF_LIGHT_ACCESSORS
#undef DEF_LIGHT_GETTER
#undef DEF_LIGHT_SETTER

#define GET_PROPERTY_ID(property)			\
  scm_to_int(scm_hash_ref(light_properties, property,	\
			  scm_from_int(LIGHT_PROPERTY_UNSUPPORTED)))

static SCM
set_light_property_x(SCM light, SCM property, SCM value) {
  int l = scm_to_int(light);
  int property_id = GET_PROPERTY_ID(property);
  (*light_property_setters[property_id])(l, value);
  return SCM_UNSPECIFIED;
}

static SCM
light_property(SCM light, SCM property) {
   int l = scm_to_int(light);
   int property_id = GET_PROPERTY_ID(property);
   return (*light_property_getters[property_id])(l);
}

#undef GET_PROPERTY_ID

static inline void
init_lights() {
  light_properties = gc_protected(scm_c_make_hash_table(NUM_LIGHT_PROPERTIES));
  
#define INIT_ACCESSORS(name, NAME)					\
  scm_hash_set_x(light_properties, symbol(name),			\
		 scm_from_int(LIGHT_PROPERTY_##NAME));			\
  light_property_setters[LIGHT_PROPERTY_##NAME] = light_##NAME##_setter; \
  light_property_getters[LIGHT_PROPERTY_##NAME] = light_##NAME##_getter

  INIT_ACCESSORS("*", UNSUPPORTED);

  INIT_ACCESSORS("ambient", AMBIENT);
  INIT_ACCESSORS("diffuse", DIFFUSE);
  INIT_ACCESSORS("specular", SPECULAR);
  INIT_ACCESSORS("position", POSITION);
  INIT_ACCESSORS("direction", DIRECTION);
  INIT_ACCESSORS("exponent", EXPONENT);
  INIT_ACCESSORS("cutoff", CUTOFF);
  INIT_ACCESSORS("constant-attenuation", CONSTANT_ATTENUATION);
  INIT_ACCESSORS("linear-attenuation", LINEAR_ATTENUATION);
  INIT_ACCESSORS("quadratic-attenuation", QUADRATIC_ATTENUATION);

#undef INIT_ACCESSORS
}

static SCM
make_light() {
  WARN_ONCE("The lighting subsystem needs to be tested for "
	    "adding and removing lights");
  int current_light;
  if (scm_is_null(removed_lights)) {
    if (next_light >= GL_MAX_LIGHTS) {
      WARN("The maximum number of %d lights has been exceeded", GL_MAX_LIGHTS);
      return SCM_BOOL_F;
    }
    
    WARN_ONCE("GL_LIGHT0 == %i", (int) GL_LIGHT0);
    current_light = GL_LIGHT0 + next_light++;
    glEnable(current_light);
    return scm_from_int(current_light);
  }
  SCM l = scm_car(scm_gc_unprotect_object(removed_lights));
  removed_lights = scm_cdr(removed_lights);
  current_light = scm_to_int(l);
  glEnable(current_light);
  return l;
}

static SCM
remove_light_x(SCM light) {
  ASSERT_SCM_TYPE(integer, light, 1);
  int l = scm_to_int(light);
#ifndef NDEBUG
  if (l > (GL_LIGHT0 + next_light) - 1) {
    WARN("Trying to remove unallocated light (%d)", (int) l);
    return SCM_UNSPECIFIED;
  }
  for (SCM x = removed_lights; !scm_is_null(x); x = scm_cdr(x)) {
    if (scm_is_eq(scm_car(x), light)) {
      WARN("Trying to remove a light %d that's already been removed", l);
      return SCM_UNSPECIFIED;
    }
  }
#endif
  glDisable(l);
  if (l == (GL_LIGHT0 + next_light) - 1) {
    --next_light;
  }
  else {
    WARN_ONCE("If possible, it's better to remove the lights in the "
	      "opposite order than allocating them, i.e. to remove "
	      "the last allocated light first.");
    removed_lights = gc_protected(scm_cons(light, removed_lights));
  }
  return SCM_UNSPECIFIED;
}

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

#define EXPORT_OBJECT(name, c_name) \
  scm_c_define(name, c_name); \
  scm_c_export(name, NULL)

  EXPORT_PROCEDURE("z-index", 2, 0, 0, z_index);
  EXPORT_PROCEDURE("display-index", 2, 0, 0, display_index);
  EXPORT_PROCEDURE("set-display-index!", 1, 0, 0, set_display_index_x);
  EXPORT_PROCEDURE("max-display-index", 0, 0, 0, max_display_index);

  EXPORT_PROCEDURE("multiply-matrix!", 1, 0, 0, multiply_matrix_x);
  EXPORT_PROCEDURE("push-matrix!", 0, 0, 0, push_matrix_x);
  EXPORT_PROCEDURE("pop-matrix!", 0, 0, 0, pop_matrix_x);
  EXPORT_PROCEDURE("translate-view!", 1, 0, 0, translate_view_x);
  EXPORT_PROCEDURE("rotate-view!", 1, 0, 0, rotate_view_x);
  EXPORT_PROCEDURE("scale-view!", 1, 2, 0, scale_view_x);
  EXPORT_PROCEDURE("load-identity!", 0, 0, 0, load_identity_x);
  EXPORT_PROCEDURE("set-viewport!", 4, 0, 0, set_viewport_x);
  EXPORT_PROCEDURE("current-viewport", 0, 0, 0, current_viewport);
  EXPORT_PROCEDURE("current-projection", 0, 0, 0, 
		   current_projection);
  EXPORT_PROCEDURE("unproject", 5, 1, 0, unproject);

  EXPORT_PROCEDURE("quaternion->matrix", 1, 0, 0, quaternion_to_matrix);
  EXPORT_PROCEDURE("translation-matrix", 1, 0, 0, translation_matrix);

  EXPORT_PROCEDURE("current-matrix", 0, 0, 0, current_matrix);
  EXPORT_PROCEDURE("perspective-projection", 1, 3, 0, 
		   perspective_projection);
  EXPORT_PROCEDURE("set-perspective-projection!", 1, 3, 0, 
		   set_perspective_projection_x);
  EXPORT_PROCEDURE("set-orthographic-projection!", 4, 2, 0, 
		   set_orthographic_projection_x);
  EXPORT_PROCEDURE("set-vertex-array!", 1, 0, 0, 
		   set_vertex_array_x);
  EXPORT_PROCEDURE("set-color-array!", 1, 0, 0, 
		   set_color_array_x);
  EXPORT_PROCEDURE("set-normal-array!", 1, 0, 0, 
		   set_normal_array_x);
  EXPORT_PROCEDURE("forget-array!", 1, 0, 0, forget_array_x);
  EXPORT_PROCEDURE("set-color!", 1, 0, 0, set_color_x);

  EXPORT_PROCEDURE("set-texture-coord-array!", 1, 0, 0, 
		   set_texture_coord_array_x);
  EXPORT_PROCEDURE("draw-faces!", 2, 0, 0, draw_faces_x);

  EXPORT_PROCEDURE("set-light-property!", 3, 0, 0, set_light_property_x);
  EXPORT_PROCEDURE("light-property", 2, 0, 0, light_property);

  EXPORT_PROCEDURE("make-light-", 0, 0, 0, make_light);
  EXPORT_PROCEDURE("remove-light!", 1, 0, 0, remove_light_x);

#undef EXPORT_PROCEDURE
#undef EXPORT_OBJECT
}

void
init_3d() {
  init_GLtypes();
  init_GLnames();
  init_color_setters();
  init_lights();

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  glEnable(GL_DEPTH_TEST);
  glClearDepth(1.0);
  glDepthMask(GL_TRUE);

  glClearColor(0, 0, 0, 0);
  glShadeModel(GL_SMOOTH);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glWindowPos2i(0, 0);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  //OUT("There are %d lights available", GL_MAX_LIGHTS);

  glClearStencil(0);
  glEnable(GL_STENCIL_TEST);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
