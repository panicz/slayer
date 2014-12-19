#include "slayer.h"
#include "extend.h"
#include "utils.h"
#include <limits.h>
#include "symbols.h"
#include "3d.hh"

#if !HAVE_GL_WINDOW_POS2I
// code borrowed from 
// https://code.google.com/p/lasermission/source/browse/trunk/source/Emulation/glWindowPos.cpp
// (modified)
// courtesy of Mike MacFerrin
void 
glWindowPos2i(int x, int y) {
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

#include "3d/extensions-def.m4.c"

#ifdef NO_GL_GEN_FRAMEBUFFERS
void (*glGenFramebuffers)(GLsizei, GLuint *) = NULL;
#endif

#ifdef NO_GL_GEN_RENDERBUFFERS
void (*glGenRenderbuffers)(GLsizei, GLuint *) = NULL;
#endif

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

#define DEF_GL_PARAM_ACCESSORS(parameter_name, ParameterName,		\
			       PARAMETER_NAME, type, Type, C)		\
  static SCM								\
  set_##parameter_name##_x(SCM _value) {				\
    GL##type value = (GL##type) scm_to_##C(_value);			\
    gl##ParameterName(value);						\
    return SCM_UNSPECIFIED;						\
  }									\
  static SCM get_##parameter_name() {					\
    GL##type result;							\
    glGet##Type##v(GL_##PARAMETER_NAME, &result);			\
    return scm_from_##C((C) result);					\
  }

DEF_GL_PARAM_ACCESSORS(point_size, PointSize, POINT_SIZE, 
		       float, Float, double)
DEF_GL_PARAM_ACCESSORS(line_width, LineWidth, LINE_WIDTH, 
		       float, Float, double)

#undef DEF_GL_PARAM_ACCESSORS

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

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

  EXPORT_PROCEDURE("current-matrix", 0, 0, 0, current_matrix);
  EXPORT_PROCEDURE("set-perspective-projection!", 1, 3, 0, 
		   set_perspective_projection_x);
  EXPORT_PROCEDURE("set-orthographic-projection!", 4, 2, 0, 
		   set_orthographic_projection_x);

#define EXPORT_ACCESSORS(scm_name, c_name)				\
  EXPORT_PROCEDURE("set-" scm_name "!", 1, 0, 0, set_##c_name##_x);	\
  EXPORT_PROCEDURE(scm_name, 0, 0, 0, get_##c_name)

  EXPORT_ACCESSORS("line-width", line_width);
  EXPORT_ACCESSORS("point-size", point_size);

#undef EXPORT_ACCESSORS

#undef EXPORT_PROCEDURE
}

void
init_3d() {
  OUT("OpenGL %s using %s from %s", 
      glGetString(GL_VERSION),
      glGetString(GL_RENDERER), 
      glGetString(GL_VENDOR));

#include "3d/info.m4.c"

#include "3d/extensions-init.m4.c"

  init_arrays();
  init_color();
  init_lights();
  init_buffers();
  init_transforms();

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glWindowPos2i(0, 0);

  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
