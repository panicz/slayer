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
