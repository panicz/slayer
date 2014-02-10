#include "3d.hh"

static SCM
z_index(SCM X, SCM Y) {
  return scm_from_double(_z_index(scm_to_int(X), scm_to_int(Y)));
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

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

  EXPORT_PROCEDURE("z-index", 2, 0, 0, z_index);
  EXPORT_PROCEDURE("display-index", 2, 0, 0, display_index);
  EXPORT_PROCEDURE("set-display-index!", 1, 0, 0, set_display_index_x);
  EXPORT_PROCEDURE("max-display-index", 0, 0, 0, max_display_index);

#undef EXPORT_PROCEDURE
}

void
init_buffers() {
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  glEnable(GL_DEPTH_TEST);
  glClearDepth(1.0);
  glDepthMask(GL_TRUE);

  glClearStencil(0);
  glEnable(GL_STENCIL_TEST);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);

  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
