#include "extend.h"
#include "utils.h"
#include "vmx.hh"
#include "video.h"

extern "C" void glWindowPos2i (GLint x, GLint y);

#define DEF_SCM_VECTOR_TO_V(n,t,ype)					\
static inline v##n##t							\
scm_to_v##n##t(SCM vector) {						\
  int i; v##n##t v;							\
  if (scm_is_uniform_vector(vector))					\
    for (i = 0; i < n; ++i)						\
      v[i] = scm_to_##t##ype(scm_uniform_vector_ref(vector, scm_from_int(i))); \
  else if (scm_is_vector(vector))					\
    for (i = 0; i < n; ++i)						\
      v[i] = scm_to_##t##ype(scm_c_vector_ref(vector, i));		\
  else									\
    WARN("function called on a non-vector");				\
  return v;								\
}

DEF_SCM_VECTOR_TO_V(2,f,loat);
DEF_SCM_VECTOR_TO_V(2,d,ouble);
DEF_SCM_VECTOR_TO_V(3,f,loat);
DEF_SCM_VECTOR_TO_V(3,d,ouble);
DEF_SCM_VECTOR_TO_V(4,f,loat);
DEF_SCM_VECTOR_TO_V(4,d,ouble);

#undef DEF_SCM_VECTOR_TO_V

static inline qtf
scm_to_qtf(SCM vector) {
  return qtf(scm_to_v4f(vector));
}

static inline qtd
scm_to_qtd(SCM vector) {
  return qtd(scm_to_v4d(vector));
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
rotate_view_x(SCM quaternion) {
  qtf q = scm_to_qtf(quaternion);
  glRotatef(rad2deg(2*acos(q.s)), q.v.x, q.v.y, q.v.z);
  return SCM_UNSPECIFIED;
}

static SCM
set_viewport_x(SCM x, SCM y, SCM w, SCM h) {
  glViewport((GLint) scm_to_int(x), (GLint) screen->h - scm_to_int(y) - scm_to_int(h), 
	     (GLsizei) scm_to_int(w), (GLsizei) scm_to_int(h));
  return SCM_UNSPECIFIED;
}

static SCM
current_viewport() {
  GLint params[4];
  glGetIntegerv(GL_VIEWPORT, params);
  return scm_list_4(scm_from_int(params[0]), scm_from_int(params[1]),
		    scm_from_int(params[2]), scm_from_int(params[3]));
}

static SCM
draw_mesh(SCM mesh) {
  glColor3f(1.0, 0.7, 0.3);
  glBegin(GL_TRIANGLES);

  glVertex3i(0, 0, 0);
  glVertex3i(0, 1, 0);
  glVertex3i(1, 0, 0);

  glVertex3i(0, 0, 0);
  glVertex3i(0, -1, 0);
  glVertex3i(-1, 0, 0);

  glColor3f(0.7, 1.0, 0.3);
  glVertex3i(0, 0, 0);
  glVertex3i(0, -2, 0);
  glVertex3i(2, 0, 0);

  glEnd();
  glFinish();
  return SCM_UNSPECIFIED;
}

static void
export_symbols() {
  scm_c_define_gsubr("push-matrix!", 0, 0, 0, (scm_t_subr) push_matrix_x);
  scm_c_define_gsubr("pop-matrix!", 0, 0, 0, (scm_t_subr) pop_matrix_x);
  scm_c_define_gsubr("translate-view!", 1, 0, 0, (scm_t_subr) translate_view_x);
  scm_c_define_gsubr("rotate-view!", 1, 0, 0, (scm_t_subr) rotate_view_x);
  scm_c_define_gsubr("set-viewport!", 4, 0, 0, (scm_t_subr) set_viewport_x);
  scm_c_define_gsubr("current-viewport", 0, 0, 0, (scm_t_subr) current_viewport);
  scm_c_define_gsubr("draw-mesh", 1, 0, 0, (scm_t_subr) draw_mesh);
}

extern "C" void
init_3d(Uint16 w, Uint16 h) {
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  glEnable(GL_DEPTH_TEST);
  glClearDepth(1.0);
  glDepthMask(GL_TRUE);
  glClearColor(0, 0, 0, 0);
  glShadeModel(GL_SMOOTH);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  
  //    glDisable(GL_COLOR_MATERIAL);
  glViewport(0, 0, (GLsizei) w, (GLsizei) h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(65, (GLfloat) w/(GLfloat) h, 0.01, 20000.0);
  //gluOrtho2D(0.0, (GLdouble) w, 0.0, (GLdouble) h);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glWindowPos2i(0, 0);

  export_symbols();
}
