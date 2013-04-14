#include "extend.h"
#include "utils.h"
#include "vmx.hh"
#include "video.h"

extern "C" void glWindowPos2i (GLint x, GLint y);

#define DEF_SCM_TO_V(n,t,ype) \
static inline v##n##t \
scm_to_v##n##t(SCM vector) { \
  int i; v##n##t v; \
  if (scm_is_uniform_vector(vector)) \
    for (i = 0; i < n; ++i) \
      v[i] = scm_to_##t##ype(scm_uniform_vector_ref(vector, scm_from_int(i))); \
  else if (scm_is_vector(vector)) \
    for (i = 0; i < n; ++i) \
      v[i] = scm_to_##t##ype(scm_c_vector_ref(vector, i)); \
  else \
    WARN("function called on a non-vector"); \
  return v; \
}

DEF_SCM_TO_V(2,f,loat);
DEF_SCM_TO_V(2,d,ouble);
DEF_SCM_TO_V(3,f,loat);
DEF_SCM_TO_V(3,d,ouble);
DEF_SCM_TO_V(4,f,loat);
DEF_SCM_TO_V(4,d,ouble);

#undef DEF_SCM_TO_V

static inline qtf
scm_to_qtf(SCM pair) {
  qtf q;
  q.s = scm_to_float(SCM_CAR(pair));
  q.v = scm_to_v3f(SCM_CDR(pair));
  return q;
}

static inline qtd
scm_to_qtd(SCM pair) {
  qtd q;
  q.s = scm_to_double(SCM_CAR(pair));
  q.v = scm_to_v3d(SCM_CDR(pair));
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
rotate_view_x(SCM quaternion) {
  qtf q = scm_to_qtf(quaternion);
  glRotatef(rad2deg(2*acos(q.s)), q.v.x, q.v.y, q.v.z);
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
  return scm_list_4(scm_from_int(s.x), scm_from_int(s.y),
		    scm_from_int(s.w), scm_from_int(s.h));
}

static SCM
perspective_projection_x(SCM FOVY, SCM ASPECT, SCM NEAR, SCM FAR) {
  GLdouble fovy = scm_to_float(FOVY);
  
  GLdouble aspect;
  if ((ASPECT == SCM_UNDEFINED) 
      || (ASPECT == SCM_UNSPECIFIED)
      || (ASPECT == SCM_BOOL_F)) {
    struct { GLint x, y, w, h; } s;
    glGetIntegerv(GL_VIEWPORT, (GLint *) &s);
    aspect = (GLdouble) s.w / (GLdouble) s.h; 
  }
  else {
    aspect = (GLdouble) scm_to_double(ASPECT);
  }

  GLdouble near = (NEAR == SCM_UNDEFINED)
    ? 0.1 : (GLdouble) scm_to_double(NEAR);
  GLdouble far = (FAR == SCM_UNDEFINED)
    ? 1000.0 : (GLdouble) scm_to_double(FAR);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(fovy, aspect, near, far);

  glMatrixMode(GL_MODELVIEW);
  // by convention, each function that ever
  // alters a matrix mode, should leave GL_MODELVIEW
  // as the current matrix mode.
    
  return SCM_UNSPECIFIED;
}

static SCM
ortographic_projection_x() {
  WARN("unimplemented");
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
  end:							\
    scm_array_handle_release(&handle);			\
    return SCM_UNSPECIFIED;				\
  }

DEF_SET_GL_ARRAY(vertices, glVertexPointer, GL_VERTEX_ARRAY, 3);
DEF_SET_GL_ARRAY(colors, glColorPointer, GL_COLOR_ARRAY, 3);
DEF_SET_GL_ARRAY(texture_coords, glTexCoordPointer, 
		 GL_TEXTURE_COORD_ARRAY, 3);

static inline void
_glNormalPointer(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer) {
  return glNormalPointer(type, stride, pointer);
}
DEF_SET_GL_ARRAY(normal, _glNormalPointer, GL_NORMAL_ARRAY, 3);

#undef DEF_SET_GL_ARRAY
#undef NOT_SUPPORTED

static SCM GLnames;
static inline void
init_GLnames() {
  GLnames = scm_c_make_hash_table(64);
  hold_scm(GLnames);

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
  
#undef SET_VALUE
}

static SCM
draw_faces_x(SCM type, SCM array) {
#define GET_VALUE(name) \
  scm_to_int(scm_hash_ref(GLnames, name, SCM_BOOL_F))

  if (!scm_is_array(array)) {
    WARN("argument is not an array");
    return SCM_UNSPECIFIED;
  }

  scm_t_array_handle handle;
  scm_array_get_handle(array, &handle);

  size_t nelems = scm_array_handle_nelems(&handle);
  
  char *name = as_c_string(type);
  //WARN("drawing %s (%d) with %d vertices", name, GET_VALUE(type), nelems);
  free(name);
  const void *data 
    = scm_array_handle_uniform_writable_elements(&handle);

  glDrawElements(GET_VALUE(type), nelems, 
		 GLtypes[handle.element_type], data);


  //OUT("glDrawElements(%d, %d, %d, %p", GET_VALUE(type), nelems, GLtypes[handle.element_type], data);

  scm_array_handle_release(&handle);
  return SCM_UNSPECIFIED;
#undef GET_VALUE
}

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("push-matrix!", 0, 0, 0, push_matrix_x);
  EXPORT_PROCEDURE("pop-matrix!", 0, 0, 0, pop_matrix_x);
  EXPORT_PROCEDURE("translate-view!", 1, 0, 0, translate_view_x);
  EXPORT_PROCEDURE("rotate-view!", 1, 0, 0, rotate_view_x);
  EXPORT_PROCEDURE("set-viewport!", 4, 0, 0, set_viewport_x);
  EXPORT_PROCEDURE("current-viewport", 0, 0, 0, current_viewport);
  EXPORT_PROCEDURE("perspective-projection!", 1, 3, 0, 
		      perspective_projection_x);
  EXPORT_PROCEDURE("ortographic-projection!", 4, 2, 0, 
		      ortographic_projection_x);
  EXPORT_PROCEDURE("set-vertices-array!", 1, 0, 0, 
		   set_vertices_array_x);
  EXPORT_PROCEDURE("set-colors-array!", 1, 0, 0, 
		      set_colors_array_x);
  EXPORT_PROCEDURE("set-normal-array!", 1, 0, 0, 
		   set_normal_array_x);

  EXPORT_PROCEDURE("set-texture-coords-array!", 1, 0, 0, 
		      set_texture_coords_array_x);
  EXPORT_PROCEDURE("draw-faces!", 2, 0, 0, draw_faces_x);

#undef EXPORT_PROCEDURE
}

extern "C" void
init_3d(Uint16 w, Uint16 h) {
  init_GLtypes();
  init_GLnames();

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  glEnable(GL_DEPTH_TEST);
  glClearDepth(1.0);
  glDepthMask(GL_TRUE);

  glClearColor(0, 0, 0, 0);
  glShadeModel(GL_SMOOTH);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

     //    glDisable(GL_COLOR_MATERIAL);
  //gluLookAt (0.0, 0.0, 21.0, 0.0, 0.0, -100.0, 0.0, 1.0, 0.0);

  glWindowPos2i(0, 0);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  scm_c_define_module("slayer 3d", export_symbols, NULL);
}


