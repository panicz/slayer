#include "3d.hh"

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

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

  EXPORT_PROCEDURE("set-vertex-array!", 1, 0, 0, 
		   set_vertex_array_x);
  EXPORT_PROCEDURE("set-color-array!", 1, 0, 0, 
		   set_color_array_x);
  EXPORT_PROCEDURE("set-normal-array!", 1, 0, 0, 
		   set_normal_array_x);
  EXPORT_PROCEDURE("forget-array!", 1, 0, 0, forget_array_x);
  EXPORT_PROCEDURE("set-texture-coord-array!", 1, 0, 0, 
		   set_texture_coord_array_x);
  EXPORT_PROCEDURE("draw-faces!", 2, 0, 0, draw_faces_x);

#undef EXPORT_PROCEDURE
}

void
init_arrays() {
  init_GLtypes();
  init_GLnames();
  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
