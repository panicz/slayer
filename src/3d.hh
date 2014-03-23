#ifndef _3D_HH
#define _3D_HH
#include "utils.h"
#include "video.h"
#include "symbols.h"
#include "vmx.hh"

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

static inline double 
_z_index(int x, int y) {
  float z;
  glReadPixels(x, screen->h - y - 1, 1, 1, 
	       GL_DEPTH_COMPONENT, 
	       GL_FLOAT, 
	       (GLvoid *) &z);
  return (double) z;
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

DECLARE void init_lights();
DECLARE void init_arrays();
DECLARE void init_color();
DECLARE void init_buffers();
DECLARE void init_transforms();

#endif // _3D_HH
