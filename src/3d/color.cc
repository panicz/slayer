#include "3d.hh"

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

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

  EXPORT_PROCEDURE("set-color!", 1, 0, 0, set_color_x);

#undef EXPORT_PROCEDURE
}

void
init_color() {
  glClearColor(0, 0, 0, 1);
  glShadeModel(GL_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  init_color_setters();
  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
