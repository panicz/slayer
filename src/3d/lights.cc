#include "slayer.h"
#include "extend.h"
#include "utils.h"
#include "video.h"
#include "3d.hh"
#include "symbols.h"
// this module is used only if 3D is available

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

const char *light_properties_names[] = {
  "unsupported",
  "ambient",
  "diffuse",
  "specular",
  "position",
  "direction",
  "exponent",
  "cutoff",
  "constant-attenuation",
  "linear-attenuation",
  "quadratic-attenuation"
};

// up to GL_MAX_LIGHTS can be allocated
static SCM removed_lights = SCM_EOL;
static int next_light = 0;
static SCM light_properties; // hash from symbols to light_properties_enum
// initialzed in init_light_properties()
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
  /*
  OUT_("setting light property %s of light %i to ",
      light_properties_names[property_id], l - GL_LIGHT0);
  scm_display(value, scm_current_output_port());
  OUT();
  */
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
init_light_properties() {
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

  EXPORT_PROCEDURE("set-light-property!", 3, 0, 0, set_light_property_x);
  EXPORT_PROCEDURE("light-property", 2, 0, 0, light_property);

  EXPORT_PROCEDURE("make-light-", 0, 0, 0, make_light);
  EXPORT_PROCEDURE("remove-light!", 1, 0, 0, remove_light_x);

#undef EXPORT_PROCEDURE
}

void
init_lights() {
  init_light_properties();
  //  glEnable(GL_LIGHTING);
  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
