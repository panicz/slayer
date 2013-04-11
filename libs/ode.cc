#include <ode/ode.h>
#include "utils.h"
#include "extend.h"

scm_t_bits ode_tag;

enum ODE_TYPES {
  WORLD = 0,
  SPACE = 1,
  BODY = 2,
  JOINT = 3,
  JOINT_GROUP = 4,
  GEOM = 5,
  ODE_TYPES = 6
};

static char const *ode_types[] = {
  "world",
  "space",
  "body",
  "joint",
  "joint-group",
  "geom"
};

static SCM s_f32;
static SCM s_f64;

#if defined(dSINGLE)
# define ARRAY_TYPE s_f32
#else
# define ARRAY_TYPE s_f64
#endif

#define DEF_SCM_FROM_DVECTOR(n)						\
  static inline SCM							\
  scm_from_dVector##n(dVector##n v) {					\
    SCM V = scm_make_typed_array(ARRAY_TYPE, SCM_UNSPECIFIED,		\
				 scm_list_1(scm_from_int(n)));		\
    scm_t_array_handle h;						\
    scm_array_get_handle(V, &h);					\
    dReal *elements							\
      = (dReal *) scm_array_handle_uniform_writable_elements(&h);	\
    for(int i = 0; i < n; ++i) {					\
      elements[i] = v[i];						\
    }									\
    scm_array_handle_release(&h);					\
    return V;								\
  } 


DEF_SCM_FROM_DVECTOR(3);
DEF_SCM_FROM_DVECTOR(4);

#undef SCM_FROM_DVECTOR


#define DEF_SCM_TO_DVECTOR(n)					\
  static inline void						\
  scm_to_dVector##n(SCM V, dVector##n *v) {			\
    scm_t_array_handle h;					\
    if (scm_is_array(V)) {					\
      scm_array_get_handle(V, &h);				\
      if(scm_is_typed_array(V, s_f32)) {			\
	float const *elements					\
	  = scm_array_handle_f32_elements(&h);			\
	for(int i = 0; i < n; ++i) {				\
	  (*v)[i] = elements[i];				\
	}							\
      }								\
      else if(scm_is_typed_array(V, s_f64)) {			\
	double const *elements					\
	  = scm_array_handle_f64_elements(&h);			\
	for(int i = 0; i < n; ++i) {				\
	  (*v)[i] =  elements[i];				\
	}							\
      }								\
      else {							\
	SCM *elements = (SCM *) scm_array_handle_elements(&h);	\
	for(int i = 0; i < n; ++i) {				\
	  (*v)[i] = (dReal) scm_to_double(elements[i]);		\
	}							\
      }								\
      scm_array_handle_release(&h);				\
    }								\
    else {							\
      WARN("Unable to convert SCM to dVector"#n);	       	\
    }								\
  }

DEF_SCM_TO_DVECTOR(3);
DEF_SCM_TO_DVECTOR(4);
#undef SCM_TO_DVECTOR

/*
  ODE's matrices are such that:
  for 4x4: B11 = B[0], B12 = B[1], ..., B14 = B[3],
           B21 = B[4], ... , B24 = B[7],
	   ...
	   B41 = B[12], ...,  B44 = B[15],
  or, more generally, Bij = B[(i-1)*4+(j-1)],
  
  for 3x3 there's a quirk, as 3x3 matrices are
  actually 3x4, because 3-dimensional vectors are
  actually 4-dimensional, so 
  m3x3 = [x1 y1 z1 w1 x2 y2 z2 w2 x3 y3 z3 w3], or
  so again Bij = [B(i-1)*4+(j-1)]
  
  Guile is more interactive than C++, so we'd like to
  use its uniform arrays
  
 */

#define DEF_SCM_FROM_DMATRIX(n)					\
  static inline SCM						\
  scm_from_dMatrix##n(dMatrix##n m) {				\
    SCM M = scm_make_typed_array(ARRAY_TYPE, SCM_UNSPECIFIED,	\
				 scm_list_2(scm_from_int(n),	\
					    scm_from_int(n)));	\
    scm_t_array_handle h;					\
    scm_array_get_handle(M, &h);				\
    dReal *elements						\
      =(dReal *)scm_array_handle_uniform_writable_elements(&h);	\
    for(int i = 0; i < n; ++i) {				\
      for(int j = 0; j < n; ++j) {				\
	elements[n*i+j] = m[4*i+j];				\
      }								\
    }								\
    scm_array_handle_release(&h);				\
    return M;							\
  }

DEF_SCM_FROM_DMATRIX(3);
DEF_SCM_FROM_DMATRIX(4);

#undef DEF_SCM_FROM_DMATRIX

#define DEF_SCM_TO_DMATRIX(n) \
  static inline void						\
  scm_to_dMatrix##n(SCM M, dMatrix##n *m) {			\
    scm_t_array_handle h;					\
    if (scm_is_array(M)) {					\
      scm_array_get_handle(M, &h);				\
      if (scm_is_typed_array(M, s_f32)) {			\
	float const *elements					\
	  = scm_array_handle_f32_elements(&h);			\
	for(int i = 0; i < n; ++i) {				\
	  for(int j = 0; j < n; ++j) {				\
	    (*m)[4*i+j] = (dReal) elements[n*i+j];		\
	  }							\
	}							\
      }								\
      else if(scm_is_typed_array(M, s_f64)) {			\
	double const *elements					\
	  = scm_array_handle_f64_elements(&h);			\
	for(int i = 0; i < n; ++i) {				\
	  for(int j = 0; j < n; ++j) {				\
	    (*m)[4*i+j] = (dReal) elements[n*i+j];		\
	  }							\
	}							\
      }								\
      else {							\
	SCM *elements = (SCM *) scm_array_handle_elements(&h);	\
	for(int i = 0; i < n; ++i) {				\
	  for(int j = 0; j < n; ++j) {				\
	    (*m)[4*i+j]						\
	      = (dReal) scm_to_double(elements[n*i+j]);		\
	  }							\
	}							\
      }								\
      scm_array_handle_release(&h);				\
    }								\
    else {							\
      WARN("Unable to convert SCM to dMatrix"#n);		\
    }								\
  }

DEF_SCM_TO_DMATRIX(3);
DEF_SCM_TO_DMATRIX(4);

#undef DEF_SCM_FROM_DMATRIX


static SCM
create_world() {
  SCM smob;
  dWorldID id = dWorldCreate();
  SCM_NEWSMOB(smob, ode_tag, id);
  SCM_SET_SMOB_FLAGS(smob, WORLD);
  return smob;
}


static SCM
set_world_gravity_x(SCM world, SCM gravity) {  
  unsigned short type = SCM_SMOB_FLAGS(world);
  if(type != WORLD) {
    WARN("function called on a non-world");
    return SCM_UNSPECIFIED;
  }
  dWorldID w = (dWorldID) SCM_SMOB_DATA(world);

  dVector3 g;
  scm_to_dVector3(gravity, &g);
  OUT("Setting world gravity to [%f, %f, %f]", g[0], g[1], g[2]);
  dWorldSetGravity(w, g[0], g[1], g[2]);

  scm_remember_upto_here_1(world);
  return SCM_UNSPECIFIED;
}

static SCM
get_world_gravity(SCM world) {
  unsigned short type = SCM_SMOB_FLAGS(world);
  if(type != WORLD) {
    WARN("function called on a non-world");
    return SCM_UNSPECIFIED;
  }
  dWorldID w = (dWorldID) SCM_SMOB_DATA(world);
  dVector3 g;
  dWorldGetGravity(w, g);
  scm_remember_upto_here_1(world);
  return scm_from_dVector3(g);
}

#define DEF_WORLD_GETSET_FUNC(name, orig, convert, back, cast)		\
  static SCM								\
  set_world_##name##_x(SCM world, SCM param) {				\
    unsigned short type = SCM_SMOB_FLAGS(world);			\
    if(type != WORLD) {							\
      WARN("function called on a non-world");				\
      return SCM_BOOL_F;						\
    }									\
    dWorldID w = (dWorldID) SCM_SMOB_DATA(world);			\
    dWorldSet##orig(w, (cast) convert(param));				\
    scm_remember_upto_here_1(world);					\
    return SCM_UNSPECIFIED;						\
  }									\
									\
  static SCM								\
  get_world_##name(SCM world) {						\
    unsigned short type = SCM_SMOB_FLAGS(world);			\
    if(type != WORLD) {							\
      WARN("function called on a non-world");				\
      return SCM_BOOL_F;						\
    }									\
    dWorldID w = (dWorldID) SCM_SMOB_DATA(world);			\
    scm_remember_upto_here_1(world);					\
    return back(dWorldGet##orig(w));					\
  }


DEF_WORLD_GETSET_FUNC(erp, ERP, scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(cfm, CFM, scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(auto_disable_flag, AutoDisableFlag, scm_to_bool, 
		      scm_from_bool, int);
DEF_WORLD_GETSET_FUNC(auto_disable_linear_threshold, AutoDisableLinearThreshold,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(auto_disable_angular_threshold, AutoDisableAngularThreshold,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(auto_disable_steps, AutoDisableSteps,
		      scm_to_int, scm_from_int, int);
DEF_WORLD_GETSET_FUNC(auto_disable_time, AutoDisableTime,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(quick_step_iterations, QuickStepNumIterations,
		      scm_to_int, scm_from_int, int);
DEF_WORLD_GETSET_FUNC(quick_step_relaxation, QuickStepW,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(linear_damping, LinearDamping,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(angular_damping, AngularDamping,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(linear_damping_threshold, LinearDampingThreshold,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(angular_damping_threshold, AngularDampingThreshold,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(max_angular_speed, MaxAngularSpeed,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(contact_max_correcting_velocity, ContactMaxCorrectingVel,
		      scm_to_double, scm_from_double, dReal);
DEF_WORLD_GETSET_FUNC(contact_surface_layer, ContactSurfaceLayer,
		      scm_to_double, scm_from_double, dReal);

static size_t
free_ode(SCM smob) {
  unsigned short type = SCM_SMOB_FLAGS(smob);
  if(type == WORLD) {
    dWorldID id = (dWorldID) SCM_SMOB_DATA(smob);
    dWorldDestroy(id);
  }
  else {
    WARN("Unimplemented ODE destructor");
  }

  return 0;
}

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {
  void *data = (void *) SCM_SMOB_DATA(ode);
  unsigned short type = SCM_SMOB_FLAGS(ode);
  if(type < ODE_TYPES) {
    scm_puts("#<ode-", port);
    scm_puts(ode_types[type], port);
    scm_puts(" (implement detailed info for specific types)>", port);
  }
  else {
    scm_throw(symbol("unknown-ode-type"), scm_list_1(scm_from_uint16(type)));
  }
  
  scm_remember_upto_here_1(ode);
  return 1;
}

static void
export_symbols() {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

#define EXPORT_WORLD_GETSET_FUNC(name, cname) \
  EXPORT_PROCEDURE("set-world-" name "!", 2, 0, 0, set_world_##cname##_x);\
  EXPORT_PROCEDURE("get-world-" name, 1, 0, 0, get_world_##cname); 

  EXPORT_PROCEDURE("create-world", 0, 0, 0, create_world);
  EXPORT_WORLD_GETSET_FUNC("gravity", gravity);
  EXPORT_WORLD_GETSET_FUNC("erp", erp);
  EXPORT_WORLD_GETSET_FUNC("cfm", cfm);
  EXPORT_WORLD_GETSET_FUNC("auto-disable-flag", auto_disable_flag);
  EXPORT_WORLD_GETSET_FUNC("auto-disable-linear-threshold", 
			   auto_disable_linear_threshold);
  EXPORT_WORLD_GETSET_FUNC("auto-disable-angular-threshold", 
			   auto_disable_angular_threshold);
  EXPORT_WORLD_GETSET_FUNC("auto-disable-steps", auto_disable_steps);
  EXPORT_WORLD_GETSET_FUNC("auto-disable-time", auto_disable_time);
  EXPORT_WORLD_GETSET_FUNC("quick-step-iterations", quick_step_iterations);
  EXPORT_WORLD_GETSET_FUNC("quick-step-relaxation", quick_step_relaxation);
  EXPORT_WORLD_GETSET_FUNC("linear-damping", linear_damping);
  EXPORT_WORLD_GETSET_FUNC("angular-damping", angular_damping);
  EXPORT_WORLD_GETSET_FUNC("linear-damping-threshold", linear_damping_threshold);
  EXPORT_WORLD_GETSET_FUNC("angular-damping-threshold", angular_damping_threshold);
  EXPORT_WORLD_GETSET_FUNC("contact-max-correcting-velocity", 
			   contact_max_correcting_velocity);
  EXPORT_WORLD_GETSET_FUNC("contact-surface-layer", contact_surface_layer);

#undef EXPORT_WORLD_GETSET_FUNC
#undef EXPORT_PROCEDURE
}

extern "C" void
ode_init() {
  s_f32 = symbol("f32");
  s_f64 = symbol("f64");
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);
  export_symbols();
}
