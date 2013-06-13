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
  MASS = 6,
  ODE_TYPES = 7,
  ODE_TYPE_MASK = 0b1111,
  ODE_TYPE_SHIFT = 0
};

static char const *ode_types[] = {
  "world",
  "space",
  "body",
  "joint",
  "joint-group",
  "geom",
  "mass"
};

enum JOINT_TYPES {
  BALL = 0,
  HINGE = 1,
  SLIDER = 2,
  CONTACT = 3, 
  UNIVERSAL = 4,
  HINGE2 = 5,
  PR = 6,
  PU = 7,
  PISTON = 8,
  FIXED = 9,
  AMOTOR = 10,
  LMOTOR = 11,
  PLANE2D = 12,
  JOINT_TYPES = 13,
  JOINT_TYPE_MASK = 0b11110000,
  JOINT_TYPE_SHIFT = 4
};

static char const *joint_types[] = {
  "ball",
  "hinge",
  "slider",
  "contact",
  "universal",
  "hinge2",
  "PR",
  "PU",
  "piston",
  "fixed",
  "amotor",
  "lmotor",
  "plane2d"
};


/*************************** MATRICES AND VECTORS ***************************/

static SCM s_f32;
static SCM s_f64;

#if defined(dSINGLE)
# define ARRAY_TYPE s_f32
#else
# define ARRAY_TYPE s_f64
#endif

#define DEF_SCM_FROM_DVECTOR(n)						\
  static inline SCM							\
  scm_from_dVector##n(const dVector##n v) {				\
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

/********************************* WORLD ************************************/

static SCM
create_world() {
  SCM smob;
  dWorldID id = dWorldCreate();
  SCM_NEWSMOB(smob, ode_tag, id);
  SCM_SET_SMOB_FLAGS(smob, WORLD);
  return smob;
}

#define WORLD_CONDITIONAL_ASSIGN(scm_var, c_var)			\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(((SCM_SMOB_FLAGS(scm_var) & ODE_TYPE_MASK)	>> ODE_TYPE_SHIFT)	\
     != WORLD) {							\
    WARN("function called on a non-world");				\
    return SCM_BOOL_F;							\
  }									\
  dWorldID c_var = (dWorldID) SCM_SMOB_DATA(scm_var)

static SCM
set_world_gravity_x(SCM world, SCM gravity) {
  WORLD_CONDITIONAL_ASSIGN(world, w);

  dVector3 g;
  scm_to_dVector3(gravity, &g);
  dWorldSetGravity(w, g[0], g[1], g[2]);

  scm_remember_upto_here_1(world);
  return SCM_UNSPECIFIED;
}

static SCM
get_world_gravity(SCM world) {
  WORLD_CONDITIONAL_ASSIGN(world, w);
  dVector3 g;
  dWorldGetGravity(w, g);
  scm_remember_upto_here_1(world);
  return scm_from_dVector3(g);
}

#define DEF_WORLD_GETSET_FUNC(name, orig, convert, back, cast)		\
  static SCM								\
  set_world_##name##_x(SCM world, SCM param) {				\
    WORLD_CONDITIONAL_ASSIGN(world, w);					\
    dWorldSet##orig(w, (cast) convert(param));				\
    scm_remember_upto_here_1(world);					\
    return SCM_UNSPECIFIED;						\
  }									\
									\
  static SCM								\
  get_world_##name(SCM world) {						\
    WORLD_CONDITIONAL_ASSIGN(world, w);					\
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

static SCM
world_step_x(SCM world, SCM stepsize) {
  dWorldID w = (dWorldID) SCM_SMOB_DATA(world);
  dReal step = ((stepsize == SCM_UNDEFINED) 
		? 0.001 
		: scm_to_double(stepsize));
  dWorldStep(w, step);
  scm_remember_upto_here_1(world);
  return SCM_UNSPECIFIED;
}

static SCM
quick_step_x(SCM world, SCM stepsize) {
  dWorldID w = (dWorldID) SCM_SMOB_DATA(world);
  dReal step = ((stepsize == SCM_UNDEFINED) 
		? 0.001 
		: scm_to_double(stepsize));
  dWorldQuickStep(w, step);
  scm_remember_upto_here_1(world);
  return SCM_UNSPECIFIED;
}

#undef DEF_WORLD_GETSET_FUNC

/********************************* BODY *************************************/

static SCM
create_body(SCM world) {
  WORLD_CONDITIONAL_ASSIGN(world, w);
  dBodyID id = dBodyCreate(w);
  SCM smob;
  SCM_NEWSMOB(smob, ode_tag, id);
  SCM_SET_SMOB_FLAGS(smob, BODY);
  scm_gc_protect_object(smob);
  return smob;
}

#define BODY_CONDITIONAL_ASSIGN(scm_var, c_var)				\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(((SCM_SMOB_FLAGS(scm_var) & ODE_TYPE_MASK)	>> ODE_TYPE_SHIFT)	\
     != BODY) {								\
    WARN("function called on a non-body");				\
    return SCM_BOOL_F;							\
  }									\
  dBodyID c_var = (dBodyID) SCM_SMOB_DATA(scm_var)

#define MASS_CONDITIONAL_ASSIGN(scm_var, c_var)				\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(((SCM_SMOB_FLAGS(scm_var) & ODE_TYPE_MASK) >> ODE_TYPE_SHIFT)	\
     != MASS) {								\
    WARN("function called on a non-mass");				\
    return SCM_BOOL_F;							\
  }									\
  dMass *c_var = (dMass *) SCM_SMOB_DATA(scm_var)


#define DEF_BODY_GETSETV3_FUNC(name, orig)	\
  static SCM					\
  set_body_##name##_x(SCM body, SCM V) {	\
    BODY_CONDITIONAL_ASSIGN(body, b);		\
    dVector3 v;					\
    scm_to_dVector3(V, &v);			\
    dBodySet##orig(b, v[0], v[1], v[2]);	\
    return SCM_UNSPECIFIED;			\
  }						\
						\
  static SCM					\
  get_body_##name(SCM body) {			\
    BODY_CONDITIONAL_ASSIGN(body, b);		\
    dReal const *v = dBodyGet##orig(b);		\
    return scm_from_dVector3((dReal *) v);	\
  }
  
DEF_BODY_GETSETV3_FUNC(position, Position);
DEF_BODY_GETSETV3_FUNC(linear_velocity, LinearVel);
DEF_BODY_GETSETV3_FUNC(angular_velocity, AngularVel);
DEF_BODY_GETSETV3_FUNC(force, Force);
DEF_BODY_GETSETV3_FUNC(torque, Torque);

#undef DEF_BODY_GETSETV3_FUNC

static SCM
set_body_rotation_x(SCM body, SCM rotation) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dMatrix3 M;
  scm_to_dMatrix3(rotation, &M);
  dBodySetRotation(b, M);
  return SCM_UNSPECIFIED;
}

static SCM
get_body_rotation(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dReal const *v = dBodyGetRotation(b);
  return scm_from_dMatrix3((dReal *) v);
}

static SCM
set_body_quaternion_x(SCM body, SCM quaternion) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector4 q;
  scm_to_dVector4(quaternion, &q);
  dBodySetQuaternion(b, q);
  return SCM_UNSPECIFIED;
}

static SCM
create_mass(SCM mass, SCM center, SCM inertia) {
  dMass *m = new dMass;
  SCM smob;
  m->mass = (dReal) scm_to_double(mass);
  scm_to_dVector3(center, &m->c);
  scm_to_dMatrix3(inertia, &m->I);
  SCM_NEWSMOB(smob, ode_tag, m);
  SCM_SET_SMOB_FLAGS(smob, MASS);
  return smob;
}

static SCM
get_body_quaternion(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dReal const *v = dBodyGetQuaternion(b);
  return scm_from_dVector4((dReal *) v);
}

static SCM
set_body_mass_x(SCM body, SCM mass) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  MASS_CONDITIONAL_ASSIGN(mass, m);
  dBodySetMass(b, m);
  return SCM_UNSPECIFIED;
}

static SCM
get_body_mass(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  SCM smob;
  dMass *m = new dMass;
  dBodyGetMass(b, m);
  SCM_NEWSMOB(smob, ode_tag, m);
  SCM_SET_SMOB_FLAGS(smob, MASS);
  return smob;
}

static SCM
add_body_force_x(SCM body, SCM force) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f;
  scm_to_dVector3(force, &f);
  dBodyAddForce(b, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_local_force_x(SCM body, SCM force) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f;
  scm_to_dVector3(force, &f);
  dBodyAddRelForce(b, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_force_at_position_x(SCM body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f, p;
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddForceAtPos(b, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}
static SCM
add_body_force_at_relative_position_x(SCM body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f, p;
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddForceAtRelPos(b, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_local_force_at_position_x(SCM body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f, p;
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddRelForceAtPos(b, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_local_force_at_relative_position_x(SCM body, SCM force, SCM pos) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f, p;
  scm_to_dVector3(force, &f);
  scm_to_dVector3(pos, &p);
  dBodyAddRelForceAtRelPos(b, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_torque_x(SCM body, SCM torque) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f;
  scm_to_dVector3(torque, &f);
  dBodyAddTorque(b, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
add_body_local_torque_x(SCM body, SCM torque) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dVector3 f;
  scm_to_dVector3(torque, &f);
  dBodyAddRelTorque(b, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

#define DEF_BODY_VECTOR_CONVERSION(name, conversion)	\
  static SCM						\
  name(SCM body, SCM V) {				\
    BODY_CONDITIONAL_ASSIGN(body, b);			\
    dVector3 v, w;					\
    scm_to_dVector3(V, &v);				\
    conversion(b, v[0], v[1], v[2], w);			\
    return scm_from_dVector3(w);			\
  }

DEF_BODY_VECTOR_CONVERSION(global_to_local_position, dBodyGetPosRelPoint);
DEF_BODY_VECTOR_CONVERSION(local_to_global_position, dBodyGetRelPointPos);
DEF_BODY_VECTOR_CONVERSION(global_to_local_velocity, dBodyGetPointVel);
DEF_BODY_VECTOR_CONVERSION(local_to_global_velocity, dBodyGetRelPointVel);
DEF_BODY_VECTOR_CONVERSION(body_vector_to_world, dBodyVectorToWorld);
DEF_BODY_VECTOR_CONVERSION(world_vector_to_body, dBodyVectorFromWorld);

#undef DEF_BODY_VECTOR_CONVERSION

static SCM
enable_body_x(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dBodyEnable(b);
  return SCM_UNSPECIFIED;
}

static SCM
disable_body_x(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  dBodyDisable(b);
  return SCM_UNSPECIFIED;
}

static SCM
enabled_body_p(SCM body) {
  BODY_CONDITIONAL_ASSIGN(body, b);
  return scm_from_bool(dBodyIsEnabled(b));
}

#define DEF_BODY_GETSET_FUNC(name, orig, convert, back, cast)		\
  static SCM								\
  set_body_##name##_x(SCM body, SCM param) {				\
    BODY_CONDITIONAL_ASSIGN(body, b);					\
    dBodySet##orig(b, (cast) convert(param));				\
    scm_remember_upto_here_1(body);					\
    return SCM_UNSPECIFIED;						\
  }									\
									\
  static SCM								\
  get_body_##name(SCM body) {						\
    BODY_CONDITIONAL_ASSIGN(body, b);					\
    scm_remember_upto_here_1(body);					\
    return back(dBodyGet##orig(b));					\
  }

DEF_BODY_GETSET_FUNC(auto_disable_flag, AutoDisableFlag, scm_to_bool, 
		     scm_from_bool, int);
DEF_BODY_GETSET_FUNC(auto_disable_linear_threshold, AutoDisableLinearThreshold,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(auto_disable_angular_threshold, AutoDisableAngularThreshold,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(auto_disable_steps, AutoDisableSteps,
		     scm_to_int, scm_from_int, int);
DEF_BODY_GETSET_FUNC(auto_disable_time, AutoDisableTime,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(auto_disable_average_samples_count, 
		     AutoDisableAverageSamplesCount,
		     scm_to_uint, scm_from_uint, unsigned int);
DEF_BODY_GETSET_FUNC(linear_damping, LinearDamping,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(angular_damping, AngularDamping,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(linear_damping_threshold, LinearDampingThreshold,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(angular_damping_threshold, AngularDampingThreshold,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(max_angular_speed, MaxAngularSpeed,
		     scm_to_double, scm_from_double, dReal);
DEF_BODY_GETSET_FUNC(finite_rotation_mode, FiniteRotationMode,
		     scm_to_int, scm_from_int, int);
DEF_BODY_GETSET_FUNC(gravity_mode, GravityMode,
		     scm_to_int, scm_from_int, int);

#undef BODY_GETSET_FUNC

/******************************* JOINTS *************************************/

#define JOINT_GROUP_CONDITIONAL_ASSIGN(scm_var, c_var)			\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(((SCM_SMOB_FLAGS(scm_var) & ODE_TYPE_MASK) >> ODE_TYPE_SHIFT)	\
     != JOINT_GROUP) {							\
    WARN("function called on something that isn't a joint group");	\
    return SCM_BOOL_F;							\
  }									\
  dJointGroupID c_var = (dJointGroupID) SCM_SMOB_DATA(scm_var)

#define JOINT_CONDITIONAL_ASSIGN(scm_var, c_var)			\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(((SCM_SMOB_FLAGS(scm_var) & ODE_TYPE_MASK) >> ODE_TYPE_SHIFT)	\
     != JOINT) {							\
    WARN("function called on a non-joint");				\
    return SCM_BOOL_F;							\
  }									\
  dJointID c_var = (dJointID) SCM_SMOB_DATA(scm_var)

static SCM
create_joint_group() {
  dJointGroupID jg = dJointGroupCreate(0);
  SCM smob;
  SCM_NEWSMOB(smob, ode_tag, jg);
  SCM_SET_SMOB_FLAGS(smob, JOINT_GROUP);
  return smob;
}

#define DEF_CREATE_JOINT(type, Type, TYPE)				\
  static SCM								\
  create_##type##_joint(SCM world, SCM group) {				\
    WORLD_CONDITIONAL_ASSIGN(world, w);					\
    JOINT_GROUP_CONDITIONAL_ASSIGN(group, jg);				\
    dJointID j = dJointCreate##Type(w, jg);				\
    SCM smob;								\
    SCM_NEWSMOB(smob, ode_tag, j);					\
    SCM_SET_SMOB_FLAGS(smob, JOINT | (TYPE << JOINT_TYPE_SHIFT));	\
    return smob;							\
  }

DEF_CREATE_JOINT(ball, Ball, BALL);
DEF_CREATE_JOINT(hinge, Hinge, HINGE);
DEF_CREATE_JOINT(slider, Slider, SLIDER);
DEF_CREATE_JOINT(universal, Universal, UNIVERSAL);
DEF_CREATE_JOINT(hinge2, Hinge2, HINGE2);
DEF_CREATE_JOINT(PR, PR, PR);
DEF_CREATE_JOINT(PU, PU, PU);
DEF_CREATE_JOINT(piston, Piston, PISTON);
DEF_CREATE_JOINT(fixed, Fixed, FIXED);
DEF_CREATE_JOINT(amotor, AMotor, AMOTOR);
DEF_CREATE_JOINT(lmotor, LMotor, LMOTOR);
DEF_CREATE_JOINT(plane2d, Plane2D, PLANE2D);

#undef DEF_CREATE_JOINT

#undef WORLD_CONDITIONAL_ASSIGN
#undef BODY_CONDITIONAL_ASSIGN
#undef MASS_CONDITIONAL_ASSIGN
#undef JOINT_GROUP_CONDITIONAL_ASSIGN
#undef JOINT_CONDITIONAL_ASSIGN

/******************************* GENERAL ************************************/
// It is a big question: how should the memory be managed?
// Firstly, we have a world which points to various objects. If the world
// is no longer available to any variable, it should be garbage-collected.
// What about the bodies? We know, that they all belong to the world -- therefore
// garbage collector has no right to delete it, unless it was explicitely deleted
// by the user.
// 

#warning "MEMORY MANAGEMENT ISN'T YET SUPPORTED (ALL OBJECTS ARE PERSISTENT)"

static size_t
free_ode(SCM smob) {
  unsigned short type = SCM_SMOB_FLAGS(smob);
  if(type == WORLD) {
    dWorldID id = (dWorldID) SCM_SMOB_DATA(smob);
    OUT("Attempting to release world");
    OUT("%i, %i", sizeof(SCM), sizeof(void *));
    //dWorldDestroy(id);
  }
  else if(type == BODY) {
    dBodyID id = (dBodyID) SCM_SMOB_DATA(smob);
    OUT("Attempting to release body");
    //dBodyDestroy(id);
  }
  else if(type == MASS) {
    dMass *m = (dMass *) SCM_SMOB_DATA(smob);
    OUT("Attempting to release mass");
    //delete m;
  }
  else if(type == JOINT_GROUP) {
    dJointGroupID jg = (dJointGroupID) SCM_SMOB_DATA(smob);
    OUT("Attempting to release joint group");
    //dJointGroupDestroy(jg);
  }
  else if(type == JOINT) {
    OUT("Attempting to release joint");
  }
  else {
    WARN("Unimplemented ODE destructor");
  }
  return 0;
}

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {
  unsigned short type = ((SCM_SMOB_FLAGS(ode) & ODE_TYPE_MASK) >> ODE_TYPE_SHIFT);
  if(type < ODE_TYPES) {
    scm_puts("#<ode-", port);
    scm_puts(ode_types[type], port);
    char * string;
    if(type == WORLD) {
      //dWorldID w = (dWorldID) SCM_SMOB_DATA(ode);

    }
    else if(type == BODY) {
      dBodyID b = (dBodyID) SCM_SMOB_DATA(ode);
      dReal const *r = dBodyGetPosition(b);
      dReal const *q = dBodyGetQuaternion(b);
      asprintf(&string, " (%.2f %.2f %.2f)@(%.2f %.2f %.2f %.2f)", 
	       r[0], r[1], r[2], q[0], q[1], q[2], q[3]);
      scm_puts(string, port);
      free(string);
    }
    else if(type == JOINT) {
      unsigned short joint_type 
	= ((SCM_SMOB_FLAGS(ode) & JOINT_TYPE_MASK) >> JOINT_TYPE_SHIFT);
      scm_puts(" ", port);
      scm_puts(joint_types[joint_type], port);

    }

    scm_puts(">", port);
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

  /*** WORLD ***/
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
  EXPORT_WORLD_GETSET_FUNC("linear-damping-threshold", 
			   linear_damping_threshold);
  EXPORT_WORLD_GETSET_FUNC("angular-damping-threshold", 
			   angular_damping_threshold);
  EXPORT_WORLD_GETSET_FUNC("max-angular-speed", max_angular_speed);
  EXPORT_WORLD_GETSET_FUNC("contact-max-correcting-velocity", 
			   contact_max_correcting_velocity);
  EXPORT_WORLD_GETSET_FUNC("contact-surface-layer", contact_surface_layer);

  EXPORT_PROCEDURE("world-step!", 1, 1, 0, world_step_x);
  EXPORT_PROCEDURE("quick-step!", 1, 1, 0, quick_step_x);

#undef EXPORT_WORLD_GETSET_FUNC  

  /*** BODY ***/

  EXPORT_PROCEDURE("create-body", 1, 0, 0, create_body);
  EXPORT_PROCEDURE("create-mass", 3, 0, 0, create_mass);
#define EXPORT_BODY_GETSET_FUNC(name, cname) \
  EXPORT_PROCEDURE("set-body-" name "!", 2, 0, 0, set_body_##cname##_x);\
  EXPORT_PROCEDURE("get-body-" name, 1, 0, 0, get_body_##cname); 

  EXPORT_BODY_GETSET_FUNC("position", position);
  EXPORT_BODY_GETSET_FUNC("rotation", rotation);
  EXPORT_BODY_GETSET_FUNC("quaternion", quaternion);
  EXPORT_BODY_GETSET_FUNC("linear-velocity", linear_velocity);
  EXPORT_BODY_GETSET_FUNC("angular-velocity", angular_velocity);
  EXPORT_BODY_GETSET_FUNC("mass", mass);
  EXPORT_BODY_GETSET_FUNC("force", force);
  EXPORT_BODY_GETSET_FUNC("torque", torque);

  EXPORT_PROCEDURE("add-body-force!", 2, 0, 0, add_body_force_x);
  EXPORT_PROCEDURE("add-body-local-force!", 2, 0, 0, add_body_local_force_x);
  EXPORT_PROCEDURE("add-body-force-at-position!", 3, 0, 0, 
		   add_body_force_at_position_x);
  EXPORT_PROCEDURE("add-body-force-at-relative-position!", 3, 0, 0, 
		   add_body_force_at_relative_position_x);
  EXPORT_PROCEDURE("add-body-local-force-at-position!", 3, 0, 0, 
		   add_body_local_force_at_position_x);
  EXPORT_PROCEDURE("add-body-local-force-at-relative-position!", 3, 0, 0, 
		   add_body_local_force_at_relative_position_x);
  EXPORT_PROCEDURE("add-body-torque!", 2, 0, 0, add_body_torque_x);
  EXPORT_PROCEDURE("add-body-local-torque!", 2, 0, 0, add_body_local_torque_x);

  EXPORT_PROCEDURE("global->local/position", 2, 0, 0, global_to_local_position);
  EXPORT_PROCEDURE("local->global/position", 2, 0, 0, local_to_global_position);
  EXPORT_PROCEDURE("global->local/velocity", 2, 0, 0, global_to_local_velocity);
  EXPORT_PROCEDURE("local->global/velocity", 2, 0, 0, local_to_global_velocity);
  EXPORT_PROCEDURE("body-vector->world", 2, 0, 0, body_vector_to_world);
  EXPORT_PROCEDURE("world-vector->body", 2, 0, 0, world_vector_to_body);

  EXPORT_PROCEDURE("enable-body!", 1, 0, 0, enable_body_x);
  EXPORT_PROCEDURE("disable-body!", 1, 0, 0, disable_body_x);
  EXPORT_PROCEDURE("enabled-body?", 1, 0, 0, enabled_body_p);

  EXPORT_BODY_GETSET_FUNC("auto-disable-flag", auto_disable_flag);
  EXPORT_BODY_GETSET_FUNC("auto-disable-linear-threshold", 
			   auto_disable_linear_threshold);
  EXPORT_BODY_GETSET_FUNC("auto-disable-angular-threshold", 
			   auto_disable_angular_threshold);
  EXPORT_BODY_GETSET_FUNC("auto-disable-steps", auto_disable_steps);
  EXPORT_BODY_GETSET_FUNC("auto-disable-time", auto_disable_time);
  EXPORT_BODY_GETSET_FUNC("auto-disable-average-samples-count", 
			  auto_disable_average_samples_count);
  EXPORT_BODY_GETSET_FUNC("linear-damping", linear_damping);
  EXPORT_BODY_GETSET_FUNC("angular-damping", angular_damping);
  EXPORT_BODY_GETSET_FUNC("linear-damping-threshold", 
			  linear_damping_threshold);
  EXPORT_BODY_GETSET_FUNC("angular-damping-threshold", 
			  angular_damping_threshold);
  EXPORT_BODY_GETSET_FUNC("max-angular-speed", max_angular_speed);
  EXPORT_BODY_GETSET_FUNC("finite-rotation-mode", finite_rotation_mode);
  EXPORT_BODY_GETSET_FUNC("gravity-mode", gravity_mode);

#undef EXPORT_BODY_GETSET_FUNC
  /*** JOINTS ***/
  EXPORT_PROCEDURE("create-joint-group", 0, 0, 0, create_joint_group);

#define EXPORT_JOINT_CONSTRUCTOR(name)		\
  EXPORT_PROCEDURE("create-" # name "-joint", 2, 0, 0, create_##name##_joint)

  EXPORT_JOINT_CONSTRUCTOR(ball);
  EXPORT_JOINT_CONSTRUCTOR(hinge);
  EXPORT_JOINT_CONSTRUCTOR(slider);
  EXPORT_JOINT_CONSTRUCTOR(universal);
  EXPORT_JOINT_CONSTRUCTOR(hinge2);
  EXPORT_JOINT_CONSTRUCTOR(PR);
  EXPORT_JOINT_CONSTRUCTOR(PU);
  EXPORT_JOINT_CONSTRUCTOR(piston);
  EXPORT_JOINT_CONSTRUCTOR(fixed);
  EXPORT_JOINT_CONSTRUCTOR(amotor);
  EXPORT_JOINT_CONSTRUCTOR(lmotor);
  EXPORT_JOINT_CONSTRUCTOR(plane2d);

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
  dInitODE();
}
