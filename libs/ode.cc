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

static SCM
create_world() {
  SCM smob;
  dWorldID id = dWorldCreate();
  SCM_NEWSMOB(smob, ode_tag, id);
  SCM_SET_SMOB_FLAGS(smob, WORLD);
  return smob;
}

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

static void
export_symbols() {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("create-world", 0, 0, 0, create_world);

#undef EXPORT_PROCEDURE
}

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

extern "C" void
ode_init() {
  s_f32 = symbol("f32");
  s_f64 = symbol("f64");
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);
  export_symbols();
}
