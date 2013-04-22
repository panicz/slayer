#ifndef _PHYSICS_H
#define _PHYSICS_H

#include <ode/ode.h>
#include <list>
#include <vector>
#include <functional>
#include <algorithm>
#include <unordered_map>
#include <tuple>
#include <cmath>
#include "utils.h"
#include "extend.h"

using namespace std;

extern SCM s_f32;
extern SCM s_f64;

struct scm_eq : binary_function<SCM, SCM, bool> {
  bool operator() (const SCM& x, const SCM& y) const {
    return (bool) scm_is_eq(x,y);
  }
};

struct pair_scm_int_eq : binary_function<pair<SCM, int>, pair<SCM, int>, bool> {
  bool operator() (const pair<SCM, int>& x, const pair<SCM, int>& y) const {
    return (bool) (x.second == y.second) && scm_is_eq(x.first, y.first);
  }
};

// hash function borrowed from boost, according to
// http://stackoverflow.com/questions/738054/hash-function-for-a-pair-of-long-long
struct hash_pair_scm_int : unary_function<pair<SCM, int>, size_t> {
  size_t operator()(const pair<SCM, int>& p) const {
    hash<SCM> first_hash;
    hash<int> second_hash;
    size_t seed = second_hash(p.second);
    return first_hash(p.first) + 0x9e3779b9 + (seed<<6) + (seed>>2);
  }
};

#define MAX_CONTACTS 12

enum {
  SIM = 0,
  RIG = 1,
  BODY = 2,
  JOINT = 3,

  NTYPES = 4
};

struct body_t;
struct rig_t;
struct sim_t;

typedef struct body_t {
  rig_t *parent;
  int id;
  dBodyID body;
  dGeomID geom;
} body_t;

typedef unordered_map<SCM, int, hash<SCM>, scm_eq> 
  symbol_index_map_t;

typedef struct rig_t {
  sim_t *parent;
  symbol_index_map_t id;
  vector<body_t *> bodies;
  vector<dJointID> joints;
  dSpaceID space;
  dJointGroupID group;
} rig_t;

typedef struct sim_t {
  dWorldID world;
  dSpaceID space;
  list<rig_t *> rigs;
  dJointGroupID contact_group;
  list<dJointID> contacts;
  dReal dt;
  //SCM defs;
} sim_t;

typedef unordered_map <SCM, body_t *(*)(sim_t *, rig_t *), hash<SCM>, scm_eq>
  body_maker_map_t;

typedef unordered_map <pair<SCM, int>, void (*)(body_t *, SCM), 
  hash_pair_scm_int, pair_scm_int_eq> body_property_setter_map_t;

typedef unordered_map <pair<SCM, int>, SCM (*)(body_t *), 
  hash_pair_scm_int, pair_scm_int_eq> body_property_getter_map_t;

typedef unordered_map <SCM, dJointID (*)(sim_t *, rig_t *), hash<SCM>, scm_eq>
  joint_maker_map_t;

typedef unordered_map <pair<SCM, int>, void (*)(dJointID, SCM),
  hash_pair_scm_int, pair_scm_int_eq> joint_property_setter_map_t;

typedef unordered_map <pair<SCM, int>, SCM (*)(dJointID),
  hash_pair_scm_int, pair_scm_int_eq> joint_property_getter_map_t;


#define MDEF_CONDITIONAL_ASSIGN(TYPE, scm_var, c_type, c_var, d_val)	\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(SCM_SMOB_FLAGS(scm_var) != TYPE) {					\
    WARN("FUNCTION CALLED ON A NON-" # TYPE);				\
    return d_val;							\
  }									\
  c_type c_var = (c_type) SCM_SMOB_DATA(scm_var)

#define SIM_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)		\
  MDEF_CONDITIONAL_ASSIGN(SIM, scm_var, sim_t *, c_var, d_val)

#define RIG_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)		\
  MDEF_CONDITIONAL_ASSIGN(RIG, scm_var, rig_t *, c_var, d_val)

#define BODY_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(BODY, scm_var, body_t *, c_var, d_val)

#define JOINT_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(JOINT, scm_var, dJointID, c_var, d_val)


#define SET_SMOB_TYPE(type, smob, c_var)	\
  SCM_NEWSMOB(smob, ode_tag, c_var);		\
  SCM_SET_SMOB_FLAGS(smob, type)

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

#endif // _PHYSICS_H 
