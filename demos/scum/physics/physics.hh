#ifndef _PHYSICS_HH
#define _PHYSICS_HH

#include <ode/ode.h>
#include <list>
#include <vector>
#include <functional>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <type_traits>
#include <tuple>
#include <cmath>
#include "utils.h"
#include "vmx.hh"

using namespace std;

// the following symbols must be defined in a file that includes this one:
// scm_t_bits ode_tag;
// SCM s_f32;
// SCM s_f64;

struct scm_eq : binary_function<SCM, SCM, bool> {
  bool operator() (const SCM& x, const SCM& y) const {
    return (bool) scm_is_eq(x,y);
  }
};

struct pair_scm_int_eq : binary_function<pair<SCM,int>,pair<SCM, int>,bool> {
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
  CONTACT = 4,

  NTYPES = 5
};

struct body_t;
struct rig_t;
struct sim_t;
struct joint_t;

typedef struct joint_t {
  SCM self_smob;
  SCM name;
  struct rig_t *parent;
  dJointID joint;
  int id;
  int body1_id;
  int body2_id;
} joint_t;

typedef struct body_t {
  SCM self_smob;
  SCM name;
  struct rig_t *parent;
  int id;
  dBodyID body;
  dGeomID geom;
  union {
    struct {
      SCM vertices;
      SCM indices;
    } trimesh;
  } data;
} body_t;

typedef unordered_map<SCM, int, hash<SCM>, scm_eq> symbol_index_map_t;

typedef unordered_map<dGeomID, body_t *, hash<dGeomID> > dGeom_body_map_t;
typedef unordered_map<dBodyID, body_t *, hash<dBodyID> > dBody_body_map_t;

typedef unordered_map<body_t *, const char *, hash<body_t *> > body_name_map_t;
typedef unordered_map<joint_t *, const char *, hash<joint_t *> > joint_name_map_t;

typedef unordered_map<body_t *, list<vector<dContact> *> *> body_contacts_map_t;

//typedef unordered_map <SCM, SCM, hash<SCM>, scm_eq> general_scm_map_t;

typedef struct rig_t {
  SCM self_smob;
  SCM name;
  struct sim_t *parent;
  vector<body_t *> bodies;
  vector<joint_t *> joints;
  dSpaceID space;
  dJointGroupID group;
} rig_t;

typedef struct sim_t {
  SCM self_smob;
  dWorldID world;
  dSpaceID space;
  list<rig_t *> rigs;
  dJointGroupID contact_group;
  dGeom_body_map_t dGeom_body;
  dBody_body_map_t dBody_body;
  body_name_map_t body_name;
  joint_name_map_t joint_name;
  body_contacts_map_t body_contacts;
  dReal dt;
  int step;
  struct dSurfaceParameters default_contact_parameters;
} sim_t;

typedef unordered_map <SCM, body_t *(*)(rig_t *), hash<SCM>, scm_eq>
  body_maker_map_t;

typedef unordered_map <pair<SCM, int>, void (*)(body_t *, SCM), 
  hash_pair_scm_int, pair_scm_int_eq> body_property_setter_map_t;

typedef unordered_map <pair<SCM, int>, SCM (*)(body_t *), 
  hash_pair_scm_int, pair_scm_int_eq> body_property_getter_map_t;

typedef unordered_map <SCM, dJointID(*)(rig_t *), hash<SCM>, scm_eq>
  joint_maker_map_t;

typedef unordered_map <pair<SCM, int>, void (*)(joint_t *, SCM),
  hash_pair_scm_int, pair_scm_int_eq> joint_property_setter_map_t;

typedef unordered_map <pair<SCM, int>, SCM (*)(joint_t *),
  hash_pair_scm_int, pair_scm_int_eq > joint_property_getter_map_t;

typedef unordered_map<SCM, void (*)(sim_t *, SCM), hash<SCM>, scm_eq>
  sim_property_setter_map_t;

typedef unordered_map<SCM, SCM (*)(sim_t *), hash<SCM>, scm_eq>
  sim_property_getter_map_t;

#define MDEF_CONDITIONAL_ASSIGN(TYPE, tag, scm_var, c_type, c_var, d_val) \
  scm_assert_smob_type(tag, scm_var);					\
  if(SCM_SMOB_FLAGS(scm_var) != TYPE) {					\
    WARN("FUNCTION CALLED ON A NON-" # TYPE);				\
    return d_val;							\
  }									\
  c_type c_var = (c_type) SCM_SMOB_DATA(scm_var)

#define SIM_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(SIM, ode_tag, scm_var, sim_t *, c_var, d_val)

#define RIG_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(RIG, ode_tag, scm_var, rig_t *, c_var, d_val)

#define BODY_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(BODY, ode_tag, scm_var, body_t *, c_var, d_val)

#define JOINT_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(JOINT, ode_tag, scm_var, joint_t *, c_var, d_val)

#define SET_SMOB_TYPE(type, smob, c_var)	\
  SCM_NEWSMOB(smob, ode_tag, c_var);		\
  SCM_SET_SMOB_FLAGS(smob, type)

#define DEF_TYPE_TO_SMOB(TYPE, type)		\
  static inline SCM				\
  type##_to_smob(type##_t *object) {		\
    SCM smob;					\
    SET_SMOB_TYPE(TYPE, smob, object);		\
    return smob;				\
  }

DEF_TYPE_TO_SMOB(BODY, body); // body_to_smob
DEF_TYPE_TO_SMOB(RIG, rig); // rig_to_smob
DEF_TYPE_TO_SMOB(SIM, sim); // sim_to_smob
DEF_TYPE_TO_SMOB(JOINT, joint); // joint_to_smob

#undef DEF_TYPE_TO_SMOB

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

#undef DEF_SCM_FROM_DVECTOR

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
#undef DEF_SCM_TO_DVECTOR

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
  scm_from_dMatrix##n(const dMatrix##n m) {			\
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

#define DEF_SCM_TO_DMATRIX(n)					\
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
	    (*m)[n*i+j] = (dReal) elements[n*i+j];		\
	  }							\
	}							\
      }								\
      else if(scm_is_typed_array(M, s_f64)) {			\
	double const *elements					\
	  = scm_array_handle_f64_elements(&h);			\
	for(int i = 0; i < n; ++i) {				\
	  for(int j = 0; j < n; ++j) {				\
	    (*m)[n*i+j] = (dReal) elements[n*i+j];		\
	  }							\
	}							\
      }								\
      else {							\
	SCM *elements = (SCM *) scm_array_handle_elements(&h);	\
	for(int i = 0; i < n; ++i) {				\
	  for(int j = 0; j < n; ++j) {				\
	    (*m)[n*i+j]						\
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

#undef DEF_SCM_TO_DMATRIX

static inline void
scm_to_dQuaternion(SCM Q, dQuaternion *q) {
  if (scm_is_array(Q)) {
    scm_to_dVector4(Q, ((dVector4 *) q));
    return;
  }
  if (scm_is_pair(Q)) {
    scm_to_dVector3(scm_cdr(Q), ((dVector3 *) &((*q)[1])));
    ((dReal *) q)[0] = (dReal) scm_to_double(scm_car(Q));
    return;
  }
  WARN("Conversion to quaternion failed");
}

static inline SCM
scm_from_dQuaternion(const dQuaternion q) {
  SCM Q = scm_make_typed_array(ARRAY_TYPE, SCM_UNSPECIFIED,
			       scm_list_1(scm_from_int(3)));
  scm_t_array_handle h;
  scm_array_get_handle(Q, &h);
  dReal *elements
    = (dReal *) scm_array_handle_uniform_writable_elements(&h);
  for(int i = 0; i < 3; ++i) {
    elements[i] = q[i+1];
  }
  scm_array_handle_release(&h);
  return scm_cons(scm_from_double((double) q[0]), Q);
}

static inline void
scm_to_dReal(SCM S, dReal *s) {
  *s = scm_to_double(S);
}

static inline SCM
scm_from_dReal(dReal d) {
  return scm_from_double(d);
}

#if defined(dSINGLE)
typedef qtf qtReal;
typedef v3f v3Real;
#else
typedef qtd qtReal;
typedef v3d v3Real;
#endif

#endif // _PHYSICS_HH
