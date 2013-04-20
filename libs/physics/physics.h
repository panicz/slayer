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

typedef struct rig_t {
  sim_t *parent;
  unordered_map<SCM, int, hash<SCM>, scm_eq> id;
  vector<body_t *> bodies;
  vector<dJointID> joints;
  dSpaceID space;
} rig_t;

typedef struct sim_t {
  dWorldID world;
  dSpaceID space;
  list<rig_t *> rigs;
  dJointGroupID contact_group;
  list<dJointID> contacts;
  dReal dt;
} sim_t;

typedef unordered_map <SCM, body_t *(*)(sim_t *, rig_t *), hash<SCM>, scm_eq>
body_maker_map_t;

typedef unordered_map <pair<SCM, int>, void (*)(body_t *, SCM), 
			 hash<pair<SCM,int> >, pair_scm_int_eq>
body_property_assigner_map_t;


#define MDEF_CONDITIONAL_ASSIGN(TYPE, scm_var, c_type, c_var)		\
  scm_assert_smob_type(ode_tag, scm_var);				\
  if(SCM_SMOB_FLAGS(scm_var) != TYPE) {					\
    WARN("FUNCTION CALLED ON A NON-" # TYPE);				\
    return SCM_BOOL_F;							\
  }									\
  c_type c_var = (c_type) SCM_SMOB_DATA(scm_var)

#define SIM_CONDITIONAL_ASSIGN(scm_var, c_var)		\
  MDEF_CONDITIONAL_ASSIGN(SIM, scm_var, sim_t *, c_var)

#define RIG_CONDITIONAL_ASSIGN(scm_var, c_var)		\
  MDEF_CONDITIONAL_ASSIGN(RIG, scm_var, rig_t *, c_var)

#define BODY_CONDITIONAL_ASSIGN(scm_var, c_var)		\
  MDEF_CONDITIONAL_ASSIGN(BODY, scm_var, body_t *, c_var)

#define SET_SMOB_TYPE(type, smob, c_var)	\
  SCM_NEWSMOB(smob, ode_tag, c_var);		\
  SCM_SET_SMOB_FLAGS(smob, type)


#endif // _PHYSICS_H 
