#include <ode/ode.h>
#include <list>
#include <vector>
#include <functional>
#include <algorithm>
#include <unordered_map>
#include <tuple>
#include "utils.h"
#include "extend.h"

// primitive C functions (to be implemented!)
// (make-simulation)
// (make-rig simulation)
// (make-body sim rig type name) ??

// (set-body-property! body (name <string>) value)
// (body-property body property-name)
// (add-body! rig body #:optional name)
// (make-joint type)
// (body-named- name rig)
// (set-joint-property! joint (property-name <string>) value)
// (joint-property property-name joint)
// (add-rig! rig simulation)

using namespace std;

struct scm_eq : binary_function<SCM,SCM,bool> {
  bool operator() (const SCM& x, const SCM& y) const
  {return (bool) scm_is_eq(x,y);}
};

#define MAX_CONTACTS 12

scm_t_bits ode_tag;
static SCM s_f32;
static SCM s_f64;

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

static unordered_map<SCM, body_t *(*)(sim_t *, rig_t *)> body_maker;

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


#define DEF_MAKE_SOME_BODY(create_body, set_body, shape, Shape, ...)	\
  static body_t *							\
  make_##shape(sim_t *sim, rig_t *rig) {				\
    body_t *body = new body_t;						\
    body->body = create_body(sim->world);				\
    body->geom = dCreate##Shape(rig->space, ## __VA_ARGS__ );		\
    set_body(body->geom, body->body);					\
    rig->bodies.push_back(body);					\
    body->id = rig->bodies.size();					\
    return body;							\
  }

#define DEF_MAKE_BODY(shape, Shape, ...)	\
  DEF_MAKE_SOME_BODY(dBodyCreate, dGeomSetBody, shape, Shape, ## __VA_ARGS__)

DEF_MAKE_SOME_BODY(ZILCH, DONT, plane, Plane, 0, 0, 1, 0);
DEF_MAKE_BODY(cylinder, Cylinder, 0.5, 1.0);
DEF_MAKE_BODY(box, Box, 1, 1, 1);

#undef DEF_MAKE_BODY

#define SET_BODY_MAKER(shape)					\
  body_maker[gc_protected(symbol(# shape))] = make_##shape
 
static void
init_body_maker() {

  SET_BODY_MAKER(box);
  SET_BODY_MAKER(cylinder);
  SET_BODY_MAKER(plane);



}

static SCM
make_body(SCM x_sim, SCM x_rig, SCM s_type, SCM s_name){
  SIM_CONDITIONAL_ASSIGN(x_sim, sim);
  RIG_CONDITIONAL_ASSIGN(x_rig, rig);
  body_t *body;
  SCM smob = SCM_UNSPECIFIED;
  if(!GIVEN(s_type)) {
    WARN("default argument not supported (yet)");
  }
  ASSERT_SCM_TYPE(symbol, s_type, 3);

  unordered_map<SCM, body_t *(*)(sim_t *, rig_t *)>::iterator maker
    = body_maker.find(s_type);
  
  if(maker == body_maker.end()) {
    WARN("body type not implemented");
    goto end;
  }
  
  body = (maker->second)(sim, rig);
  body->parent = rig;
  SET_SMOB_TYPE(BODY, smob, body);

  if(!GIVEN(s_type)) {
    goto end;
  }
  
  ASSERT_SCM_TYPE(symbol, s_name, 4);
  rig->id[gc_protected(s_name)] = body->id;

 end:
  scm_remember_upto_here_2(x_sim, x_rig);
  scm_remember_upto_here_2(s_type, s_name);
  return smob;
}

static void 
on_potential_collision(void *s, dGeomID a, dGeomID b) {
  sim_t *sim = (sim_t *) s;
  dContact c[MAX_CONTACTS];
  int i, n = dCollide(a, b, MAX_CONTACTS, &c[0].geom, sizeof(dContact));
  for(i = 0; i < n; ++i) {
    dJointID r = dJointCreateContact(sim->world, sim->contact_group, &c[i]);
    dJointAttach(r, dGeomGetBody(c[i].geom.g1), dGeomGetBody(c[i].geom.g2));
    sim->contacts.push_back(r);
  }
}

static SCM
make_simulation() {
  SCM smob;
  sim_t *sim = new sim_t;
  sim->world = dWorldCreate();
  sim->space = dHashSpaceCreate(0);
  sim->contact_group = dJointGroupCreate(0);
  sim->dt = 0.01;  
  SET_SMOB_TYPE(SIM, smob, sim);
  return smob;
}

static SCM
make_rig(SCM x_sim) {
  SCM smob;
  SIM_CONDITIONAL_ASSIGN(x_sim, sim);
  rig_t *rig = new rig_t;
  rig->space = dSimpleSpaceCreate(sim->space);
  rig->parent = sim;
  SET_SMOB_TYPE(RIG, smob, rig);
  scm_remember_upto_here_1(x_sim);
  return smob;
}

static SCM
simulation_step(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim);
  dJointGroupEmpty(sim->contact_group);
  dSpaceCollide(sim->space, sim, &on_potential_collision);
  dWorldStep(sim->world, sim->dt);
  return SCM_UNSPECIFIED;
}

static void
export_symbols(void *unused) {
#define DEFINE_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);

#define EXPORT_PROCEDURE(name, required, optional, rest, proc)	\
  DEFINE_PROCEDURE(name,required,optional,rest,proc);		\
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("make-simulation-", 0, 0, 0, make_simulation);
  EXPORT_PROCEDURE("make-rig", 1, 0, 0, make_rig);
  EXPORT_PROCEDURE("simulation-step!", 1, 0, 0, simulation_step);

  EXPORT_PROCEDURE("make-body", 2, 2, 0, make_body);
  
  //EXPORT_PROCEDURE("make-rig", 0, 0, 0, scm_make_rig);

#undef EXPORT_PROCEDURE
#undef DEFINE_PROCEDURE
}

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {

  scm_assert_smob_type(ode_tag, ode);
  int type = SCM_SMOB_FLAGS(ode);
  char *string;

#define PRINT(msg, ...)				\
  if(asprintf(&string, msg, ## __VA_ARGS__) == -1)	\
    return 0;						\
  scm_puts(string, port);				\
  free(string)

  if(type == SIM) {
    sim_t *sim = (sim_t *) SCM_SMOB_DATA(ode);
    PRINT("#<simulation %p (dt %f)>", (void *) sim, sim->dt);
  }
  else if(type == RIG) {
    rig_t *rig = (rig_t *) SCM_SMOB_DATA(ode);
    PRINT("#<rig %p (%d bodies) (%d joints)>", (void *) rig, 
	  (int) rig->bodies.size(), (int) rig->joints.size());
  }
  else if(type == BODY) {
    body_t *body = (body_t *) SCM_SMOB_DATA(ode);
    PRINT("#<body %p shape mass>", (void *) body);
  }




  scm_remember_upto_here_1(ode);
#undef PRINT
  return 1;
}


static size_t
free_ode(SCM smob) {
  OUT("Attempting to release smob, but that just isn't implemented!");
  return 0;
}


extern "C" void 
init() {
  init_body_maker();

  s_f32 = gc_protected(symbol("f32"));
  s_f64 = gc_protected(symbol("f64"));

  //scm_c_define_module("lib physics", export_symbols, NULL);
  export_symbols(NULL);
  
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);

  dInitODE();
  dAllocateODEDataForThread(dAllocateMaskAll);
}
