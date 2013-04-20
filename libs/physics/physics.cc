#include "physics.h"
// primitive C functions (to be implemented!)
// (make-simulation)
// (make-rig simulation)
// (make-body sim rig type name) ??

// (set-body-property! body (name <string>) value)
// (body-property body property-name)
// (make-joint type)
// (body-named- name rig)
// (set-joint-property! joint (property-name <string>) value)
// (joint-property property-name joint)

// MODULE'S GLOBALS
scm_t_bits ode_tag;
SCM s_f32;
SCM s_f64;
body_maker_map_t body_maker;
body_property_setter_map_t body_property_setter;
body_property_getter_map_t body_property_getter;

// based on ode/collision.h enum, lines 880-902
char const *class_name[] = {
  "sphere", "box", "capsule", "cylinder", "plane",
  "ray", "convex", "transform", "trimesh", "height-field",
  "simple-space", "hash-space", "sweep-n-prune-space",
  "quad-tree-space", "user-class-1", "user-class-2",
  "user-class-3", "user-class-4"
};

#include "body.cc"

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
make_simulation_() {
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

  EXPORT_PROCEDURE("make-simulation-", 0, 0, 0, make_simulation_);
  EXPORT_PROCEDURE("make-rig", 1, 0, 0, make_rig);
  EXPORT_PROCEDURE("simulation-step!", 1, 0, 0, simulation_step);

  EXPORT_PROCEDURE("make-body", 2, 2, 0, make_body);

  EXPORT_PROCEDURE("set-body-property!", 3, 0, 0, set_body_property_x);
  EXPORT_PROCEDURE("body-property", 2, 0, 0, body_property);
  
  //EXPORT_PROCEDURE("make-rig", 0, 0, 0, scm_make_rig);

#undef EXPORT_PROCEDURE
#undef DEFINE_PROCEDURE
}

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {

  scm_assert_smob_type(ode_tag, ode);
  int type = SCM_SMOB_FLAGS(ode);
  char *string;

#define PRINT(msg, ...)					\
  if(asprintf(&string, msg, ## __VA_ARGS__) == -1)	\
    return 0;						\
  scm_puts(string, port);				\
  free(string)

  if(type == SIM) {
    sim_t *sim = (sim_t *) SCM_SMOB_DATA(ode);
    PRINT("#<simulation %p (dt %.2f)>", (void *) sim, sim->dt);
  }
  else if(type == RIG) {
    rig_t *rig = (rig_t *) SCM_SMOB_DATA(ode);
    PRINT("#<rig %p (%d bodies) (%d joints)>", (void *) rig, 
	  (int) rig->bodies.size(), (int) rig->joints.size());
  }
  else if(type == BODY) {
    body_t *body = (body_t *) SCM_SMOB_DATA(ode);
    dReal mass = INFINITY;
    if(body->body) {
      dMass m;
      dBodyGetMass(body->body, &m);
      mass = m.mass;
    }
    PRINT("#<body %p %s %.2f>", (void *) body, 
	  class_name[dGeomGetClass(body->geom)], mass);
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
  init_body_property_accessors();

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
