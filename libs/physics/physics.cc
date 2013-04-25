#include "physics.h"
// primitive C functions (to be implemented!)
// (make-simulation)
// (make-rig simulation)
// (make-body sim rig type name) ??
// (set-body-property! body (name <string>) value)
// (body-property body property-name)

// (make-joint sim rig type)
// (body-named- name rig)
// (set-joint-property! joint (property-name <string>) value)
// (joint-property property-name joint)

scm_t_bits ode_tag;
SCM s_f32;
SCM s_f64;

// based on src/ode/collision.h enum, lines 880-902
static char const *class_name[] = {
  "sphere", "box", "capsule", "cylinder", "plane",
  "ray", "convex", "transform", "trimesh", "height-field",
  "simple-space", "hash-space", "sweep-n-prune-space",
  "quad-tree-space", "user-class-1", "user-class-2",
  "user-class-3", "user-class-4"
};

// based on include/ode/common.h dJointType enum, lines 251-267
static char const *joint_type_name[] = {
  "unknown", "ball", "hinge", "slider", "contact",
  "universal", "hinge-2", "fixed", "null", "angular-motor",
  "linear-motor", "plane-2d", "PR", "PU", "piston"
};

#include "body.cc"
#include "joint.cc"

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
primitive_make_simulation() {
  sim_t *sim = new sim_t;
  sim->world = dWorldCreate();
  sim->space = dHashSpaceCreate(0);
  sim->contact_group = dJointGroupCreate(0);
  sim->dt = 0.01;  
  return sim_to_smob(sim);
}

static SCM
primitive_make_rig(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  rig_t *rig = new rig_t;
  rig->space = dSimpleSpaceCreate(sim->space);
  rig->parent = sim;
  sim->rigs.push_back(rig);
  rig->group = dJointGroupCreate(0);
  scm_remember_upto_here_1(x_sim);
  return rig_to_smob(rig);
}


static SCM
simulation_rigs(SCM x_sim) {
  SCM result = SCM_EOL;
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, result);
  list<rig_t *>::iterator rig;
  for(rig = sim->rigs.begin(); rig != sim->rigs.end(); ++rig) {
    result = scm_cons(rig_to_smob(*rig), result);
  }

  scm_remember_upto_here_1(x_sim);
  return result;
}

static SCM
rig_bodies(SCM x_rig) {
  SCM result = SCM_EOL;
  RIG_CONDITIONAL_ASSIGN(x_rig, rig, result);
  vector<body_t *>::iterator body;
  for(body = rig->bodies.begin(); body != rig->bodies.end(); ++body) {
    result = scm_cons(body_to_smob(*body), result);
  }

  scm_remember_upto_here_1(x_rig);
  return result;
}

static SCM
simulation_step(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  dJointGroupEmpty(sim->contact_group);
  dSpaceCollide(sim->space, sim, &on_potential_collision);
  dWorldStep(sim->world, sim->dt);
  return SCM_UNSPECIFIED;
}

static SCM
set_simulation_rig_maker_x(SCM x_sim, SCM s_rig_name, SCM f_rig_maker) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_rig_name, 2);
  ASSERT_SCM_TYPE(procedure, f_rig_maker, 3);
  general_scm_map_t::iterator rig_def = sim->rig_defs.find(s_rig_name);
  if(rig_def == sim->rig_defs.end()) {
    scm_gc_protect_object(s_rig_name);
  }
  else {
    scm_gc_unprotect_object(rig_def->second);
  }
  sim->rig_defs[s_rig_name] = gc_protected(f_rig_maker);
  
  return SCM_UNSPECIFIED;
}

static SCM
simulation_rig_maker(SCM x_sim, SCM s_rig_name) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_rig_name, 2);
  general_scm_map_t::iterator rig_def = sim->rig_defs.find(s_rig_name);
  if(rig_def == sim->rig_defs.end()) {
    char *name = as_c_string(s_rig_name);
    WARN("undefined rig: %s", name);
    free(name);
    return SCM_BOOL_F;
  }
  return rig_def->second;
}

static void
export_symbols(void *unused) {
#define DEFINE_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);

#define EXPORT_PROCEDURE(name, required, optional, rest, proc)	\
  DEFINE_PROCEDURE(name,required,optional,rest,proc);		\
  scm_c_export(name,NULL);

  DEFINE_PROCEDURE("primitive-make-simulation", 0, 0, 0, 
		   primitive_make_simulation);
  DEFINE_PROCEDURE("primitive-make-rig", 1, 0, 0, primitive_make_rig);
  EXPORT_PROCEDURE("simulation-step!", 1, 0, 0, simulation_step);
  EXPORT_PROCEDURE("set-simulation-rig-maker!", 3, 0, 0, 
		   set_simulation_rig_maker_x);
  EXPORT_PROCEDURE("simulation-rig-maker", 2, 0, 0, simulation_rig_maker);

  EXPORT_PROCEDURE("make-body", 3, 0, 0, make_body);
  EXPORT_PROCEDURE("set-body-property!", 2, 1, 0, set_body_property_x);
  EXPORT_PROCEDURE("body-property", 2, 0, 0, body_property);
  EXPORT_PROCEDURE("body-type", 1, 0, 0, body_type);
  DEFINE_PROCEDURE("body-named", 2, 0, 0, body_named);

  EXPORT_PROCEDURE("make-joint", 2, 0, 0, make_joint);
  EXPORT_PROCEDURE("set-joint-property!", 3, 0, 0, set_joint_property_x);
  EXPORT_PROCEDURE("joint-property", 2, 0, 0, joint_property);
  EXPORT_PROCEDURE("joint-type", 1, 0, 0, joint_type);

  EXPORT_PROCEDURE("rig-bodies", 1, 0, 0, rig_bodies);
  EXPORT_PROCEDURE("simulation-rigs", 1, 0, 0, simulation_rigs);

#undef EXPORT_PROCEDURE
#undef DEFINE_PROCEDURE
}

static SCM
mark_ode(SCM ode_smob) {
  if(SCM_SMOB_FLAGS(ode_smob) == SIM) {
    sim_t *sim = (sim_t *) SCM_SMOB_DATA(ode_smob);
    general_scm_map_t::iterator r;
    for(r = sim->rig_defs.begin(); r != sim->rig_defs.end(); ++r) {
      scm_gc_mark(r->first);
      scm_gc_mark(r->second);
    }
  }
  return SCM_BOOL_F;
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
    PRINT("#<simulation %p (dt %.2f) (%i rigs)>", (void *) sim, sim->dt,
	  (int) sim->rigs.size());
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
  else if(type == JOINT) {
    joint_t *joint = (joint_t *) SCM_SMOB_DATA(ode);
    PRINT("#<joint %p %s>", (void *) joint, 
	  joint_type_name[dJointGetType(joint->joint)]);
  }

  scm_remember_upto_here_1(ode);
#undef PRINT
  return 1;
}

static size_t
free_ode(SCM smob) {
  OUT("Attempting to release ode smob, but that just isn't implemented!");
  return 0;
}

extern "C" void 
init() {
  init_body_maker();
  init_body_property_accessors();
  
  init_joint_maker();
  init_joint_property_accessors();

  s_f32 = gc_protected(symbol("f32"));
  s_f64 = gc_protected(symbol("f64"));

  //scm_c_define_module("lib physics", export_symbols, NULL);
  export_symbols(NULL);
  
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);
  scm_set_smob_mark(ode_tag, mark_ode);
  
  dInitODE();
  dAllocateODEDataForThread(dAllocateMaskAll);
}
