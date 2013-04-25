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

#include "body.cc"
#include "joint.cc"
#include "rig.cc"
#include "sim.cc"

static void
export_symbols(void *unused) {
#define DEFINE_PROC(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);
  
#define EXPORT_PROC(name, required, optional, rest, proc)	\
  DEFINE_PROC(name,required,optional,rest,proc);		\
  scm_c_export(name,NULL);
  
  EXPORT_SIM_PROCEDURES;
  EXPORT_RIG_PROCEDURES;
  EXPORT_BODY_PROCEDURES;
  EXPORT_JOINT_PROCEDURES;

#undef EXPORT_PROC
#undef DEFINE_PROC
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

  INIT_SIM_MODULE;
  INIT_RIG_MODULE;
  INIT_BODY_MODULE;
  INIT_JOINT_MODULE;

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
