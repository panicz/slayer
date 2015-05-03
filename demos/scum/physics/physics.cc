// these need to be defined before including "physics.h"
#include "extend.h"
static scm_t_bits ode_tag;
static SCM s_f32;
static SCM s_f64;

static SCM s_u8;
static SCM s_s8;
static SCM s_u16;
static SCM s_s16;
static SCM s_u32;
static SCM s_s32;
static SCM s_u64;
static SCM s_s64;

#include "physics.hh"

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
  if(dGeomIsSpace(a) || dGeomIsSpace(b)) {
    dSpaceCollide2(a, b, s, &on_potential_collision);
    if(dGeomIsSpace(a)) {
      dSpaceCollide((dSpaceID) a, s, &on_potential_collision);
    }
    if(dGeomIsSpace(b)) {
      dSpaceCollide((dSpaceID) b, s, &on_potential_collision);
    }
  }
  else {
    if(dGeomGetBody(a) && dGeomGetBody(b)
       && dAreConnectedExcluding(dGeomGetBody(a), dGeomGetBody(b), 
				 dJointTypeContact)) { 
      return; 
    }

    sim_t *sim = (sim_t *) s;

    dContact c[MAX_CONTACTS];
    int i, n = dCollide(a, b, MAX_CONTACTS ,//| CONTACTS_UNIMPORTANT,
			&c[0].geom, sizeof(dContact));

    if(n <= 0) {
      return;
    }
    
    vector<dContact> *contacts = new vector<dContact>(n);

    for(i = 0; i < n; ++i) {
      c[i].surface = sim->default_contact_parameters;
      (*contacts)[i] = c[i];
      dJointID r = dJointCreateContact(sim->world, sim->contact_group, &c[i]);
      dJointAttach(r, dGeomGetBody(c[i].geom.g1), dGeomGetBody(c[i].geom.g2));
    }

    dGeom_body_map_t::iterator dGeom_body1 = sim->dGeom_body.find(a);
    if((dGeom_body1 != sim->dGeom_body.end())) {
      dGeom_body_map_t::iterator dGeom_body2 = sim->dGeom_body.find(b);
      if((dGeom_body2 != sim->dGeom_body.end())) {
	body_t *body1 = dGeom_body1->second;
	body_t *body2 = dGeom_body2->second;
	sim->body_contacts[body1]->push_back(contacts);
	sim->body_contacts[body2]->push_back(contacts);
      }
    }
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

/* this causes core dumps
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
*/

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {
  scm_assert_smob_type(ode_tag, ode);

  static char *noname = (char *) "???";
  char *name = NULL;

#define ASSIGN_STRING_OR_NONAME(v, s) v=as_c_string(s); if(!v) v=noname
#define FREE_IF_NOT_NULL_OR_NONAME(v) if(v && v != noname) free(v)

  int type = SCM_SMOB_FLAGS(ode);

  if(type == SIM) {
    sim_t *sim = (sim_t *) SCM_SMOB_DATA(ode);
    DISPLAY(port, "#<simulation %p (dt %.2f) (%i rigs)>", (void *) sim, sim->dt,
	    (int) sim->rigs.size());
  }

  else if(type == RIG) {
    rig_t *rig = (rig_t *) SCM_SMOB_DATA(ode);
    ASSIGN_STRING_OR_NONAME(name, rig->name);
    DISPLAY(port, "#<rig %p %s (%d bodies) (%d joints)>", (void *) rig,
	    name, (int) rig->bodies.size(), (int) rig->joints.size());
    FREE_IF_NOT_NULL_OR_NONAME(name);
  }

  else if(type == BODY) {
    body_t *body = (body_t *) SCM_SMOB_DATA(ode);
    dReal mass = INFINITY;
    if(body->body) {
      dMass m;
      dBodyGetMass(body->body, &m);
      mass = m.mass;
    }
    ASSIGN_STRING_OR_NONAME(name, body->name);
    DISPLAY(port, "#<body %p %s %s %.2f>", (void *) body, 
	    class_name[dGeomGetClass(body->geom)], name, mass);
    FREE_IF_NOT_NULL_OR_NONAME(name);
  }

  else if(type == JOINT) {
    joint_t *joint = (joint_t *) SCM_SMOB_DATA(ode);
    ASSIGN_STRING_OR_NONAME(name, joint->name);
    DISPLAY(port, "#<joint %p %s %s>", (void *) joint, 
	    joint_type_name[dJointGetType(joint->joint)], name);
    FREE_IF_NOT_NULL_OR_NONAME(name);
  }

  else {
    DISPLAY(port, "#<ode (unknown) %p %i>", (void *) SCM_SMOB_DATA(ode), 
	    (int) SCM_SMOB_FLAGS(ode));
  }

#undef FREE_IF_NOT_NULL_OR_NONAME
#undef ASSIGN_STRING_OR_NONAME
  scm_remember_upto_here_1(ode);
  return 1;
}

static size_t
free_ode(SCM smob) {
  SCM scmerr = scm_current_error_port();
  scm_puts("#;(Attempting to release ODE smob ", scmerr);
  scm_display(smob, scmerr);
  scm_puts(" but that just isn't implemented)", scmerr);
  return 0;
}

extern "C" void 
init() {

  INIT_SIM_MODULE;
  INIT_RIG_MODULE;
  INIT_BODY_MODULE;
  INIT_JOINT_MODULE;

#define INIT_SYMBOL(sym) s_##sym = gc_protected(symbol(# sym))

  INIT_SYMBOL(f32);
  INIT_SYMBOL(f64);
  INIT_SYMBOL(u8);
  INIT_SYMBOL(s8);
  INIT_SYMBOL(u16);
  INIT_SYMBOL(s16);
  INIT_SYMBOL(u32);
  INIT_SYMBOL(s32);
  INIT_SYMBOL(u64);
  INIT_SYMBOL(s64);

#undef INIT_SYMBOL

  //scm_c_define_module("lib physics", export_symbols, NULL);
  export_symbols(NULL);
  
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);
  //scm_set_smob_mark(ode_tag, mark_ode);
  
  dInitODE();
  dAllocateODEDataForThread(dAllocateMaskAll);
}
