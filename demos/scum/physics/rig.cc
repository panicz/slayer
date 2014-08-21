static SCM
primitive_make_rig(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  rig_t *rig = new rig_t;
  rig->space = dSimpleSpaceCreate(sim->space);
  rig->parent = sim;
  sim->rigs.push_back(rig);
  rig->group = dJointGroupCreate(0);
  rig->self_smob = gc_protected(rig_to_smob(rig));
  scm_remember_upto_here_1(x_sim);
  return rig->self_smob;
}

#define DEF_RIG_GETTER(type, plural)				\
  static SCM							\
  rig_##plural(SCM x_rig) {					\
    SCM result = SCM_EOL;					\
    RIG_CONDITIONAL_ASSIGN(x_rig, rig, result);			\
    vector<type##_t *>::iterator i;				\
    for(i = rig->plural.begin(); i != rig->plural.end(); ++i) {	\
      result = scm_cons((*i)->self_smob, result);		\
    }								\
    scm_remember_upto_here_1(x_rig);				\
    return result;						\
  }

DEF_RIG_GETTER(body, bodies); // rig_bodies
DEF_RIG_GETTER(joint, joints); // rig_joints

#undef DEF_RIG_GETTER

static SCM
rig_p(SCM smob) {
  return (SCM_SMOB_PREDICATE(ode_tag, smob) && (SCM_SMOB_FLAGS(smob) != RIG))
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_RIG_PROCEDURES						\
  DEFINE_PROC("primitive-make-rig", 1, 0, 0, primitive_make_rig);	\
  EXPORT_PROC("rig?", 1, 0, 0, rig_p)					\
  EXPORT_PROC("rig-bodies", 1, 0, 0, rig_bodies)			\
  EXPORT_PROC("rig-joints", 1, 0, 0, rig_joints)

#define INIT_RIG_MODULE do {} while(0)
