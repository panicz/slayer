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

// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_RIG_PROCEDURES						\
  DEFINE_PROC("primitive-make-rig", 1, 0, 0, primitive_make_rig);	\
  EXPORT_PROC("rig-bodies", 1, 0, 0, rig_bodies)
