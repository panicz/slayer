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


// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_SIM_PROCEDURES						\
  DEFINE_PROC("primitive-make-simulation",0,0,0,primitive_make_simulation); \
  EXPORT_PROC("simulation-step!",1,0,0,simulation_step);		\
  EXPORT_PROC("set-simulation-rig-maker!",3,0,0,set_simulation_rig_maker_x); \
  EXPORT_PROC("simulation-rig-maker",2,0,0,simulation_rig_maker);	\
  EXPORT_PROC("simulation-rigs",1,0,0,simulation_rigs)
