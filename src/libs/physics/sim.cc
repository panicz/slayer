sim_property_setter_map_t sim_property_setter;
sim_property_getter_map_t sim_property_getter;

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

static void
simulation_gravity_setter(sim_t *sim, SCM value) {
  dVector3 v;
  scm_to_dVector3(value, &v);
  dWorldSetGravity(sim->world, v[0], v[1], v[2]);
}

static SCM
simulation_gravity_getter(sim_t *sim) {
  dVector3 v;
  dWorldGetGravity(sim->world, v);
  return scm_from_dVector3(v);
}

static SCM
set_simulation_property_x(SCM x_sim, SCM s_prop, SCM value) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);
  
  sim_property_setter_map_t::iterator setter 
    = sim_property_setter.find(s_prop);

  if(setter == sim_property_setter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("simulation property `%s` not implemented", prop);
    free(prop);
    return SCM_BOOL_F;
  }

  (setter->second)(sim, value);

  scm_remember_upto_here_2(s_prop, value);
  scm_remember_upto_here_1(x_sim);
  return SCM_UNSPECIFIED;
}

static SCM
simulation_property(SCM x_sim, SCM s_prop) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);

  sim_property_getter_map_t::iterator getter
    = sim_property_getter.find(s_prop);

  if(getter == sim_property_getter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("simulation property `%s` not implemented", prop);
    free(prop);
    return SCM_UNSPECIFIED;
  }
  scm_remember_upto_here_2(x_sim, s_prop);
  return (getter->second)(sim);
}

static void
init_sim_property_accessors() {
  SCM name;
#define SET_SIM_ACCESSORS(property)					\
  name = gc_protected(symbol(# property));				\
  sim_property_setter[name] = simulation_##property##_setter;		\
  sim_property_getter[name] = simulation_##property##_getter
  
  SET_SIM_ACCESSORS(gravity);
  
#undef SET_SIM_ACCESSORS  
}



// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_SIM_PROCEDURES						\
  DEFINE_PROC("primitive-make-simulation",0,0,0,primitive_make_simulation); \
  EXPORT_PROC("simulation-step!",1,0,0,simulation_step);		\
  EXPORT_PROC("set-simulation-rig-maker!",3,0,0,set_simulation_rig_maker_x); \
  EXPORT_PROC("simulation-rig-maker",2,0,0,simulation_rig_maker);	\
  EXPORT_PROC("simulation-rigs",1,0,0,simulation_rigs);			\
  EXPORT_PROC("simulation-property",2,0,0,simulation_property);		\
  EXPORT_PROC("set-simulation-property!",3,0,0,set_simulation_property_x)

#define INIT_SIM_MODULE				\
  init_sim_property_accessors()
