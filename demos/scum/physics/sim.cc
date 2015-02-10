sim_property_setter_map_t sim_property_setter;
sim_property_getter_map_t sim_property_getter;

static SCM
primitive_make_simulation() {
  sim_t *sim = new sim_t;
  sim->world = dWorldCreate();
  sim->space = dHashSpaceCreate(0);
  sim->contact_group = dJointGroupCreate(0);
  sim->dt = 0.05;
  sim->step = 0;
  sim->self_smob = gc_protected(sim_to_smob(sim));
  struct dSurfaceParameters c;
  c.mode = dContactSlip1 | dContactSlip2 | dContactSoftERP 
    | dContactSoftCFM | dContactApprox1 | dContactBounce;
  c.mu = dInfinity;
  c.mu2 = dInfinity;
  c.bounce = 0;
  c.soft_erp = 0.8;
  c.soft_cfm = 1.0;
  c.slip1 = 0.1;
  c.slip2 = 0.1;
  sim->default_contact_parameters = c;

  return sim->self_smob;
}

static SCM
simulation_rigs(SCM x_sim) {
  SCM result = SCM_EOL;
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, result);
  std::list<rig_t *>::iterator rig;
  for(rig = sim->rigs.begin(); rig != sim->rigs.end(); ++rig) {
    result = scm_cons((*rig)->self_smob, result);
  }
  scm_remember_upto_here_1(x_sim);
  return result;
}

static SCM
make_simulation_step(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  dSpaceCollide(sim->space, sim, &on_potential_collision);

  std::list<rig_t *>::iterator rig;
  for(rig = sim->rigs.begin(); rig != sim->rigs.end(); ++rig) {
    dSpaceCollide((*rig)->space, sim, &on_potential_collision);
  }
  dWorldQuickStep(sim->world, sim->dt);
  dJointGroupEmpty(sim->contact_group);

  sim->step++;
  return SCM_UNSPECIFIED;
}

static SCM
current_simulation_step(SCM x_sim) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  return scm_from_int(sim->step);
}

static void
simulation_time_step_setter(sim_t *sim, SCM value) {
  sim->dt = scm_to_double(value);
}

#define DEF_SIMULATION_CONTACT_ACCESSORS(param)				\
  static void simulation_contact_##param##_setter(sim_t *sim, SCM v) {	\
    sim->default_contact_parameters.param = scm_to_double(v);		\
  }									\
  static SCM simulation_contact_##param##_getter(sim_t *sim) {		\
    return scm_from_double(sim->default_contact_parameters.param);	\
  }

DEF_SIMULATION_CONTACT_ACCESSORS(mu)
DEF_SIMULATION_CONTACT_ACCESSORS(bounce)
DEF_SIMULATION_CONTACT_ACCESSORS(slip1)
DEF_SIMULATION_CONTACT_ACCESSORS(slip2)
DEF_SIMULATION_CONTACT_ACCESSORS(soft_erp)
DEF_SIMULATION_CONTACT_ACCESSORS(soft_cfm)

#undef DEF_SIMULATION_CONTACT_ACCESSORS

static SCM
simulation_time_step_getter(sim_t *sim) {
  return scm_from_double(sim->dt);
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

#define DEF_SIMULATION_ACCESSOR(name, orig, convert, back, cast)	\
  static void								\
  simulation_##name##_setter(sim_t *sim, SCM param) {			\
    dWorldSet##orig(sim->world, (cast) convert(param));			\
  }									\
									\
  static SCM								\
  simulation_##name##_getter(sim_t *sim) {				\
    return back(dWorldGet##orig(sim->world));				\
  }

DEF_SIMULATION_ACCESSOR(erp, ERP, scm_to_double, scm_from_double, dReal);
DEF_SIMULATION_ACCESSOR(cfm, CFM, scm_to_double, scm_from_double, dReal);
DEF_SIMULATION_ACCESSOR(auto_disable_flag, AutoDisableFlag, scm_to_bool, 
			scm_from_bool, int);

DEF_SIMULATION_ACCESSOR(auto_disable_linear_threshold,
			AutoDisableLinearThreshold,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(auto_disable_angular_threshold, 
			AutoDisableAngularThreshold,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(auto_disable_steps, AutoDisableSteps,
			scm_to_int, scm_from_int, int);

DEF_SIMULATION_ACCESSOR(auto_disable_time, AutoDisableTime,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(quick_step_iterations, QuickStepNumIterations,
			scm_to_int, scm_from_int, int);

DEF_SIMULATION_ACCESSOR(quick_step_relaxation, QuickStepW,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(linear_damping, LinearDamping,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(angular_damping, AngularDamping,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(linear_damping_threshold, LinearDampingThreshold,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(angular_damping_threshold, AngularDampingThreshold,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(max_angular_speed, MaxAngularSpeed,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(contact_max_correcting_velocity, ContactMaxCorrectingVel,
			scm_to_double, scm_from_double, dReal);

DEF_SIMULATION_ACCESSOR(contact_surface_layer, ContactSurfaceLayer,
			scm_to_double, scm_from_double, dReal);

#undef DEF_SIMULATION_ACCESSOR

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
  SCM s_name;
#define SET_SIM_NAMED_ACCESSORS(property, name)				\
  s_name = gc_protected(symbol(name));					\
  sim_property_setter[s_name] = simulation_##property##_setter;		\
  sim_property_getter[s_name] = simulation_##property##_getter

#define SET_SIM_ACCESSORS(property)		\
  SET_SIM_NAMED_ACCESSORS(property, # property)

  SET_SIM_ACCESSORS(gravity);
  SET_SIM_NAMED_ACCESSORS(time_step, "time-step");

  SET_SIM_NAMED_ACCESSORS(erp, "error-reduction-parameter");
  SET_SIM_NAMED_ACCESSORS(cfm, "constraint-force-mixing");

  SET_SIM_NAMED_ACCESSORS(contact_soft_cfm, "contact-cfm");
  SET_SIM_NAMED_ACCESSORS(contact_soft_erp, "contact-erp");
  SET_SIM_NAMED_ACCESSORS(contact_mu, "contact-mu");
  SET_SIM_NAMED_ACCESSORS(contact_slip1, "contact-slip-1");
  SET_SIM_NAMED_ACCESSORS(contact_slip2, "contact-slip-2");
  SET_SIM_NAMED_ACCESSORS(contact_bounce, "contact-bounce");

  SET_SIM_NAMED_ACCESSORS(auto_disable_flag, "auto-disable");

  SET_SIM_NAMED_ACCESSORS(auto_disable_linear_threshold,
			  "auto-disable-linear-threshold");

  SET_SIM_NAMED_ACCESSORS(auto_disable_angular_threshold,
			  "auto-disable-angular-threshold");

  SET_SIM_NAMED_ACCESSORS(auto_disable_steps, "auto-disable-steps");
  SET_SIM_NAMED_ACCESSORS(auto_disable_time, "auto-disable-time");

  SET_SIM_NAMED_ACCESSORS(quick_step_iterations, "quick-step-iterations");
  SET_SIM_NAMED_ACCESSORS(quick_step_relaxation, "quick-step-relaxation");

  SET_SIM_NAMED_ACCESSORS(linear_damping, "linear-damping");
  SET_SIM_NAMED_ACCESSORS(angular_damping, "angular-damping");

  SET_SIM_NAMED_ACCESSORS(linear_damping_threshold, 
			  "linear-damping-threshold");
  SET_SIM_NAMED_ACCESSORS(angular_damping_threshold, 
			  "angular-damping-threshold");

  SET_SIM_NAMED_ACCESSORS(max_angular_speed, "max-angular-speed");
  SET_SIM_NAMED_ACCESSORS(contact_max_correcting_velocity, 
			  "contact-max-correcting-velocity");

  SET_SIM_NAMED_ACCESSORS(contact_surface_layer, "contact-surface-layer");

#undef SET_SIM_ACCESSORS  
#undef SET_SIM_NAMED_ACCESSORS
}

static SCM
simulation_p(SCM smob) {
  return (SCM_SMOB_PREDICATE(ode_tag, smob) && (SCM_SMOB_FLAGS(smob) != SIM))
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_SIM_PROCEDURES						\
  DEFINE_PROC("primitive-make-simulation",0,0,0,primitive_make_simulation); \
  EXPORT_PROC("simulation?", 1, 0, 0, simulation_p)			\
  EXPORT_PROC("make-simulation-step!",1,0,0,make_simulation_step);	\
  EXPORT_PROC("current-simulation-step",1,0,0,current_simulation_step);	\
  EXPORT_PROC("simulation-rigs",1,0,0,simulation_rigs);			\
  EXPORT_PROC("simulation-property",2,0,0,simulation_property);		\
  EXPORT_PROC("set-simulation-property!",3,0,0,set_simulation_property_x)

#define INIT_SIM_MODULE				\
  init_sim_property_accessors()
