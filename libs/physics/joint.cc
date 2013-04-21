static joint_maker_map_t joint_maker;
static joint_property_setter_map_t joint_property_setter;
static joint_property_getter_map_t joint_property_getter;


#define DEF_MAKE_JOINT(type, Type)				\
  static dJointID						\
  make_##type(sim_t *sim, rig_t *rig) {				\
    return dJointCreate##Type(sim->world, rig->group);		\
  }

DEF_MAKE_JOINT(ball, Ball);
DEF_MAKE_JOINT(hinge, Hinge);
DEF_MAKE_JOINT(hinge2, Hinge2);
DEF_MAKE_JOINT(slider, Slider);
DEF_MAKE_JOINT(universal, Universal);
DEF_MAKE_JOINT(fixed, Fixed);
DEF_MAKE_JOINT(amotor, AMotor);

#undef DEF_MAKE_JOINT

static void
init_joint_maker() {
#define SET_JOINT_MAKER(type)					\
  joint_maker[gc_protected(symbol(# type))] = make_##type

#define SET_NAMED_JOINT_MAKER(type, name)		\
  joint_maker[gc_protected(symbol(name))] = make_##type

  SET_NAMED_JOINT_MAKER(ball, "ball-socket");
  SET_JOINT_MAKER(hinge);
  SET_NAMED_JOINT_MAKER(hinge2, "hinge-2");
  SET_JOINT_MAKER(slider);
  SET_JOINT_MAKER(universal);
  SET_JOINT_MAKER(fixed);
  SET_NAMED_JOINT_MAKER(amotor, "angular-motor");

#undef SET_NAMED_JOINT_MAKER
#undef SET_JOINT_MAKER
}

static SCM
make_joint(SCM x_sim, SCM x_rig, SCM s_type) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim, SCM_BOOL_F);
  RIG_CONDITIONAL_ASSIGN(x_rig, rig, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_type, 3);
  dJointID joint;
  SCM smob = SCM_UNSPECIFIED;

  joint_maker_map_t::iterator maker = joint_maker.find(s_type);
  
  if(maker == joint_maker.end()) {
    char *type = as_c_string(s_type);
    WARN("joint type %s not implemented", type);
    free(type);
    goto end;
  }

  joint = (maker->second)(sim, rig);
  SET_SMOB_TYPE(JOINT, smob, joint);
  
 end:
  scm_remember_upto_here_2(x_sim, x_rig);
  scm_remember_upto_here_1(s_type);
  return smob;
}

static void
joint_body1_setter(dJointID joint, SCM value) {
  BODY_CONDITIONAL_ASSIGN(value, body1,/*void*/);
  dBodyID body2 = dJointGetBody(joint, 1);
  dJointAttach(joint, body1->body, body2);
}

static SCM
joint_body1_getter(dJointID joint) {
  dBodyID body = dJointGetBody(joint, 0);
  if(body) {
    SCM smob;
    body_t *true_body = (body_t *) dBodyGetData(body);
    SET_SMOB_TYPE(BODY, smob, true_body);
    return smob;
  }
  return SCM_BOOL_F;
}

static void
joint_body2_setter(dJointID joint, SCM value) {
  BODY_CONDITIONAL_ASSIGN(value, body2,/*void*/);
  dBodyID body1 = dJointGetBody(joint, 0);
  dJointAttach(joint, body1, body2->body);
}

static SCM
joint_body2_getter(dJointID joint) {
  dBodyID body = dJointGetBody(joint, 1);
  if(body) {
    SCM smob;
    body_t *true_body = (body_t *) dBodyGetData(body);
    SET_SMOB_TYPE(BODY, smob, true_body);
    return smob;
  }
  return SCM_BOOL_F;
}

static void
init_joint_property_accessors() {
  pair<SCM, int> index;
#define SET_JOINT_NAMED_ACCESSORS(ode_type, prefix, property, name)	\
  index = make_pair(gc_protected(symbol(name)), ode_type);		\
  joint_property_setter[index] = joint_##prefix##property##_setter;	\
  joint_property_getter[index] = joint_##prefix##property##_getter

#define SET_JOINT_ACCESSORS(ode_type, prefix, property)			\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix, property, # property))

  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2,, body1, "body-1");
  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2,, body2, "body-2");

#undef SET_JOINT_ACCESSORS
#undef SET_JOINT_NAMED_ACCESSORS
}
