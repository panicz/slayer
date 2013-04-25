static joint_maker_map_t joint_maker;
static joint_property_setter_map_t joint_property_setter;
static joint_property_getter_map_t joint_property_getter;


#define DEF_MAKE_JOINT(type, Type)				\
  static dJointID						\
  make_##type(rig_t *rig) {					\
    return dJointCreate##Type(rig->parent->world, rig->group);	\
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
make_joint(SCM x_rig, SCM s_type) {
  RIG_CONDITIONAL_ASSIGN(x_rig, rig, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_type, 2);
  joint_t *joint = new joint_t;

  joint_maker_map_t::iterator maker = joint_maker.find(s_type);
  
  if(maker == joint_maker.end()) {
    char *type = as_c_string(s_type);
    WARN("joint type %s not implemented", type);
    free(type);
    return SCM_UNSPECIFIED;
  }

  joint->joint = (maker->second)(rig);
  joint->body1_id = -1;
  joint->body2_id = -1;
  joint->parent = rig;
  rig->joints.push_back(joint);

  return joint_to_smob(joint);
}

static void
joint_body1_setter(joint_t *joint, SCM value) {
  BODY_CONDITIONAL_ASSIGN(value, body1,);
  joint->body1_id = body1->id;
  if (joint->body2_id > -1) {
    body_t *body2 = joint->parent->bodies[joint->body2_id];
    dJointAttach(joint->joint, body1->body, body2->body);
  }
}

static SCM
joint_body1_getter(joint_t * joint) {
  dBodyID body = dJointGetBody(joint->joint, 0);
  if(body) {
    return body_to_smob((body_t *) dBodyGetData(body));
  }
  return SCM_BOOL_F;
}

static void
joint_body2_setter(joint_t * joint, SCM value) {
  BODY_CONDITIONAL_ASSIGN(value, body2,);
  joint->body2_id = body2->id;
  if (joint->body1_id > -1) {
    body_t *body1 = joint->parent->bodies[joint->body1_id];
    dJointAttach(joint->joint, body1->body, body2->body);
  }
}

static SCM
joint_body2_getter(joint_t * joint) {
  dBodyID body = dJointGetBody(joint->joint, 1);
  if(body) {
    return body_to_smob((body_t *) dBodyGetData(body));
  }
  return SCM_BOOL_F;
}

#define DEF_JOINT_VECTOR_SETTER(property_name, PropertyName)	\
  static void							\
  joint_##property_name##_setter(joint_t * joint, SCM value) {	\
    dVector3 v;							\
    scm_to_dVector3(value, &v);					\
    dJointSet##PropertyName(joint->joint, v[0], v[1], v[2]);	\
    scm_remember_upto_here_1(value);				\
  }

#define DEF_JOINT_VECTOR_GETTER(property_name, PropertyName)	\
  static SCM							\
  joint_##property_name##_getter(joint_t * joint) {		\
    dVector3 v;							\
    dJointGet##PropertyName(joint->joint, v);			\
    return scm_from_dVector3(v);				\
  }

#define DEF_JOINT_VECTOR_ACCESSORS(property_name, PropertyName)	\
  DEF_JOINT_VECTOR_SETTER(property_name, PropertyName)		\
  DEF_JOINT_VECTOR_GETTER(property_name, PropertyName)

DEF_JOINT_VECTOR_ACCESSORS(hinge_anchor, HingeAnchor);
DEF_JOINT_VECTOR_ACCESSORS(hinge_axis, HingeAxis);

DEF_JOINT_VECTOR_ACCESSORS(slider_axis, SliderAxis);

DEF_JOINT_VECTOR_ACCESSORS(universal_anchor, UniversalAnchor);
DEF_JOINT_VECTOR_ACCESSORS(universal_axis1, UniversalAxis1);
DEF_JOINT_VECTOR_ACCESSORS(universal_axis2, UniversalAxis2);

DEF_JOINT_VECTOR_ACCESSORS(hinge2_anchor, Hinge2Anchor);
DEF_JOINT_VECTOR_ACCESSORS(hinge2_axis1, Hinge2Axis1);
DEF_JOINT_VECTOR_ACCESSORS(hinge2_axis2, Hinge2Axis2);

#undef DEF_JOINT_VECTOR_ACCESSORS
#undef DEF_JOINT_VECTOR_GETTER
#undef DEF_JOINT_VECTOR_SETTER

#define DEF_JOINT_REAL_GETTER(property_name, PropertyName)		\
  static SCM								\
  joint_##property_name##_getter(joint_t * joint) {			\
    return scm_from_double(dJointGet##PropertyName(joint->joint));	\
  }


#undef DEF_JOINT_REAL_GETTER

#define DEF_JOINT_PARAM_SETTER(type, Type, name, Name)			\
  static void								\
  joint_##type##_##name##_setter(joint_t * joint, SCM value) {		\
    dJointSet##Type##Param(joint->joint, Name, (dReal) scm_to_double(value)); \
  }

#define DEF_JOINT_PARAM_GETTER(type, Type, name, Name)			\
  static SCM								\
  joint_##type##_##name##_getter(joint_t * joint) {			\
    return scm_from_double((dReal) dJointGet##Type##Param(joint->joint, Name)); \
  }

#define DEF_JOINT_PARAM_ACCESSOR(type, Type, name, Name)	\
  DEF_JOINT_PARAM_GETTER(type, Type, name, Name)		\
  DEF_JOINT_PARAM_SETTER(type, Type, name, Name)

#define DEF_ALL_JOINT_PARAM_ACCESSORS(type, Type)			\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, suspension_erp, dParamSuspensionERP) \
  DEF_JOINT_PARAM_ACCESSOR(type, Type, suspension_cfm, dParamSuspensionCFM) \
  DEF_JOINT_PARAM_ACCESSOR(type, Type, velocity, dParamVel)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, max_force, dParamFMax)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, lo_stop, dParamLoStop)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, hi_stop, dParamHiStop)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, fudge_factor, dParamFudgeFactor)	\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, bounce, dParamBounce)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, cfm, dParamCFM)			\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, stop_erp, dParamStopERP)		\
  DEF_JOINT_PARAM_ACCESSOR(type, Type, stop_cfm, dParamStopCFM)

DEF_ALL_JOINT_PARAM_ACCESSORS(hinge, Hinge);
DEF_ALL_JOINT_PARAM_ACCESSORS(hinge2, Hinge2);
DEF_ALL_JOINT_PARAM_ACCESSORS(slider, Slider);
DEF_ALL_JOINT_PARAM_ACCESSORS(universal, Universal);
DEF_ALL_JOINT_PARAM_ACCESSORS(angular_motor, AMotor);

#undef DEF_ALL_JOINT_PARAM_ACCESSORS
#undef DEF_JOINT_PARAM_SETTER
#undef DEF_JOINT_PARAM_GETTER
#undef DEF_JOINT_PARAM_ACCESSOR

static void
init_joint_property_accessors() {
  pair<SCM, int> index;
#define SET_JOINT_NAMED_ACCESSORS(ode_type, prefix, property, name)	\
  index = make_pair(gc_protected(symbol(name)), ode_type);		\
  joint_property_setter[index] = joint_##prefix##property##_setter;	\
  joint_property_getter[index] = joint_##prefix##property##_getter

#define SET_JOINT_ACCESSORS(ode_type, prefix, property)			\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix, property, # property)

  SET_JOINT_ACCESSORS(dJointTypeHinge, hinge_, anchor);
  SET_JOINT_ACCESSORS(dJointTypeHinge, hinge_, axis);

  SET_JOINT_ACCESSORS(dJointTypeSlider, slider_, axis);

  SET_JOINT_ACCESSORS(dJointTypeUniversal, universal_, anchor);
  SET_JOINT_NAMED_ACCESSORS(dJointTypeUniversal, universal_, axis1, "axis-1");
  SET_JOINT_NAMED_ACCESSORS(dJointTypeUniversal, universal_, axis2, "axis-2");

  SET_JOINT_ACCESSORS(dJointTypeHinge2, hinge2_, anchor);
  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2, hinge2_, axis1, "axis-1");
  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2, hinge2_, axis2, "axis-2");

  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2, hinge2_, suspension_erp,
			    "suspension-erp");

  SET_JOINT_NAMED_ACCESSORS(dJointTypeHinge2, hinge2_, suspension_cfm,
			    "suspension-cfm");
  
#define SET_ALL_JOINT_PARAM_ACCESSORS(ode_type, prefix)			\
  SET_JOINT_NAMED_ACCESSORS(ode_type,prefix##_,suspension_erp,"suspension-erp"); \
  SET_JOINT_NAMED_ACCESSORS(ode_type,prefix##_,suspension_cfm,"suspension-cfm"); \
  SET_JOINT_ACCESSORS(ode_type, prefix##_, velocity);			\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, max_force, "max-force"); \
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, lo_stop, "lo-stop");	\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, hi_stop, "hi-stop");	\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, fudge_factor, "fudge-factor"); \
  SET_JOINT_ACCESSORS(ode_type, prefix##_, bounce);			\
  SET_JOINT_ACCESSORS(ode_type, prefix##_, cfm);			\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, stop_erp, "stop-erp");	\
  SET_JOINT_NAMED_ACCESSORS(ode_type, prefix##_, stop_cfm, "stop-cfm");
  
  SET_ALL_JOINT_PARAM_ACCESSORS(dJointTypeHinge, hinge);
  SET_ALL_JOINT_PARAM_ACCESSORS(dJointTypeHinge2, hinge2);
  SET_ALL_JOINT_PARAM_ACCESSORS(dJointTypeSlider, slider);
  SET_ALL_JOINT_PARAM_ACCESSORS(dJointTypeUniversal, universal);
  SET_ALL_JOINT_PARAM_ACCESSORS(dJointTypeAMotor, angular_motor);

#undef SET_ALL_JOINT_PARAM_ACCESSORS

  for(int i = 0; i <= dJointTypePiston; ++i) {
    SET_JOINT_NAMED_ACCESSORS(i,, body1, "body-1");
    SET_JOINT_NAMED_ACCESSORS(i,, body2, "body-2");
  }

#undef SET_JOINT_ACCESSORS
#undef SET_JOINT_NAMED_ACCESSORS
}


static SCM
set_joint_property_x(SCM x_joint, SCM s_prop, SCM value) {
  JOINT_CONDITIONAL_ASSIGN(x_joint, joint, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);
  int cl = dJointGetType(joint->joint);

  joint_property_setter_map_t::iterator setter 
    = joint_property_setter.find(make_pair(s_prop, cl));

  if(setter == joint_property_setter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("joint property `%s` not implemented for %s", prop, 
	 joint_type_name[cl]);
    free(prop);
    return SCM_BOOL_F;
  }  
  (setter->second)(joint, value);

  return SCM_UNSPECIFIED;
}

static SCM
joint_property(SCM x_joint, SCM s_prop) {
  JOINT_CONDITIONAL_ASSIGN(x_joint, joint, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);
  int cl = dJointGetType(joint->joint); 

  joint_property_getter_map_t::iterator getter
    = joint_property_getter.find(make_pair(s_prop, cl));

  if(getter == joint_property_getter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("joint property `%s` not implemented for %s", prop, 
	 joint_type_name[cl]);
    free(prop);
    return SCM_UNSPECIFIED;
  }

  scm_remember_upto_here_2(x_joint, s_prop);
  return (getter->second)(joint);
}

static SCM
joint_type(SCM x_joint) {
  JOINT_CONDITIONAL_ASSIGN(x_joint, joint, SCM_BOOL_F);
  return symbol(joint_type_name[dJointGetType(joint->joint)]);
}

// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_JOINT_PROCEDURES						\
  EXPORT_PROC("make-joint", 2, 0, 0, make_joint);			\
  EXPORT_PROC("set-joint-property!", 3, 0, 0, set_joint_property_x);	\
  EXPORT_PROC("joint-property", 2, 0, 0, joint_property);		\
  EXPORT_PROC("joint-type", 1, 0, 0, joint_type)
