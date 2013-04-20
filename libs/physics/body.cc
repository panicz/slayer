
#define DEF_MAKE_SOME_BODY(create_body, set_body, shape, Shape, ...)	\
  static body_t *							\
  make_##shape(sim_t *sim, rig_t *rig) {				\
    body_t *body = new body_t;						\
    body->body = create_body(sim->world);				\
    body->geom = dCreate##Shape(rig->space, ## __VA_ARGS__ );		\
    set_body(body->geom, body->body);					\
    rig->bodies.push_back(body);					\
    body->id = rig->bodies.size();					\
    return body;							\
  }

#define DEF_MAKE_BODY(shape, Shape, ...)				\
  DEF_MAKE_SOME_BODY(dBodyCreate, dGeomSetBody, shape, Shape, ## __VA_ARGS__)

DEF_MAKE_SOME_BODY(ZILCH, DONT, plane, Plane, 0, 0, 1, 0);
DEF_MAKE_BODY(cylinder, Cylinder, 0.5, 1.0);
DEF_MAKE_BODY(box, Box, 1, 1, 1);
DEF_MAKE_BODY(sphere, Sphere, 0.5);

#undef DEF_MAKE_BODY
#undef DEF_MAKE_SOME_BODY

static void
init_body_maker() {
#define SET_BODY_MAKER(shape)					\
  body_maker[gc_protected(symbol(# shape))] = make_##shape

  SET_BODY_MAKER(box);
  SET_BODY_MAKER(cylinder);
  SET_BODY_MAKER(plane);
  SET_BODY_MAKER(sphere);

#undef SET_BODY_MAKER
}

static void
box_dimensions_setter(body_t *body, SCM value) {
  dVector3 v;
  scm_to_dVector3(value, &v);
  dGeomBoxSetLengths(body->geom, v[0], v[1], v[2]);
}

static SCM
box_dimensions_getter(body_t *body) {
  dVector3 v;
  dGeomBoxGetLengths(body->geom, v);
  return scm_from_dVector3(v);
}

static void
sphere_radius_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dGeomSphereSetRadius(body->geom, scm_to_double(value));
  scm_remember_upto_here_1(value);
}

static SCM
sphere_radius_getter(body_t *body) {
  return scm_from_double(dGeomSphereGetRadius(body->geom));
}

static void
position_setter(body_t *body, SCM value) {
  if(!(body->body)) {
    WARN("function called on void body");
    return;
  }
  dVector3 v;
  scm_to_dVector3(value, &v);
  dBodySetPosition(body->body, v[0], v[1], v[2]);
}

static SCM
position_getter(body_t *body) {
  if(!(body->body)) {
    WARN("function called on void body");
    return SCM_UNSPECIFIED;;
  }
  dReal const *v = dBodyGetPosition(body->body);
  return scm_from_dVector3(v);
}

static void
mass_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  if(!(body->body)) {
    WARN("function called on void body");
    return;
  }
  dMass m;
  dBodyGetMass(body->body, &m);
  m.mass = scm_to_double(value);
  dBodySetMass(body->body, &m);
}

static SCM
mass_getter(body_t *body) {
  if(!(body->body)) {
    return scm_from_double(INFINITY);
  }
  dMass m;
  dBodyGetMass(body->body, &m);
  return scm_from_double(m.mass);
}

static void
cylinder_height_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  dGeomCylinderSetParams(body->geom, radius, scm_to_double(value));
  scm_remember_upto_here_1(value);
}

static SCM
cylinder_height_getter(body_t *body) {
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  return scm_from_double(height);
}

static void
cylinder_radius_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  dGeomCylinderSetParams(body->geom, scm_to_double(value), height);
  scm_remember_upto_here_1(value);
}

static SCM
cylinder_radius_getter(body_t *body) {
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  return scm_from_double(radius);
}

static void
init_body_property_accessors() {
  pair<SCM, int> index;
#define SET_BODY_ACCESSORS(ode_type, prefix, property)			\
  index = make_pair(gc_protected(symbol(# property)), ode_type);	\
  body_property_setter[index] = prefix##property##_setter;		\
  body_property_getter[index] = prefix##property##_getter

  SET_BODY_ACCESSORS(dBoxClass, box_, dimensions);
  SET_BODY_ACCESSORS(dCylinderClass, cylinder_, height);
  SET_BODY_ACCESSORS(dCylinderClass, cylinder_, radius);
  SET_BODY_ACCESSORS(dSphereClass, sphere_, radius);
  
  for(int i = 0; i < dFirstSpaceClass; ++i) {
    //if placable
    SET_BODY_ACCESSORS(i,, mass);
    SET_BODY_ACCESSORS(i,, position);
    //endif
  }

#undef SET_BODY_ACCESSORS
}

static SCM
make_body(SCM x_sim, SCM x_rig, SCM s_type, SCM s_name) {
  SIM_CONDITIONAL_ASSIGN(x_sim, sim);
  RIG_CONDITIONAL_ASSIGN(x_rig, rig);
  body_t *body;
  SCM smob = SCM_UNSPECIFIED;
  if(!GIVEN(s_type)) {
    WARN("default argument not supported (yet)");
  }
  ASSERT_SCM_TYPE(symbol, s_type, 3);

  body_maker_map_t::iterator maker = body_maker.find(s_type);
  
  if(maker == body_maker.end()) {
    WARN("body type not implemented");
    goto end;
  }

  body = (maker->second)(sim, rig);
  body->parent = rig;
  SET_SMOB_TYPE(BODY, smob, body);

  if(!GIVEN(s_type)) {
    goto end;
  }
  
  ASSERT_SCM_TYPE(symbol, s_name, 4);
  rig->id[gc_protected(s_name)] = body->id;

 end:
  scm_remember_upto_here_2(x_sim, x_rig);
  scm_remember_upto_here_2(s_type, s_name);
  return smob;
}

static SCM
set_body_property_x(SCM x_body, SCM s_prop, SCM s_value) {
  BODY_CONDITIONAL_ASSIGN(x_body, body);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);
  int cl = dGeomGetClass(body->geom);

  body_property_setter_map_t::iterator setter 
    = body_property_setter.find(make_pair(s_prop, cl));

  if(setter == body_property_setter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("body property `%s` not implemented for %s", prop, class_name[cl]);
    free(prop);
    goto end;
  }  
  (setter->second)(body, s_value);

 end:
  scm_remember_upto_here_2(s_prop, s_value);
  scm_remember_upto_here_1(x_body);
  return SCM_UNSPECIFIED;
}

static SCM
body_property(SCM x_body, SCM s_prop) {
  BODY_CONDITIONAL_ASSIGN(x_body, body);
  ASSERT_SCM_TYPE(symbol, s_prop, 2);
  int cl = dGeomGetClass(body->geom); 

  body_property_getter_map_t::iterator getter
    = body_property_getter.find(make_pair(s_prop, cl));

  if(getter == body_property_getter.end()) {
    char *prop = as_c_string(s_prop);
    WARN("body property `%s` not implemented for %s", prop, class_name[cl]);
    free(prop);
    return SCM_UNSPECIFIED;
  }

  scm_remember_upto_here_2(x_body, s_prop);
  return (getter->second)(body);
}
