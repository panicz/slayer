static body_maker_map_t body_maker;
static body_property_setter_map_t body_property_setter;
static body_property_getter_map_t body_property_getter;

#define DEF_MAKE_SOME_BODY(create_body, set_body, shape, Shape, ...)	\
  static body_t *							\
  make_##shape(rig_t *rig) {						\
    body_t *body = new body_t;						\
    body->body = create_body(rig->parent->world);			\
    body->geom = dCreate##Shape(rig->space, ## __VA_ARGS__ );		\
    set_body(body->geom, body->body);					\
    if(body->body) {							\
      dBodySetData(body->body, (void *) body);				\
    }									\
    body->id = rig->bodies.size();					\
    rig->bodies.push_back(body);					\
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
body_box_dimensions_setter(body_t *body, SCM value) {
  dVector3 v;
  scm_to_dVector3(value, &v);
  dGeomBoxSetLengths(body->geom, v[0], v[1], v[2]);
}

static SCM
body_box_dimensions_getter(body_t *body) {
  dVector3 v;
  dGeomBoxGetLengths(body->geom, v);
  return scm_from_dVector3(v);
}

static void
body_sphere_radius_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dGeomSphereSetRadius(body->geom, scm_to_double(value));
  scm_remember_upto_here_1(value);
}

static SCM
body_sphere_radius_getter(body_t *body) {
  return scm_from_double(dGeomSphereGetRadius(body->geom));
}

static void
body_position_setter(body_t *body, SCM value) {
  if(!(body->body)) {
    WARN("function called on void body");
    return;
  }
  dVector3 v;
  scm_to_dVector3(value, &v);
  dBodySetPosition(body->body, v[0], v[1], v[2]);
}

static SCM
body_position_getter(body_t *body) {
  if(!(body->body)) {
    WARN("function called on void body");
    return SCM_UNSPECIFIED;;
  }
  dReal const *v = dBodyGetPosition(body->body);
  return scm_from_dVector3(v);
}

static void
body_mass_setter(body_t *body, SCM value) {
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
body_mass_getter(body_t *body) {
  if(!(body->body)) {
    return scm_from_double(INFINITY);
  }
  dMass m;
  dBodyGetMass(body->body, &m);
  return scm_from_double(m.mass);
}

static void
body_cylinder_height_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  dGeomCylinderSetParams(body->geom, radius, scm_to_double(value));
  scm_remember_upto_here_1(value);
}

static SCM
body_cylinder_height_getter(body_t *body) {
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  return scm_from_double(height);
}

static void
body_cylinder_radius_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  dGeomCylinderSetParams(body->geom, scm_to_double(value), height);
  scm_remember_upto_here_1(value);
}

static SCM
body_cylinder_radius_getter(body_t *body) {
  dReal radius, height;
  dGeomCylinderGetParams(body->geom, &radius, &height);
  return scm_from_double(radius);
}

static void
init_body_property_accessors() {
  pair<SCM, int> index;
#define SET_BODY_ACCESSORS(ode_type, prefix, property)			\
  index = make_pair(gc_protected(symbol(# property)), ode_type);	\
  body_property_setter[index] = body_##prefix##property##_setter;	\
  body_property_getter[index] = body_##prefix##property##_getter

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
make_body(SCM x_rig, SCM s_type, SCM s_name) {
  RIG_CONDITIONAL_ASSIGN(x_rig, rig, SCM_BOOL_F);
  ASSERT_SCM_TYPE(symbol, s_type, 2);
  ASSERT_SCM_TYPE(symbol, s_name, 3);

  body_t *body;

  body_maker_map_t::iterator maker = body_maker.find(s_type);
  
  if(maker == body_maker.end()) {
    char *type = as_c_string(s_type);
    WARN("body type not %s implemented", type);
    free(type);
    return SCM_UNSPECIFIED;
  }

  body = (maker->second)(rig);
  body->parent = rig;
  dBodySetData(body->body, body);

  rig->id[gc_protected(s_name)] = body->id;

  scm_remember_upto_here_1(x_rig);
  scm_remember_upto_here_2(s_type, s_name);  
  return body_to_smob(body);
}

static SCM
body_named(SCM s_name, SCM x_rig) {
  ASSERT_SCM_TYPE(symbol, s_name, 1);
  RIG_CONDITIONAL_ASSIGN(x_rig, rig, SCM_BOOL_F);

  symbol_index_map_t::iterator id = rig->id.find(s_name);

  if(id == rig->id.end()) {
    char *name = as_c_string(s_name);
    WARN("no body named '%s' found in rig %p", name, rig);
    free(name);
    return SCM_BOOL_F;
  }

  return body_to_smob(rig->bodies[id->second]);
}


static SCM
set_body_property_x(SCM x_body, SCM s_prop, SCM value) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
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
  (setter->second)(body, value);

 end:
  scm_remember_upto_here_2(s_prop, value);
  scm_remember_upto_here_1(x_body);
  return SCM_UNSPECIFIED;
}

static SCM
body_property(SCM x_body, SCM s_prop) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
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

static SCM
body_type(SCM x_body) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  scm_remember_upto_here_1(x_body);
  return symbol(class_name[dGeomGetClass(body->geom)]);
}
