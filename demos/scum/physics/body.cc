static body_maker_map_t body_maker;
static body_property_setter_map_t body_property_setter;
static body_property_getter_map_t body_property_getter;

static float
(*body_body_distance[dGeomNumClasses][dGeomNumClasses])(body_t *, body_t *);

/*************************** BODY CONSTRUCTION ***************************/

#define DEF_MAKE_SOME_BODY(create_body, set_body, shape, Shape, init, ...) \
  static body_t *							\
  make_##shape(rig_t *rig) {						\
    body_t *body = new body_t;						\
    body->body = create_body(rig->parent->world);			\
    body->geom = dCreate##Shape(rig->space, ## __VA_ARGS__ );		\
    WARN(# shape ": %p", body->geom);					\
    set_body(body->geom, body->body);					\
    body->id = rig->bodies.size();					\
    rig->bodies.push_back(body);					\
    init;								\
    return body;							\
  }

#define DEF_MAKE_BODY(shape, Shape, init, ...)				\
  DEF_MAKE_SOME_BODY(dBodyCreate, dGeomSetBody, shape, Shape,		\
		     init, ## __VA_ARGS__)

DEF_MAKE_SOME_BODY(ZILCH, DONT, plane, Plane, DONT(), 0, 0, 1, 0);
DEF_MAKE_BODY(cylinder, Cylinder, DONT(), 0.5, 1.0);
DEF_MAKE_BODY(capsule, CCylinder, DONT(), 0.5, 1.0);
DEF_MAKE_BODY(box, Box, DONT(), 1, 1, 1);
DEF_MAKE_BODY(sphere, Sphere, DONT(), 0.5);
DEF_MAKE_BODY(trimesh, TriMesh, {
    dGeomTriMeshSetData(body->geom, dGeomTriMeshDataCreate());
    body->data.trimesh.vertices = SCM_UNDEFINED;
    body->data.trimesh.indices = SCM_UNDEFINED;
    }, NULL, NULL, NULL, NULL);

#undef DEF_MAKE_BODY
#undef DEF_MAKE_SOME_BODY

static void
init_body_maker() {
#define SET_BODY_MAKER(shape)					\
  body_maker[gc_protected(symbol(# shape))] = make_##shape

  SET_BODY_MAKER(box);
  SET_BODY_MAKER(cylinder);
  SET_BODY_MAKER(capsule);
  SET_BODY_MAKER(plane);
  SET_BODY_MAKER(sphere);
  SET_BODY_MAKER(trimesh);

#undef SET_BODY_MAKER
}

/********************************* GEOMS *********************************/

/******************************** TRIMESH ********************************/

template <typename T> SCM 
_dTriIndex_uniform_vector_internal(SCM v);

#define BEGIN_TRIINDEX_UNIFORM_VECTOR_INTERNAL(bits)			\
  template<> SCM							\
  _dTriIndex_uniform_vector_internal< scm_t_uint##bits >(SCM v) {	\
  char *s = as_c_string(s_u##bits);					\
  free(s);								\
  if(scm_is_typed_array(v, s_u##bits)) {				\
    return v;								\
  }									\
  scm_t_array_handle H, h;						\
  scm_array_get_handle(v, &h);						\
  int i, n = scm_array_handle_nelems(&h);				\
  SCM V = scm_make_u##bits##vector(scm_from_int(n), SCM_UNDEFINED);	\
  scm_array_get_handle(V, &H);						\
  scm_t_uint##bits *dest						\
  = scm_array_handle_u##bits##_writable_elements(&H);			\
  if(0){}

#define TRIINDEX_CONVERT_ARRAY_IF(type, src_t, dest_t)			\
  else if(scm_is_typed_array(v, s_##type)) {				\
    scm_t_##src_t *src							\
      = scm_array_handle_##type##_writable_elements(&h);		\
    for(i = 0; i < n; ++i) {						\
      dest[i] = (scm_t_##dest_t) src[i];				\
    }									\
  }

#define END_TRIINDEX_UNIFORM_VECTOR_INTERNAL(bits)			\
  else {								\
    WARN("Invalid argument type (expecting uniform array)");		\
    V = SCM_BOOL_F;							\
  }									\
  scm_array_handle_release(&h);						\
  scm_array_handle_release(&H);						\
  return V;								\
  }

BEGIN_TRIINDEX_UNIFORM_VECTOR_INTERNAL(32)
TRIINDEX_CONVERT_ARRAY_IF(u8, uint8, uint32)
TRIINDEX_CONVERT_ARRAY_IF(s8, int8, uint32)
TRIINDEX_CONVERT_ARRAY_IF(u16, uint16, uint32)
TRIINDEX_CONVERT_ARRAY_IF(s16, int16, uint32)
TRIINDEX_CONVERT_ARRAY_IF(s32, int32, uint32)
TRIINDEX_CONVERT_ARRAY_IF(u64, uint64, uint32)
TRIINDEX_CONVERT_ARRAY_IF(s64, int64, uint32)
END_TRIINDEX_UNIFORM_VECTOR_INTERNAL(32)

BEGIN_TRIINDEX_UNIFORM_VECTOR_INTERNAL(16)
TRIINDEX_CONVERT_ARRAY_IF(u8, uint8, uint16)
TRIINDEX_CONVERT_ARRAY_IF(s8, int8, uint16)
TRIINDEX_CONVERT_ARRAY_IF(s16, int16, uint16)
TRIINDEX_CONVERT_ARRAY_IF(u32, uint32, uint16)
TRIINDEX_CONVERT_ARRAY_IF(s32, int32, uint16)
TRIINDEX_CONVERT_ARRAY_IF(u64, uint64, uint16)
TRIINDEX_CONVERT_ARRAY_IF(s64, int64, uint16)
END_TRIINDEX_UNIFORM_VECTOR_INTERNAL(16)

#undef END_TRIINDEX_UNIFORM_VECTOR_INTERNAL
#undef TRIINDEX_CONVERT_ARRAY_IF
#undef BEGIN_TRIINDEX_UNIFORM_VECTOR_INTERNAL

static inline
SCM scm_dTriIndex_uniform_vector(SCM v) { 
  return _dTriIndex_uniform_vector_internal<dTriIndex>(v); 
}

static void
body_trimesh_mesh_setter(body_t *body, SCM vertices_indices) {
  if (!scm_is_pair(vertices_indices)) {
    return WARN("Invalid argument (expecting pair)");
  }
  SCM Vertices = SCM_CAR(vertices_indices);
  if (!scm_is_array(Vertices)) {
    return WARN("Invalid car(argument) (expecting array of vertices)");
  }
  SCM Indices = SCM_CDR(vertices_indices);
  if (!scm_is_array(Indices)) {
    return WARN("Invalid cdr(argument) (expecting array of indices)");
  }
  Indices = scm_dTriIndex_uniform_vector(Indices);
  dTriMeshDataID mesh;

#define ASSIGN_MESH(ctype, stype, ftype) 				\
  mesh = dGeomTriMeshGetTriMeshDataID(body->geom);			\
									\
  ASSIGN_SCM(body->data.trimesh.vertices, Vertices);			\
  ASSIGN_SCM(body->data.trimesh.indices, Indices);			\
  scm_t_array_handle vh, ih;						\
  scm_array_get_handle(Vertices, &vh);					\
  scm_array_get_handle(Indices, &ih);					\
									\
  ctype const *vertices = scm_array_handle_##stype##_elements(&vh);	\
  dTriIndex const *indices						\
    = (dTriIndex const *) scm_array_handle_uniform_elements(&ih);	\
									\
  dGeomTriMeshDataBuild##ftype(mesh, vertices, 3*sizeof(ctype),		\
			       scm_array_handle_nelems(&vh)/3,		\
			       indices,	scm_array_handle_nelems(&ih),	\
			       3*sizeof(dTriIndex));			\
  scm_array_handle_release(&ih);					\
  scm_array_handle_release(&vh)

  if (scm_is_typed_array(Vertices, s_f32)) {
    ASSIGN_MESH(float, f32, Single);
  }
  else if (scm_is_typed_array(Vertices, s_f64)) {
    ASSIGN_MESH(double, f64, Double);
  }
  else {
    return WARN("Invalid argument (expecting f32 or f64 uniform array)");
  }
#undef ASSIGN_MESH
  dGeomTriMeshSetData(body->geom, mesh);
  scm_remember_upto_here_1(vertices_indices);
}

static SCM
body_trimesh_mesh_getter(body_t *body) {
  if (dGeomTriMeshGetTriMeshDataID(body->geom)) {
    return scm_cons(body->data.trimesh.vertices,
		    body->data.trimesh.indices);
  }
  return SCM_BOOL_F;
}

/********************************* PLANE *********************************/

static SCM
body_plane_normal_getter(body_t *body) {
  dVector4 p;
  dGeomPlaneGetParams(body->geom, p);
  double f = 1.0/sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  dVector3 v;
  for(int i = 0; i < 3; ++i) {
    v[i] = p[i]*f;
  }
  return scm_from_dVector3(v);
}

static void
body_plane_normal_setter(body_t *body, SCM value) {
  dVector3 p;
  dGeomPlaneGetParams(body->geom, p);
  double f = 1.0/sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  double d = p[3]*f;
  scm_to_dVector3(value, &p);
   f = 1.0/sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  for(int i = 0; i < 3; ++i) {
    p[i] = p[i]*f;
  }
  dGeomPlaneSetParams(body->geom, p[0], p[1], p[2], d);
}

static SCM
body_plane_displacement_getter(body_t *body) {
  dVector4 p;
  dGeomPlaneGetParams(body->geom, p);
  double f = 1.0/sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  return scm_from_double(p[3]*f);
}

static void
body_plane_displacement_setter(body_t *body, SCM value) {
  dVector4 p;
  dGeomPlaneGetParams(body->geom, p);
  p[3] = scm_to_double(value);
  dGeomPlaneSetParams(body->geom, p[0], p[1], p[2], p[3]);
}


// returns normal * displacement vector
static SCM
body_plane_position_getter(body_t *body) {
  dVector4 p;
  dGeomPlaneGetParams(body->geom, p);
  dVector3 v;
  for(int i = 0; i < 3; ++i) {
    v[i] = p[i]*p[3];
  }
  return scm_from_dVector3(v);
}

// sets displacement to the dot product of plane's normal and value
static void
body_plane_position_setter(body_t *body, SCM value) {
  dVector4 p;
  dGeomPlaneGetParams(body->geom, p);
  dVector3 v;
  scm_to_dVector3(value, &v);
  p[3] = 0;
  for(int i = 0; i < 3; ++i) {
    p[3] += p[i]*v[i];
  }
  dGeomPlaneSetParams(body->geom, p[0], p[1], p[2], p[3]);
}

// returns a quaternion that would rotate (0 0 1) to plane's normal
static SCM
body_plane_quaternion_getter(body_t *body) {
  dVector4 n;
  dGeomPlaneGetParams(body->geom, n);
  qtReal q(v3Real(0, 0, 1), v3Real(n[0], n[1], n[2]));
  dQuaternion Q = { q.s, q.v.x, q.v.y, q.v.z };
  return scm_from_dQuaternion(Q);
}

// sets normal to vector (0 0 1) rotated around value quaternion
static void
body_plane_quaternion_setter(body_t *body, SCM value) {
  dVector4 n;
  dGeomPlaneGetParams(body->geom, n);
  dQuaternion Q;
  scm_to_dQuaternion(value, &Q);
  v3Real v = qtReal(Q[0], Q[1], Q[2], Q[3]).rotate(v3Real(0, 0, 1));
  dGeomPlaneSetParams(body->geom, v.x, v.y, v.z, n[3]);
}

/********************************** BOX **********************************/

static void
body_box_dimensions_setter(body_t *body, SCM value) {
  dVector3 v;
  DEPRECATED("Please use #:x. #:y and #:z accessors instead.");
  scm_to_dVector3(value, &v);
  dGeomBoxSetLengths(body->geom, v[0], v[1], v[2]);
  scm_remember_upto_here_1(value);
}

static SCM
body_box_dimensions_getter(body_t *body) {
  dVector3 v;
  DEPRECATED("Please use #:x. #:y and #:z accessors instead.");
  dGeomBoxGetLengths(body->geom, v);
  return scm_from_dVector3(v);
}


#define DEF_BOX_DIMENSION_ACCESSORS(dimension, index)		\
  static void							\
  body_box_##dimension##_setter(body_t *body, SCM value) {	\
    dVector3 v;							\
    dGeomBoxGetLengths(body->geom, v);				\
    v[index] = scm_to_double(value);				\
    dGeomBoxSetLengths(body->geom, v[0], v[1], v[2]);		\
    scm_remember_upto_here_1(value);				\
  }								\
  								\
  static SCM							\
  body_box_##dimension##_getter(body_t *body) {			\
    dVector3 v;							\
    dGeomBoxGetLengths(body->geom, v);				\
    return scm_from_double(v[index]);				\
  }

DEF_BOX_DIMENSION_ACCESSORS(x, 0)
DEF_BOX_DIMENSION_ACCESSORS(y, 1)
DEF_BOX_DIMENSION_ACCESSORS(z, 2)

#undef DEF_BOX_DIMENSION_ACCESSORS

/*************************** CYLINDER/CAPSULE ****************************/

#define DEF_XCYLINDER_PARAMETER_ACCESSORS(Type, name)			\
  static void								\
  body_##name##_height_setter(body_t *body, SCM value) {		\
    ASSERT_SCM_TYPE(real, value, 2);					\
    dReal radius, height;						\
    dGeom##Type##GetParams(body->geom, &radius, &height);		\
    dGeom##Type##SetParams(body->geom, radius, scm_to_double(value));	\
    scm_remember_upto_here_1(value);					\
  }									\
									\
  static SCM								\
  body_##name##_height_getter(body_t *body) {				\
    dReal radius, height;						\
    dGeom##Type##GetParams(body->geom, &radius, &height);		\
    return scm_from_double(height);					\
  }									\
									\
  static void								\
  body_##name##_radius_setter(body_t *body, SCM value) {		\
    ASSERT_SCM_TYPE(real, value, 2);					\
    dReal radius, height;						\
    dGeom##Type##GetParams(body->geom, &radius, &height);		\
    dGeom##Type##SetParams(body->geom, scm_to_double(value), height);	\
    scm_remember_upto_here_1(value);					\
  }									\
									\
  static SCM								\
  body_##name##_radius_getter(body_t *body) {				\
    dReal radius, height;						\
    dGeom##Type##GetParams(body->geom, &radius, &height);		\
    return scm_from_double(radius);					\
  }

DEF_XCYLINDER_PARAMETER_ACCESSORS(Cylinder, cylinder)
DEF_XCYLINDER_PARAMETER_ACCESSORS(CCylinder, capsule)

#undef DEF_XCYLINDER_PARAMETER_ACCESSORS

/******************************** SPHERE *********************************/

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

/******************************** GENERAL ********************************/

//static dVector3 null_dVector3 = {0.0, 0.0, 0.0, 0.0};
//static dQuaternion neutral_dQuaternion = {1.0, 0.0, 0.0, 0.0};

#define DEF_BODY_GETTER(property, dType, dGet, scm_from_dType)		\
  static SCM body_##property##_getter(body_t *body) {			\
    if(!body->body) {							\
      WARN_UPTO(3, "function called on void body");			\
      return SCM_UNSPECIFIED;						\
    }									\
    dType value = dGet(body->body);					\
    return scm_from_dType(value);					\
  }

#define DEF_BODY_SETTER(property, dType, dSet, scm_to_dType, name, ...) \
  static void body_##property##_setter(body_t *body, SCM value) {	\
    if(!body->body) {							\
      return WARN("function called on void body");			\
    }									\
    dType name;								\
    scm_to_dType(value, &name);						\
    dSet(body->body, ## __VA_ARGS__ );					\
  }

#define DEF_BODY_COMPOUND_ACCESSORS(prop, Prop, dType)			\
  DEF_BODY_GETTER(prop, dReal const *, dBodyGet##Prop, scm_from_##dType) \
  DEF_BODY_SETTER(prop, dType, dBodySet##Prop, scm_to_##dType, v, v)

#define DEF_BODY_VECTOR3_ACCESSORS(prop, Prop)				\
  DEF_BODY_GETTER(prop, dReal const *, dBodyGet##Prop, scm_from_dVector3) \
  DEF_BODY_SETTER(prop, dVector3, dBodySet##Prop, scm_to_dVector3, v,	\
		  v[0], v[1], v[2])

#define DEF_BODY_SCALAR_ACCESSORS(prop, Prop)				\
  DEF_BODY_GETTER(prop, dReal, dBodyGet##Prop, scm_from_dReal)		\
  DEF_BODY_SETTER(prop, dReal, dBodySet##Prop, scm_to_dReal, s, s)

DEF_BODY_VECTOR3_ACCESSORS(position, Position)
DEF_BODY_VECTOR3_ACCESSORS(velocity, LinearVel)
DEF_BODY_VECTOR3_ACCESSORS(angular_velocity, AngularVel)
DEF_BODY_VECTOR3_ACCESSORS(force, Force)
DEF_BODY_VECTOR3_ACCESSORS(torque, Torque)

DEF_BODY_SCALAR_ACCESSORS(linear_damping, LinearDamping)
DEF_BODY_SCALAR_ACCESSORS(angular_damping, AngularDamping)
DEF_BODY_SCALAR_ACCESSORS(linear_damping_threshold, LinearDampingThreshold)
DEF_BODY_SCALAR_ACCESSORS(angular_damping_threshold, AngularDampingThreshold)


DEF_BODY_COMPOUND_ACCESSORS(rotation, Rotation, dMatrix3)
DEF_BODY_COMPOUND_ACCESSORS(quaternion, Quaternion, dQuaternion)

#undef DEF_BODY_SCALAR_ACCESSORS
#undef DEF_BODY_VECTOR3_ACCESSORS
#undef DEF_BODY_COMPOUND_ACCESSORS
#undef DEF_BODY_SETTER
#undef DEF_BODY_GETTER

static void
body_mass_setter(body_t *body, SCM value) {
  ASSERT_SCM_TYPE(real, value, 2);
  if(!(body->body)) {
    return WARN("function called on void body");
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
body_mass_distribution_setter(body_t *body, SCM center_tensor) {
  if(!(body->body)) {
    return WARN("Unable to retrieve body mass");
  }
  if (!scm_is_pair(center_tensor)) {
    return WARN("Invalid argument (expecting pair)");
  }
  dMass M;
  dBodyGetMass(body->body, &M);
  scm_to_dVector3(SCM_CAR(center_tensor), &M.c);
  scm_to_dMatrix3(SCM_CDR(center_tensor), &M.I);
#define M_I(i, j) M.I[(i)*3+j]
  /*
  OUT("Setting mass distribution to \n"
      "[ %f, %f, %f ;\n"
      "  %f, %f, %f ;\n"
      "  %f, %f, %f ] at [ %f, %f, %f ]\n", 
      M_I(0,0), M_I(0,1), M_I(0,2),
      M_I(1,0), M_I(1,1), M_I(1,2),
      M_I(2,0), M_I(2,1), M_I(2,2), M.c[0], M.c[1], M.c[2]);
  */
  dMassSetParameters(&M, M.mass, M.c[0], M.c[1], M.c[2],
		     M_I(0,0), M_I(1,1), M_I(2,2),
		     M_I(0,1), M_I(0,2), M_I(1,2));
#undef M_I
  dBodySetMass(body->body, &M);
}

static SCM
body_mass_distribution_getter(body_t *body) {
  if(!(body->body)) {
    return scm_from_double(INFINITY);
  }
  dMass m;
  dBodyGetMass(body->body, &m);
  return scm_cons(scm_from_dVector3(m.c), scm_from_dMatrix3(m.I));
}

static void
init_body_property_accessors() {
  pair<SCM, int> index;
#define SET_BODY_NAMED_ACCESSORS(ode_type, prefix, property, name)	\
  index = make_pair(gc_protected(symbol(name)), ode_type);		\
  body_property_setter[index] = body_##prefix##property##_setter;	\
  body_property_getter[index] = body_##prefix##property##_getter

#define SET_BODY_ACCESSORS(ode_type, prefix, property)		\
  SET_BODY_NAMED_ACCESSORS(ode_type, prefix, property, # property)
  
  SET_BODY_ACCESSORS(dBoxClass, box_, dimensions);
  SET_BODY_ACCESSORS(dBoxClass, box_, x);
  SET_BODY_ACCESSORS(dBoxClass, box_, y);
  SET_BODY_ACCESSORS(dBoxClass, box_, z);
  SET_BODY_ACCESSORS(dCylinderClass, cylinder_, height);
  SET_BODY_ACCESSORS(dCylinderClass, cylinder_, radius);
  SET_BODY_ACCESSORS(dCCylinderClass, capsule_, height);
  SET_BODY_ACCESSORS(dCCylinderClass, capsule_, radius);
  SET_BODY_ACCESSORS(dSphereClass, sphere_, radius);
  SET_BODY_ACCESSORS(dPlaneClass, plane_, normal);
  SET_BODY_ACCESSORS(dPlaneClass, plane_, displacement);
  SET_BODY_ACCESSORS(dTriMeshClass, trimesh_, mesh);
  
  for(int i = 0; i < dFirstSpaceClass; ++i) {
    SET_BODY_ACCESSORS(i,, mass);
    SET_BODY_ACCESSORS(i,, position);
    SET_BODY_ACCESSORS(i,, velocity);
    SET_BODY_ACCESSORS(i,, force);
    SET_BODY_ACCESSORS(i,, torque);
    SET_BODY_ACCESSORS(i,, rotation);
    SET_BODY_ACCESSORS(i,, quaternion);

    SET_BODY_NAMED_ACCESSORS(i,, angular_velocity, "angular-velocity");
    SET_BODY_NAMED_ACCESSORS(i,, linear_damping, "linear-damping");
    SET_BODY_NAMED_ACCESSORS(i,, angular_damping, "angular-damping");
    SET_BODY_NAMED_ACCESSORS(i,, linear_damping_threshold,
			     "linear-damping-threshold");
    SET_BODY_NAMED_ACCESSORS(i,, angular_damping_threshold,
			     "angular-damping-threshold");

    SET_BODY_NAMED_ACCESSORS(i,, mass_distribution, "mass-distribution");
  }

  SET_BODY_ACCESSORS(dPlaneClass, plane_, position);
  SET_BODY_ACCESSORS(dPlaneClass, plane_, quaternion);

#undef SET_BODY_ACCESSORS
#undef SET_BODY_NAMED_ACCESSORS
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
  if(body->body) { // e.g. planes have no associated body data
    dBodySetData(body->body, (void *) body);
  }

  rig->id[gc_protected(s_name)] = body->id;

  body->self_smob = gc_protected(body_to_smob(body));
  scm_remember_upto_here_1(x_rig);
  scm_remember_upto_here_2(s_type, s_name);
  return body->self_smob;
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
  return rig->bodies[id->second]->self_smob;
}

static SCM
body_name(SCM x_body) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  symbol_index_map_t::iterator it;
  it = find_if(body->parent->id.begin(), 
	       body->parent->id.end(),
	       [&](const symbol_index_map_t::value_type& the) -> bool { 
		 return the.second == body->id;
	       });
  if(it == body->parent->id.end()) {
    return SCM_BOOL_F;
  }
  return it->first;
}

static SCM
body_add_force_x(SCM x_body, SCM force) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  dBodyAddForce(body->body, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_local_force_x(SCM x_body, SCM force) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  dBodyAddRelForce(body->body, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_force_at_position_x(SCM x_body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f, p;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddForceAtPos(body->body, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}
static SCM
body_add_force_at_relative_position_x(SCM x_body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f, p;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddForceAtRelPos(body->body, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_local_force_at_position_x(SCM x_body, SCM force, SCM position) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f, p;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  scm_to_dVector3(position, &p);
  dBodyAddRelForceAtPos(body->body, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_local_force_at_relative_position_x(SCM x_body, SCM force, SCM pos) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f, p;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(force, &f);
  scm_to_dVector3(pos, &p);
  dBodyAddRelForceAtRelPos(body->body, f[0], f[1], f[2], p[0], p[1], p[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_torque_x(SCM x_body, SCM torque) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(torque, &f);
  dBodyAddTorque(body->body, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
}

static SCM
body_add_local_torque_x(SCM x_body, SCM torque) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  dVector3 f;
  if(!(body->body)) {
    WARN_UPTO(3, "function called on void body");
    return SCM_BOOL_F;
  }
  scm_to_dVector3(torque, &f);
  dBodyAddRelTorque(body->body, f[0], f[1], f[2]);
  return SCM_UNSPECIFIED;
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
    return SCM_BOOL_F;
  }  
  (setter->second)(body, value);

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
body_rig(SCM x_body) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  scm_remember_upto_here_1(x_body);
  return body->parent->self_smob;
}

static SCM
body_type(SCM x_body) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  scm_remember_upto_here_1(x_body);
  return symbol(class_name[dGeomGetClass(body->geom)]);
}

static SCM
body_id(SCM x_body) {
  BODY_CONDITIONAL_ASSIGN(x_body, body, SCM_BOOL_F);
  scm_remember_upto_here_1(x_body);
  return scm_from_size_t((size_t) body);
}

static SCM
body_p(SCM smob) {
  return (SCM_SMOB_PREDICATE(ode_tag, smob) && (SCM_SMOB_FLAGS(smob) != BODY))
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

/******************************** DISTANCE *******************************/

#define DISTANCE_FUNCTION(X, Y)					\
  static inline dReal X##_##Y##_distance(body_t *X, body_t *Y); \
  static dReal							\
  Y##_##X##_distance(body_t *Y, body_t *X) {			\
    return X##_##Y##_distance(X, Y);				\
  }								\
  static inline dReal						\
  X##_##Y##_distance(body_t *X, body_t *Y)

DISTANCE_FUNCTION(sphere, plane) {
  dVector4 p;
  dGeomPlaneGetParams(plane->geom, p);
  // according to the manual, ODE represents planes in the form
  // a*x + b*y + c*z = d; equivalently, it could be written as
  // a*x + b*y + c*z - d = 0, where a, b, c, d are (presumably)
  // p[0], p[1], p[2] and p[3], respectively.
  // The distance between the plane (a,b,c,-d) and a point (x,y,z)
  // can therefore be expressed as (ax+by+cz-d)/sqrt(aa+bb+cc)

  dReal r = dGeomSphereGetRadius(sphere->geom);
  dReal const *v = dBodyGetPosition(sphere->body);

  return ((p[0]*v[0]+p[1]*v[1]+p[2]*v[2]-p[3])
	  / sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2])) - r;
}

DISTANCE_FUNCTION(trimesh, plane) {
  dVector4 p;
  dGeomPlaneGetParams(plane->geom, p);
  v3f N(p[0], p[1], p[2]);
  float d = p[3];
  assert(abs(N.norm() - 1.0) < tolerance);
  
  SCM vertices = trimesh->data.trimesh.vertices;
  
  dReal const *position = dBodyGetPosition(trimesh->body);
  v3f r(position[0], position[1], position[2]);
  dReal const *rotation = dBodyGetRotation(trimesh->body);
  m3x3f M(rotation[0], rotation[1], rotation[2],
	  rotation[4], rotation[5], rotation[6],
	  rotation[8], rotation[9], rotation[10]);

  scm_t_array_handle H;
  scm_array_get_handle(vertices, &H);
  size_t i, number_of_vertices = scm_array_handle_nelems(&H) / 3;

  dReal distance = INFINITY;

#define MIN_DISTANCE_CASE(type, bits)					\
  CASE(scm_is_typed_array(vertices, s_f##bits)) {			\
    const v3##type *v =							\
      (const v3##type *) scm_array_handle_f##bits##_elements(&H);	\
    for(i = 0; i < number_of_vertices; ++i) {				\
      distance = MIN(distance, (M*v[i] + r)*N - d);			\
    }									\
  }

  BEGIN_CASES
    MIN_DISTANCE_CASE(f, 32)
    MIN_DISTANCE_CASE(d, 64)
  END_CASES

#undef MIN_DISTANCE_CASE

  scm_array_handle_release(&H);

  return distance;
}

DISTANCE_FUNCTION(box, plane) {
  dVector4 p;
  dGeomPlaneGetParams(plane->geom, p);
  v3f N(p[0], p[1], p[2]);
  float d = p[3];
  assert(abs(N.norm() - 1.0) < tolerance);

  dVector3 l;
  dGeomBoxGetLengths(box->geom, l);

  dReal const *position = dBodyGetPosition(box->body);
  v3f r(position[0], position[1], position[2]);
  dReal const *rotation = dBodyGetRotation(box->body);
  m3x3f M(rotation[0], rotation[1], rotation[2],
	  rotation[4], rotation[5], rotation[6],
	  rotation[8], rotation[9], rotation[10]);
  v3f v[8] = {
    r + M*v3f(-l[0]/2, -l[1]/2, -l[2]/2),
    r + M*v3f(-l[0]/2, -l[1]/2, +l[2]/2),
    r + M*v3f(-l[0]/2, +l[1]/2, -l[2]/2),
    r + M*v3f(-l[0]/2, +l[1]/2, +l[2]/2),
    r + M*v3f(+l[0]/2, -l[1]/2, -l[2]/2),
    r + M*v3f(+l[0]/2, -l[1]/2, +l[2]/2),
    r + M*v3f(+l[0]/2, +l[1]/2, -l[2]/2),
    r + M*v3f(+l[0]/2, +l[1]/2, +l[2]/2)
  };
  dReal distance = INFINITY;
  for(int i = 0; i < NELEMS(v); ++i) {
    distance = MIN(v[i]*N - d, distance);
  }
  return distance;
}

#undef DISTANCE_FUNCTION

static SCM
body_distance(SCM x_body_1, SCM x_body_2) {
  BODY_CONDITIONAL_ASSIGN(x_body_1, body1, SCM_BOOL_F);
  BODY_CONDITIONAL_ASSIGN(x_body_2, body2, SCM_BOOL_F);
  int t1 = dGeomGetClass(body1->geom);
  int t2 = dGeomGetClass(body2->geom);
  return scm_from_double(body_body_distance[t1][t2](body1, body2));
}

static dReal
distance_not_supported(body_t *b1, body_t *b2) {
  WARN("distance between %s and %s not supported (yet?), yielding infinity",
       class_name[dGeomGetClass(b1->geom)],
       class_name[dGeomGetClass(b2->geom)]);
  return INFINITY;
}

static void
init_body_distance() {
  for(int i = 0; i < dGeomNumClasses; ++i) {
    for(int j = 0; j < dGeomNumClasses; ++j) {
      body_body_distance[i][j] = distance_not_supported;
    }
  }
  body_body_distance[dSphereClass][dPlaneClass] = sphere_plane_distance;
  body_body_distance[dPlaneClass][dSphereClass] = plane_sphere_distance;

  body_body_distance[dTriMeshClass][dPlaneClass] = trimesh_plane_distance;
  body_body_distance[dPlaneClass][dTriMeshClass] = plane_trimesh_distance;

  body_body_distance[dBoxClass][dPlaneClass] = box_plane_distance;
  body_body_distance[dPlaneClass][dBoxClass] = plane_box_distance;
}

/******************************** EXPORT *********************************/

// to understand what's goint on here, see the definition of `export-symbols'
// function in `physics.cc' file
#define EXPORT_BODY_PROCEDURES						\
  EXPORT_PROC("make-body", 3, 0, 0, make_body);				\
  EXPORT_PROC("set-body-property!", 2, 1, 0, set_body_property_x);	\
  EXPORT_PROC("body-property", 2, 0, 0, body_property);			\
  EXPORT_PROC("body-rig", 1, 0, 0, body_rig);				\
  EXPORT_PROC("body-type", 1, 0, 0, body_type);				\
  EXPORT_PROC("body-id", 1, 0, 0, body_id);				\
  EXPORT_PROC("body-named", 2, 0, 0, body_named);			\
  EXPORT_PROC("body-name", 1, 0, 0, body_name);				\
  EXPORT_PROC("body-add-force!", 2, 0, 0, body_add_force_x);		\
  EXPORT_PROC("body-add-local-force!", 2, 0, 0, body_add_local_force_x); \
  EXPORT_PROC("body-add-force-at-position!", 3, 0, 0,			\
	      body_add_force_at_position_x);				\
  EXPORT_PROC("body-add-force-at-relative-position!", 3, 0, 0,		\
	      body_add_force_at_relative_position_x);			\
  EXPORT_PROC("body-add-local-force-at-position!", 3, 0, 0,		\
	      body_add_local_force_at_position_x);			\
  EXPORT_PROC("body-add-local-force-at-relative-position!", 3, 0, 0,	\
	      body_add_local_force_at_relative_position_x);		\
  EXPORT_PROC("body-add-torque!", 2, 0, 0, body_add_torque_x);		\
  EXPORT_PROC("body?", 1, 0, 0, body_p)					\
  EXPORT_PROC("body-distance", 2, 0, 0, body_distance)			\
  EXPORT_PROC("body-add-local-torque!", 2, 0, 0,			\
	      body_add_local_torque_x)

#define INIT_BODY_MODULE			\
  init_body_distance();				\
  init_body_maker();				\
  init_body_property_accessors()
