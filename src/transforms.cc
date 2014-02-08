#include "3d.hh"


static SCM
quaternion_to_matrix(SCM quaternion) {
  qtd q = scm_to_qtd(quaternion);
  return scm_from_m4x4d(m4x4d(m3x3d(q), v4d(0,0,0,1)));
}

static SCM
translation_matrix(SCM vector) {
  v3d v = scm_to_v3d(vector);
  return scm_from_m4x4d(m4x4d(m3x3d(), v4d(v, 1)));
}

#define VIEWPORT_LIST_TO_STRUCT(list_name, struct_name)			\
  if(!GIVEN(list_name)							\
     || isnt(scm_list_p(list_name))					\
     || (scm_to_int(scm_length(list_name)) < 4)				\
     ) {								\
    glGetIntegerv(GL_VIEWPORT, (GLint *) &struct_name);			\
  }									\
  else {								\
    SCM_LIST_TO_C_ARRAY(int, &struct_name, 4, scm_to_int, list_name);	\
    struct_name.y = screen->h - struct_name.y - struct_name.h;		\
  }

static SCM
world_to_screen_coordinates(SCM _x, SCM _y, SCM _z,
			    SCM _modelview_matrix,
			    SCM _projection_matrix,
			    SCM _viewport_list) {
  v3d m;
  m.x = scm_to_double(_x);
  m.y = scm_to_double(_y);
  m.z = scm_to_double(_z);

  m4x4d modelview_matrix = scm_to_m4x4d(_modelview_matrix);
  m4x4d projection_matrix = scm_to_m4x4d(_projection_matrix);
  struct { GLint x, y, w, h; } s;
  VIEWPORT_LIST_TO_STRUCT(_viewport_list, s);
  v3d v;
  GLint result = gluProject(m.x, m.y, m.z, 
			    (GLdouble *) &modelview_matrix,
			    (GLdouble *) &projection_matrix,
			    (GLint *) &s,
			    &v.x, &v.y, &v.z);
  if(result == GL_FALSE) {
    return SCM_BOOL_F;
  }
  return scm_list_3(scm_from_int(rint(v.x)), 
		    scm_from_int(screen->h - rint(v.y) - 1),
		    scm_from_double(v.z));
}

static SCM
screen_to_world_coordinates(SCM _x, SCM _y, SCM _z,
			    SCM _modelview_matrix,
			    SCM _projection_matrix,
			    SCM _viewport_list) {
  v3d m;
  m.x = scm_to_double(_x);
  m.y = scm_to_double(_y);
  m.z = isnt(_z) ?_z_index((int) m.x, (int) m.y) : scm_to_double(_z);

  m4x4d modelview_matrix = scm_to_m4x4d(_modelview_matrix);
  m4x4d projection_matrix = scm_to_m4x4d(_projection_matrix);
  struct { GLint x, y, w, h; } s;
  VIEWPORT_LIST_TO_STRUCT(_viewport_list, s);
  v3d v;
  GLint result = gluUnProject(m.x, screen->h - m.y - 1, m.z, 
			      (GLdouble *) &modelview_matrix,
			      (GLdouble *) &projection_matrix,
			      (GLint *) &s,
			      &v.x, &v.y, &v.z);
  if(result == GL_FALSE) {
    return SCM_BOOL_F;
  }  
  return scm_from_v3d(v);
}

#undef VIEWPORT_LIST_TO_STRUCT

// by convention, each function that ever
// alters a matrix mode, should leave GL_MODELVIEW
// as the current matrix mode.

// this code is a travesty of gluPerspective from the GLU library
static SCM
perspective_projection(SCM _fovy, SCM _aspect, SCM _near, SCM _far) {
  double fovy = scm_to_double(_fovy);
  double aspect = _get_aspect_from_scm(_aspect);
  double radians = deg2rad(0.5*fovy);
  double near = GIVEN(_near) ? scm_to_double(_near) : 0.1;
  double far = GIVEN(_far) ? scm_to_double(_far) : 1000.0;
  double d = far-near;
  double sine = sin(radians);
  if((d == 0) || (sine == 0) || (aspect == 0)) {
    WARN("Invalid argument");
    return SCM_UNSPECIFIED;
  }
  d = 1/d;
  double cotangent = cos(radians)/sine;
  return scm_from_m4x4d
    (m4x4d(cotangent/aspect, 0,                0,                0,
	   0,                cotangent,        0,                0,
	   0,                0,                -(far+near)*d,   -2*near*far*d,
	   0,                0,                -1,               0));
}


static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)

  EXPORT_PROCEDURE("screen->world/coordinates", 5, 1, 0, 
		   screen_to_world_coordinates);

  EXPORT_PROCEDURE("world->screen/coordinates", 5, 1, 0, 
		   world_to_screen_coordinates);

  EXPORT_PROCEDURE("quaternion->matrix", 1, 0, 0, quaternion_to_matrix);
  EXPORT_PROCEDURE("translation-matrix", 1, 0, 0, translation_matrix);

  EXPORT_PROCEDURE("perspective-projection", 1, 3, 0, 
		   perspective_projection);

#undef EXPORT_PROCEDURE
}

void
init_transforms() {
  scm_c_define_module("slayer 3d", export_symbols, NULL);
}
