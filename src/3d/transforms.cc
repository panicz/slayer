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
push_matrix_x() {
  glPushMatrix();
  return SCM_UNSPECIFIED;
}

static SCM
pop_matrix_x() {
  glPopMatrix();
  return SCM_UNSPECIFIED;
}

static SCM
translate_view_x(SCM vector) {
  v3f v = scm_to_v3f(vector);
  glTranslatef(v.x, v.y, v.z);
  return SCM_UNSPECIFIED;
}

static SCM
load_identity_x() {
  glLoadIdentity();
  return SCM_UNSPECIFIED;
}

static SCM
multiply_matrix_x(SCM M) {
  scm_t_array_handle h;
  if (scm_is_array(M)) {
    scm_array_get_handle(M, &h);
    size_t size = scm_array_handle_nelems(&h);
    if (size < 16) {
      WARN("Trying to multiply matrix by too few elements: %d", (int) size);
      goto release;
    }
    if (scm_is_typed_array(M, s_f32)) {
      float const *elements
	= scm_array_handle_f32_elements(&h);
      glMultMatrixf(elements); 
    }
    else if(scm_is_typed_array(M, s_f64)) {
      double const *elements
	= scm_array_handle_f64_elements(&h);
      glMultMatrixd(elements);
    }
    else {
      SCM *elements = (SCM *) scm_array_handle_elements(&h);
      m4x4d matrix;
      for(int i = 0; i < 4; ++i) {
	for(int j = 0; j < 4; ++j) {
	  matrix[j][i] = scm_to_double(elements[4*i+j]);
	}
      }
      glMultMatrixd((double *) &matrix);
    }
  release:
    scm_array_handle_release(&h);
  }
  return SCM_UNSPECIFIED;
}

static SCM
rotate_view_x(SCM quaternion) {
  qtf q = scm_to_qtf(quaternion);
  glRotatef(rad2deg(2*acos(q.s)), q.v.x, q.v.y, q.v.z);
  return SCM_UNSPECIFIED;
}

static SCM
scale_view_x(SCM X, SCM Y, SCM Z) {
  float x, y, z;
  if (!GIVEN(Y)) {
    x = y = z = (float) scm_to_double(X);
  }
  else {
    x = (float) scm_to_double(X);
    y = (float) scm_to_double(Y);
    z = GIVEN(Z) ? (float) scm_to_double(Z) : 1.0;
  }
  glScalef(x, y, z);
  return SCM_UNSPECIFIED;
}

static SCM
set_viewport_x(SCM X, SCM Y, SCM W, SCM H) {
  GLsizei w = scm_to_int(W);
  GLsizei h = scm_to_int(H);

  GLint x = scm_to_int(X);
  GLint y = screen->h - scm_to_int(Y) - h;

  glViewport(x, y, w, h);
  return SCM_UNSPECIFIED;
}

static SCM
current_viewport() {
  struct { GLint x, y, w, h; } s;
  glGetIntegerv(GL_VIEWPORT, (GLint *) &s);

  return scm_list_4(scm_from_int(s.x), 
		    scm_from_int(screen->h - s.y - s.h),
		    scm_from_int(s.w), 
		    scm_from_int(s.h));
}

static SCM
current_projection() {
  m4x4d M;
  glGetDoublev(GL_PROJECTION_MATRIX, (GLdouble *) &M);
  return scm_from_m4x4d(M);
}

static SCM
current_matrix() {
  m4x4d M;
  glGetDoublev(GL_MODELVIEW_MATRIX, (GLdouble *) &M);
  return scm_from_m4x4d(M);
}

static SCM
set_perspective_projection_x(SCM _fovy, SCM _aspect, SCM _near, SCM _far) {
  GLdouble fovy = scm_to_float(_fovy);

  GLdouble aspect = _get_aspect_from_scm(_aspect);

  GLdouble near = (_near == SCM_UNDEFINED)
    ? 0.1 : (GLdouble) scm_to_double(_near);
  GLdouble far = (_far == SCM_UNDEFINED)
    ? 1000.0 : (GLdouble) scm_to_double(_far);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(fovy, aspect, near, far);

  glMatrixMode(GL_MODELVIEW);

  return SCM_UNSPECIFIED;
}

static SCM
set_orthographic_projection_x(SCM left, SCM right, 
			      SCM bottom, SCM top,
			      SCM near, SCM far) {
  
  struct { GLint x, y, w, h; } s;
  glGetIntegerv(GL_VIEWPORT, (GLint *) &s);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(GIVEN(left) ? scm_to_double(left) : (GLdouble) s.x, 
	  GIVEN(right) ? scm_to_double(right) : (GLdouble) (s.x + s.w),
	  GIVEN(bottom) ? scm_to_double(bottom) : (GLdouble) s.y, 
	  GIVEN(top) ? scm_to_double(top) : (GLdouble) (s.y + s.h),
	  GIVEN(near) ? scm_to_double(near) : -1.0,
	  GIVEN(far) ? scm_to_double(far) : 1.0);

  glMatrixMode(GL_MODELVIEW);

  return SCM_UNSPECIFIED;
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

  EXPORT_PROCEDURE("multiply-matrix!", 1, 0, 0, multiply_matrix_x);
  EXPORT_PROCEDURE("push-matrix!", 0, 0, 0, push_matrix_x);
  EXPORT_PROCEDURE("pop-matrix!", 0, 0, 0, pop_matrix_x);
  EXPORT_PROCEDURE("translate-view!", 1, 0, 0, translate_view_x);
  EXPORT_PROCEDURE("rotate-view!", 1, 0, 0, rotate_view_x);
  EXPORT_PROCEDURE("scale-view!", 1, 2, 0, scale_view_x);
  EXPORT_PROCEDURE("load-identity!", 0, 0, 0, load_identity_x);
  EXPORT_PROCEDURE("set-viewport!", 4, 0, 0, set_viewport_x);
  EXPORT_PROCEDURE("current-viewport", 0, 0, 0, current_viewport);
  EXPORT_PROCEDURE("current-projection", 0, 0, 0, 
		   current_projection);

  EXPORT_PROCEDURE("current-matrix", 0, 0, 0, current_matrix);
  EXPORT_PROCEDURE("set-perspective-projection!", 1, 3, 0, 
		   set_perspective_projection_x);
  EXPORT_PROCEDURE("set-orthographic-projection!", 4, 2, 0, 
		   set_orthographic_projection_x);

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
