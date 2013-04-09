#include <ode/ode.h>
#include "utils.h"
#include "extend.h"


scm_t_bits ode_tag;

enum ODE_TYPES {
  WORLD = 0,
  SPACE = 1,
  BODY = 2,
  JOINT = 3,
  GEOM = 4,
  ODE_TYPES = 5
};


static char *ode_types[] = {
  "world",
  "space",
  "body",
  "joint",
  "geom"
};


SCM
create_world() {
  SCM smob;
  dWorldID id = dWorldCreate();
  SCM_NEWSMOB(smob, ode_tag, id);
  SCM_SET_SMOB_FLAGS(smob, WORLD);
  return smob;
}

/*
static inline SCM
scm_from_dVector3(dVector3 v) {

}

static inline dVector3
scm_to_dVector3(SCM v) {
  
}

static inline dMatrix3
scm_to_dMatrix3() {

}
*/
static void
export_symbols() {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("create-world", 0, 0, 0, create_world);

#undef EXPORT_PROCEDURE
}

static size_t
free_ode(SCM smob) {
  unsigned short type = SCM_SMOB_FLAGS(smob);
  if(type == WORLD) {
    dWorldID id = (dWorldID) SCM_SMOB_DATA(smob);
    dWorldDestroy(id);
  }
  else {
    WARN("Unimplemented ODE destructor");
  }

  return 0;
}

static int
print_ode(SCM ode, SCM port, scm_print_state *pstate) {
  void *data = (void *) SCM_SMOB_DATA(ode);
  unsigned short type = SCM_SMOB_FLAGS(ode);
  if(type < ODE_TYPES) {
    scm_puts("#<ode-", port);
    scm_puts(ode_types[type], port);
    scm_puts(" (implement detailed info for specific types)>", port);
  }
  else {
    scm_throw(symbol("unknown-ode-type"), scm_list_1(scm_from_uint16(type)));
  }
  
  scm_remember_upto_here_1(ode);
  return 1;
}

void
ode_init() {
  ode_tag = scm_make_smob_type("ode", sizeof(void *));
  scm_set_smob_free(ode_tag, free_ode);
  scm_set_smob_print(ode_tag, print_ode);
  export_symbols();
}
