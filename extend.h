#ifndef EXTEND_H
#define EXTEND_H

#include <libguile.h>
//#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static inline int not(SCM value) {
  return scm_is_false(value);
}

static inline int indeed(SCM value) {
  return scm_is_true(value);
}

static inline int equal(SCM a, SCM b) {
  return scm_is_true(scm_equal_p(a, b));
}

static inline char *as_c_string(SCM object) {
  SCM port = scm_open_output_string();
  scm_display(object, port);
  return scm_to_locale_string(scm_get_output_string(port));
  
}

static inline SCM eval(const char *str) {
  return scm_c_eval_string(str);
}

static inline SCM evalf(const char *fmt, ...) {
  SCM ret;
  int n;
  char *strp;
  va_list ap;
  va_start(ap, fmt);
  n = vasprintf(&strp, fmt, ap);
  va_end(ap);
  if(n == -1)
    return SCM_UNDEFINED;
  ret = eval(strp);
  free(strp);
  return ret;
}

static inline SCM symbol(const char *str) {
  return scm_from_locale_symbol(str);
}


static inline SCM funcall_0(const char *fname) {
  return scm_apply_0(eval(fname), SCM_EOL);
}
static inline SCM funcall_1(const char *fname, SCM arg1) {
  return scm_apply_1(eval(fname), arg1, SCM_EOL);
}
static inline SCM funcall_2(const char *fname, SCM arg1, SCM arg2) {
  return scm_apply_2(eval(fname), arg1, arg2, SCM_EOL);
}
static inline SCM funcall_3(const char *fname, SCM arg1, SCM arg2, SCM arg3) {
  return scm_apply_3(eval(fname), arg1, arg2, arg3, SCM_EOL);
}


static inline int is_scm_defined(SCM s) {
  return scm_is_true(scm_defined_p(s, SCM_UNDEFINED));
}
static inline int is_scm_procedure(SCM s) {
  return scm_is_true(scm_procedure_p(s));
}
static inline int stands_for_scm_procedure(SCM s) {
  return is_scm_procedure(scm_primitive_eval(s));
}
static inline int is_scm_macro(SCM s) {
  return scm_is_true(scm_macro_p(s));
}
static inline int stands_for_scm_macro(SCM symbol) {
  return is_scm_macro(scm_primitive_eval(symbol));
}
static inline int is_scm_symbol(SCM s) {
  return scm_is_true(scm_symbol_p(s));
}
static inline int is_scm_hash_table(SCM s) {
  return scm_is_true(scm_hash_table_p(s));
}

static inline int is_scm_pair(SCM s) {
  return scm_is_true(scm_pair_p(s));
}



static inline void release_scm(SCM s) {
  scm_gc_unprotect_object(s);
  s = SCM_UNSPECIFIED;
}

static inline void hold_scm(SCM s) {
  scm_gc_protect_object(s);
}



#endif /* EXTEND_H */
