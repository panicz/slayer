#ifndef EXTEND_H
#define EXTEND_H

#include <libguile.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "utils.h"

#ifdef __cplusplus 
# define DECLARE extern "C"
#else // !__cplusplus
# define DECLARE extern
#endif // !__cplusplus

#define WRONG_TYPE_ARG(message, args)		\
  scm_error_scm(s_wrong_type_arg, scm_from_locale_string(__FUNCTION__),	\
		scm_from_locale_string(message), args, SCM_BOOL_F)

#define DISPLAY(port, msg, ...)					\
  ({ char *string;						\
    if(asprintf(&string, msg, ## __VA_ARGS__) == -1) {		\
      FATAL("failed to allocate memory for %s, ...", msg);	\
    }								\
    scm_puts(string, port);					\
    free(string);						\
  })

static inline size_t
scm_array_handle_nelems(scm_t_array_handle *handle) {
  scm_t_array_dim *dims = scm_array_handle_dims(handle);
  int rank = scm_array_handle_rank(handle);
  int i, nelems = (rank > 0) ? 1 : 0;
  for (i = 0; i < rank; ++i) {
    nelems *= abs(dims[i].ubnd - dims[i].lbnd) + 1;
  }
  return nelems;
}

#define GIVEN(scm) (!SCM_UNBNDP(scm))

#define ZILCH(val, ...) NULL
#define DONT(val, ...) do {} while(0)

#define MDEF_CONDITIONAL_ASSIGN(TYPE,tag,scm_var,c_type,c_var,d_val)	\
  scm_assert_smob_type(tag, scm_var);					\
  if(SCM_SMOB_FLAGS(scm_var) != TYPE) {					\
    WARN("FUNCTION CALLED ON A NON-" # TYPE);				\
    return d_val;							\
  }									\
  c_type c_var = (c_type) SCM_SMOB_DATA(scm_var)

#define ASSIGN_SCM(object, value) 				\
  if(!SCM_IMP(object)) {					\
    scm_gc_unprotect_object(object);				\
  }								\
  object = SCM_IMP(value) ? value : gc_protected(value)

#define ASSERT_SCM_TYPE(type, var, pos)					\
  SCM_ASSERT_TYPE(scm_is_##type(var), var, pos, __FUNCTION__, # type)

#define SCM_LIST_TO_C_ARRAY(type, dest, size, conv, list)	\
  ({								\
    SCM _head; int _i;						\
    for((_i = 0), _head = list;					\
	(_i < size) && scm_is_pair(_head);			\
	(_i++), _head = SCM_CDR(_head)) {			\
      ((type *) dest)[_i] = conv(SCM_CAR(_head));		\
    }})

#define DEFINE_ARRAY_GETTER(name, type, failval, getter)	\
  static inline type						\
  name(SCM array, ...) {					\
    va_list args;						\
    scm_t_array_handle h;					\
    type value failval;						\
    scm_t_array_dim *dims;					\
    ssize_t i, rank, index, pos = 0;				\
    type const *elements;					\
    								\
    va_start(args, array);					\
    scm_array_get_handle(array, &h);				\
    								\
    rank = scm_array_handle_rank(&h);				\
    dims = scm_array_handle_dims(&h);				\
    								\
    for(i = 0; i < rank; ++i) {					\
      index = va_arg(args, ssize_t);				\
      if (index < dims[i].lbnd || index > dims[i].ubnd) {	\
	goto end;						\
      }								\
      pos += (index - dims[i].lbnd) * dims[i].inc;		\
    }								\
    elements = getter(&h);					\
    value = elements[pos];					\
    								\
  end:								\
    va_end(args);						\
    scm_array_handle_release(&h);				\
    return value;						\
  }

DEFINE_ARRAY_GETTER(scm_c_array_ref, SCM, = SCM_UNSPECIFIED,
		    scm_array_handle_elements);

DEFINE_ARRAY_GETTER(scm_c_array_u8_ref, scm_t_uint8,,
		    scm_array_handle_u8_elements);
DEFINE_ARRAY_GETTER(scm_c_array_s8_ref, scm_t_int8,,
		    scm_array_handle_s8_elements);
DEFINE_ARRAY_GETTER(scm_c_array_u16_ref, scm_t_uint16,,
		    scm_array_handle_u16_elements);
DEFINE_ARRAY_GETTER(scm_c_array_s16_ref, scm_t_int16,,
		    scm_array_handle_s16_elements);
DEFINE_ARRAY_GETTER(scm_c_array_u32_ref, scm_t_uint32,,
		    scm_array_handle_u32_elements);
DEFINE_ARRAY_GETTER(scm_c_array_s32_ref, scm_t_int32,,
		    scm_array_handle_s32_elements);
DEFINE_ARRAY_GETTER(scm_c_array_u64_ref, scm_t_uint64,,
		    scm_array_handle_u64_elements);
DEFINE_ARRAY_GETTER(scm_c_array_s64_ref, scm_t_int64,,
		    scm_array_handle_s64_elements);
DEFINE_ARRAY_GETTER(scm_c_array_f32_ref, float,,
		    scm_array_handle_f32_elements);
DEFINE_ARRAY_GETTER(scm_c_array_f64_ref, double,,
		    scm_array_handle_f64_elements);
DEFINE_ARRAY_GETTER(scm_c_array_c32_ref, float,,
		    scm_array_handle_c32_elements);
DEFINE_ARRAY_GETTER(scm_c_array_c64_ref, double,,
		    scm_array_handle_c64_elements);

#undef DEFINE_ARRAY_GETTER

#define DEFINE_ARRAY_SETTER(name, type, getter)	\
  static inline type						\
  name(SCM array, type value, ...) {				\
    va_list args;						\
    scm_t_array_handle h;					\
    scm_t_array_dim *dims;					\
    ssize_t i, rank, index, pos = 0;				\
    type *elements;						\
    								\
    va_start(args, value);					\
    scm_array_get_handle(array, &h);				\
    								\
    rank = scm_array_handle_rank(&h);				\
    dims = scm_array_handle_dims(&h);				\
    								\
    for(i = 0; i < rank; ++i) {					\
      index = va_arg(args, ssize_t);				\
      if (index < dims[i].lbnd || index > dims[i].ubnd) {	\
	goto end;						\
      }								\
      pos += (index - dims[i].lbnd) * dims[i].inc;		\
    }								\
    elements = getter(&h);					\
    elements[pos] = value;					\
    								\
  end:								\
    va_end(args);						\
    scm_array_handle_release(&h);				\
    return value;						\
  }

DEFINE_ARRAY_SETTER(scm_c_array_set_x, SCM,
		    scm_array_handle_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_u8_set_x, scm_t_uint8,
		    scm_array_handle_u8_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_s8_set_x, scm_t_int8,
		    scm_array_handle_s8_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_u16_set_x, scm_t_uint16,
		    scm_array_handle_u16_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_s16_set_x, scm_t_int16,
		    scm_array_handle_s16_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_u32_set_x, scm_t_uint32,
		    scm_array_handle_u32_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_s32_set_x, scm_t_int32,
		    scm_array_handle_s32_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_u64_set_x, scm_t_uint64,
		    scm_array_handle_u64_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_s64_set_x, scm_t_int64,
		    scm_array_handle_s64_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_f32_set_x, float,
		    scm_array_handle_f32_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_f64_set_x, double,
		    scm_array_handle_f64_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_c32_set_x, float,
		    scm_array_handle_c32_writable_elements);
DEFINE_ARRAY_SETTER(scm_c_array_c64_set_x, double,
		    scm_array_handle_c64_writable_elements);

static inline float
scm_to_float(SCM number) {
  return (float) scm_to_double(number);
}

static inline SCM
scm_from_float(float number) {
  return scm_from_double((float) number);
}

static inline int 
isnt(SCM value) {
  return scm_is_false(value);
}

static inline int 
indeed(SCM value) {
  return scm_is_true(value);
}

static inline int 
equal(SCM a, SCM b) {
  return scm_is_true(scm_equal_p(a, b));
}

static inline char *
as_c_string(SCM object) {
  SCM port = scm_open_output_string();
  scm_display(object, port);
  SCM string = scm_get_output_string(port);
  scm_close_port(port);
  return scm_to_utf8_string(string);
}

static inline SCM 
eval(const char *str) {
  return scm_c_eval_string(str);
}

static inline SCM 
evalf(const char *fmt, ...) {
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

static inline void
cond_expand_provide(const char *symbols) {
  evalf("(cond-expand-provide (current-module) '(%s))", symbols);
}

static inline SCM 
symbol(const char *str) {
  return scm_from_utf8_symbol(str);
}

static inline int 
is_scm_defined(SCM s) {
  return scm_is_true(scm_defined_p(s, SCM_UNDEFINED));
}

static inline int 
is_scm_procedure(SCM s) {
  return scm_is_true(scm_procedure_p(s));
}

static inline int
scm_is_thunk(SCM s) {
  return scm_is_true(scm_thunk_p(s));
}

#ifndef scm_is_procedure
# define scm_is_procedure is_scm_procedure
#endif // scm_is_procedure

static inline int 
stands_for_scm_procedure(SCM s) {
  return is_scm_procedure(scm_primitive_eval(s));
}

static inline int 
is_scm_macro(SCM s) {
  return scm_is_true(scm_macro_p(s));
}

static inline int 
stands_for_scm_macro(SCM symbol) {
  return is_scm_macro(scm_primitive_eval(symbol));
}

static inline int 
is_scm_symbol(SCM s) {
  return scm_is_true(scm_symbol_p(s));
}

static inline int 
is_scm_hash_table(SCM s) {
  return scm_is_true(scm_hash_table_p(s));
}

static inline int 
is_scm_pair(SCM s) {
  return scm_is_true(scm_pair_p(s));
}

static inline void 
release_scm(SCM s) {
  scm_gc_unprotect_object(s);
}

static inline void 
hold_scm(SCM s) {
  scm_gc_protect_object(s);
}

static inline SCM
gc_protected(SCM s) {
  scm_gc_protect_object(s);
  return s;
}

#endif /* EXTEND_H */
