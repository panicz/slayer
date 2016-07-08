#include <SDL/SDL.h>
#include <cairo.h>

#include "utils.h"
#include "slayer.h"
#include "extend.h"
#include "video.h"
#include "symbols.h"
#include "image.h"

/*
 * "SDL's 32 bit pixel formats with per pixel alpha are not supported 
 * directly by cairo because cairo and SDL disagree on the interpretation
 * of pixel values with alpha: 
 *
 * SDL uses unpremultiplied pixels but cairo uses premultiplied pixels."
 *
 * [ https://www.cairographics.org/SDL/ ]
 *
 * The code here was heavily inspired by the CairoSDL library:
 * https://cgit.freedesktop.org/~joonas/cairosdl/
 */

DEF_MAP(Uint32)

typedef struct {
  SDL_Surface *surface;
  cairo_t *cairo;
  cairo_surface_t *target;
} drawing_context_t;

struct list *drawing_contexts = NULL;

#define CURRENT_DRAWING_CONTEXT	((drawing_context_t *)(drawing_contexts->data))
#define CURRENT_CAIRO_STATE (CURRENT_DRAWING_CONTEXT->cairo)

static inline cairo_line_join_t
scm_to_line_join_t(SCM symbol) {
  if(scm_is_eq(symbol, s_bevel)) {
    return CAIRO_LINE_JOIN_BEVEL;
  }
  else if(scm_is_eq(symbol, s_round)) {
    return CAIRO_LINE_JOIN_ROUND;
  }
  else if(scm_is_eq(symbol, s_miter)) {
   return CAIRO_LINE_JOIN_MITER;
  }
  else {
    char *name = as_c_string(symbol);
    WARN("Unsupported line join value: %s", name);
    free(name);
    return CAIRO_LINE_JOIN_MITER;
  }
}

static inline SCM
scm_from_line_join_t(cairo_line_join_t line_join) {
  switch(line_join) {
  case CAIRO_LINE_JOIN_MITER:
    return s_miter;
  case CAIRO_LINE_JOIN_ROUND:
    return s_round;
  case CAIRO_LINE_JOIN_BEVEL:
    return s_bevel;
  default:
    WARN("unrecognized line join value: %i", line_join);
    return SCM_UNSPECIFIED;
  }
}

static inline cairo_antialias_t
scm_to_antialias_t(SCM symbol) {
  if(scm_is_eq(symbol, s_default)) {
    return CAIRO_ANTIALIAS_DEFAULT;
  }
  else if(scm_is_eq(symbol, s_none)) {
    return CAIRO_ANTIALIAS_NONE;
  }
  else if(scm_is_eq(symbol, s_gray)) {
    return CAIRO_ANTIALIAS_GRAY;
  }
  else if(scm_is_eq(symbol, s_subpixel)) {
    return CAIRO_ANTIALIAS_SUBPIXEL;
  }
  else if(scm_is_eq(symbol, s_fast)) {
    return CAIRO_ANTIALIAS_FAST;
  }
  else if(scm_is_eq(symbol, s_good)) {
    return CAIRO_ANTIALIAS_GOOD;
  }
  else if(scm_is_eq(symbol, s_best)) {
    return CAIRO_ANTIALIAS_BEST;
  }
  else {
    char *name = as_c_string(symbol);
    WARN("Unsupported antialias value: %s", name);
    free(name);
    return CAIRO_ANTIALIAS_DEFAULT;
  }
}

static inline SCM
scm_from_antialias_t(cairo_antialias_t antialias) {
  switch(antialias) {
  case CAIRO_ANTIALIAS_NONE:
    return s_none;
  case CAIRO_ANTIALIAS_SUBPIXEL:
    return s_subpixel;
  case CAIRO_ANTIALIAS_GRAY:
    return s_gray;
  case CAIRO_ANTIALIAS_FAST:
    return s_fast;
  case CAIRO_ANTIALIAS_GOOD:
    return s_good;
  case CAIRO_ANTIALIAS_BEST:
    return s_best;
  case CAIRO_ANTIALIAS_DEFAULT:
  default:
    return s_default;
  }
}

#define PRECISION 16

Uint32 reciprocals[256];

static inline void
reciprocals_init () {
  int i;
  for(i = 0; i < NELEMS(reciprocals); ++i) {
    reciprocals[i] = i ? ((255*(1<<PRECISION)) + (i-1))/i : 0;
  }
}

static inline Uint32
unpremultiply(Uint32 pixel) {
  SDL_Color c = sdl_color(pixel);
  Uint32 R = reciprocals[c.unused];
  return (Uint32)
    ((SHIFT_LEFT(c.r * R, COLOR_SHIFT_RED - PRECISION) & COLOR_MASK_RED)
     | (SHIFT_LEFT(c.g * R, COLOR_SHIFT_GREEN - PRECISION) & COLOR_MASK_GREEN)
     | (SHIFT_LEFT(c.b * R, COLOR_SHIFT_BLUE - PRECISION) & COLOR_MASK_BLUE)
     | (pixel & COLOR_MASK_ALPHA));
}

static inline Uint32
premultiply(Uint32 pixel) {
  SDL_Color c = sdl_color(pixel);
  return (Uint32)
    ((SHIFT_LEFT(c.r*c.unused*257 + (1 << (PRECISION-1)),
		 COLOR_SHIFT_RED - PRECISION)
      & COLOR_MASK_RED)
     | (SHIFT_LEFT(c.g*c.unused*257 + (1 << (PRECISION-1)),
		   COLOR_SHIFT_GREEN - PRECISION)
	& COLOR_MASK_GREEN)
     | (SHIFT_LEFT(c.b*c.unused*257 + (1 << (PRECISION-1)),
		   COLOR_SHIFT_BLUE - PRECISION)
	& COLOR_MASK_BLUE)
     | (pixel & COLOR_MASK_ALPHA));
}

#undef PRECISION

static inline drawing_context_t *
create_drawing_context(SDL_Surface *surface) {
  drawing_context_t *context = malloc(sizeof(drawing_context_t));
  SDL_PixelFormat *format = surface->format;
  context->surface = surface;
  if(format->BytesPerPixel != 4
     || format->BitsPerPixel != 32
     || format->Rmask != COLOR_MASK_RED
     || format->Gmask != COLOR_MASK_GREEN
     || format->Bmask != COLOR_MASK_BLUE
     || format->Amask != COLOR_MASK_ALPHA) {
    WARN("The format of SDL surface <%p> not supported by Cairo", surface);
    context->target = cairo_image_surface_create((cairo_format_t) -1, 0, 0);
    context->cairo = cairo_create(context->target);
    return context;
  }  
  context->target = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
					       surface->w, surface->h);
  WITH_LOCKED_SURFACE(surface,
		      mapUint32((Uint32 *)
				cairo_image_surface_get_data(context->target),
				unpremultiply,
				(Uint32 *) surface->pixels,
				surface->w * surface->h));

  context->cairo = cairo_create(context->target);
  return context;
}

static inline void
destroy_drawing_context(drawing_context_t *context) {
  cairo_destroy(context->cairo);
  cairo_surface_destroy(context->target);
  free(context);
}

static inline void
flush_drawing_context(drawing_context_t *context) {
  WITH_LOCKED_SURFACE(context->surface,
		      mapUint32((Uint32 *) context->surface->pixels,
				premultiply,
				(Uint32 *)
				cairo_image_surface_get_data(context->target),
				context->surface->w * context->surface->h));
}

static void
on_enter_drawing_context(SCM image) {
  SDL_Surface *surface = SURFACE(image);
  scm_gc_protect_object(image);
  drawing_contexts = cons(create_drawing_context(surface), drawing_contexts);
}
			 	
static void
on_exit_drawing_context(SCM image) {
  flush_drawing_context(CURRENT_DRAWING_CONTEXT);
  destroy_drawing_context(CURRENT_DRAWING_CONTEXT);
  drawing_contexts = decap(drawing_contexts);  
  scm_gc_unprotect_object(image);
}

#define BEGIN_DRAWING_CONTEXT(surface)					\
  scm_dynwind_begin(SCM_F_DYNWIND_REWINDABLE);				\
  scm_dynwind_unwind_handler((void(*)(void *)) on_exit_drawing_context,	\
			     surface, SCM_F_WIND_EXPLICITLY);		\
  scm_dynwind_rewind_handler((void(*)(void *)) on_enter_drawing_context, \
			     surface, SCM_F_WIND_EXPLICITLY)

#define END_DRAWING_CONTEXT() scm_dynwind_end()

static SCM
with_output_to_surface(SCM image, SCM thunk) {
  scm_assert_smob_type(image_tag, image);
  SCM_ASSERT_TYPE(IS_SURFACE(image), image, SCM_ARG1, __FUNCTION__,
		  "surface (not texture)");
  BEGIN_DRAWING_CONTEXT(image);
  SCM result = scm_call_0(thunk);
  END_DRAWING_CONTEXT();
  return result;
}

#undef END_DRAWING_CONTEXT
#undef BEGIN_DRAWING_CONTEXT

#define DEF_CAIRO_GETTER_SETTER(property, type)				\
  static SCM								\
  set_##property(SCM v) {						\
    cairo_set_##property(CURRENT_CAIRO_STATE, scm_to_##type(v));	\
    return SCM_UNSPECIFIED;						\
  }									\
  static SCM								\
  get_##property() {							\
    return								\
      scm_from_##type(cairo_get_##property(CURRENT_CAIRO_STATE));	\
  }

DEF_CAIRO_GETTER_SETTER(line_width, double);
DEF_CAIRO_GETTER_SETTER(miter_limit, double);
DEF_CAIRO_GETTER_SETTER(tolerance, double);
DEF_CAIRO_GETTER_SETTER(antialias, antialias_t);
DEF_CAIRO_GETTER_SETTER(line_join, line_join_t);

#define DEF_CAIRO_0(name)			\
  static SCM					\
  name##_x() {					\
    cairo_##name(CURRENT_CAIRO_STATE);		\
    return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_2DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b) {			\
    cairo_##name(CURRENT_CAIRO_STATE,		\
		 scm_to_double(a),		\
		 scm_to_double(b));		\
    return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_3DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b, SCM c) {		\
    cairo_##name(CURRENT_CAIRO_STATE,		\
		 scm_to_double(a),		\
		 scm_to_double(b),		\
		 scm_to_double(c));		\
      return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_4DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b, SCM c, SCM d) {	\
    cairo_##name(CURRENT_CAIRO_STATE,		\
		 scm_to_double(a),		\
		 scm_to_double(b),		\
		 scm_to_double(c),		\
		 scm_to_double(d));		\
      return SCM_UNSPECIFIED;			\
  }

DEF_CAIRO_0(stroke);
DEF_CAIRO_0(fill);
DEF_CAIRO_0(paint);

DEF_CAIRO_2DOUBLE(move_to);
DEF_CAIRO_2DOUBLE(line_to);
DEF_CAIRO_3DOUBLE(set_source_rgb);
DEF_CAIRO_4DOUBLE(set_source_rgba);
DEF_CAIRO_4DOUBLE(rectangle);

static void 
export_symbols(void *unused) {

#define EXPORT_PROCEDURE(name, arity, proc)			\
  scm_c_define_gsubr(name, arity, 0, 0, (scm_t_subr)proc);	\
  scm_c_export(name, NULL)

#define EXPORT_GETTER_SETTER(c_property, s_property)		\
  EXPORT_PROCEDURE("set-" s_property "!", 1, set_##c_property);	\
  EXPORT_PROCEDURE(s_property, 0, get_##c_property)

  EXPORT_PROCEDURE("with-output-to-surface", 2, with_output_to_surface);

  EXPORT_PROCEDURE("move-to!", 2, move_to_x);
  EXPORT_PROCEDURE("line-to!", 2, line_to_x);
  
  EXPORT_PROCEDURE("set-source-rgb!", 3, set_source_rgb_x);
  EXPORT_PROCEDURE("set-source-rgba!", 4, set_source_rgba_x);
  EXPORT_PROCEDURE("rectangle!", 4, rectangle_x);
  
  EXPORT_PROCEDURE("stroke!", 0, stroke_x);
  EXPORT_PROCEDURE("fill!", 0, fill_x);
  EXPORT_PROCEDURE("paint!", 0, paint_x);
  
  EXPORT_GETTER_SETTER(line_width, "line-width");
  EXPORT_GETTER_SETTER(miter_limit, "miter-limit");
  EXPORT_GETTER_SETTER(tolerance, "tolerance");
  EXPORT_GETTER_SETTER(antialias, "antialias");
  EXPORT_GETTER_SETTER(line_join, "line-join");

#undef EXPORT_GETTER_SETTER
#undef EXPORT_PROCEDURE
}

void 
drawing_init() {
  reciprocals_init();
  scm_c_define_module("slayer drawing", export_symbols, NULL);
}
