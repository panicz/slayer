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

static struct list *drawing_contexts = NULL;

#define CURRENT_DRAWING_CONTEXT	((cairo_t *)(drawing_contexts->data))

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

static inline cairo_font_slant_t
scm_to_font_slant_t(SCM symbol) {
  if(scm_is_eq(symbol, s_italic)) {
    return CAIRO_FONT_SLANT_ITALIC;
  }
  else if(scm_is_eq(symbol, s_oblique)) {
    return CAIRO_FONT_SLANT_OBLIQUE;
  }
  else if(scm_is_eq(symbol, s_normal)) {
    return CAIRO_FONT_SLANT_NORMAL;
  }
  else {
    char *name = as_c_string(symbol);
    WARN("Unsupported font slant value: %s", name);
    free(name);
    return CAIRO_FONT_SLANT_NORMAL;
  }
}

static inline SCM
scm_from_font_slant_t(cairo_font_slant_t slant) {
  switch(slant) {
  case CAIRO_FONT_SLANT_ITALIC:
    return s_italic;
  case CAIRO_FONT_SLANT_OBLIQUE:
    return s_oblique;
  case CAIRO_FONT_SLANT_NORMAL:
  default:
    return s_normal;
  }
}

static inline cairo_font_weight_t
scm_to_font_weight_t(SCM symbol) {
  if(scm_is_eq(symbol, s_bold)) {
    return CAIRO_FONT_WEIGHT_BOLD;
  }
  else if(scm_is_eq(symbol, s_normal)) {
    return CAIRO_FONT_WEIGHT_NORMAL;
  }
  else {
    char *name = as_c_string(symbol);
    WARN("Unsupported font weight value: %s", name);
    free(name);
    return CAIRO_FONT_WEIGHT_NORMAL;
  }
}

static inline SCM
scm_from_font_weight_t(cairo_font_weight_t weight) {
  switch(weight) {
  case CAIRO_FONT_WEIGHT_BOLD:
    return s_bold;
  case CAIRO_FONT_WEIGHT_NORMAL:
  default:
    return s_normal;
  }
}

#define PRECISION 16

static Uint32 reciprocals[256];

static inline void
reciprocals_init () {
  int i;
  for(i = 0; i < NELEMS(reciprocals); ++i) {
    reciprocals[i] = i ? ((255*(1<<PRECISION)) + (i-1))/i : 0;
  }
}

static inline Uint32
unpremultiply(Uint32 pixel) {
  static Uint32 previous_pixel = 0;
  static Uint32 previous_value = 0;
  if(pixel == previous_pixel) {
    return previous_value;
  }
  else {
    SDL_Color c = sdl_color(pixel);
    Uint32 R = reciprocals[c.unused];
    previous_pixel = pixel;
    return previous_value = (Uint32)
      ((SHIFT_LEFT(c.r * R, COLOR_SHIFT_RED - PRECISION) & COLOR_MASK_RED)
       | (SHIFT_LEFT(c.g * R, COLOR_SHIFT_GREEN - PRECISION) & COLOR_MASK_GREEN)
       | (SHIFT_LEFT(c.b * R, COLOR_SHIFT_BLUE - PRECISION) & COLOR_MASK_BLUE)
       | (pixel & COLOR_MASK_ALPHA));
  }
}

static inline Uint32
premultiply(Uint32 pixel) {
  static Uint32 previous_pixel = 0;
  static Uint32 previous_value = 0;
  if(pixel == previous_pixel) {
    return previous_value;
  }
  else {
    SDL_Color c = sdl_color(pixel);
    previous_pixel = pixel;
    return previous_value = (Uint32)
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
}

#undef PRECISION

static cairo_user_data_key_t const CAIROSDL_TARGET_KEY[1] = {{1}};

cairo_t *
create_drawing_context(SDL_Surface *surface) {
  SDL_PixelFormat *format = surface->format;
  cairo_surface_t *target;
  cairo_t *cairo;
  if(format->BytesPerPixel != 4
     || format->BitsPerPixel != 32
     || format->Rmask != COLOR_MASK_RED
     || format->Gmask != COLOR_MASK_GREEN
     || format->Bmask != COLOR_MASK_BLUE) {
    WARN("The format of SDL surface <%p> not supported by Cairo", surface);
    target = cairo_image_surface_create((cairo_format_t) -1, 0, 0);
  }
  else if(format->Amask == COLOR_MASK_ALPHA) {
    target = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
					surface->w, surface->h);
    WITH_LOCKED_SURFACE
      (surface,
       mapUint32((Uint32 *)
		 cairo_image_surface_get_data(target),
		 unpremultiply,
		 (Uint32 *) surface->pixels,
		 surface->w * surface->h)
       );
  }
  else {
    target = cairo_image_surface_create_for_data(surface->pixels,
						 CAIRO_FORMAT_RGB24,
						 surface->w,
						 surface->h,
						 surface->pitch);
  }
  surface->refcount++;
  cairo_surface_set_user_data(target,
			      CAIROSDL_TARGET_KEY,
			      surface,
			      (PROC) SDL_FreeSurface);

  cairo = cairo_create(target);
  return cairo;
}

static inline SDL_Surface *
target_surface(cairo_surface_t *target) {
  return (SDL_Surface *)
    cairo_surface_get_user_data(target, CAIROSDL_TARGET_KEY);
}

static inline void
flush_drawing_context(cairo_t *cairo) {
  cairo_surface_t *target = cairo_get_target(cairo);
  SDL_Surface *surface = target_surface(target);
  cairo_surface_flush(target);
  if(surface->format->Amask == COLOR_MASK_ALPHA) {
    WITH_LOCKED_SURFACE(surface,
			mapUint32((Uint32 *) surface->pixels,
				  premultiply,
				  (Uint32 *)
				  cairo_image_surface_get_data(target),
				  surface->w * surface->h));
  }
}

static inline void
enter_drawing_context(SDL_Surface *surface) {
  drawing_contexts = cons(create_drawing_context(surface), drawing_contexts);
  if(SDL_MUSTLOCK(surface)) {
    TRY_SDL(SDL_LockSurface(surface));
  }
}

static void
on_enter_drawing_context(SCM image) {
  enter_drawing_context(SURFACE(image));
  scm_gc_protect_object(image);
}

static inline void
exit_drawing_context(SDL_Surface *surface)
{
  cairo_surface_t *target;
  if(SDL_MUSTLOCK(surface)) {
    SDL_UnlockSurface(surface);
  }

  flush_drawing_context(CURRENT_DRAWING_CONTEXT);
  target = cairo_get_target(CURRENT_DRAWING_CONTEXT);
  cairo_destroy(CURRENT_DRAWING_CONTEXT);
  cairo_surface_destroy(target); 
  drawing_contexts = decap(drawing_contexts);
}

static void
on_exit_drawing_context(SCM image) {
  exit_drawing_context(SURFACE(image));
  scm_gc_unprotect_object(image);
}

#define BEGIN_DRAWING_CONTEXT(surface)					\
  scm_dynwind_begin(SCM_F_DYNWIND_REWINDABLE);				\
  scm_dynwind_unwind_handler((PROC) on_exit_drawing_context,		\
			     surface, SCM_F_WIND_EXPLICITLY);		\
  scm_dynwind_rewind_handler((PROC) on_enter_drawing_context,		\
			     surface, SCM_F_WIND_EXPLICITLY)

#define END_DRAWING_CONTEXT() scm_dynwind_end()

static SCM
with_drawing_output_to_surface(SCM image, SCM thunk) {
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

#define BEGIN_MEASURE_CONTEXT()						\
  scm_dynwind_begin(SCM_F_DYNWIND_REWINDABLE);				\
  scm_dynwind_unwind_handler((PROC) on_exit_measure_context,		\
			     NULL, SCM_F_WIND_EXPLICITLY);		\
  scm_dynwind_rewind_handler((PROC) on_enter_measure_context,		\
			     NULL, SCM_F_WIND_EXPLICITLY)

#define END_MEASURE_CONTEXT() scm_dynwind_end()


static void
on_enter_measure_context() {
  cairo_surface_t *target
    = cairo_recording_surface_create(CAIRO_CONTENT_COLOR, NULL);
  drawing_contexts = cons(cairo_create(target), drawing_contexts);
}

static void
on_exit_measure_context() {
  cairo_surface_t *target = cairo_get_target(CURRENT_DRAWING_CONTEXT);
  cairo_destroy(CURRENT_DRAWING_CONTEXT);
  cairo_surface_destroy(target); 
  drawing_contexts = decap(drawing_contexts);
}

static SCM
measure_without_drawing(SCM thunk) {
  double x, y, w, h;
  SCM result;
  BEGIN_MEASURE_CONTEXT();
  scm_call_0(thunk);
  cairo_recording_surface_ink_extents(cairo_get_target(CURRENT_DRAWING_CONTEXT),
				      &x, &y, &w, &h);
  result = scm_list_4(scm_from_double(x), scm_from_double(y),
		      scm_from_double(x+w), scm_from_double(y+h));
  END_MEASURE_CONTEXT();
  return result;
}

#define DEF_CAIRO_GETTER_SETTER(property, type)				\
  static SCM								\
  set_##property(SCM v) {						\
    cairo_set_##property(CURRENT_DRAWING_CONTEXT, scm_to_##type(v));	\
    return SCM_UNSPECIFIED;						\
  }									\
  static SCM								\
  get_##property() {							\
    return								\
      scm_from_##type(cairo_get_##property(CURRENT_DRAWING_CONTEXT));	\
  }

DEF_CAIRO_GETTER_SETTER(line_width, double);
DEF_CAIRO_GETTER_SETTER(miter_limit, double);
DEF_CAIRO_GETTER_SETTER(tolerance, double);
DEF_CAIRO_GETTER_SETTER(antialias, antialias_t);
DEF_CAIRO_GETTER_SETTER(line_join, line_join_t);

#undef DEF_CAIRO_GETTER_SETTER

#define DEF_CAIRO_0(name)			\
  static SCM					\
  name##_x() {					\
    cairo_##name(CURRENT_DRAWING_CONTEXT);	\
    return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_2DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b) {			\
    cairo_##name(CURRENT_DRAWING_CONTEXT,	\
		 scm_to_double(a),		\
		 scm_to_double(b));		\
    return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_3DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b, SCM c) {		\
    cairo_##name(CURRENT_DRAWING_CONTEXT,	\
		 scm_to_double(a),		\
		 scm_to_double(b),		\
		 scm_to_double(c));		\
      return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_4DOUBLE(name)			\
  static SCM					\
  name##_x(SCM a, SCM b, SCM c, SCM d) {	\
    cairo_##name(CURRENT_DRAWING_CONTEXT,	\
		 scm_to_double(a),		\
		 scm_to_double(b),		\
		 scm_to_double(c),		\
		 scm_to_double(d));		\
      return SCM_UNSPECIFIED;			\
  }

#define DEF_CAIRO_5DOUBLE(name)				\
  static SCM						\
  name##_x(SCM a, SCM b, SCM c, SCM d, SCM e) {		\
    cairo_##name(CURRENT_DRAWING_CONTEXT,		\
		 scm_to_double(a),			\
		 scm_to_double(b),			\
		 scm_to_double(c),			\
		 scm_to_double(d),			\
		 scm_to_double(e));			\
    return SCM_UNSPECIFIED;				\
  }

#define DEF_CAIRO_6DOUBLE(name)				\
  static SCM						\
  name##_x(SCM a, SCM b, SCM c, SCM d, SCM e, SCM f) {	\
    cairo_##name(CURRENT_DRAWING_CONTEXT,		\
		 scm_to_double(a),			\
		 scm_to_double(b),			\
		 scm_to_double(c),			\
		 scm_to_double(d),			\
		 scm_to_double(e),			\
		 scm_to_double(f));			\
    return SCM_UNSPECIFIED;				\
  }

DEF_CAIRO_0(stroke);
DEF_CAIRO_0(fill);
DEF_CAIRO_0(paint);
DEF_CAIRO_0(new_path);
DEF_CAIRO_0(close_path);

DEF_CAIRO_2DOUBLE(move_to);
DEF_CAIRO_2DOUBLE(line_to);
DEF_CAIRO_3DOUBLE(set_source_rgb);
DEF_CAIRO_4DOUBLE(set_source_rgba);
DEF_CAIRO_4DOUBLE(rectangle);

DEF_CAIRO_5DOUBLE(arc);

DEF_CAIRO_6DOUBLE(curve_to);

#undef DEF_CAIRO_6DOUBLE
#undef DEF_CAIRO_5DOUBLE
#undef DEF_CAIRO_4DOUBLE
#undef DEF_CAIRO_3DOUBLE
#undef DEF_CAIRO_2DOUBLE
#undef DEF_CAIRO_0

static SCM
path_extents() {
  double x1, y1, x2, y2;
  cairo_path_extents(CURRENT_DRAWING_CONTEXT, &x1, &y1, &x2, &y2);
  return scm_list_4(scm_from_double(x1), scm_from_double(y1),
		    scm_from_double(x2), scm_from_double(y2));
}

static SCM
text_extents(SCM text) {
  char *string = as_c_string(text);
  cairo_text_extents_t e;
  cairo_text_extents(CURRENT_DRAWING_CONTEXT, string, &e);
  free(string);
  return scm_list_4(scm_from_double(e.x_bearing),
		    scm_from_double(e.y_bearing),
		    scm_from_double(e.x_bearing+e.x_advance),
		    scm_from_double(e.y_bearing+e.y_advance));
}

static SCM
set_font_face_x(SCM name, SCM slant, SCM weight) {
  char *font = as_c_string(name);
  cairo_select_font_face(CURRENT_DRAWING_CONTEXT,
			 font,
			 ((GIVEN(slant) && indeed(slant))
			  ? scm_to_font_slant_t(slant)
			  : CAIRO_FONT_SLANT_NORMAL),
			 ((GIVEN(weight) && indeed(weight))
			  ? scm_to_font_weight_t(weight)
			  : CAIRO_FONT_WEIGHT_NORMAL));
  free(font);
  return SCM_UNSPECIFIED;
}

static SCM
set_font_size_x(SCM size) {
  cairo_set_font_size(CURRENT_DRAWING_CONTEXT, scm_to_double(size));
  return SCM_UNSPECIFIED;
}

static SCM
show_text_x(SCM text) {
  char *string = as_c_string(text);
  cairo_show_text(CURRENT_DRAWING_CONTEXT, string);
  free(string);
  return SCM_UNSPECIFIED;
}

static void 
export_symbols(void *unused) {

#define EXPORT_PROCEDURE(name, arity, proc)			\
  scm_c_define_gsubr(name, arity, 0, 0, (scm_t_subr)proc);	\
  scm_c_export(name, NULL)

#define EXPORT_PROCEDURE_WITH_OPTIONALS(name, arity, opt, proc)	\
  scm_c_define_gsubr(name, arity, opt, 0, (scm_t_subr)proc);	\
  scm_c_export(name, NULL)

#define EXPORT_GETTER_SETTER(c_property, s_property)		\
  EXPORT_PROCEDURE("set-" s_property "!", 1, set_##c_property);	\
  EXPORT_PROCEDURE(s_property, 0, get_##c_property)

  EXPORT_PROCEDURE("with-drawing-output-to-surface", 2,
		   with_drawing_output_to_surface);

  EXPORT_PROCEDURE("measure-without-drawing", 1,
		   measure_without_drawing);

  EXPORT_PROCEDURE("path-extents", 0, path_extents);
  EXPORT_PROCEDURE("text-extents", 1, text_extents);
  
  EXPORT_PROCEDURE("move-to!", 2, move_to_x);
  EXPORT_PROCEDURE("line-to!", 2, line_to_x);
  EXPORT_PROCEDURE("curve-to!", 6, curve_to_x);

  EXPORT_PROCEDURE("arc!", 5, arc_x);
  
  EXPORT_PROCEDURE_WITH_OPTIONALS("set-font-face!", 1, 2, set_font_face_x);
  EXPORT_PROCEDURE("set-font-size!", 1, set_font_size_x);
  EXPORT_PROCEDURE("show-text!", 1, show_text_x);
  
  EXPORT_PROCEDURE("set-source-rgb!", 3, set_source_rgb_x);
  EXPORT_PROCEDURE("set-source-rgba!", 4, set_source_rgba_x);
  EXPORT_PROCEDURE("rectangle!", 4, rectangle_x);
  
  EXPORT_PROCEDURE("stroke!", 0, stroke_x);
  EXPORT_PROCEDURE("fill!", 0, fill_x);
  EXPORT_PROCEDURE("paint!", 0, paint_x);
  EXPORT_PROCEDURE("new-path!", 0, new_path_x);
  EXPORT_PROCEDURE("close-path!", 0, close_path_x);
  
  EXPORT_GETTER_SETTER(line_width, "line-width");
  EXPORT_GETTER_SETTER(miter_limit, "miter-limit");
  EXPORT_GETTER_SETTER(tolerance, "tolerance");
  EXPORT_GETTER_SETTER(antialias, "antialias");
  EXPORT_GETTER_SETTER(line_join, "line-join");

#undef EXPORT_GETTER_SETTER
#undef EXPORT_PROCEDURE_WITH_OPTIONALS
#undef EXPORT_PROCEDURE
}

void 
drawing_init() {
  reciprocals_init();
  drawing_contexts = cons(create_drawing_context(screen), drawing_contexts);
  scm_c_define_module("slayer drawing", export_symbols, NULL);
  scm_c_define_module("slayer", (PROC) cond_expand_provide, "vector-graphics");
}
