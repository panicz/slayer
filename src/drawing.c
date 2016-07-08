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

Uint32 reciprocals[256];

static inline void
reciprocals_init () {
  int i;
  for(i = 0; i < NELEMS(reciprocals); ++i) {
    reciprocals[i] = i ? ((255*(1<<16)) + (i-1))/i : 0;
  }
}

static Uint32
unpremultiply(Uint32 pixel) {
  return pixel;
}

static Uint32
premultiply(Uint32 pixel) {
  return pixel;
}

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
		      mapUint32((Uint32 *) context->target,
				premultiply,
				(Uint32 *) surface->pixels,
				surface->w * surface->h));
  context->cairo = cairo_create(context->target);
  return context;
}

static void
destroy_drawing_context(drawing_context_t *context) {
  cairo_destroy(context->cairo);
  cairo_surface_destroy(context->target);
  free(context);
}

static void
flush_drawing_context(drawing_context_t *context) {
  WITH_LOCKED_SURFACE(context->surface,
		      mapUint32((Uint32 *) context->surface->pixels,
				unpremultiply,
				(Uint32 *) context->target,
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
  SDL_Surface *surface = SURFACE(image);
  flush_drawing_context((drawing_context_t *) drawing_contexts->data);
  destroy_drawing_context((drawing_context_t *) drawing_contexts->data);
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

static inline SCM
c_with_output_to_surface(SCM image, SCM (*proc)(void *), void *data) {
  scm_assert_smob_type(image_tag, image);
  SCM_ASSERT_TYPE(IS_SURFACE(image), image, SCM_ARG1, __FUNCTION__,
		  "surface (not texture)");
  BEGIN_DRAWING_CONTEXT(image);
  SCM result = proc(data);
  END_DRAWING_CONTEXT();
  return result;
}

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

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)		\
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);	\
  scm_c_export(name,NULL)
  
  EXPORT_PROCEDURE("with-output-to-surface", 2, 0, 0, with_output_to_surface);

#undef EXPORT_PROCEDURE
}

void 
drawing_init() {
  scm_c_define_module("slayer drawing", export_symbols, NULL);
}
