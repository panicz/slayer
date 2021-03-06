#include "slayer.h"
#include <SDL/SDL_ttf.h>
#include "utils.h"
#include "video.h"
#include "image.h"

scm_t_bits font_tag;

SDL_PixelFormat rgba32 = {
  .palette = NULL,
  .BitsPerPixel = 32,
  .BytesPerPixel = 4,
  .Rloss = 0, .Gloss = 0, .Bloss = 0, .Aloss = 0,
  .Rshift = 0, .Gshift = 8, .Bshift = 16, .Ashift = 24,
  .Rmask = 0xff, .Gmask = 0xff00, .Bmask = 0xff0000, .Amask = 0xff000000,
  .colorkey = 0,
  .alpha = 0xff
};

static size_t 
free_font(SCM font_smob) {
  TTF_Font *font = (TTF_Font *) SCM_SMOB_DATA(font_smob);
  TTF_CloseFont(font);
  return 0;
}

static int 
print_font(SCM font, SCM port, scm_print_state *state) {
  char *string;
  if(asprintf(&string, "#<font %p>", (void *) SCM_SMOB_DATA(font)) == -1)
    return 0;
  scm_puts(string, port);
  free(string);
  scm_remember_upto_here_1(font);
  return 1;
}

SCM 
load_font(SCM path, SCM ptsize) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL) {
    return SCM_BOOL_F;
  }
  TTF_Font *font = TTF_OpenFont(filename, scm_to_int(ptsize));
  free(filename);
  SCM_NEWSMOB(smob, font_tag, font);
  return smob;
}

SCM 
set_font_style(SCM font, SCM style) {  
  TTF_SetFontStyle((TTF_Font *) SCM_SMOB_DATA(font), scm_to_int(style));
  return SCM_UNSPECIFIED;
}


SCM 
render_text(SCM text, SCM font, SCM color, SCM bgcolor) {
  SDL_Surface *surface = NULL;

  char *string = as_c_string(text);
  if (string == NULL)
    return SCM_BOOL_F;
  TTF_Font *ttf = (TTF_Font *) SCM_SMOB_DATA(font);

  if (!GIVEN(color)) {
    color = scm_from_uint(0xffffff);
  }

  if (!*string) {
    surface = sdl_surface(1, TTF_FontLineSkip(ttf), 1);
  } 
  else {
    surface = (GIVEN(bgcolor) && scm_is_integer(bgcolor))
      ? TTF_RenderUTF8_Shaded(ttf, string, sdl_color(scm_to_uint(color)), 
			      sdl_color(scm_to_uint(bgcolor)))
      : TTF_RenderUTF8_Blended(ttf, string, sdl_color(scm_to_uint(color)));
  }

  if (!surface) {
    WARN("failed to render text '%s'", string);
    surface = sdl_surface(1, TTF_FontLineSkip(ttf), 1);
  }
  free(string);

  if (!surface) {
    WARN("unable to create surface");
    return SCM_BOOL_F;
  }
  SDL_Surface *image = SDL_ConvertSurface(surface, &rgba32, SDL_SRCALPHA);
  SDL_FreeSurface(surface);
  if(video_mode & SDL_OPENGL) {
    SCM tex = texture_smob_from_pixel_data(image->w, image->h, image->pixels,
					   NO(mirror_x), DO(mirror_y));
    SDL_FreeSurface(image);
    return tex;
  }
  scm_remember_upto_here_1(font);
  return surface_smob(image, AS(x, 0), AS(y, 0), image->w, image->h, 
		      IMAGE_ACCESS_PROXY);
}

SCM 
font_line_skip(SCM font) {
  TTF_Font *ttf = (TTF_Font *) SCM_SMOB_DATA(font);
  int skip = TTF_FontLineSkip(ttf);
  scm_remember_upto_here_1(font);
  return scm_from_int(skip);
}

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("load-font", 2, 0, 0, load_font);
  EXPORT_PROCEDURE("render-text", 2, 2, 0, render_text);
  EXPORT_PROCEDURE("set-font-style!", 2, 0, 0, set_font_style);
  EXPORT_PROCEDURE("font-line-skip", 1, 0, 0, font_line_skip);

#undef EXPORT_PROCEDURE
}

void 
font_init() {
  TTF_Init();
  font_tag = scm_make_smob_type("font", sizeof(TTF_Font *));
  scm_set_smob_free(font_tag, free_font);
  scm_set_smob_print(font_tag, print_font);  
  scm_c_define_module("slayer font", export_symbols, NULL);
}
