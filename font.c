#include "slayer.h"
#include <SDL/SDL_ttf.h>
#include "image.h"
#include "utils.h"

#ifdef USE_OPENGL
#include "video.h"
#endif

scm_t_bits font_tag;

static size_t free_font(SCM font_smob) {
  TTF_Font *font = (TTF_Font *) SCM_SMOB_DATA(font_smob);
  TTF_CloseFont(font);
  return 0;
}

static int print_font(SCM font, SCM port, scm_print_state *state) {
  char *string;
  if(asprintf(&string, "#<font %p>", (void *) SCM_SMOB_DATA(font)) == -1)
    return 0;
  scm_puts(string, port);
  free(string);
  scm_remember_upto_here_1(font);
  return 1;
}

SCM load_font(SCM path, SCM ptsize) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL)
    return SCM_BOOL_F;
  TTF_Font *font = TTF_OpenFont(filename, scm_to_int(ptsize));
  free(filename);
  SCM_NEWSMOB(smob, font_tag, font);
  return smob;
}

SCM set_font_style(SCM font, SCM style) {  
  TTF_SetFontStyle((TTF_Font *) SCM_SMOB_DATA(font), scm_to_int(style));
  return SCM_UNSPECIFIED;
}

SCM render_text(SCM text, SCM font, SCM color, SCM bgcolor) {
  //SCM smob;

  char *string = as_c_string(text);
  SDL_Surface *surface = NULL;
  if (string == NULL)
    return SCM_BOOL_F;

  TTF_Font *ttf = (TTF_Font *) SCM_SMOB_DATA(font);

  if (color == SCM_UNDEFINED) {
    color = scm_from_uint(0xffffff);
  }

  if (!*string) {
    surface = sdl_surface(1, TTF_FontLineSkip(ttf), 1);
  }
  if (!surface && (bgcolor == SCM_UNDEFINED)) {
    surface = TTF_RenderUTF8_Blended(ttf, string, sdl_color(scm_to_uint(color)));
    bgcolor = scm_from_uint(0);
  }
  if (!surface) {
    surface = TTF_RenderUTF8_Shaded(ttf, string, sdl_color(scm_to_uint(color)),
				    sdl_color(scm_to_uint(bgcolor)));
  }
  
  free(string);

  if (!surface) {
    WARN("failed to render text '%s'", string);
    surface = sdl_surface(1, TTF_FontLineSkip(ttf), 1);
  }


  SCM smob = rectangle(scm_from_int(surface->w), scm_from_int(surface->h), 
		       (bgcolor == SCM_UNDEFINED) 
		       ? scm_from_uint(0xff000000)
		       : bgcolor, scm_from_int(4));
  
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(smob);
  
  if (!image) {
    //image = render;
    WARN("failed to create image");
    SCM_SET_SMOB_DATA(smob, surface);
  } 
  else {
    SDL_BlitSurface(surface, NULL, image, NULL);
    SDL_FreeSurface(surface);
  }

  //print_sdl_surface(image);
  //SCM_NEWSMOB(smob, image_tag, image);
  scm_remember_upto_here_1(font);
  return smob;
}

SCM font_line_skip(SCM font) {
  TTF_Font *ttf = (TTF_Font *) SCM_SMOB_DATA(font);
  int skip = TTF_FontLineSkip(ttf);
  scm_remember_upto_here_1(font);
  return scm_from_int(skip);
}

static void export_functions() {
  scm_c_define_gsubr("load-font", 2, 0, 0, load_font);
  scm_c_define_gsubr("render-text", 2, 2, 0, render_text);
  scm_c_define_gsubr("set-font-style!", 2, 0, 0, set_font_style);
  scm_c_define_gsubr("font-line-skip", 1, 0, 0, font_line_skip);
}

void font_init() {
  TTF_Init();
  font_tag = scm_make_smob_type("font", sizeof(TTF_Font *));
  scm_set_smob_free(font_tag, free_font);
  scm_set_smob_print(font_tag, print_font);  
  export_functions();
}
