#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "extend.h"
#include "video.h"

static scm_t_bits image_tag;

static inline SDL_Rect sdl_rect(Sint16 x, Sint16 y, Uint16 w, Uint16 h) {
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  return rect;
}

static size_t free_image(SCM image_smob) {
  SDL_Surface *surface = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SDL_FreeSurface(surface);
  return 0;
}

static int print_image(SCM image_smob, SCM port, scm_print_state *pstate) {
  char *string;
  if(asprintf(&string, "#<image %p>", (void *) SCM_SMOB_DATA(image_smob)) == -1)
    return 0;
  scm_puts(string, port);
  free(string);
  return 1;
}

SCM load_image(SCM path) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL)
    return SCM_BOOL_F;
  SDL_Surface *image = IMG_Load(filename);
  free(filename);
  SCM_NEWSMOB(smob, image_tag, image);
  return smob;
}

SCM draw_image(SCM image_smob, SCM x, SCM y) {
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SDL_Rect area = sdl_rect(scm_to_int16(x), scm_to_int16(y), -1, -1);
  SDL_BlitSurface(image, NULL, screen, &area);
  SDL_Flip(screen);
  return SCM_UNSPECIFIED;
}

static void export_functions() {
  scm_c_define_gsubr("load-image", 1, 0, 0, load_image);
  scm_c_define_gsubr("draw-image", 3, 0, 0, draw_image);
}



void image_init() {
  image_tag = scm_make_smob_type("image", sizeof(SDL_Surface *));
  scm_set_smob_free(image_tag, free_image);
  scm_set_smob_print(image_tag, print_image);
  export_functions();
}
