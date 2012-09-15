#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "utils.h"
#include "slayer.h"
#include "extend.h"
#include "video.h"

scm_t_bits image_tag;


static size_t free_image(SCM image_smob) {
  SDL_Surface *surface = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SDL_FreeSurface(surface);
  return 0;
}

static int print_image(SCM image, SCM port, scm_print_state *pstate) {
  char *string;
  if(asprintf(&string, "#<image %p>", (void *) SCM_SMOB_DATA(image)) == -1)
    return 0;
  scm_puts(string, port);
  free(string);
  scm_remember_upto_here_1(image);
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
  scm_assert_smob_type(image_tag, image_smob);
  
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SDL_Rect area = sdl_rect(scm_to_int16(x), scm_to_int16(y), -1, -1);
  SDL_BlitSurface(image, NULL, screen, &area);
  scm_remember_upto_here_1(image_smob);
  return SCM_UNSPECIFIED;
}

SCM image_width(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM w = scm_from_int(image->w);
  scm_remember_upto_here_1(image_smob);
  return w;
}

SCM image_height(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM h = scm_from_int(image->h);
  scm_remember_upto_here_1(image_smob);
  return h;
}

SCM image_size(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);

  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM l = scm_list_2(scm_from_int(image->w), scm_from_int(image->h));
  scm_remember_upto_here_1(image_smob);
  return l;
}

SCM rectangle(SCM w, SCM h, SCM color) {
  SCM smob;
  SDL_Surface *image = sdl_surface(scm_to_int(w), scm_to_int(h));
  if(color != SCM_UNDEFINED) {
    SDL_Color c = sdl_color(scm_to_uint(color));
    SDL_FillRect(image, NULL, SDL_MapRGBA(image->format, c.r, c.g, c.b, 0xff-c.unused));
  }
  SCM_NEWSMOB(smob, image_tag, image);
  return smob;
}

static void export_functions() {
  scm_c_define_gsubr("rectangle", 2, 1, 0, rectangle);
  scm_c_define_gsubr("load-image", 1, 0, 0, load_image);
  scm_c_define_gsubr("draw-image", 3, 0, 0, draw_image);
  scm_c_define_gsubr("image-width", 1, 0, 0, image_width);
  scm_c_define_gsubr("image-height", 1, 0, 0, image_height);
  scm_c_define_gsubr("image-size", 1, 0, 0, image_size);
}

void image_init() {
  image_tag = scm_make_smob_type("image", sizeof(SDL_Surface *));
  scm_set_smob_free(image_tag, free_image);
  scm_set_smob_print(image_tag, print_image);
  export_functions();
}
