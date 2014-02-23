#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "utils.h"
#include "slayer.h"
#include "extend.h"
#include "video.h"
#include "symbols.h"

scm_t_bits image_tag;

static size_t 
free_image(SCM image_smob) {
  SDL_Surface *surface = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SDL_FreeSurface(surface);
  return 0;
}

static int 
print_image(SCM image, SCM port, scm_print_state *pstate) {
  char *string;
  if(asprintf(&string, "#<image %p>", (void *) SCM_SMOB_DATA(image)) == -1)
    return 0;
  scm_puts(string, port);
  free(string);
  scm_remember_upto_here_1(image);
  return 1;
}

SCM 
load_image(SCM path) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL)
    return SCM_BOOL_F;
  SDL_Surface *image = IMG_Load(filename);
  free(filename);
  if(!image) {
    return SCM_BOOL_F;
  }
  if (image->format->BitsPerPixel != 32) {
    SDL_Surface *new_image = sdl_surface(image->w, image->h, 4);
    SDL_BlitSurface(image, NULL, new_image, NULL);
    SDL_FreeSurface(image);
    image = new_image;
  }
  SCM_NEWSMOB(smob, image_tag, image);
  return smob;
}

static SCM
decompose_color_to_rgba(SCM rgba) {
  SDL_Color c = sdl_color(scm_to_uint32(rgba));
  return scm_list_4(scm_from_uint8(c.r),
		    scm_from_uint8(c.g),
		    scm_from_uint8(c.b),
		    scm_from_uint8(c.unused));
}

static SCM
compose_color_from_rgba(SCM r, SCM g, SCM b, SCM a) {
  SDL_Color c;
  c.r = scm_to_uint8(r);
  c.g = scm_to_uint8(g);
  c.b = scm_to_uint8(b);
  c.unused = scm_to_uint8(a);
  return scm_from_uint32(rgba_color(c));
}

SCM 
draw_image_x(SCM image_smob, SCM x, SCM y) {
  scm_assert_smob_type(image_tag, image_smob);  
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
#ifdef USE_OPENGL
  int W = image->w;
  int H = image->h;
  int X = scm_to_int(x);
  int Y = screen->h - scm_to_int(y);

  if(video_mode & SDL_OPENGL) {
    WARN_ONCE("using OpenGL");

    glDisable(GL_DEPTH_TEST);
    glWindowPos2i(X, Y);
    glPixelZoom(1.0, -1.0);
    glDrawPixels(W, H, GL_RGBA, GL_UNSIGNED_BYTE, image->pixels);    
    glEnable(GL_DEPTH_TEST);
  }
  else {
#endif
    SDL_Rect area = sdl_rect(scm_to_int16(x), scm_to_int16(y), -1, -1);
    SDL_BlitSurface(image, NULL, screen, &area);
#ifdef USE_OPENGL
  }
#endif
  scm_remember_upto_here_1(image_smob);
  return SCM_UNSPECIFIED;
}

SCM 
image_width(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM w = scm_from_int(image->w);
  scm_remember_upto_here_1(image_smob);
  return w;
}

SCM 
image_height(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM h = scm_from_int(image->h);
  scm_remember_upto_here_1(image_smob);
  return h;
}

SCM 
image_size(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM l = scm_list_2(scm_from_int(image->w), scm_from_int(image->h));
  scm_remember_upto_here_1(image_smob);
  return l;
}

SCM 
rectangle(SCM w, SCM h, SCM color, SCM BytesPerPixel) {
  SCM smob;
  if(BytesPerPixel == SCM_UNDEFINED) {
    BytesPerPixel = scm_from_int(4);
  }

  SDL_Surface *image 
    = sdl_surface(scm_to_int(w), scm_to_int(h), scm_to_int(BytesPerPixel));
  if(color != SCM_UNDEFINED) {
    SDL_Color c = sdl_color(scm_to_uint(color));
    SDL_FillRect(image, NULL, 
		 SDL_MapRGBA(image->format, c.r, c.g, c.b, 0xff-c.unused));
  }
  SCM_NEWSMOB(smob, image_tag, image);
  return smob;
}

SCM
crop_image(SCM image_smob, SCM _x, SCM _y, SCM _w, SCM _h) {
  scm_assert_smob_type(image_tag, image_smob);  
  SCM smob;
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  Sint16 x = scm_to_int16(_x);
  Sint16 y = scm_to_int16(_y);
  Sint16 w = GIVEN(_w) ? scm_to_int16(_w) : 0;
  Sint16 h = GIVEN(_h) ? scm_to_int16(_h) : 0;
  if(w <= 0) {
    w += (image->w - x);
  }
  if(h <= 0) {
    h += (image->h - y);
  }
  SDL_Rect size = {
    .x = x, .y = y, .w = (Uint16) w, .h = (Uint16) h
  };
  SDL_Surface *cropped 
    = SDL_CreateRGBSurface(image->flags, size.w, size.h, 
			   image->format->BitsPerPixel,
			   image->format->Rmask, image->format->Gmask,
			   image->format->Bmask, image->format->Amask);
  Uint8 alpha = image->format->alpha;
  SDL_SetAlpha(image, 0, SDL_ALPHA_OPAQUE);
  SDL_BlitSurface(image, &size, cropped, NULL);
  SDL_SetAlpha(image, SDL_SRCALPHA, alpha);
  SCM_NEWSMOB(smob, image_tag, cropped);
  return smob;
}

SCM 
image_to_array(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM type;

  switch(image->format->BytesPerPixel) {
  case 1:
    type = s_u8;
    break;
  case 2:
    type = s_u16;
    break;
  case 4:
    type = s_u32;
    break;
  case 8:
    type = s_u64;
    break;
  default:
    WARN("Illegal BytesPerPixel count: %d", image->format->BytesPerPixel);
    return SCM_UNSPECIFIED;
  }
  SCM array = scm_make_typed_array(type, SCM_UNSPECIFIED, 
				   scm_list_2(scm_from_int(image->w),
					      scm_from_int(image->h)));
  scm_t_array_handle handle;
  scm_array_get_handle(array, &handle);
  void *data = scm_array_handle_uniform_writable_elements(&handle);
  memcpy(data, image->pixels, 
	 image->w * image->h * image->format->BytesPerPixel);
  scm_array_handle_release(&handle);
  scm_remember_upto_here_1(image_smob);
  return array;
}

#define NOT_SUPPORTED -1
static int bytesPerPixel[SCM_ARRAY_ELEMENT_TYPE_LAST+1];
static inline void 
init_bytesPerPixel() {
  unsigned int i;
  for(i = 0; i < NELEMS(bytesPerPixel); ++i)
    bytesPerPixel[i] = NOT_SUPPORTED;
#define SET_TYPE_SIZE(type, size) \
  bytesPerPixel[SCM_ARRAY_ELEMENT_TYPE_##type] = size

#define SET_SDL_INT_SIZE(sign, bits) \
  SET_TYPE_SIZE(sign##bits, sizeof(sign##int##bits))
  
  SET_SDL_INT_SIZE(U,8);
  SET_SDL_INT_SIZE(S,8);
  SET_SDL_INT_SIZE(U,16);
  SET_SDL_INT_SIZE(S,16);
  SET_SDL_INT_SIZE(U,32);
  SET_SDL_INT_SIZE(S,32);

  SET_TYPE_SIZE(F32, sizeof (float));
  SET_TYPE_SIZE(F64, sizeof (double));

#undef SET_SDL_INT_SIZE
#undef SET_TYPE_SIZE
}

SCM 
array_to_image(SCM array) {
  scm_t_array_handle handle;
  scm_array_get_handle(array, &handle);
  if (scm_array_handle_rank(&handle) != 2) {
    WARN("invalid array dimension");
    scm_array_handle_release(&handle);
    return SCM_UNSPECIFIED;
  }

  int BytesPerPixel = bytesPerPixel[handle.element_type];
  if (BytesPerPixel == NOT_SUPPORTED) {
    WARN("Unrecognized array type");
    scm_array_handle_release(&handle);
    return SCM_UNSPECIFIED;
  }

  scm_t_array_dim *dims = scm_array_handle_dims(&handle);

  int w = abs(dims[0].ubnd - dims[0].lbnd) + 1;
  int h = abs(dims[1].ubnd - dims[1].lbnd) + 1;

  SCM image_smob = rectangle(scm_from_int(w), scm_from_int(h), 
			     SCM_UNDEFINED, scm_from_int(BytesPerPixel));
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);

  void *data = scm_array_handle_uniform_writable_elements(&handle);
  memcpy(image->pixels, data, w * h * image->format->BytesPerPixel);
  //print_sdl_surface(image);

  scm_array_handle_release(&handle);
  return image_smob;
}
#undef NOT_SUPPORTED

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("rectangle", 2, 2, 0, rectangle);
  EXPORT_PROCEDURE("crop-image", 3, 2, 0, crop_image);
  EXPORT_PROCEDURE("load-image", 1, 0, 0, load_image);
  EXPORT_PROCEDURE("draw-image!", 3, 0, 0, draw_image_x);
  EXPORT_PROCEDURE("image-width", 1, 0, 0, image_width);
  EXPORT_PROCEDURE("image-height", 1, 0, 0, image_height);
  EXPORT_PROCEDURE("image-size", 1, 0, 0, image_size);
  EXPORT_PROCEDURE("image->array", 1, 0, 0, image_to_array);
  EXPORT_PROCEDURE("array->image", 1, 0, 0, array_to_image);
  EXPORT_PROCEDURE("decompose-color-to-rgba", 1, 0, 0, decompose_color_to_rgba);
  EXPORT_PROCEDURE("compose-color-from-rgba", 4, 0, 0, compose_color_from_rgba);
  
#undef EXPORT_PROCEDURE
}

static void
cond_expand_provide(void *unused) {
  eval("(cond-expand-provide (current-module) '(slayer-image))");
}

void 
image_init() {
  image_tag = scm_make_smob_type("image", sizeof(SDL_Surface *));
  scm_set_smob_free(image_tag, free_image);
  scm_set_smob_print(image_tag, print_image);
  init_bytesPerPixel();
  scm_c_define_module("slayer image", export_symbols, NULL);
  scm_c_define_module("slayer", cond_expand_provide, NULL);
}
