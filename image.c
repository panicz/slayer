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
  //print_sdl_surface(image);
  free(filename);
  SCM_NEWSMOB(smob, image_tag, image);
  return smob;
}

SCM 
draw_image(SCM image_smob, SCM x, SCM y) {
  scm_assert_smob_type(image_tag, image_smob);  
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
#ifdef USE_OPENGL
  int W = image->w;
  int H = image->h;
  int X = scm_to_int(x);
  int Y = screen->h - scm_to_int(y);

  if(video_mode & SDL_OPENGL) {
    WARN_ONCE("using OpenGL");

    glWindowPos2i(X, Y);
    glPixelZoom(1.0, -1.0);
    glDrawPixels(W, H, GL_RGBA, GL_UNSIGNED_BYTE, image->pixels);    
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

  SDL_Surface *image = sdl_surface(scm_to_int(w), scm_to_int(h), scm_to_int(BytesPerPixel));
  if(color != SCM_UNDEFINED) {
    SDL_Color c = sdl_color(scm_to_uint(color));
    SDL_FillRect(image, NULL, SDL_MapRGBA(image->format, c.r, c.g, c.b, 0xff-c.unused));
  }
  SCM_NEWSMOB(smob, image_tag, image);
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
  memcpy(data, image->pixels, image->w * image->h * image->format->BytesPerPixel);

  scm_array_handle_release(&handle);
  scm_remember_upto_here_1(image_smob);
  return array;
}

static int array_type_to_BytesPerPixel[SCM_ARRAY_ELEMENT_TYPE_LAST+1];
static inline void 
init_array_type_to_BytesPerPixel() {
  int i;
  for(i = 0; i < NELEMS(array_type_to_BytesPerPixel); ++i)
    array_type_to_BytesPerPixel[i] = -1;
#define SET_TYPE_SIZE(type, size) \
  array_type_to_BytesPerPixel[SCM_ARRAY_ELEMENT_TYPE_##type] = size

#define SET_SDL_INT_SIZE(sign, bits) \
  SET_TYPE_SIZE(sign##bits, sizeof(sign##int##bits))
  
  SET_SDL_INT_SIZE(U,8);
  SET_SDL_INT_SIZE(S,8);
  SET_SDL_INT_SIZE(U,16);
  SET_SDL_INT_SIZE(S,16);
  SET_SDL_INT_SIZE(U,32);
  SET_SDL_INT_SIZE(S,32);

  SET_TYPE_SIZE(F32, sizeof(float));
  SET_TYPE_SIZE(F64, sizeof(double));

#undef SET_SDL_INT_SIZE
#undef SET_TYPE_SIZE
}


SCM 
array_to_image(SCM array) {
  //WARN("TODO: assuming 32-bit pixels (this should be taken from array type)");
  scm_t_array_handle handle;
  scm_array_get_handle(array, &handle);
  if(scm_array_handle_rank(&handle) != 2) {
    return SCM_UNSPECIFIED;
  }


  int BytesPerPixel = array_type_to_BytesPerPixel[handle.element_type];
  if(BytesPerPixel == -1) {
    WARN("Unrecognized array type");
    return SCM_UNSPECIFIED;
  }


  /*
  SCM type = scm_array_handle_element_type(&handle);
  int BytesPerPixel;
  if(equal(type, s_u32)) {
    BytesPerPixel = 4;
  } else if(equal(type, s_u16)) {
    BytesPerPixel = 2;
  } else if(equal(type, s_u8)) {
    BytesPerPixel = 1;
  } else if(equal(type, s_u64)) {
    BytesPerPixel = 8;
  } else {
    WARN("Unrecognized array type");
    return SCM_UNSPECIFIED;
  }
  */

  scm_t_array_dim *dims = scm_array_handle_dims(&handle);

  int w = abs(dims[0].ubnd - dims[0].lbnd) + 1;
  int h = abs(dims[1].ubnd - dims[1].lbnd) + 1;

  SCM image_smob = rectangle(scm_from_int(w), scm_from_int(h), SCM_UNDEFINED, scm_from_int(BytesPerPixel));
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);

  void *data = scm_array_handle_uniform_writable_elements(&handle);
  memcpy(image->pixels, data, w * h * image->format->BytesPerPixel);
  //print_sdl_surface(image);  
  scm_array_handle_release(&handle);
  return image_smob;
}


static void 
export_symbols() {
  scm_c_define_gsubr("rectangle", 2, 2, 0, rectangle);
  scm_c_define_gsubr("load-image", 1, 0, 0, load_image);
  scm_c_define_gsubr("draw-image", 3, 0, 0, draw_image);
  scm_c_define_gsubr("image-width", 1, 0, 0, image_width);
  scm_c_define_gsubr("image-height", 1, 0, 0, image_height);
  scm_c_define_gsubr("image-size", 1, 0, 0, image_size);
  scm_c_define_gsubr("image->array", 1, 0, 0, image_to_array);
  scm_c_define_gsubr("array->image", 1, 0, 0, array_to_image);
}

void 
image_init() {
  image_tag = scm_make_smob_type("image", sizeof(SDL_Surface *));
  scm_set_smob_free(image_tag, free_image);
  scm_set_smob_print(image_tag, print_image);
  init_array_type_to_BytesPerPixel();
  export_symbols();
}
