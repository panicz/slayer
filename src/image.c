#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "utils.h"
#include "slayer.h"
#include "extend.h"
#include "video.h"
#include "symbols.h"

scm_t_bits image_tag = 0;
SCM current_screen_fluid;

// this data is used when one tries to draw output to an image
struct {
  GLuint framebuffer;
  GLuint texture;
  GLuint depth_buffer;
  GLenum draw_buffers[1];
} output_image;

static inline void
init_current_screen_fluid() {
  if(!image_tag) {
    FATAL("the function needs to be initialized after image_tag");
  }
  SCM screen_smob;
  SCM_NEWSMOB(screen_smob, image_tag, screen);
  scm_gc_protect_object(screen_smob);
  current_screen_fluid = scm_make_fluid_with_default(screen_smob);
  scm_gc_protect_object(current_screen_fluid);
  glGenFramebuffers(1, &output_image.framebuffer);
  glGenTextures(1, &output_image.texture);
  glGenRenderbuffers(1, &output_image.depth_buffer);
  output_image.draw_buffers[0] = GL_COLOR_ATTACHMENT0;
}

inline SCM
current_screen() {
  return scm_fluid_ref(current_screen_fluid);
}

static SCM
call_with_video_output_to(SCM target, SCM thunk) {
  void on_exit_context(SCM image_smob) {
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
  }

  void on_enter_context(SCM image_smob) {
    scm_assert_smob_type(image_tag, image_smob); 
    SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
    glBindFramebuffer(GL_FRAMEBUFFER, output_image.framebuffer);
    glBindTexture(GL_TEXTURE_2D, output_image.texture);
    glBindRenderbuffer(GL_RENDERBUFFER, output_image.depth_buffer);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 
		 image->w, image->h, 0, GL_RGBA, 
		 GL_UNSIGNED_INT_8_8_8_8, image->pixels);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, 
			  image->w, image->h);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, 
			      GL_DEPTH_ATTACHMENT,
			      GL_RENDERBUFFER, 
			      output_image.depth_buffer);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
			 output_image.texture, 0);
    glDrawBuffers(NELEMS(output_image.draw_buffers),
		  output_image.draw_buffers);
  }
  scm_dynwind_begin(SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_unwind_handler((void(*)(void *)) on_exit_context, 
			     target, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_rewind_handler((void(*)(void *)) on_enter_context, 
			     target, SCM_F_WIND_EXPLICITLY);

  SCM result = scm_with_fluid(current_screen_fluid, target, thunk);

  scm_dynwind_end();
  return result;
}

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

static SCM 
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

static SCM 
draw_image_x(SCM image_smob, SCM x, SCM y, SCM target_smob) {
  scm_assert_smob_type(image_tag, image_smob); 
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  int X = GIVEN(x) ? scm_to_int(x) : 0;
  int Y = GIVEN(y) ? scm_to_int(y) : 0;
  if(GIVEN(target_smob)) {
    scm_assert_smob_type(image_tag, target_smob);
  } 
  else {
    target_smob = current_screen();
  }
  SDL_Surface *target = (SDL_Surface *) SCM_SMOB_DATA(target_smob);
  if(0) {}
#ifdef USE_OPENGL  
  else if((video_mode & SDL_OPENGL) && (target == screen)) {
    WARN_ONCE("using OpenGL");
    glDisable(GL_DEPTH_TEST);
    glWindowPos2i(X, screen->h - Y);
    glPixelZoom(1.0, -1.0);
    glDrawPixels(image->w, image->h, GL_RGBA, GL_UNSIGNED_BYTE, image->pixels);    
    glEnable(GL_DEPTH_TEST);
  }
#endif
  else {
    SDL_Rect at = sdl_rect(X, Y, -1, -1);
    SDL_BlitSurface(image, NULL, target, &at);
  }
  scm_remember_upto_here_2(image_smob, target_smob);
  return SCM_UNSPECIFIED;
}

static SCM 
image_width(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM w = scm_from_int(image->w);
  scm_remember_upto_here_1(image_smob);
  return w;
}

static SCM 
image_height(SCM image_smob) {
  scm_assert_smob_type(image_tag, image_smob);
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  SCM h = scm_from_int(image->h);
  scm_remember_upto_here_1(image_smob);
  return h;
}

static SCM 
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

static SCM
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
  scm_remember_upto_here_1(image_smob);
  return smob;
}

static SCM 
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

static SCM 
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

  EXPORT_PROCEDURE("call-with-video-output-to", 2, 0, 0, 
		   call_with_video_output_to);
  EXPORT_PROCEDURE("rectangle", 2, 2, 0, rectangle);
  EXPORT_PROCEDURE("crop-image", 3, 2, 0, crop_image);
  EXPORT_PROCEDURE("load-image", 1, 0, 0, load_image);
  EXPORT_PROCEDURE("draw-image!", 1, 3, 0, draw_image_x);
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
  init_current_screen_fluid();
  init_bytesPerPixel();
  scm_c_define_module("slayer image", export_symbols, NULL);
  scm_c_define_module("slayer", cond_expand_provide, NULL);
}
