#include "_image.h"

scm_t_bits image_tag = 0;
SCM current_video_output_fluid;

static SCM
surface_p(SCM image) {
  return IS_SURFACE(image) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM
texture_p(SCM image) {
  return IS_TEXTURE(image) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM
screen_p(SCM image) {
  return (IS_SURFACE(image) && (SURFACE(image) == screen))
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

static inline void
init_current_video_output_fluid() {
  if(!image_tag) {
    FATAL("the function needs to be initialized after image_tag");
  }
  SCM screen_smob = surface_smob(screen, 0, 0, screen->w, screen->h,
				 IMAGE_ACCESS_PROXY);
  scm_gc_protect_object(screen_smob);
  current_video_output_fluid = scm_make_fluid_with_default(screen_smob);
  scm_gc_protect_object(current_video_output_fluid);
}

static inline SCM
surface_to_texture(SCM image_smob) {
  TODO("make new texture");
  return SCM_UNSPECIFIED;
}

inline SCM
current_video_output() {
  return scm_fluid_ref(current_video_output_fluid);
}

#ifdef USE_OPENGL
// this data is used when one tries to draw output to an image in 3d mode
struct {
  GLuint frame;
  GLuint color;
  GLuint depth;
  GLuint draw;
} output_buffer;

static inline void
init_output_buffer() {

#define ASSIGN_GL_RESOURCE_TO_RELEASE(dest, Type)	\
  dest = glGen##Type();					\
  REMEMBER_TO_RELEASE(&dest, glDelete##Type##p)

  if(video_mode & SDL_OPENGL) {
    ASSIGN_GL_RESOURCE_TO_RELEASE(output_buffer.frame, Framebuffer);
    ASSIGN_GL_RESOURCE_TO_RELEASE(output_buffer.color, Renderbuffer);
    ASSIGN_GL_RESOURCE_TO_RELEASE(output_buffer.depth, Renderbuffer);
  }
#undef ASSIGN_GL_RESOURCE_TO_RELEASE
}

static void
on_enter_video_context(SCM image) {
  int w = IMAGE_W(image);
  int h = IMAGE_H(image);

  CAUTIOUSLY(glBindFramebuffer(GL_FRAMEBUFFER, output_buffer.frame));

  CAUTIOUSLY(glBindRenderbuffer(GL_RENDERBUFFER, output_buffer.color));
  CAUTIOUSLY(glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, w, h));
  
  CAUTIOUSLY(glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
				       GL_RENDERBUFFER, output_buffer.color));
  
  CAUTIOUSLY(glBindRenderbuffer(GL_RENDERBUFFER, output_buffer.depth));

  CAUTIOUSLY(glRenderbufferStorage(GL_RENDERBUFFER, 
				   GL_DEPTH_COMPONENT24, w, h));

  CAUTIOUSLY(glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
				       GL_RENDERBUFFER, output_buffer.depth));
  CAUTIOUSLY(glDrawBuffer(GL_COLOR_ATTACHMENT0));
  if(glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
    WARN("Unsupported framebuffer object configuration");
  }
}

static void
on_exit_video_context(SCM image) {
  CAUTIOUSLY(glFinish());
  GLenum code = CAUTIOUSLY(glCheckFramebufferStatus(GL_FRAMEBUFFER));
  if(code == GL_FRAMEBUFFER_COMPLETE) {
    if(IS_SURFACE(image)) {
      int w = IMAGE_W(image);
      int h = IMAGE_H(image);

      CAUTIOUSLY(glReadBuffer(GL_COLOR_ATTACHMENT0));

      if(IS_SURFACE(image)) {
	SDL_Surface *s = SURFACE(image);
	CAUTIOUSLY(glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, 
				s->pixels));
	flip_surface_vertically(s);
      }
    }
  }
  else {
    WARN("Failed to complete framebuffer request: %i", code);
  }
  CAUTIOUSLY(glBindFramebuffer(GL_FRAMEBUFFER, 0));
}
#endif

#ifdef USE_OPENGL
#define BEGIN_VIDEO_CONTEXT(image)					\
  if(video_mode & SDL_OPENGL) {						\
    scm_dynwind_begin(SCM_F_DYNWIND_REWINDABLE);			\
    scm_dynwind_unwind_handler((void(*)(void *)) on_exit_video_context,	\
			       image, SCM_F_WIND_EXPLICITLY);		\
    scm_dynwind_rewind_handler((void(*)(void *)) on_enter_video_context, \
			       image, SCM_F_WIND_EXPLICITLY);		\
  }

#define END_VIDEO_CONTEXT()			\
  if(video_mode & SDL_OPENGL) {			\
    scm_dynwind_end();				\
  }
#else
#define BEGIN_VIDEO_CONTEXT(image)
#define END_VIDEO_CONTEXT() 
#endif

static inline SCM
c_call_with_video_output_to(SCM image, SCM (*proc)(void *), void *data) {
  scm_assert_smob_type(image_tag, image);
  BEGIN_VIDEO_CONTEXT(image);
  SCM result = scm_c_with_fluid(current_video_output_fluid, image, proc, data);
  END_VIDEO_CONTEXT();
  return result;
}

static SCM
call_with_video_output_to(SCM image, SCM thunk) {
  scm_assert_smob_type(image_tag, image);
  BEGIN_VIDEO_CONTEXT(image);
  SCM result = scm_with_fluid(current_video_output_fluid, image, thunk);
  END_VIDEO_CONTEXT();
  return result;
}

#undef BEGIN_VIDEO_CONTEXT
#undef END_VIDEO_CONTEXT

static size_t 
free_image(SCM image_smob) {
  if(IS_SURFACE(image_smob)) {
    SDL_Surface *surface =  SURFACE(image_smob);
    SDL_FreeSurface(surface);
  }
  else if(IS_TEXTURE(image_smob)) {
    GLuint id = TEXTURE(image_smob);
    GLuint_ref_count_t *ref_count = texture_ref_count(id);
    --ref_count->count;
    if(ref_count->count == 0) {
      glDeleteTexture(id);
    }
  }
  else {
    WARN("Unable to collect garbage: unrecognised image variant");
  }
  return 0;
}

static int
print_image(SCM image, SCM port, scm_print_state *pstate) {
  char *string;
  if(IS_SURFACE(image)) {
    SDL_Surface *surface = SURFACE(image);
    if(asprintf(&string, "#<image %p: surface %i x %i>", 
		surface, surface->w, surface->h) == -1) {
      return 0;
    }
    scm_puts(string, port);
    free(string);
  }
  else if(IS_TEXTURE(image)) {
    GLuint texture = TEXTURE(image);
    if(asprintf(&string, "#<image %i: texture %i x %i>", 
		texture, IMAGE_W(image), IMAGE_H(image)) == -1) {
      return 0;
    }
    scm_puts(string, port);
    free(string);    
  }
  else {
    FATAL("Unknown image variant");
  }
  scm_remember_upto_here_1(image);
  return 1;
}

static SCM 
load_image(SCM path) {
  char *filename = as_c_string(path);
  if(filename == NULL) {
    return SCM_BOOL_F;
  }
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
  return surface_smob(image, 0, 0, image->w, image->h, IMAGE_ACCESS_PROXY);
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

static inline SDL_Surface *
copy_surface(SDL_Surface *surface, SDL_Rect *area) {
  SDL_Surface *copy
    = SDL_CreateRGBSurface(surface->flags, 
			   (area ? area->w : surface->w),
			   (area ? area->h : surface->h),
			   surface->format->BitsPerPixel,
			   surface->format->Rmask, surface->format->Gmask,
			   surface->format->Bmask, surface->format->Amask);
  Uint8 alpha = surface->format->alpha;
  SDL_SetAlpha(surface, 0, SDL_ALPHA_OPAQUE);
  SDL_BlitSurface(surface, area, copy, NULL);
  SDL_SetAlpha(surface, SDL_SRCALPHA, alpha);
  return copy;
}

static inline void
draw_surface_on_surface(SCM image_smob, int x, int y, SCM target_smob) {
    SDL_Surface *image = SURFACE(image_smob);
    SDL_Surface *target = SURFACE(target_smob);
    SDL_Rect clip = IMAGE_AREA(target_smob);
    image_access_t target_access = IMAGE_ACCESS(target_smob);
    if(target_access == IMAGE_ACCESS_VIEW) {
      return WARN("attempt to draw on a read-only surface");
    }
    if(target_access == IMAGE_ACCESS_COPY) {
      pointer_ref_count_t *target_ref_count = surface_ref_count(target);
      if(target_ref_count->count > 1) { // we need to make a copy of target
	WARN("making copy of target surface (%i references)",
	     target_ref_count->count);
	target_ref_count->count--;
	target = copy_surface(target, &clip);
	SCM_SET_SMOB_DATA(target_smob, target);
	SET_IMAGE_XY(target_smob, 0, 0);
	clip.x = 0;
	clip.y = 0;
      }
    }

    if(0) {}
#ifdef USE_OPENGL
    else if((video_mode & SDL_OPENGL)) {
      CAUTIOUSLY(glDisable(GL_DEPTH_TEST));
      CAUTIOUSLY(glWindowPos2i(x, target->h - y));
      CAUTIOUSLY(glPixelZoom(1.0, -1.0));
      CAUTIOUSLY(glDrawPixels(image->w, image->h, GL_RGBA, GL_UNSIGNED_BYTE, 
			      image->pixels));
      CAUTIOUSLY(glEnable(GL_DEPTH_TEST));
    }
#endif
    else {
      SDL_Rect at = sdl_rect(x, y, -1, -1);
      SDL_BlitSurface(image, &clip, target, &at);
    }
}

static inline void
draw_surface_on_texture(SCM image_smob, int x, int y, SCM target_smob) {
  WARN("not implemented");
}

static inline void
draw_texture_on_surface(SCM image_smob, int x, int y, SCM target_smob) {
  WARN("not implemented");
}

static inline void
draw_texture_on_texture(SCM image_smob, int x, int y, SCM target_smob) {
  WARN("not implemented");
}

static SCM 
draw_image_x(SCM image_smob, SCM X, SCM Y, SCM target_smob) {
  scm_assert_smob_type(image_tag, image_smob); 
  int x = GIVEN(X) ? scm_to_int(X) : 0;
  int y = GIVEN(Y) ? scm_to_int(Y) : 0;
  if(GIVEN(target_smob)) {
    scm_assert_smob_type(image_tag, target_smob);
  } 
  else {
    target_smob = current_video_output();
  }

  if(IS_SURFACE(image_smob) && IS_SURFACE(target_smob)) {
    draw_surface_on_surface(image_smob, x, y, target_smob);
  }
  else if(IS_SURFACE(image_smob) && IS_TEXTURE(target_smob)) {
    draw_surface_on_texture(image_smob, x, y, target_smob);
  }
  else if(IS_TEXTURE(image_smob) && IS_SURFACE(target_smob)) {
    draw_texture_on_surface(image_smob, x, y, target_smob);
  }
  else if(IS_TEXTURE(image_smob) && IS_TEXTURE(target_smob)) {
    draw_texture_on_texture(image_smob, x, y, target_smob);
  }
  else {
    FATAL("Unknown image variant");
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
  SCM h;
  if(IS_SURFACE(image_smob)) {
    SDL_Surface *image = SURFACE(image_smob);
    h = scm_from_int(image->h);
  }
  else if(IS_TEXTURE(image_smob)) {
    h = scm_from_int(IMAGE_H(image_smob));
  }
  else {
    FATAL("Unknown image variant");
  }
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
  return surface_smob(image, 0, 0, image->w, image->h, IMAGE_ACCESS_PROXY);
}

static SCM
crop_image(SCM image_smob, SCM _x, SCM _y, SCM _w, SCM _h, SCM _access) {
  scm_assert_smob_type(image_tag, image_smob);  
  SDL_Surface *image = (SDL_Surface *) SCM_SMOB_DATA(image_smob);
  Sint16 x = scm_to_int16(_x);
  Sint16 y = scm_to_int16(_y);
  Sint16 w = (GIVEN(_w) && indeed(_w)) ? scm_to_int16(_w) : 0;
  Sint16 h = (GIVEN(_h) && indeed(_h)) ? scm_to_int16(_h) : 0;

  if(w <= 0) {
    w += (image->w - x);
  }
  if(h <= 0) {
    h += (image->h - y);
  }
  SDL_Rect size = {
    .x = x, .y = y, .w = (Uint16) w, .h = (Uint16) h
  };
  image_access_t access;
  if(!GIVEN(_access)) {
    access = IMAGE_ACCESS_PROXY;
  }
  else if(scm_is_eq(_access, s_copy)) {
    access = IMAGE_ACCESS_COPY;
  }
  else if(scm_is_eq(_access, s_view)) {
    access = IMAGE_ACCESS_VIEW;
  }
  else if(scm_is_eq(_access, s_proxy)) {
    access = IMAGE_ACCESS_PROXY;
  }
  else {
    WRONG_TYPE_ARG("Unknown access type: ~A. Allowed values are symbols: "
		   "'copy 'view 'proxy", scm_list_1(_access));
  }

  SDL_Surface *cropped 
    = SDL_CreateRGBSurface(image->flags, size.w, size.h, 
			   image->format->BitsPerPixel,
			   image->format->Rmask, image->format->Gmask,
			   image->format->Bmask, image->format->Amask);
  Uint8 alpha = image->format->alpha;
  SDL_SetAlpha(image, 0, SDL_ALPHA_OPAQUE);
  SDL_BlitSurface(image, &size, cropped, NULL);
  SDL_SetAlpha(image, SDL_SRCALPHA, alpha);
  scm_remember_upto_here_1(image_smob);
  return surface_smob(cropped, 0, 0, cropped->w, cropped->h, access);
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
  EXPORT_PROCEDURE("crop-image", 3, 3, 0, crop_image);
  EXPORT_PROCEDURE("load-image", 1, 0, 0, load_image);
  EXPORT_PROCEDURE("draw-image!", 1, 3, 0, draw_image_x);
  EXPORT_PROCEDURE("image-width", 1, 0, 0, image_width);
  EXPORT_PROCEDURE("image-height", 1, 0, 0, image_height);
  EXPORT_PROCEDURE("image-size", 1, 0, 0, image_size);
  EXPORT_PROCEDURE("image->array", 1, 0, 0, image_to_array);
  EXPORT_PROCEDURE("array->image", 1, 0, 0, array_to_image);
  EXPORT_PROCEDURE("decompose-color-to-rgba", 1, 0, 0,
		   decompose_color_to_rgba);
  EXPORT_PROCEDURE("compose-color-from-rgba", 4, 0, 0,
		   compose_color_from_rgba);

  EXPORT_PROCEDURE("surface?", 1, 0, 0, surface_p);
  EXPORT_PROCEDURE("texture?", 1, 0, 0, texture_p);
  EXPORT_PROCEDURE("screen?", 1, 0, 0, screen_p);

#undef EXPORT_PROCEDURE
}

void 
image_init() {
  image_tag = scm_make_smob_type("image", 0);
  scm_set_smob_free(image_tag, free_image);
  scm_set_smob_print(image_tag, print_image);
#ifdef USE_OPENGL
  init_output_buffer();
#endif
  init_current_video_output_fluid();
  init_bytesPerPixel();
  scm_c_define_module("slayer image", export_symbols, NULL);
  scm_c_define_module("slayer", cond_expand_provide, "slayer-image");
}
