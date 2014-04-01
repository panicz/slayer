#ifndef _IMAGE_H
#define _IMAGE_H

#include <SDL/SDL.h>
#include "extend.h"
#include "utils.h"

typedef enum {
  IMAGE_TYPE_SURFACE = 0,
  IMAGE_TYPE_TEXTURE = 1
} image_type_t;

#define IMAGE_TYPE_SHIFT 0
#define IMAGE_TYPE_BITS 1
#define IMAGE_TYPE_MASK 1

typedef enum {
  IMAGE_ACCESS_COPY = 0, // an image is meant to be a copy of another image.
  // When two images share the same data and one of them is to be modified,
  // the data is copied and the reference counter to the original data is
  // decreased

  IMAGE_ACCESS_PROXY = 1, // an image is meant to be a proxy of the data,
  // i.e. it is allowed to modify the content of the data

  IMAGE_ACCESS_VIEW = 2, // an image is meant to be a view of another image.
  // It is illegal to modify the view, and the result of such operation is
  // unspecified
} image_access_t;

#define IMAGE_ACCESS_SHIFT (IMAGE_TYPE_SHIFT + IMAGE_TYPE_BITS)
#define IMAGE_ACCESS_BITS 2
#define IMAGE_ACCESS_MASK 3

// we use X, Y, W and H to optimize cropped textures and surfaces: instead
// of making a copy, we just increase reference counter and use shared
// images; the actual copy is made when one attempts to modify an image
// whose reference count is greater than 1.
#define IMAGE_X(img) ((Uint16) (0xffff & (SCM_SMOB_DATA_2(img) >> 16)))
#define IMAGE_Y(img) ((Uint16) (0xffff & (SCM_SMOB_DATA_2(img))))
#define IMAGE_W(img) ((Uint16) (0xffff & (SCM_SMOB_DATA_3(img) >> 16)))
#define IMAGE_H(img) ((Uint16) (0xffff & (SCM_SMOB_DATA_3(img))))

#define SET_IMAGE_XY(img, x, y) \
  SCM_SET_SMOB_DATA_2(img, ((scm_t_bits) (0xffffffff & ((x << 16) | y))))

#define SET_IMAGE_WH(img, w, h) \
  SCM_SET_SMOB_DATA_3(img, ((scm_t_bits) (0xffffffff & ((w << 16) | h))))

#define IMAGE_AREA(img)							\
  sdl_rect(IMAGE_X(img), IMAGE_Y(img), IMAGE_W(img), IMAGE_H(img))

#define SURFACE(img) ((SDL_Surface *) SCM_SMOB_DATA(img))

#define TEXTURE(img) ((GLuint) SCM_SMOB_DATA(img))

#define IS_SURFACE(smob)						\
  !(IMAGE_TYPE_MASK & (SCM_SMOB_FLAGS(smob) >> IMAGE_TYPE_SHIFT))

#define IS_TEXTURE(smob)						\
  (IMAGE_TYPE_MASK & (SCM_SMOB_FLAGS(smob) >> IMAGE_TYPE_SHIFT))



#define TEXTURE_MIRROR_Y_SHIFT (IMAGE_ACCESS_SHIFT + IMAGE_ACCESS_BITS)
#define TEXTURE_MIRROR_X_SHIFT (TEXTURE_MIRROR_Y_SHIFT + 1)
#define TEXTURE_MIRROR_MASK 1

// the "mirror" property for y's is needed, because for OpenGL the SDL
// surfaces appear as "upside down", and since SDL surfaces are used to
// load images, we treat their convention as valid
#define SET_TEXTURE_FLAGS(tex, access, mirror_x, mirror_y)		\
  SCM_SET_SMOB_FLAGS(tex, (IMAGE_TYPE_TEXTURE << IMAGE_TYPE_SHIFT)	\
		     | ((IMAGE_ACCESS_MASK & access) << IMAGE_ACCESS_SHIFT) \
		     | (mirror_x << TEXTURE_MIRROR_X_SHIFT)		\
		     | (mirror_y << TEXTURE_MIRROR_Y_SHIFT))

#define SET_SURFACE_FLAGS(surf, access)					\
  SCM_SET_SMOB_FLAGS(surf, (IMAGE_TYPE_SURFACE << IMAGE_TYPE_SHIFT)	\
		     | ((IMAGE_ACCESS_MASK & access) << IMAGE_ACCESS_SHIFT))

#define IMAGE_ACCESS(img)				\
  (IMAGE_ACCESS_MASK & (SCM_SMOB_FLAGS(img) >> IMAGE_ACCESS_SHIFT))

#define TEXTURE_MIRROR_X(tex)					\
  ((bool) (TEXTURE_MIRROR_MASK					\
	   & (SCM_SMOB_FLAGS(tex) >> TEXTURE_MIRROR_X_SHIFT)))

#define TEXTURE_MIRROR_Y(tex)					\
  ((bool) (TEXTURE_MIRROR_MASK					\
	   & (SCM_SMOB_FLAGS(tex) >> TEXTURE_MIRROR_Y_SHIFT)))

DECLARE SCM
surface_smob(SDL_Surface *surface, Uint16 x, Uint16 y, Uint16 w, Uint16 h, 
	     image_access_t access);

DECLARE SCM
texture_smob(GLuint texture_id, bool mirror_x, bool mirror_y,
	     Uint16 x, Uint16 y, Uint16 w, Uint16 h, image_access_t access);

#endif // _IMAGE_H
