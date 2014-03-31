#ifndef _IMAGE_H
#define _IMAGE_H

// This header file is not meant to provide a public interface for other
// modules. Instead it contains definitions of constants, structures and
// static inline functions that are used within image.c in order to reduce
// the mess a little bit.

#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "utils.h"
#include "slayer.h"
#include "extend.h"
#include "video.h"
#include "symbols.h"

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

typedef enum {
  IMAGE_TYPE_SURFACE = 0,
  IMAGE_TYPE_TEXTURE = 1
} image_type_t;

#define IMAGE_TYPE_SHIFT 0
#define IMAGE_TYPE_BITS 1
#define IMAGE_TYPE_MASK 1

#define IS_SURFACE(smob)						\
  !(IMAGE_TYPE_MASK & (SCM_SMOB_FLAGS(smob) >> IMAGE_TYPE_SHIFT))

#define IS_TEXTURE(smob)						\
  (IMAGE_TYPE_MASK & (SCM_SMOB_FLAGS(smob) >> IMAGE_TYPE_SHIFT))

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

// it's best to use some prime number as the size for optimal distribution
#define DEF_REFERENCE_COUNTER(type, purpose, size, hash)		\
  typedef struct {							\
    type object;							\
    int count;								\
  } type##_ref_count_t;							\
									\
  static inline type##_ref_count_t *					\
  new_##type##_ref_count_t(type object, int count) {			\
    type##_ref_count_t *rc						\
      = (type##_ref_count_t *) malloc(sizeof(type##_ref_count_t));	\
    rc->object = object;						\
    rc->count = count;							\
    return rc;								\
  }									\
									\
  static struct list *purpose##_ref_counts[size];			\
									\
  static inline type##_ref_count_t *					\
  purpose##_ref_count(type object) {					\
    bool contains_object(struct list *l) {				\
      return ((type##_ref_count_t *) l->data)->object == object;	\
    }									\
    struct list *suspects = purpose##_ref_counts[hash(object) % size];	\
    struct list *ref_count_cell = list_ref(suspects, contains_object);	\
    if(ref_count_cell) {						\
      return (type##_ref_count_t *) (ref_count_cell->data);		\
    }									\
    return NULL;							\
  }									\
									\
  static inline void							\
  increase_##purpose##_ref_count(type object) {				\
    type##_ref_count_t *ref_count = purpose##_ref_count(object);	\
    if(!ref_count) {							\
      ref_count = new_##type##_ref_count_t(object, 1);			\
      LIST_PUSH(purpose##_ref_counts[hash(object) % size], ref_count);	\
    }									\
    else {								\
      ref_count->count++;						\
    }									\
  }

#define IDENTITY(x) (x)
#define CAST_TO_SIZE_T(x) ((size_t) x)

DEF_REFERENCE_COUNTER(GLuint, texture, 101, IDENTITY);
DEF_REFERENCE_COUNTER(pointer, surface, 127, CAST_TO_SIZE_T);

#undef CAST_TO_SIZE_T
#undef IDENTITY
#undef DEF_REFERENCE_COUNTER

static inline SCM
surface_smob(SDL_Surface *surface, 
	     Uint16 x, Uint16 y, Uint16 w, Uint16 h, 
	     image_access_t access) {
  SCM smob;
  SCM_NEWSMOB3(smob, image_tag, surface, 0, 0);
  SET_IMAGE_XY(smob, x, y);
  SET_IMAGE_WH(smob, w, h);
  SET_SURFACE_FLAGS(smob, access);
  increase_surface_ref_count(surface);
  return smob;
}

static inline SCM
texture_smob(GLuint texture_id, bool mirror_x, bool mirror_y,
	     Uint16 x, Uint16 y, Uint16 w, Uint16 h,
	     image_access_t access) {
  SCM smob;
  SCM_NEWSMOB3(smob, image_tag, texture_id, 0, 0);
  SET_IMAGE_XY(smob, x, y);
  SET_IMAGE_WH(smob, w, h);
  SET_TEXTURE_FLAGS(smob, access, mirror_x, mirror_y);
  increase_texture_ref_count(texture_id);
  return smob;
}

#endif // _IMAGE_H
