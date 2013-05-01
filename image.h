#ifndef IMAGE_H
#define IMAGE_H
#include "extend.h"

extern scm_t_bits image_tag;
extern void image_init();
extern SCM rectangle(SCM w, SCM h, SCM color, SCM BytesPerPixel);


#endif // IMAGE_H
