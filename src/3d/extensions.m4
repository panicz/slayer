ifdef(`header',`
#ifndef _3D_EXTENSIONS_H
#define _3D_EXTENSIONS_H
')
// GENERATED AUTOMATICALLY FROM __file__, DO NOT EDIT
dnl Of course, the comment above regards the generated C code,
dnl not this m4 file that you're looking at right now

ifdef(`def',`
DECLARE CODE glGetProcAddress (const char *);
')

ifdef(`init',`
#define TRY_LOAD_GL_EXTENSION(type, name)		\
  if(!name) {						\
    name = (type) glGetProcAddress(# name);		\
  }							\
  if(!name) {						\
    name = (type) glGetProcAddress(# name "ARB");	\
  }							\
  if(!name) {						\
    name = (type) glGetProcAddress(# name "EXT");	\
  } 	   	  		     	  		\
  if(!name) {						\
    WARN(# name ": failed to obtain extension");	\
    name = (type) no_##name;				\
  }
')

dnl $1=EXTENSION NAME, $2=FUNCTION (C NAME), $3=RETURN TYPE $4=(ARG TYPES)
define(`OPENGL_EXTENSION',
ifdef(`header', `
#  ifndef HAVE_$1
#    ifdef HAVE_$1_ARB
#      define $2 $2ARB
#    else // !HAVE_$1_ARB
#      ifdef HAVE_$1_EXT
#        define $2 $2EXT
#      else // !HAVE_$1_EXT
#        define NO_$1
#      endif // !HAVE_$1_EXT
#    endif // !HAVE_$1_ARB
#  endif // HAVE_$1

#  ifdef NO_$1
DECLARE $3 (*$2)$4;
#  else // !NO_$1
DECLARE $3 $2 $4;
#  endif // !NO_$1
',
ifdef(`init',`
#ifdef NO_$1
TRY_LOAD_GL_EXTENSION($3(*)$4, $2);
#endif // NO_$1
',
ifdef(`def',`
#ifdef NO_$1
$3 (*$2)$4 = NULL;
static inline void
no_$2() {
  WARN("$2: extension not available");
}
#endif // NO_$1
'))))

define(`OPENGL_UNIT_CONSTRUCTOR_DESTRUCTOR',
ifdef(`header', `
static inline void
glDelete$1(GLuint x) {
  glDelete$1s(1, &x);
}

static inline void
glDelete$1p(GLuint *x) {
  glDelete$1s(1, x);
}

static inline GLuint
glGen$1() {
  GLuint x;
  glGen$1s(1, &x);
  return x;
}
'))

define(`_OPENGL_ALLOCATION_EXTENSION', `
  OPENGL_EXTENSION(GL_GEN_$1S, glGen$2s, void, (GLsizei, GLuint *))
  OPENGL_EXTENSION(GL_DELETE_$1S, glDelete$2s, void, (GLsizei, const GLuint *))
  OPENGL_EXTENSION(GL_BIND_$1, glBind$2, void, (GLenum, GLuint))
  OPENGL_UNIT_CONSTRUCTOR_DESTRUCTOR($2)
')

define(`OPENGL_ALLOCATION_EXTENSION', `
  _OPENGL_ALLOCATION_EXTENSION(translit($1, `a-z', `A-Z'), $1)
')

OPENGL_UNIT_CONSTRUCTOR_DESTRUCTOR(Texture)

OPENGL_ALLOCATION_EXTENSION(Framebuffer)

OPENGL_ALLOCATION_EXTENSION(Renderbuffer)

OPENGL_ALLOCATION_EXTENSION(Buffer)

OPENGL_EXTENSION(GL_DRAW_BUFFERS, glDrawBuffers,
		 void, (GLsizei, const GLenum *))
OPENGL_EXTENSION(GL_FRAMEBUFFER_RENDERBUFFER, glFramebufferRenderbuffer,
		 void, (GLenum, GLenum, GLenum, GLuint))
OPENGL_EXTENSION(GL_FRAMEBUFFER_TEXTURE, glFramebufferTexture,
		 void, (GLenum, GLenum, GLuint, GLint))
OPENGL_EXTENSION(GL_FRAMEBUFFER_TEXTURE_2D, glFramebufferTexture2D,
		 void, (GLenum, GLenum, GLenum, GLuint, GLint))
OPENGL_EXTENSION(GL_RENDERBUFFER_STORAGE, glRenderbufferStorage,
		 void, (GLenum, GLenum, GLsizei, GLsizei))
OPENGL_EXTENSION(GL_CHECK_FRAMEBUFFER_STATUS, glCheckFramebufferStatus,
		 GLenum, (GLenum))
OPENGL_EXTENSION(GL_GET_TEX_LEVEL_PARAMETER, glGetTexLevelParameteriv,
		 void, (GLenum, GLint, GLenum, GLint *))

OPENGL_EXTENSION(GL_MAP_BUFFER, glMapBuffer, void *, (GLenum, GLenum))
OPENGL_EXTENSION(GL_UNMAP_BUFFER, glUnmapBuffer, GLboolean, (GLenum))
OPENGL_EXTENSION(GL_BUFFER_DATA, glBufferData, void,
		 (GLenum, GLsizeiptr, const GLvoid *, GLenum))

ifdef(`init',`
#undef TRY_LOAD_GL_EXTENSION
')

ifdef(`header',`
#endif // _3D_EXTENSIONS_H
')
