ifdef(`header',`
#ifndef _3D_EXTENSIONS_H
#define _3D_EXTENSIONS_H
')
// GENERATED AUTOMATICALLY FROM __file__, DO NOT EDIT

ifdef(`init',`
#define TRY_LOAD_GL_EXTENSION(type, name)		\
  if(!name) {						\
    name = (type) wglGetProcAddress(# name);		\
  }							\
  if(!name) {						\
    name = (type) wglGetProcAddress(# name "ARB");	\
  }							\
  if(!name) {						\
    name = (type) wglGetProcAddress(# name "EXT");	\
  }							\
  if(!name) {						\
    WARN("Unable to load extension: " # name);		\
    name = (type) ({ void __fn__ () {			\
	  WARN(# name ": extension not available");	\
	} __fn__; });					\
  }
')

dnl $1=EXTENSION NAME, $2=FUNCTION (C NAME), $3=RETURN $4=ARGS
define(`OPENGL_EXTENSION',
ifdef(`header', `
#  ifndef HAVE_$1
#    ifdef HAVE_$1_ARB
#      define $2 $2ARB
#    else 
#      ifdef HAVE_$1_EXT
#        define $2 $2EXT
#      else
#        define NO_$1
#      endif
#    endif 
#  endif

#  ifdef NO_$1
DECLARE $3 (*$2)$4;
#  else
DECLARE $3 $2 $4;
#  endif
',
ifdef(`init',`
#ifdef NO_$1
TRY_LOAD_GL_EXTENSION($3(*)$4, $2);
#endif
',
ifdef(`def',`
#ifdef NO_$1
$3 (*$2)$4 = NULL;
#endif
'))))

OPENGL_EXTENSION(GL_GEN_FRAMEBUFFERS, glGenFramebuffers, 
		 void, (GLsizei, GLuint *))
OPENGL_EXTENSION(GL_GEN_RENDERBUFFERS, glGenRenderbuffers, 
		 void, (GLsizei, GLuint *))
OPENGL_EXTENSION(GL_BIND_FRAMEBUFFER, glBindFramebuffer,
		void, (GLenum, GLuint))
OPENGL_EXTENSION(GL_BIND_RENDERBUFFER, glBindRenderbuffer,
		void, (GLenum, GLuint))
OPENGL_EXTENSION(GL_DRAW_BUFFERS, glDrawBuffers,
		 void, (GLsizei, const GLenum *))
OPENGL_EXTENSION(GL_FRAMEBUFFER_RENDERBUFFER, glFramebufferRenderbuffer,
		 void, (GLenum, GLenum, GLenum, GLuint))
OPENGL_EXTENSION(GL_FRAMEBUFFER_TEXTURE, glFramebufferTexture,
		 void, (GLenum, GLenum, GLuint, GLint))
OPENGL_EXTENSION(GL_RENDERBUFFER_STORAGE, glRenderbufferStorage,
		 void, (GLenum, GLenum, GLsizei, GLsizei))

ifdef(`init',`
#undef TRY_LOAD_GL_EXTENSION
')

ifdef(`header',`
#endif // _3D_EXTENSIONS_H
')
