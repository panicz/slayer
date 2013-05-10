AC_INIT([slayer], [1.0.0], [godek.maciek@gmail.com])
AC_PREREQ([2.68])
AC_CONFIG_AUX_DIR([build-aux])

AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([-Wall -Werror])
LT_INIT
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET

PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([SDL], [sdl])

PKG_CHECK_MODULES([SDL_MIXER], [SDL_mixer],
		  [AC_DEFINE([USE_SDL_MIXER], [1], [Use SDL_Mixer])])

PKG_CHECK_MODULES([SDL_IMAGE], [SDL_image])

PKG_CHECK_MODULES([OPENGL], [gl glu],
		  [AC_DEFINE([USE_OPENGL], [1], [Use OpenGL])])

AC_CHECK_LIB([SDL_ttf], [TTF_Init])

#AC_CHECK_LIB([GL], [glEnable])
#AC_CHECK_LIB([GLU], [gluPerspective])
#AC_CHECK_LIB([SDL_mixer], [Mix_OpenAudio])

GUILE_PROGS


# Checks for libraries.
# AC_CHECK_LIB([SDL], [SDL_Init])
# AC_CHECK_LIB([SDL_image], [IMG_Load])
# search for 'Mix_' | grep 'Mix_[a-zA-Z0-9_]*\s*[(]' -o | grep 'Mix_[a-zA-Z0-9_]*' -o | sort | uniq



AC_CHECK_LIB([c], [main])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h limits.h stdlib.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([atexit regcomp setenv sqrt])

##################
#  Output

pkgdatadir="$datadir/$PACKAGE"
if test "x$guilesitedir" = "x"; then
   guilesitedir="$pkgdatadir"
fi

AC_CONFIG_FILES([Makefile src/Makefile tools/Makefile guile-modules/Makefile])

AC_OUTPUT
