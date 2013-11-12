AC_INIT([slayer], [1.0.0], [bugs-slayer@gnu.org.ua])
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

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
              [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])

PKG_CHECK_MODULES([SDL], [sdl])


AC_CHECK_LIB([c], [asprintf], 
  [AC_DEFINE([HAVE_ASPRINTF], [1], [`asprintf' is available in libc])], 
  [AC_DEFINE([HAVE_ASPRINTF], [0], [Define own version of `asprintf'])])

AC_CHECK_LIB([c], [vasprintf], 
  [AC_DEFINE([HAVE_VASPRINTF], [1], [`vasprintf' is available in libc])], 
  [AC_DEFINE([HAVE_VASPRINTF], [0], [Define own version of `vasprintf'])])

AC_ARG_WITH([opengl], 
  AS_HELP_STRING([--with-opengl], 
    [Build with OpenGL support (requires the -e3d option at runtime, \
unless --enable-default-3d is passed to the configure script)]),
  [],
  [with_opengl=check])

if test "x$with_opengl" != xno; then
  PKG_CHECK_MODULES([OPENGL], [gl glu], 
    [AC_DEFINE([USE_OPENGL], [1], [Use OpenGL])
     with_opengl=yes
     AC_CHECK_LIB([GL], [glWindowPos2i], 
       [AC_DEFINE([HAVE_GL_WINDOW_POS2I], [1], 
       		  [`glWindowPos2i' is available from OpenGL])],
       [AC_DEFINE([HAVE_GL_WINDOW_POS2I], [0], 
       	          [Define own version of `glWindowPos2i'])])],
    [if test "x$with_opengl" != xcheck; then
       AC_MSG_FAILURE([OpenGL not found])
     fi]);
fi

AM_CONDITIONAL([OPENGL], [test "x$with_opengl" = xyes])

AC_ARG_ENABLE([default-3d],
  AS_HELP_STRING([--enable-default-3d],
    [Assume -e3d argument (use OpenGL for drawing) on runtime [default=no]]),
  [if test "x$enable_default_3d" != xno; then
     if test "x$with_opengl" != xyes; then
       AC_MSG_FAILURE([OpenGL package not available for --enable-default-3d])
     fi
     AC_DEFINE([ENABLE_DEFAULT_3D], [1],
[Do not require -e3d option to use OpenGL for drawing])
   fi])

AC_ARG_WITH([sdl-mixer],
  AS_HELP_STRING([--with-sdl-mixer],
    [Build with audio support through SDL_mixer]),
    [],
    [with_sdl_mixer=check])

if test "x$with_sdl_mixer" != xno; then
  PKG_CHECK_MODULES([SDL_MIXER], [SDL_mixer], 
    [AC_DEFINE([USE_SDL_MIXER], [1], [Use SDL_Mixer])
    with_sdl_mixer=yes],
    [if test "x$with_sdl_mixer" != xcheck; then
       AC_MSG_FAILURE([SDL_mixer not found])
     fi])
fi

AM_CONDITIONAL([SDL_MIXER], test "x$with_sdl_mixer" = xyes)

PKG_CHECK_MODULES([SDL_IMAGE], [SDL_image], [], 
  [AC_MSG_FAILURE([SDL_image not found (required)])])

AC_CHECK_LIB([SDL_ttf], [TTF_Init], [],
  [AC_MSG_FAILURE([SDL_ttf not found (required)])])

GUILE_PROGS

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


if test "x$guilesitedir" = "x"; then
   guilesitedir="$datadir/guile/site"
fi

AC_SUBST([guilesitedir])

AC_CONFIG_FILES([Makefile src/Makefile guile-modules/Makefile doc/Makefile tools/Makefile])

AC_OUTPUT

# if test "x$GUILE_SITE" != "x"; then
   if test "$guilesitedir" != "$GUILE_SITE"; then
      # Guile has a different prefix than this module
      AC_MSG_WARN([]
      [The Guile modules will be installed in ${guilesitedir}.]
      [You should probably re-run `configure' with]
      [`--with-guilesitedir=$GUILE_SITE']
      [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
      [variable.])
   fi
# fi
