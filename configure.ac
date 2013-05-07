AC_INIT([slayer], [1.0.0], [godek.maciek@gmail.com])
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

# Checks for libraries.
AC_CHECK_LIB([GL], [glClear])
AC_CHECK_LIB([GL], [glClearColor])
AC_CHECK_LIB([GL], [glClearDepth])
AC_CHECK_LIB([GL], [glColor])
AC_CHECK_LIB([GL], [glColor3d])
AC_CHECK_LIB([GL], [glColor3ui])
AC_CHECK_LIB([GL], [glColor4d])
AC_CHECK_LIB([GL], [glColor4ub])
AC_CHECK_LIB([GL], [glColor4ui])
AC_CHECK_LIB([GL], [glColorPointer])
AC_CHECK_LIB([GL], [glDepthMask])
AC_CHECK_LIB([GL], [glDisable])
AC_CHECK_LIB([GL], [glDisableClientState])
AC_CHECK_LIB([GL], [glDrawElements])
AC_CHECK_LIB([GL], [glDrawPixels])
AC_CHECK_LIB([GL], [glEnable])
AC_CHECK_LIB([GL], [glEnableClientState])
AC_CHECK_LIB([GL], [glGetIntegerv])
AC_CHECK_LIB([GL], [glLoadIdentity])
AC_CHECK_LIB([GL], [glMatrixMode])
AC_CHECK_LIB([GL], [glMultMatrixd])
AC_CHECK_LIB([GL], [glMultMatrixf])
AC_CHECK_LIB([GL], [glNormalPointer])
AC_CHECK_LIB([GL], [glPixelStorei])
AC_CHECK_LIB([GL], [glPixelZoom])
AC_CHECK_LIB([GL], [glPopMatrix])
AC_CHECK_LIB([GL], [glPushMatrix])
AC_CHECK_LIB([GL], [glRotatef])
AC_CHECK_LIB([GL], [glShadeModel])
AC_CHECK_LIB([GL], [glTexCoordPointer])
AC_CHECK_LIB([GL], [glTranslatef])
AC_CHECK_LIB([GL], [glVertexPointer])
AC_CHECK_LIB([GL], [glViewport])
AC_CHECK_LIB([GL], [glWindowPos2i])

AC_CHECK_LIB([GLU], [gluPerspective])

AC_CHECK_LIB([SDL], [SDL_BlitSurface])
AC_CHECK_LIB([SDL], [SDL_ConvertSurface])
AC_CHECK_LIB([SDL], [SDL_CreateRGBSurface])
AC_CHECK_LIB([SDL], [SDL_EnableKeyRepeat])
AC_CHECK_LIB([SDL], [SDL_EnableUNICODE])
AC_CHECK_LIB([SDL], [SDL_FillRect])
AC_CHECK_LIB([SDL], [SDL_Flip])
AC_CHECK_LIB([SDL], [SDL_FreeSurface])
AC_CHECK_LIB([SDL], [SDL_GetError])
AC_CHECK_LIB([SDL], [SDL_GetTicks])
AC_CHECK_LIB([SDL], [SDL_GL_SetAttribute])
AC_CHECK_LIB([SDL], [SDL_GL_SwapBuffers])
AC_CHECK_LIB([SDL], [SDL_Init])
AC_CHECK_LIB([SDL], [SDL_InitSubSystem])
AC_CHECK_LIB([SDL], [SDL_MapRGBA])
AC_CHECK_LIB([SDL], [SDL_PeepEvents])
AC_CHECK_LIB([SDL], [SDL_PollEvent])
AC_CHECK_LIB([SDL], [SDL_Quit])
AC_CHECK_LIB([SDL], [SDL_QuitSubSystem])
AC_CHECK_LIB([SDL], [SDL_SetVideoMode])
AC_CHECK_LIB([SDL], [SDL_ShowCursor])
AC_CHECK_LIB([SDL], [SDL_WaitEvent])
AC_CHECK_LIB([SDL], [SDL_WM_GrabInput])
AC_CHECK_LIB([SDL], [SDL_WM_SetCaption])

AC_CHECK_LIB([SDL_image], [IMG_Load])

# search for 'Mix_' | grep 'Mix_[a-zA-Z0-9_]*\s*[(]' -o | grep 'Mix_[a-zA-Z0-9_]*' -o | sort | uniq
AC_CHECK_LIB([SDL_mixer], [Mix_ChannelFinished])
AC_CHECK_LIB([SDL_mixer], [Mix_CloseAudio])
AC_CHECK_LIB([SDL_mixer], [Mix_FreeChunk])
AC_CHECK_LIB([SDL_mixer], [Mix_FreeMusic])
AC_CHECK_LIB([SDL_mixer], [Mix_HookMusicFinished])
AC_CHECK_LIB([SDL_mixer], [Mix_LoadMUS])
AC_CHECK_LIB([SDL_mixer], [Mix_LoadWAV])
AC_CHECK_LIB([SDL_mixer], [Mix_OpenAudio])
AC_CHECK_LIB([SDL_mixer], [Mix_PauseMusic])
AC_CHECK_LIB([SDL_mixer], [Mix_PlayChannel])
AC_CHECK_LIB([SDL_mixer], [Mix_PlayMusic])
AC_CHECK_LIB([SDL_mixer], [Mix_ResumeMusic])

AC_CHECK_LIB([SDL_ttf], [TTF_OpenFont])
AC_CHECK_LIB([SDL_ttf], [TTF_SetFontStyle])
AC_CHECK_LIB([SDL_ttf], [TTF_RenderUTF8_Blended])
AC_CHECK_LIB([SDL_ttf], [TTF_RenderUTF8_Shaded])
AC_CHECK_LIB([SDL_ttf], [TTF_FontLineSkip])
AC_CHECK_LIB([SDL_ttf], [TTF_Init])

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

AC_CONFIG_FILES([Makefile src/Makefile])

AC_OUTPUT
