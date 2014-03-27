// GENERATED AUTOMATICALLY FROM __file__, DO NOT EDIT

dnl $1=UPPERCASE_WITH_UNDERSCORES
dnl $2=lowercase_with_underscores
dnl $3=lowercase with spaces
define(`__GL_INFO', `
#ifdef $1
  {
    GLint $2;
    glGetIntegerv($1, &$2);
    OUT("$3: %i", $2);
  }
#endif // $1
')
define(`_GL_INFO', `__GL_INFO($1, $2, translit($2, `_', ` '))')
define(`GL_INFO',`_GL_INFO(GL_$1, translit($1, `A-Z', `a-z'))')

GL_INFO(MAX_DRAW_BUFFERS)
GL_INFO(MAX_COLOR_ATTACHMENTS)
GL_INFO(AUX_BUFFERS)
GL_INFO(MAX_LIGHTS)
