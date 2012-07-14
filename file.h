#ifndef FILE_H
#define FILE_H

#include "extend.h"

int file_exists(const char *filename);
int file_empty(const char *filename);
int file_writable(const char *filename);
int file_readable(const char *filename);

int file_create(const char *filename);
int file_write(const char *filename, const char *string);
SCM file_eval(const char *filename);

#endif // FILE_H
