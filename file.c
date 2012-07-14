#include "file.h"
#include "utils.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

int file_exists(const char *filename) {
  return access(filename, F_OK) == 0;
}

int file_empty(const char *filename) {
  struct stat fileinfo;
  stat(filename, &fileinfo);
  return fileinfo.st_size == 0;
}

int file_writable(const char *filename) {
  return access(filename, W_OK) == 0;
}

int file_readable(const char *filename) {
  return access(filename, R_OK) == 0;
}

int file_write(const char *filename, const char *string) {
  FILE *file = fopen(filename, "w");
  if(file == NULL)
    return 0;
  int n = fprintf(file, "%s", string);
  fclose(file);
  return n;
}

SCM file_eval(const char *filename) {
  return scm_c_primitive_load(filename);
}

int file_create(const char *filename) {
  int fd = creat(filename, S_IRUSR | S_IWUSR);
  if(fd == -1)
    return 0;
  close(fd);
  return 1;
}








