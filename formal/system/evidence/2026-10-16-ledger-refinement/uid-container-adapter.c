#define _GNU_SOURCE
#include <unistd.h>
#include <sys/stat.h>
#include <dlfcn.h>
/* Test-container adapter: only UID reporting/ownership; no SQL/MVCC changes. */
uid_t getuid(void) { return 1; }
uid_t geteuid(void) { return 1; }
int stat(const char *p, struct stat *s) {
  int (*real_stat)(const char*,struct stat*)=dlsym(RTLD_NEXT,"stat");
  int r=real_stat(p,s); if (!r && s->st_uid==0) s->st_uid=1; return r;
}
int lstat(const char *p, struct stat *s) {
  int (*real_lstat)(const char*,struct stat*)=dlsym(RTLD_NEXT,"lstat");
  int r=real_lstat(p,s); if (!r && s->st_uid==0) s->st_uid=1; return r;
}
int fstat(int fd, struct stat *s) {
  int (*real_fstat)(int,struct stat*)=dlsym(RTLD_NEXT,"fstat");
  int r=real_fstat(fd,s); if (!r && s->st_uid==0) s->st_uid=1; return r;
}
