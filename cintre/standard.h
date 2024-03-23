#include <stdlib.h>

// string.h {{{
char*  stpcpy(char* restrict dest, const char* restrict src);
char*  strcat(char* restrict dest, const char* restrict src);
char*  strchr(const char* s, int c);
int    strcmp(const char* s1, const char* s2);
int    strcoll(const char* s1, const char* s2);
char*  strcpy(char* restrict dest, const char* restrict src);
size_t strcspn(const char* s, const char* reject);
char*  strdup(const char* s);
char*  strfry(char* string);
size_t strlen(const char* s);
char*  strncat(char* restrict dest, const char* restrict src, size_t n);
int    strncmp(const char* s1, const char* s2, size_t n);
char*  strncpy(char* restrict dest, const char* restrict src, size_t n);
char*  strpbrk(const char* s, const char* accept);
char*  strrchr(const char* s, int c);
char*  strsep(char** restrict stringp, const char* restrict delim);
size_t strspn(const char* s, const char* accept);
char*  strstr(const char* haystack, const char* needle);
char*  strtok(char* restrict s, const char* restrict delim);
size_t strxfrm(char* restrict dst, const char* restrict src, size_t n);
void*  memccpy(void* dest, const void* src, int c, size_t n);
void*  memchr(const void* s, int c, size_t n);
int    memcmp(const void* s1, const void* s2, size_t n);
void*  memcpy(void* dest, const void* src, size_t n);
void*  memfrob(void* s, size_t n);
void*  memmem(const void* haystack, size_t haystacklen, const void* needle, size_t needlelen);
void*  memmove(void* dest, const void* src, size_t n);
void*  memset(void* s, int c, size_t n);
// }}}

// stdio.h {{{
// ...
int putchar(int c);
int puts(const char *s);
// ...
// }}}
