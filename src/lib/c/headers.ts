// Minimal C standard library headers for TCC in-browser compilation
// These provide declarations only — implementations are in our ARM64 runtime

export const STDIO_H = `
#ifndef _STDIO_H
#define _STDIO_H

typedef __SIZE_TYPE__ size_t;
typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

int printf(const char *fmt, ...);
int puts(const char *s);
int putchar(int c);
int snprintf(char *str, size_t size, const char *fmt, ...);
int sprintf(char *str, const char *fmt, ...);

#endif
`;

export const STDLIB_H = `
#ifndef _STDLIB_H
#define _STDLIB_H

typedef __SIZE_TYPE__ size_t;

void exit(int status);
int abs(int x);
long labs(long x);
int atoi(const char *str);
long atol(const char *str);
void *malloc(size_t size);
void free(void *ptr);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);

#define NULL ((void *)0)

#endif
`;

export const STRING_H = `
#ifndef _STRING_H
#define _STRING_H

typedef __SIZE_TYPE__ size_t;

size_t strlen(const char *s);
int strcmp(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
char *strcpy(char *dest, const char *src);
char *strncpy(char *dest, const char *src, size_t n);
char *strcat(char *dest, const char *src);
void *memcpy(void *dest, const void *src, size_t n);
void *memset(void *s, int c, size_t n);
int memcmp(const void *s1, const void *s2, size_t n);

#define NULL ((void *)0)

#endif
`;

export const STDBOOL_H = `
#ifndef _STDBOOL_H
#define _STDBOOL_H

#define bool _Bool
#define true 1
#define false 0

#endif
`;

export const STDINT_H = `
#ifndef _STDINT_H
#define _STDINT_H

typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
typedef signed long long int64_t;
typedef unsigned long long uint64_t;

typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long long intmax_t;
typedef unsigned long long uintmax_t;

#define INT8_MIN (-128)
#define INT8_MAX 127
#define UINT8_MAX 255
#define INT16_MIN (-32768)
#define INT16_MAX 32767
#define UINT16_MAX 65535
#define INT32_MIN (-2147483647-1)
#define INT32_MAX 2147483647
#define UINT32_MAX 4294967295U
#define INT64_MIN (-9223372036854775807LL-1)
#define INT64_MAX 9223372036854775807LL
#define UINT64_MAX 18446744073709551615ULL

#endif
`;

export const STDDEF_H = `
#ifndef _STDDEF_H
#define _STDDEF_H

typedef __SIZE_TYPE__ size_t;
typedef long ptrdiff_t;

#define NULL ((void *)0)
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif
`;

export const MATH_H = `
#ifndef _MATH_H
#define _MATH_H

#define M_PI    3.14159265358979323846
#define M_E     2.71828182845904523536
#define M_SQRT2 1.41421356237309504880
#define M_LN2   0.69314718055994530942
#define M_LOG2E 1.44269504088896340736

double sqrt(double x);
double cbrt(double x);
double fabs(double x);
double floor(double x);
double ceil(double x);
double round(double x);
double trunc(double x);
double sin(double x);
double cos(double x);
double tan(double x);
double asin(double x);
double acos(double x);
double atan(double x);
double atan2(double y, double x);
double sinh(double x);
double cosh(double x);
double tanh(double x);
double exp(double x);
double exp2(double x);
double log(double x);
double log2(double x);
double log10(double x);
double pow(double x, double y);
double hypot(double x, double y);
double fmin(double x, double y);
double fmax(double x, double y);
double fmod(double x, double y);

#define HUGE_VAL (1.0/0.0)
#define INFINITY (1.0/0.0)
#define NAN      (0.0/0.0)

#endif
`;

export const STDARG_H = `
#ifndef _STDARG_H
#define _STDARG_H

typedef __builtin_va_list va_list;
#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)
#define va_copy(dest, src) __builtin_va_copy(dest, src)

#endif
`;

export const HEADERS: Record<string, string> = {
	"stdio.h": STDIO_H,
	"stdlib.h": STDLIB_H,
	"string.h": STRING_H,
	"stdbool.h": STDBOOL_H,
	"stdint.h": STDINT_H,
	"stddef.h": STDDEF_H,
	"stdarg.h": STDARG_H,
	"math.h": MATH_H,
};
