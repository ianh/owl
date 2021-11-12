#ifndef ALLOC_H
#define ALLOC_H

#include <stdio.h>
#include <stdlib.h>

static inline void *checked_calloc(size_t n, size_t size)
{
    void *p = calloc(n, size);
    if (size > 0 && !p) {
        fputs("critical error: out of memory\n", stderr);
        exit(-1);
    }
    return p;
}

static inline void *checked_malloc(size_t size)
{
    void *p = malloc(size);
    if (size > 0 && !p) {
        fputs("critical error: out of memory\n", stderr);
        exit(-1);
    }
    return p;
}

static inline void *checked_realloc(void *ptr, size_t size)
{
    void *p = realloc(ptr, size);
    if (size > 0 && !p) {
        fputs("critical error: out of memory\n", stderr);
        exit(-1);
    }
    return p;
}

#define malloc checked_malloc
#define calloc checked_calloc
#define realloc checked_realloc

#endif
