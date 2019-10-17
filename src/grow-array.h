#ifndef GROW_ARRAY_H
#define GROW_ARRAY_H

#include <stdint.h>
#include <stdlib.h>

void *grow_array_using_realloc(void *a, uint32_t *size, size_t target_size);

static inline void *grow_array(void *a, uint32_t *size, size_t target_size)
{
    uint32_t n = *size;
    if (target_size <= n)
        return a;
    return grow_array_using_realloc(a, size, target_size);
}

#endif
