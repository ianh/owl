#include "grow-array.h"

#include "alloc.h"
#include <stdlib.h>
#include <string.h>

void *grow_array_using_realloc(void *a, uint32_t *size, size_t target_size)
{
    if (target_size > UINT32_MAX)
        abort();
    uint32_t n = *size;
    while (target_size > n) {
        uint32_t m = (n + 1) * 3 / 2;
        if (m < n)
            n = 0xffffffff;
        else
            n = m;
        if (n < 16)
            n = 16;
    }
    char *next_array = realloc(a, n);
    memset(next_array + *size, 0, n - *size);
    *size = n;
    return next_array;
}
