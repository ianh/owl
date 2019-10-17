#ifndef FNV_H
#define FNV_H

// This is the 32-bit FNV-1a hash diffusion algorithm.
// http://www.isthe.com/chongo/tech/comp/fnv/index.html

static uint32_t fnv(const void *dataPointer, size_t length)
{
    uint32_t hash = 0x811c9dc5;
    const unsigned char *data = dataPointer;
    for (size_t i = 0; i < length; ++i) {
        hash ^= data[i];
        hash *= 0x01000193;
    }
    return hash;
}

#endif
