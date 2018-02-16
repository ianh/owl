#ifndef _6A_GENERATE_H_
#define _6A_GENERATE_H_

#include "4-determinize.h"
#include <stdlib.h>

struct generator_internal;

struct generator {
    void (*output)(struct generator *, const char *, size_t);

    struct grammar *grammar;

    struct generator_internal *internal;
};

void generate(struct generator *);

#endif
