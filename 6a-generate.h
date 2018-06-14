#ifndef _6A_GENERATE_H_
#define _6A_GENERATE_H_

#include "5-determinize.h"
#include <stdlib.h>

struct generator_output;

struct generator {
    void (*output)(const char *, size_t);

    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;
};

void generate(struct generator *);

#endif
