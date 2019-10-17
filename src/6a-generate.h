#ifndef GENERATE_H
#define GENERATE_H

#include "5-determinize.h"
#include <stdlib.h>

struct generator {
    void (*output)(const char *, size_t);

    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;
    struct grammar_version version;
};

void generate(struct generator *);

#endif
