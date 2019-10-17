#ifndef INTERPRET_H
#define INTERPRET_H

#include "4-check-for-ambiguity.h"
#include "5-determinize.h"
#include "6b-interpret-output.h"

#include <stdio.h>

// STEP 6B - INTERPRET

struct interpreter {
    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;

    struct terminal_info terminal_info;
    struct grammar_version version;
};

// `text` is a zero-terminated string.
void interpret(struct interpreter *interpreter, const char *text, FILE *output);

// Interpret ambiguous paths and output the resulting tree using our
// tree-drawing code.
void output_ambiguity(struct interpreter *interpreter,
 struct ambiguity *ambiguity, FILE *output);

#endif
