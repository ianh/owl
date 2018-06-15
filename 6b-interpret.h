#ifndef _6B_INTERPRET_H_
#define _6B_INTERPRET_H_

#include "4-check-for-ambiguity.h"
#include "5-determinize.h"
#include "6b-fancy-tree-output.h"

#include <stdio.h>

// STEP 6B - INTERPRET

struct interpreter {
    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;

    struct terminal_info terminal_info;
};

// `text` is a zero-terminated string.
void interpret(struct interpreter *interpreter, const char *text, FILE *output);

// Interpret ambiguous paths and output the resulting tree using our fancy
// tree-drawing code.
void output_ambiguity(struct interpreter *interpreter,
 struct ambiguity *ambiguity, FILE *output);

#endif
