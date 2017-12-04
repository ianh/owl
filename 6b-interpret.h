#ifndef _6B_INTERPRET_H_
#define _6B_INTERPRET_H_

#include "4-determinize.h"

// STEP 6B - INTERPRET

// `text` is a zero-terminated string.
void interpret(struct grammar *grammar, struct combined_grammar *combined,
 struct bracket_transitions *transitions,
 struct deterministic_grammar *deterministic, struct bluebird_tree *tree,
 const unsigned char *text);

#endif
