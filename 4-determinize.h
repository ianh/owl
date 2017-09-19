#ifndef _4_DETERMINIZE_H_
#define _4_DETERMINIZE_H_

#include "automaton.h"

// STEP 4 - DETERMINIZE

// Step 2 (build) uses this function to determinize and minimize rules.
void determinize_minimize(struct automaton *input, struct automaton *result);

#endif
