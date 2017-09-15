#ifndef _5_DETERMINIZE_H_
#define _5_DETERMINIZE_H_

#include "automaton.h"

// STEP 5 - DETERMINIZE

// Step 2 (build) uses this function to determinize and minimize rules.
void determinize_minimize(struct automaton *input, struct automaton *result);

#endif
