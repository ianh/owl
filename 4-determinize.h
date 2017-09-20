#ifndef _4_DETERMINIZE_H_
#define _4_DETERMINIZE_H_

#include "3-combine.h"
#include "bitset.h"

// STEP 4 - DETERMINIZE

struct bracket_transition {
    struct bitset transition_symbols;
    symbol_id deterministic_transition_symbol;
};
struct bracket_transitions {
    // Sorted by transition_symbols (lexicographically by array order).
    struct bracket_transition *transitions;
    uint32_t transitions_allocated_bytes;
    uint32_t number_of_transitions;
};

void determinize_bracket_transitions(struct bracket_transitions *result,
 struct combined_grammar *grammar);

// Step 2 (build) uses this function to determinize and minimize rules.
void determinize_minimize(struct automaton *input, struct automaton *result);

#endif
