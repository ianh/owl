#ifndef _3_COMBINE_H_
#define _3_COMBINE_H_

#include "2-build.h"

// STEP 3 - COMBINE

struct combined_grammar {
    struct automaton automaton;
    struct automaton bracket_automaton;

    state_id final_nfa_state;
};

void combine(struct combined_grammar *result, struct grammar *grammar);

#endif
