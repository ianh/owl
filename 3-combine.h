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

// draft of state list:
//  0x0000  no action
//  0x0001  beginning of something
//  0x0002  write token to tree
//  0x0003  end of raw value output
//  0x8xxx  end of rule

#define ACTION_BEGIN 0x0001
#define ACTION_RENAME 0x4000
#define ACTION_END_RULE 0x8000

#endif
