#ifndef _3_COMBINE_H_
#define _3_COMBINE_H_

#include "2-build.h"

// STEP 3 - COMBINE

struct combined_grammar {
    struct automaton automaton;
    struct automaton bracket_automaton;

    state_id final_nfa_state;

    struct token *tokens;
    // The number of tokens, including both keyword tokens and "token class"
    // tokens (like identifier or number).
    uint32_t number_of_tokens;
    // The first `number_of_keyword_tokens` are keyword tokens.
    uint32_t number_of_keyword_tokens;

    bool root_rule_is_expression;
};

void combine(struct combined_grammar *result, struct grammar *grammar);

#endif
