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

    // These symbols continue on past the last token symbol.
    uint32_t number_of_bracket_transition_symbols;

    bool root_rule_is_expression;
};

void combine(struct combined_grammar *result, struct grammar *grammar);

void combined_grammar_destroy(struct combined_grammar *grammar);

#endif
