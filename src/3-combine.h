#ifndef COMBINE_H
#define COMBINE_H

#include "2-build.h"

// STEP 3 - COMBINE

// In this step, we combine all the automata from step 2 into just two: the
// automaton and the bracket_automaton.  The automaton is where parsing begins;
// the bracket_automaton is entered when a start token appears (and exited when
// an end token appears).

// Step 3 also annotates these automata with parse tree actions.  The actions
// along a path through the automata are interpreted to construct a parse tree.

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
    uint32_t number_of_bracket_symbols;

    bool root_rule_is_expression;
};

void combine(struct combined_grammar *result, struct grammar *grammar);

void combined_grammar_destroy(struct combined_grammar *grammar);

#define SHOULD_ALLOW_DASHES_IN_IDENTIFIERS(combined) \
 (find_token((combined)->tokens, (combined)->number_of_keyword_tokens, "-", 1, \
  TOKEN_DONT_CARE, 0) >= (combined)->number_of_keyword_tokens)

#endif
