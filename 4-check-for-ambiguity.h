#ifndef _4_CHECK_FOR_AMBIGUITY_H_
#define _4_CHECK_FOR_AMBIGUITY_H_

#include "3-combine.h"

// STEP 4 - CHECK FOR AMBIGUITY

struct ambiguity_path {
    uint16_t *actions;
    uint32_t number_of_actions;

    // These are the offsets into the tokens list corresponding to each action.
    uint32_t *offsets;
};

struct ambiguity {
    bool has_ambiguity;
    struct ambiguity_path paths[2];

    symbol_id *tokens;
    uint32_t tokens_allocated_bytes;
    uint32_t number_of_tokens;
};

void check_for_ambiguity(struct combined_grammar *combined,
 struct ambiguity *ambiguity);

#endif
