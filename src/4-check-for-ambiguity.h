#ifndef CHECK_FOR_AMBIGUITY_H
#define CHECK_FOR_AMBIGUITY_H

#include "3-combine.h"

// STEP 4 - CHECK FOR AMBIGUITY

// In step 4, we look for a sequence of tokens which can cause two different
// parse trees to be created.  If we find one, we return it as a
// `struct ambiguity`.

struct ambiguity_path {
    uint16_t *actions;
    uint32_t number_of_actions;

    // These are the offsets into the `tokens` array corresponding to each
    // action.
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
