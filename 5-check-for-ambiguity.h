#ifndef _5_CHECK_FOR_AMBIGUITY_H_
#define _5_CHECK_FOR_AMBIGUITY_H_

#include "4-determinize.h"

struct ambiguity_path {
    uint16_t *actions;
    uint32_t actions_allocated_bytes;
    size_t *offsets;
    uint32_t offsets_allocated_bytes;
    uint32_t number_of_actions;

    // TODO: the tokens ought to be shared
    symbol_id *tokens;
    uint32_t tokens_allocated_bytes;
    uint32_t number_of_tokens;
};

struct ambiguities {
    bool has_ambiguity;
    struct ambiguity_path paths[2];
};

void check_for_ambiguity(struct combined_grammar *combined,
 struct bracket_transitions *determinized_transitions,
 struct ambiguities *ambiguities);

#endif
