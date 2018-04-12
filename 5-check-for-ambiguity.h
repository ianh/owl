#ifndef _5_CHECK_FOR_AMBIGUITY_H_
#define _5_CHECK_FOR_AMBIGUITY_H_

#include "4-determinize.h"

struct ambiguity_path {
    symbol_id *symbols;
    uint16_t *actions;
    uint32_t length;
};

struct ambiguities {
    struct ambiguity_path *paths;
    uint32_t paths_allocated_bytes;
    uint32_t number_of_paths;
};

void check_for_ambiguity(struct combined_grammar *combined,
 struct bracket_transitions *determinized_transitions);

#endif
