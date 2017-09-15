#ifndef _STATE_SET_H_
#define _STATE_SET_H_

#include <stdint.h>
#include <stdlib.h>

typedef uint32_t state_id;

struct state_set {
    uint64_t *bit_groups;
    uint32_t number_of_bit_groups;
};

static inline struct state_set state_set_create_empty(uint32_t number_of_states)
{
    uint32_t number_of_groups = (number_of_states + 63) / 64;
    return (struct state_set){
        .bit_groups = calloc(number_of_groups, sizeof(uint64_t)),
        .number_of_bit_groups = number_of_groups,
    };
}

static inline void state_set_destroy(struct state_set *set)
{
    free(set->bit_groups);
}

static inline void state_set_clear(struct state_set *set)
{
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] = 0;
}

static inline bool state_set_contains(struct state_set *set, state_id state)
{
    return set->bit_groups[state / 64] & (1ULL << (state % 64));
}

static inline void state_set_add(struct state_set *set, state_id state)
{
    set->bit_groups[state / 64] |= (1ULL << (state % 64));
}

#endif
