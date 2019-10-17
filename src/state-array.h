#ifndef STATE_ARRAY_H
#define STATE_ARRAY_H

#include "grow-array.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t state_id;

struct state_array {
    state_id *states;
    uint32_t states_allocated_bytes;
    uint32_t number_of_states;
};

static inline void state_array_push(struct state_array *a, state_id state)
{
    if (a->number_of_states == UINT32_MAX)
        abort();
    uint32_t i = a->number_of_states++;
    a->states = grow_array(a->states, &a->states_allocated_bytes,
     sizeof(state_id) * a->number_of_states);
    a->states[i] = state;
}

static inline void state_array_push_array(struct state_array *a,
 struct state_array *b)
{
    a->states = grow_array(a->states, &a->states_allocated_bytes,
     sizeof(state_id) * (a->number_of_states + b->number_of_states));
    memcpy(a->states + a->number_of_states, b->states,
     b->number_of_states * sizeof(state_id));
    a->number_of_states += b->number_of_states;
}

static inline state_id state_array_pop(struct state_array *a)
{
    if (a->number_of_states == 0)
        abort();
    return a->states[--a->number_of_states];
}

static inline struct state_array state_array_move(struct state_array *a)
{
    struct state_array moved = *a;
    *a = (struct state_array){0};
    return moved;
}

static inline void state_array_clear(struct state_array *a)
{
    a->number_of_states = 0;
}

static inline void state_array_destroy(struct state_array *a)
{
    free(a->states);
    *a = (struct state_array){0};
}

#endif
