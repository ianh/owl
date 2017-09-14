#include "automaton.h"

#include "grow-array.h"

#include <stdio.h>
#include <stdlib.h>

static void state_add_transition(struct state *s, state_id target,
 symbol_id symbol)
{
    if (s->number_of_transitions == 0xffff) {
        fprintf(stderr, "error: too many transitions for a single state.\n");
        exit(-1);
    }
    uint16_t id = s->number_of_transitions++;
    s->transitions = grow_array(s->transitions, &s->transitions_allocated_bytes,
     (uint32_t)s->number_of_transitions * sizeof(struct transition));
    s->transitions[id] = (struct transition){
        .target = target,
        .symbol = symbol,
    };
}

void automaton_add_transition(struct automaton *a, state_id source,
 state_id target, symbol_id symbol)
{
    state_id max_state = source;
    if (target > source)
        max_state = target;
    if (max_state >= a->number_of_states) {
        a->number_of_states = (uint32_t)max_state + 1;
        a->states = grow_array(a->states, &a->states_allocated_bytes,
         a->number_of_states * sizeof(struct state));
    }
    state_add_transition(&a->states[source], target, symbol);
}

void automaton_mark_accepting_state(struct automaton *a, state_id state)
{
    a->states = grow_array(a->states, &a->states_allocated_bytes,
     ((uint32_t)state + 1) * sizeof(struct state));
    a->states[state].accepting = true;
}

void automaton_print(struct automaton *a)
{
    printf("start = %u\n", a->start_state);
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        struct state *s = &a->states[i];
        if (s->accepting) {
            if (s->transition_symbol)
                printf("%4u   -- accept --> %u\n", i, s->transition_symbol);
            else
                printf("%4u   -- accept -->\n", i);
        }
        for (uint32_t j = 0; j < s->number_of_transitions; ++j) {
            struct transition *t = &s->transitions[j];
            if (t->symbol == SYMBOL_EPSILON)
                printf("%4u   ---------->%4u\n", i, t->target);
            else
                printf("%4u   - %4x   ->%4u\n", i, t->symbol, t->target);
        }
    }
}
