#include "automaton.h"

#include "state-array.h"
#include "state-set.h"

#include <stdio.h>
#include <stdlib.h>

static void invalidate_epsilon_closure(struct automaton *a)
{
    if (a->epsilon_closure_for_state == 0)
        return;
    for (uint32_t i = 0; i < a->number_of_states; ++i)
        state_array_destroy(&a->epsilon_closure_for_state[i]);
    free(a->epsilon_closure_for_state);
    a->epsilon_closure_for_state = 0;
}

static void state_add_transition(struct state *s, state_id target,
 symbol_id symbol, uint16_t action)
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
        .action = action,
    };
}

void automaton_add_transition(struct automaton *a, state_id source,
 state_id target, symbol_id symbol)
{
    automaton_add_transition_with_action(a, source, target, symbol, 0);
}

static void grow_states(struct automaton *a, uint32_t state)
{
    if (state < a->number_of_states)
        return;
    a->number_of_states = (uint32_t)state + 1;
    a->states = grow_array(a->states, &a->states_allocated_bytes,
     a->number_of_states * sizeof(struct state));
}

void automaton_add_transition_with_action(struct automaton *a, state_id source,
 state_id target, symbol_id symbol, uint16_t action)
{
    invalidate_epsilon_closure(a);
    state_id max_state = source;
    if (target > source)
        max_state = target;
    grow_states(a, max_state);
    if (symbol != SYMBOL_EPSILON && symbol >= a->number_of_symbols)
        a->number_of_symbols = symbol + 1;
    state_add_transition(&a->states[source], target, symbol, action);
}

void automaton_mark_accepting_state(struct automaton *a, state_id state)
{
    grow_states(a, state);
    a->states[state].accepting = true;
}

state_id automaton_embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action)
{
    uint32_t m = from->number_of_states;
    uint32_t n = into->number_of_states;
    for (state_id i = 0; i < m; ++i) {
        struct state s = from->states[i];
        if (s.accepting) {
            automaton_add_transition_with_action(into, i + n, out_state,
             out_symbol, out_action);
        }
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            automaton_add_transition(into, i + n, t.target + n, t.symbol);
        }
    }
    return from->start_state + n;
}

void automaton_compute_epsilon_closure(struct automaton *a)
{
    if (a->epsilon_closure_for_state != 0)
        return;
    uint32_t n = a->number_of_states;
    a->epsilon_closure_for_state = calloc(n, sizeof(struct state_array));
    struct state_array worklist = {0};
    struct state_set visited = state_set_create_empty(n);
    for (state_id i = 0; i < n; ++i) {
        struct state_array *closure = &a->epsilon_closure_for_state[i];
        state_array_push(&worklist, i);
        state_set_add(&visited, i);
        while (worklist.number_of_states > 0) {
            state_id id = state_array_pop(&worklist);
            if (id != i)
                state_array_push(closure, id);
            struct state *state = &a->states[id];
            uint32_t number_of_transitions = state->number_of_transitions;
            for (uint32_t j = 0; j < number_of_transitions; ++j) {
                if (state->transitions[j].symbol != SYMBOL_EPSILON)
                    continue;
                state_id target = state->transitions[j].target;
                if (!state_set_contains(&visited, target)) {
                    state_set_add(&visited, target);
                    state_array_push(&worklist, target);
                }
            }
        }
        state_array_clear(&worklist);
        state_set_clear(&visited);
    }
    state_array_destroy(&worklist);
    state_set_destroy(&visited);
}

void automaton_reverse(struct automaton *a, struct automaton *reversed)
{
    state_id start_state = a->number_of_states;
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        struct state *s = &a->states[i];
        for (uint32_t j = 0; j < s->number_of_transitions; ++j) {
            struct transition *t = &s->transitions[j];
            automaton_add_transition(reversed, t->target, i, t->symbol);
        }
        if (s->accepting)
            automaton_add_transition(reversed, start_state, i, SYMBOL_EPSILON);
    }
    reversed->start_state = start_state;
    automaton_mark_accepting_state(reversed, a->start_state);
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
                printf("%4u   ---------->%4u", i, t->target);
            else
                printf("%4u   - %4x   ->%4u", i, t->symbol, t->target);
            if (t->action != 0)
                printf(" (%x)\n", t->action);
            else
                printf("\n");
        }
    }
}

void automaton_clear(struct automaton *a)
{
    invalidate_epsilon_closure(a);
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        free(a->states[i].transitions);
        memset(a->states, 0, a->states_allocated_bytes);
    }
    a->number_of_states = 0;
    a->number_of_symbols = 0;
    a->start_state = 0;
}

void automaton_destroy(struct automaton *a)
{
    automaton_clear(a);
    free(a->states);
}
