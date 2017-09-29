#ifndef _AUTOMATON_H_
#define _AUTOMATON_H_

#include "state-array.h"

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t state_id;
typedef uint32_t symbol_id;
typedef uint16_t token_id; // TODO: Should this be here?

#define SYMBOL_EPSILON 0xffffffff

struct automaton;
struct epsilon_closure;
struct state;
struct transition;

struct automaton {
    struct state *states;
    uint32_t states_allocated_bytes;
    uint32_t number_of_states;

    state_id start_state;

    // Used during determinization to iterate over all possible symbols.
    symbol_id number_of_symbols;

    struct epsilon_closure *epsilon_closure_for_state;
};

struct state {
    struct transition *transitions;
    uint32_t transitions_allocated_bytes;
    uint16_t number_of_transitions;

    bool accepting;
    symbol_id transition_symbol; // For accepting states in bracketed automata.
};

struct transition {
    symbol_id symbol;
    state_id target;

    uint16_t action;
};

struct epsilon_closure {
    struct state_array reachable;

    uint32_t *action_indexes;
    uint32_t action_indexes_allocated_bytes;

    uint16_t *actions;
    uint32_t actions_allocated_bytes;
    uint32_t number_of_actions;
};

void automaton_add_transition(struct automaton *a, state_id source,
 state_id target, symbol_id symbol);
void automaton_add_transition_with_action(struct automaton *a, state_id source,
 state_id target, symbol_id symbol, uint16_t action);
void automaton_set_start_state(struct automaton *a, state_id state);
void automaton_mark_accepting_state(struct automaton *a, state_id state);

void automaton_compute_epsilon_closure(struct automaton *a);
void automaton_reverse(struct automaton *a, struct automaton *reversed);
void automaton_print(struct automaton *a);

void automaton_clear(struct automaton *a);
void automaton_destroy(struct automaton *a);

#endif
