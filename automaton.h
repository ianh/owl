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
struct state;
struct transition;

struct automaton {
    struct state *states;
    uint32_t states_allocated_bytes;
    uint32_t number_of_states;

    state_id start_state;

    // Used during determinization to iterate over all possible symbols.
    symbol_id number_of_symbols;

    struct state_array *epsilon_closure_for_state;
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

void automaton_add_transition(struct automaton *a, state_id source,
 state_id target, symbol_id symbol);
void automaton_add_transition_with_action(struct automaton *a, state_id source,
 state_id target, symbol_id symbol, uint16_t action);
void automaton_mark_accepting_state(struct automaton *a, state_id state);

// Returns the start state of the embedded automaton.
state_id automaton_embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action);

void automaton_compute_epsilon_closure(struct automaton *a);
void automaton_reverse(struct automaton *a, struct automaton *reversed);
void automaton_print(struct automaton *a);

void automaton_clear(struct automaton *a);
void automaton_destroy(struct automaton *a);

#endif
