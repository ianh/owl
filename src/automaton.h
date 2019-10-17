#ifndef AUTOMATON_H
#define AUTOMATON_H

#include "state-array.h"

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t state_id;
typedef uint32_t symbol_id;

#define SYMBOL_EPSILON 0xffffffff

struct automaton;
struct epsilon_closure;
struct state;
struct transition;

enum automaton_epsilon_closure_mode {
    FOLLOW_ACTION_TRANSITIONS,
    IGNORE_ACTION_TRANSITIONS,
};

struct automaton {
    struct state *states;
    uint32_t states_allocated_bytes;
    uint32_t number_of_states;

    state_id start_state;

    // Used during determinization to iterate over all possible symbols.
    symbol_id number_of_symbols;

    enum automaton_epsilon_closure_mode epsilon_closure_mode;
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

    // If there are at least two paths to a single reachable state, we store a
    // second one here.  If the original state is reachable and the end state is
    // co-reachable, we have an ambiguity we can report.
    // If there's just one path, we store UINT32_MAX here.
    uint32_t *ambiguous_action_indexes;
    uint32_t ambiguous_action_indexes_allocated_bytes;

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

// Create a new empty state in the automaton.  You don't have to call this, but
// it can be useful for creating new states without having to add a transition,
// mark the state as accepting or set it as a start state.
state_id automaton_create_state(struct automaton *a);

void automaton_compute_epsilon_closure(struct automaton *a,
 enum automaton_epsilon_closure_mode mode);
void automaton_invalidate_epsilon_closure(struct automaton *a);
void automaton_reverse(struct automaton *a, struct automaton *reversed);
void automaton_print(struct automaton *a);

void automaton_copy(struct automaton *from, struct automaton *to);
void automaton_move(struct automaton *from, struct automaton *to);
void automaton_clear(struct automaton *a);
void automaton_destroy(struct automaton *a);

#endif
