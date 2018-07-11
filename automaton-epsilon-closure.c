#include "automaton.h"

#include "alloc.h"
#include "fnv.h"
#include <assert.h>

struct state_table {
    state_id *states;
    uint32_t *state_hashes;
    uint32_t *state_indexes;
    bool *used;

    uint32_t available_size;
    uint32_t used_size;
};

static uint32_t append_to_action_path(struct state_table *table,
 struct epsilon_closure *closure, uint32_t previous_action_indexes,
 uint16_t action);
static bool closure_entry_add(struct state_table *table,
 struct epsilon_closure *closure, state_id previous_state, state_id state,
 uint16_t action);
static void closure_action_add(struct epsilon_closure *, uint16_t action);
static uint32_t *state_table_add(struct state_table *table, state_id state);
static uint32_t state_table_lookup(struct state_table *table, state_id state,
 uint32_t hash);

void automaton_compute_epsilon_closure(struct automaton *a,
 enum automaton_epsilon_closure_mode mode)
{
    if (a->epsilon_closure_mode != mode) {
        a->epsilon_closure_mode = mode;
        automaton_invalidate_epsilon_closure(a);
    }
    if (a->epsilon_closure_for_state != 0)
        return;
    // Ensure we've actually created a start state.
    automaton_set_start_state(a, a->start_state);
    uint32_t n = a->number_of_states;
    a->epsilon_closure_for_state = calloc(n, sizeof(struct epsilon_closure));
    struct state_array worklist = {0};
    struct state_table visited = {0};
    bool ignore_action_transitions =
     a->epsilon_closure_mode == IGNORE_ACTION_TRANSITIONS;
    for (state_id i = 0; i < n; ++i) {
        struct epsilon_closure *closure = &a->epsilon_closure_for_state[i];
        state_array_push(&worklist, i);
        state_table_add(&visited, i);
        while (worklist.number_of_states > 0) {
            state_id id = state_array_pop(&worklist);
            struct state *state = &a->states[id];
            uint32_t number_of_transitions = state->number_of_transitions;
            for (uint32_t j = 0; j < number_of_transitions; ++j) {
                struct transition transition = state->transitions[j];
                if (transition.symbol != SYMBOL_EPSILON)
                    continue;
                if (ignore_action_transitions && transition.action != 0)
                    continue;
                state_id target = transition.target;
                state_id action = transition.action;
//                // Uncomment me to get ambiguous paths like "" vs "a" instead
//                // of "a" vs "a a"!
//                if (target == i)
//                    closure_entry_add(&visited, closure, UINT32_MAX, i, 0);
                if (closure_entry_add(&visited, closure, id, target, action))
                    state_array_push(&worklist, target);
            }
        }
        state_array_clear(&worklist);
        memset(visited.used, 0, visited.available_size * sizeof(bool));
        visited.used_size = 0;
    }
    state_array_destroy(&worklist);
    free(visited.states);
    free(visited.state_hashes);
    free(visited.state_indexes);
    free(visited.used);
}

static bool closure_entry_add(struct state_table *table,
 struct epsilon_closure *closure, state_id previous_state, state_id state,
 uint16_t action)
{
    uint32_t *state_index = state_table_add(table, state);
    if (*state_index == UINT32_MAX) {
        *state_index = closure->reachable.number_of_states;
        state_array_push(&closure->reachable, state);
        closure->action_indexes = grow_array(closure->action_indexes,
         &closure->action_indexes_allocated_bytes,
         sizeof(uint32_t) * closure->reachable.number_of_states);
        closure->ambiguous_action_indexes =
         grow_array(closure->ambiguous_action_indexes,
          &closure->ambiguous_action_indexes_allocated_bytes,
          sizeof(uint32_t) * closure->reachable.number_of_states);
        closure->action_indexes[*state_index] = UINT32_MAX;
        closure->ambiguous_action_indexes[*state_index] = UINT32_MAX;
    }
    if (closure->action_indexes[*state_index] != UINT32_MAX &&
     closure->ambiguous_action_indexes[*state_index] != UINT32_MAX)
        return false;
    bool first_visit = closure->action_indexes[*state_index] == UINT32_MAX;
    uint32_t previous = state_table_lookup(table, previous_state,
     fnv(&previous_state, sizeof(previous_state)));
    uint32_t previous_state_index = table->state_indexes[previous];
    if (!table->used[previous] || previous_state_index == UINT32_MAX) {
        // We'll visit this state twice in succession if there are two
        // transitions from a single state to this one.  So we have to handle
        // both cases.
        if (first_visit) {
            closure->action_indexes[*state_index] =
             append_to_action_path(table, closure, UINT32_MAX, action);
        } else {
            closure->ambiguous_action_indexes[*state_index] =
             append_to_action_path(table, closure, UINT32_MAX, action);
        }
        return true;
    }
    if (first_visit) {
        closure->action_indexes[*state_index] = append_to_action_path(table,
         closure, closure->action_indexes[previous_state_index], action);
    }
    if (closure->ambiguous_action_indexes[previous_state_index] != UINT32_MAX) {
        closure->ambiguous_action_indexes[*state_index] =
         append_to_action_path(table, closure,
         closure->ambiguous_action_indexes[previous_state_index], action);
    } else if (!first_visit) {
        closure->ambiguous_action_indexes[*state_index] =
         append_to_action_path(table, closure,
         closure->action_indexes[previous_state_index], action);
    }
    return true;
}

static uint32_t append_to_action_path(struct state_table *table,
 struct epsilon_closure *closure, uint32_t previous_action_indexes,
 uint16_t action)
{
    uint32_t action_index = closure->number_of_actions;
    if (action)
        closure_action_add(closure, action);
    if (previous_action_indexes != UINT32_MAX) {
        uint32_t i = previous_action_indexes;
        if (!action)
            return i;
        while (closure->actions[i]) {
            closure_action_add(closure, closure->actions[i]);
            i++;
        }
    }
    closure_action_add(closure, 0);
    return action_index;
}

static void closure_action_add(struct epsilon_closure *closure, uint16_t action)
{
    if (closure->number_of_actions == UINT32_MAX)
        abort();
    uint32_t index = closure->number_of_actions++;
    closure->actions = grow_array(closure->actions,
     &closure->actions_allocated_bytes,
     sizeof(uint16_t) * closure->number_of_actions);
    closure->actions[index] = action;
}

void automaton_invalidate_epsilon_closure(struct automaton *a)
{
    if (a->epsilon_closure_for_state == 0)
        return;
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        struct epsilon_closure *closure = &a->epsilon_closure_for_state[i];
        state_array_destroy(&closure->reachable);
        free(closure->ambiguous_action_indexes);
        free(closure->action_indexes);
        free(closure->actions);
    }
    free(a->epsilon_closure_for_state);
    a->epsilon_closure_for_state = 0;
}

static uint32_t *state_table_add(struct state_table *table, state_id state)
{
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        struct state_table old = *table;
        uint32_t n = old.available_size * 2;
        if (n == 0)
            n = 4;
        table->states = calloc(n, sizeof(state_id));
        table->state_hashes = calloc(n, sizeof(uint32_t));
        table->state_indexes = calloc(n, sizeof(uint32_t));
        table->used = calloc(n, sizeof(bool));
        table->available_size = n;
        table->used_size = 0;
        for (uint32_t i = 0; i < old.available_size; ++i) {
            if (!old.used[i])
                continue;
            uint32_t *state_index = state_table_add(table, old.states[i]);
            *state_index = old.state_indexes[i];
        }
        free(old.states);
        free(old.state_hashes);
        free(old.state_indexes);
        free(old.used);
    }

    uint32_t hash = fnv(&state, sizeof(state));
    uint32_t index = state_table_lookup(table, state, hash);
    if (!table->used[index]) {
        table->states[index] = state;
        table->state_hashes[index] = hash;
        table->state_indexes[index] = UINT32_MAX;
        table->used[index] = true;
        table->used_size++;
    }
    return &table->state_indexes[index];
}

static uint32_t state_table_lookup(struct state_table *table, state_id state,
 uint32_t hash)
{
    uint32_t mask = table->available_size - 1;
    uint32_t index = hash & mask;
    while (true) {
        if (!table->used[index])
            return index;
        if (table->states[index] == state)
            return index;
        index = (index + 1) & mask;
        if (index == (hash & mask))
            abort();
    }
}
