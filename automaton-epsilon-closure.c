#include "automaton.h"

#include "bitset.h"
#include "fnv.h"

struct state_table {
    state_id *states;
    uint32_t *state_hashes;
    uint32_t *state_action_indexes;
    bool *used;

    uint32_t available_size;
    uint32_t used_size;
};

static bool closure_entry_add(struct state_table *table,
 struct epsilon_closure *closure, state_id previous_state, state_id state,
 uint16_t action);
static void closure_action_add(struct epsilon_closure *, uint16_t action);
static uint32_t *state_table_add(struct state_table *table, state_id state);
static uint32_t state_table_lookup(struct state_table *table, state_id state,
 uint32_t hash);

void automaton_compute_epsilon_closure(struct automaton *a)
{
    if (a->epsilon_closure_for_state != 0)
        return;
    uint32_t n = a->number_of_states;
    a->epsilon_closure_for_state = calloc(n, sizeof(struct epsilon_closure));
    struct state_array worklist = {0};
    struct state_table visited = {0};
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
                state_id target = transition.target;
                state_id action = transition.action;
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
    free(visited.state_action_indexes);
    free(visited.used);
}

static bool closure_entry_add(struct state_table *table,
 struct epsilon_closure *closure, state_id previous_state, state_id state,
 uint16_t action)
{
    uint32_t *action_index = state_table_add(table, state);
    if (!action_index) {
        // TODO: Report ambiguity?
        return false;
    }
    uint32_t index = closure->reachable.number_of_states;
    state_array_push(&closure->reachable, state);
    closure->action_indexes = grow_array(closure->action_indexes,
     &closure->action_indexes_allocated_bytes,
     closure->reachable.number_of_states * sizeof(uint32_t));
    *action_index = closure->number_of_actions;
    closure->action_indexes[index] = closure->number_of_actions;
    if (action)
        closure_action_add(closure, action);
    uint32_t previous = state_table_lookup(table, previous_state,
     fnv(&previous_state, sizeof(state)));
    uint32_t previous_action_index = table->state_action_indexes[previous];
    if (table->used[previous] && previous_action_index != UINT32_MAX) {
        if (!action) {
            *action_index = previous_action_index;
            closure->action_indexes[index] = previous_action_index;
            return true;
        }
        uint32_t i = previous_action_index;
        while (closure->actions[i]) {
            closure_action_add(closure, closure->actions[i]);
            i++;
        }
    }
    closure_action_add(closure, 0);
    return true;
}

static void closure_action_add(struct epsilon_closure *closure, uint16_t action)
{
    uint32_t index = closure->number_of_actions++;
    closure->actions = grow_array(closure->actions,
     &closure->actions_allocated_bytes,
     closure->number_of_actions * sizeof(uint16_t));
    closure->actions[index] = action;
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
        table->state_action_indexes = calloc(n, sizeof(uint32_t));
        table->used = calloc(n, sizeof(bool));
        table->available_size = n;
        table->used_size = 0;
        for (uint32_t i = 0; i < old.available_size; ++i) {
            if (!old.used[i])
                continue;
            uint32_t *action_index = state_table_add(table, old.states[i]);
            *action_index = old.state_action_indexes[i];
        }
        free(old.states);
        free(old.state_hashes);
        free(old.state_action_indexes);
        free(old.used);
    }

    uint32_t hash = fnv(&state, sizeof(state));
    uint32_t index = state_table_lookup(table, state, hash);
    if (table->used[index])
        return 0;
    table->states[index] = state;
    table->state_hashes[index] = hash;
    table->state_action_indexes[index] = UINT32_MAX;
    table->used[index] = true;
    table->used_size++;
    return &table->state_action_indexes[index];
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
