#include "4-determinize.h"

#include "bitset.h"
#include "fnv.h"

// A subset_table is a hash table mapping subsets (represented as state arrays)
// to their state ids in the deterministic automaton.
struct subset_table {
    struct state_array **subsets;
    uint32_t *subset_hashes;
    state_id *subset_states;

    uint32_t available_size;
    uint32_t used_size;
};

// This function transfers ownership of the `subset` array to the subset table.
// If the array doesn't already appear in the table, it will be inserted.
// Returns the index of the subset in the table.
static uint32_t subset_table_adopt_subset(struct subset_table *table,
 struct state_array *subset, uint32_t hash, state_id subset_state);

// The worklist stores a list of deterministic states (and their corresponding
// subsets) whose transitions have not yet been explored.
struct worklist {
    struct state_array **subsets;
    uint32_t subsets_allocated_bytes;

    state_id *subset_states;
    uint32_t subset_states_allocated_bytes;

    uint32_t number_of_subsets;
};

// Follows a single transition and (if `map` is nonzero) records the associated
// actions into the action map.
static void follow_subset_transition(struct automaton *a,
 state_id target_nfa_state, state_id nfa_state, state_id dfa_state,
 symbol_id nfa_symbol, symbol_id dfa_symbol, struct state_array *next_subset,
 struct action_map *map);

// Insert or look up the deterministic state id for a subset.  If a new state
// is created, `*next_state` will be incremented and the new state will be added
// to the worklist.
static state_id deterministic_state_for_subset(struct subset_table *table,
 struct worklist *worklist, struct state_array *states, state_id *next_state);

// Find the set of transition symbols corresponding to an accepting
// deterministic state.
static struct bitset transition_symbols_from_state(struct automaton *a,
 struct subset_table *subsets, state_id state);

static int compare_actions(const void *a, const void *b);

enum options {
    // We provide an option to ignore the start state in order to determinize
    // reversed automata -- otherwise Brzozowski's algorithm won't work.
    INCLUDE_START_STATE = 0,
    IGNORE_START_STATE = 1,

    // Mark the accepting states of a bracket automaton using the deterministic
    // transition symbols in `in_transitions`.
    MARK_ACCEPTING_BRACKET_STATES = 2,

    // Treat epsilon transitions with actions like symbols instead of like
    // epsilon transitions.  We use this when checking for ambiguity.
    DISAMBIGUATE = 4,
};

struct context {
    struct automaton *input;
    struct automaton *result;

    struct bracket_transitions in_transitions;
    struct bracket_transitions *out_transitions;
    symbol_id next_transition_symbol;

    struct action_map *action_map;

    enum options options;
};

static void determinize_automaton(struct context context)
{
    struct automaton *a = context.input;
    struct automaton *result = context.result;
    struct bracket_transitions in_transitions = context.in_transitions;

    automaton_compute_epsilon_closure(a, (context.options & DISAMBIGUATE) ?
     IGNORE_ACTION_TRANSITIONS : FOLLOW_ACTION_TRANSITIONS);

    struct subset_table subsets = {0};
    struct worklist worklist = {0};

    struct bitset bracket_symbols = bitset_create_empty(a->number_of_symbols);
    for (uint32_t i = 0; i < in_transitions.number_of_transitions; ++i) {
        struct bracket_transition transition = in_transitions.transitions[i];
        bitset_union(&bracket_symbols, &transition.transition_symbols);
    }

    state_id next_state = 0;
    struct state_array next_subset = {0};
    if (context.options & IGNORE_START_STATE) {
        // If we're producing an action map, we need to call
        // follow_subset_transition and include the start state.
        if (context.action_map)
            abort();
        state_array_push_array(&next_subset,
         &a->epsilon_closure_for_state[a->start_state].reachable);
    } else {
        follow_subset_transition(a, a->start_state, a->start_state, UINT32_MAX,
         SYMBOL_EPSILON, SYMBOL_EPSILON, &next_subset, context.action_map);
    }
    automaton_set_start_state(result, deterministic_state_for_subset(&subsets,
     &worklist, &next_subset, &next_state));

    while (worklist.number_of_subsets > 0) {
#if 0
        printf("worklist\n");
        for (uint32_t i = 0; i < worklist.number_of_subsets; ++i) {
            printf("%u (%p): ", worklist.subset_states[i], worklist.subsets[i]);
            for (uint32_t j = 0; j < worklist.subsets[i]->number_of_states; ++j) {
                printf("%u ", worklist.subsets[i]->states[j]);
            }
            printf("\n");
        }
        printf("end worklist\n");
#endif

        uint32_t worklist_index = --worklist.number_of_subsets;
        struct state_array *subset = worklist.subsets[worklist_index];
        state_id state = worklist.subset_states[worklist_index];

        for (state_id i = 0; i < subset->number_of_states; ++i) {
            if (a->states[subset->states[i]].accepting) {
                automaton_mark_accepting_state(result, state);
                break;
            }
        }

        // There are three kinds of transitions we potentially need to visit.
        // First, we visit normal symbol transitions.
        for (symbol_id symbol = 0; symbol < a->number_of_symbols; ++symbol) {
            if (bitset_contains(&bracket_symbols, symbol)) {
                // We handle bracket symbols separately below.
                continue;
            }
            for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                struct state s = a->states[subset->states[i]];
                for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                    struct transition transition = s.transitions[j];
                    if (transition.symbol != symbol)
                        continue;
                    follow_subset_transition(a, transition.target,
                     subset->states[i], state, symbol, symbol, &next_subset,
                     context.action_map);
                }
            }
            if (next_subset.number_of_states == 0)
                continue;
            state_id target = deterministic_state_for_subset(&subsets,
             &worklist, &next_subset, &next_state);
            automaton_add_transition(result, state, target, symbol);
        }

        // Next, we visit bracket symbol transitions.
        for (uint32_t n = 0; n < in_transitions.number_of_transitions; ++n) {
            struct bracket_transition t = in_transitions.transitions[n];
            for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                struct state s = a->states[subset->states[i]];
                for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                    struct transition transition = s.transitions[j];
                    if (transition.symbol == SYMBOL_EPSILON)
                        continue;
                    if (!bitset_contains(&t.transition_symbols,
                     transition.symbol)) {
                        continue;
                    }
                    follow_subset_transition(a, transition.target,
                     subset->states[i], state, transition.symbol,
                     t.deterministic_transition_symbol, &next_subset,
                     context.action_map);
                }
            }
            if (next_subset.number_of_states == 0)
                continue;
            state_id target = deterministic_state_for_subset(&subsets,
             &worklist, &next_subset, &next_state);
            automaton_add_transition(result, state, target,
             t.deterministic_transition_symbol);
        }

        // Finally, if we're "disambiguating", we visit action transitions.
        if (context.options & DISAMBIGUATE) {
            // Collect all the actions that appear as successors.
            uint16_t *actions = 0;
            uint32_t actions_allocated_bytes = 0;
            uint32_t number_of_actions = 0;
            for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                struct state s = a->states[subset->states[i]];
                for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                    struct transition transition = s.transitions[j];
                    if (transition.action == 0)
                        continue;
                    uint32_t k = number_of_actions++;
                    actions = grow_array(actions, &actions_allocated_bytes,
                     number_of_actions * sizeof(uint16_t));
                    actions[k] = transition.action;
                }
            }
            qsort(actions, number_of_actions, sizeof(uint16_t),
             compare_actions);
            // Traverse the list of actions, and build a transition for each
            // while ignoring duplicates.
            for (uint32_t n = 0; n < number_of_actions; ++n) {
                if (n > 0 && actions[n] == actions[n - 1])
                    continue;
                for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                    struct state s = a->states[subset->states[i]];
                    for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                        struct transition transition = s.transitions[j];
                        if (transition.action != actions[n])
                            continue;
                        follow_subset_transition(a, transition.target,
                         subset->states[i], state, transition.symbol,
                         transition.symbol, &next_subset, context.action_map);
                    }
                }
                if (next_subset.number_of_states == 0)
                    continue;
                state_id target = deterministic_state_for_subset(&subsets,
                 &worklist, &next_subset, &next_state);
                automaton_add_transition_with_action(result, state, target,
                 SYMBOL_EPSILON, actions[n]);
            }
        }
    }

    for (uint32_t i = 0; i < subsets.available_size; ++i) {
        if (!subsets.subsets[i])
            continue;
        struct state *state = &result->states[subsets.subset_states[i]];
        if (!state->accepting)
            continue;

        if (context.out_transitions) {
            struct bracket_transitions *ts = context.out_transitions;
            uint32_t j = ts->number_of_transitions++;
            ts->transitions = grow_array(ts->transitions,
             &ts->transitions_allocated_bytes, ts->number_of_transitions *
             sizeof(struct bracket_transition));
            state->transition_symbol = context.next_transition_symbol++;
            ts->transitions[j].deterministic_transition_symbol =
             state->transition_symbol;
            ts->transitions[j].transition_symbols =
             transition_symbols_from_state(a, &subsets, i);
        }
        if (context.options & MARK_ACCEPTING_BRACKET_STATES) {
            struct bitset s = transition_symbols_from_state(a, &subsets, i);
            uint32_t j;
            for (j = 0; j < in_transitions.number_of_transitions; ++j) {
                struct bracket_transition t = in_transitions.transitions[j];
                if (bitset_compare(&t.transition_symbols, &s))
                    continue;
                state->transition_symbol = t.deterministic_transition_symbol;
                break;
            }
            bitset_destroy(&s);
        }
    }

    state_array_destroy(&next_subset);
}

static void add_action_map_entry(struct action_map *map,
 struct action_map_entry entry);
static int compare_action_map_entries(const void *aa, const void *bb);

static int compare_state_ids(const void *aa, const void *bb);
static int compare_bracket_transitions(const void *aa, const void *bb);
static bool equal_bracket_transitions(struct bracket_transitions *a,
 struct bracket_transitions *b);

static void follow_subset_transition(struct automaton *a,
 state_id target_nfa_state, state_id nfa_state, state_id dfa_state,
 symbol_id nfa_symbol, symbol_id dfa_symbol, struct state_array *next_subset,
 struct action_map *map)
{
    struct epsilon_closure *closure;
    closure = &a->epsilon_closure_for_state[target_nfa_state];
    struct state_array *reachable = &closure->reachable;
    if (!map) {
        state_array_push(next_subset, target_nfa_state);
        state_array_push_array(next_subset, reachable);
        return;
    }

    uint32_t action_index = map->number_of_actions;
    map->number_of_actions += closure->number_of_actions;
    map->actions = grow_array(map->actions, &map->actions_allocated_bytes,
     map->number_of_actions * sizeof(uint16_t));
    memcpy(map->actions + action_index, closure->actions,
     closure->number_of_actions * sizeof(uint16_t));

    state_array_push(next_subset, target_nfa_state);
    add_action_map_entry(map, (struct action_map_entry){
        .dfa_state = dfa_state,
        .nfa_state = nfa_state,
        .target_nfa_state = target_nfa_state,
        .dfa_symbol = dfa_symbol,
        .nfa_symbol = nfa_symbol,
        .action_index = UINT32_MAX,
    });
    for (uint32_t i = 0; i < reachable->number_of_states; ++i) {
        state_array_push(next_subset, closure->reachable.states[i]);
        add_action_map_entry(map, (struct action_map_entry){
            .dfa_state = dfa_state,
            .nfa_state = nfa_state,
            .target_nfa_state = closure->reachable.states[i],
            .dfa_symbol = dfa_symbol,
            .nfa_symbol = nfa_symbol,
            .action_index = action_index + closure->action_indexes[i],
        });
    }
}

static void add_action_map_entry(struct action_map *map,
 struct action_map_entry entry)
{
    uint32_t index = map->number_of_entries++;
    map->entries = grow_array(map->entries, &map->entries_allocated_bytes,
     map->number_of_entries * sizeof(struct action_map_entry));
    map->entries[index] = entry;
}

static int compare_action_map_entries(const void *aa, const void *bb)
{
    const struct action_map_entry *a = aa;
    const struct action_map_entry *b = bb;
#define COMPARE(field) \
    if (a->field < b->field) \
        return -1; \
    if (a->field > b->field) \
        return 1;
    COMPARE(target_nfa_state);
    COMPARE(dfa_state);
    COMPARE(dfa_symbol);
#undef COMPARE
    return 0;
}

static state_id deterministic_state_for_subset(struct subset_table *table,
 struct worklist *worklist, struct state_array *states, state_id *next_state)
{
    struct state_array *subset = calloc(1, sizeof(struct state_array));
    *subset = state_array_move(states);

    // Sort the state set and remove duplicates for hashing and comparison.
    uint32_t n = subset->number_of_states;
    qsort(subset->states, n, sizeof(state_id), compare_state_ids);
    uint32_t removed = 0;
    for (uint32_t i = 1; i < n; ++i) {
        if (subset->states[i] == subset->states[i - 1])
            removed++;
        else if (removed > 0)
            subset->states[i - removed] = subset->states[i];
    }
    subset->number_of_states -= removed;

    // Hash the subset and place it into the table.
    uint32_t hash = fnv(subset->states,
     subset->number_of_states * sizeof(state_id));
    uint32_t idx = subset_table_adopt_subset(table, subset, hash, *next_state);

    if (table->subset_states[idx] == *next_state) {
        // This is a brand new state: insert it into the worklist so we can
        // continue to add its successor states.
        uint32_t i = worklist->number_of_subsets++;
        worklist->subsets = grow_array(worklist->subsets,
         &worklist->subsets_allocated_bytes,
         worklist->number_of_subsets * sizeof(struct state_array *));
        worklist->subset_states = grow_array(worklist->subset_states,
         &worklist->subset_states_allocated_bytes,
         worklist->number_of_subsets * sizeof(state_id));
        worklist->subsets[i] = table->subsets[idx];
        worklist->subset_states[i] = *next_state;
        (*next_state)++;
    }
    return table->subset_states[idx];
}

static void find_bracket_transitions(struct context context,
 struct bracket_transitions *result)
{
    struct automaton a = {0};
    struct bracket_transitions transitions = {0};
    context.result = &a;
    context.out_transitions = result;
    while (true) {
        context.in_transitions = transitions;
        determinize_automaton(context);
        qsort(result->transitions, result->number_of_transitions,
         sizeof(struct bracket_transition), compare_bracket_transitions);
        if (equal_bracket_transitions(&transitions, result))
            break;
#if DEBUG_OUTPUT
        printf("-\n");
        for (uint32_t i = 0; i < result->number_of_transitions; ++i) {
            struct bracket_transition t = result->transitions[i];
            printf("%x: ", t.deterministic_transition_symbol);
            for (uint32_t j = 0; j < grammar->bracket_automaton.number_of_symbols; ++j) {
                if (bitset_contains(&t.transition_symbols, j))
                    printf("%x ", j);
            }
            printf("\n");
        }
#endif
        transitions = *result;
        *result = (struct bracket_transitions){0};
    }
    automaton_destroy(&a);
}

void disambiguate(struct combined_grammar *grammar,
 struct disambiguated_grammar *result, struct bracket_transitions *transitions)
{
    determinize_automaton((struct context){
        .input = &grammar->automaton,
        .result = &result->automaton,
        .in_transitions = *transitions,
        .options = DISAMBIGUATE,
    });
    determinize_automaton((struct context){
        .input = &grammar->bracket_automaton,
        .result = &result->bracket_automaton,
        .in_transitions = *transitions,
        .options = MARK_ACCEPTING_BRACKET_STATES | DISAMBIGUATE,
    });
}

void disambiguate_bracket_transitions(struct bracket_transitions *result,
 struct combined_grammar *grammar)
{
    find_bracket_transitions((struct context){
        .input = &grammar->bracket_automaton,
        .next_transition_symbol =  grammar->number_of_tokens,
        .options = DISAMBIGUATE,
    }, result);
}

void determinize(struct combined_grammar *grammar,
 struct deterministic_grammar *result, struct bracket_transitions *transitions)
{
    determinize_automaton((struct context){
        .input = &grammar->automaton,
        .result = &result->automaton,
        .in_transitions = *transitions,
        .action_map = &result->action_map,
    });
    determinize_automaton((struct context){
        .input = &grammar->bracket_automaton,
        .result = &result->bracket_automaton,
        .in_transitions = *transitions,
        .action_map = &result->bracket_action_map,
        .options = MARK_ACCEPTING_BRACKET_STATES,
    });
    qsort(result->action_map.entries, result->action_map.number_of_entries,
     sizeof(struct action_map_entry), compare_action_map_entries);
    qsort(result->bracket_action_map.entries,
     result->bracket_action_map.number_of_entries,
     sizeof(struct action_map_entry), compare_action_map_entries);
}

void determinize_bracket_transitions(struct bracket_transitions *result,
 struct combined_grammar *grammar)
{
    find_bracket_transitions((struct context){
        .input = &grammar->bracket_automaton,
        .next_transition_symbol =  grammar->number_of_tokens,
    }, result);
}

struct action_map_entry *action_map_find(struct action_map *map,
 state_id target_nfa_state, state_id dfa_state, symbol_id dfa_symbol)
{
    struct action_map_entry query = {
        .target_nfa_state = target_nfa_state,
        .dfa_state = dfa_state,
        .dfa_symbol = dfa_symbol,
    };
    return bsearch(&query, map->entries, map->number_of_entries,
     sizeof(struct action_map_entry), compare_action_map_entries);
}

// This is Brzozowski's algorithm.
void determinize_minimize(struct automaton *input, struct automaton *result)
{
    struct automaton reversed = {0};
    struct automaton dfa = {0};
    automaton_reverse(input, &reversed);
    determinize_automaton((struct context){ .input = &reversed, .result = &dfa,
     .options = IGNORE_START_STATE });
    automaton_clear(&reversed);
    automaton_reverse(&dfa, &reversed);
    determinize_automaton((struct context){ .input = &reversed,
     .result = result, .options = IGNORE_START_STATE });
    automaton_destroy(&reversed);
    automaton_destroy(&dfa);
}

static uint32_t subset_table_adopt_subset(struct subset_table *table,
 struct state_array *subset, uint32_t hash, state_id subset_state)
{
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        // The table is too small to comfortably fit another element.  Double
        // its size and reinsert all entries at their new positions.
        struct subset_table old = *table;
        uint32_t n = old.available_size * 2;
        if (n == 0)
            n = 256;
        table->subsets = calloc(n, sizeof(struct state_array *));
        table->subset_hashes = calloc(n, sizeof(uint32_t));
        table->subset_states = calloc(n, sizeof(state_id));
        table->available_size = n;
        table->used_size = 0;
        for (uint32_t i = 0; i < old.available_size; ++i) {
            if (!old.subsets[i])
                continue;
            subset_table_adopt_subset(table, old.subsets[i],
             old.subset_hashes[i], old.subset_states[i]);
        }
        free(old.subsets);
        free(old.subset_hashes);
        free(old.subset_states);
    }

    // Find the index for our subset.  If the subset is already there, just
    // return the index.  Otherwise, insert it and return the index of the
    // newly-inserted subset.
    uint32_t mask = table->available_size - 1;
    uint32_t index = hash & mask;
    while (true) {
        if (table->subsets[index] == 0) {
            // We found an empty slot.  Insert the subset here.
            table->subsets[index] = subset;
            table->subset_hashes[index] = hash;
            table->subset_states[index] = subset_state;
            table->used_size++;
            return index;
        }

        // Check whether this slot contains our subset.
        if (table->subset_hashes[index] != hash)
            goto skip;
        struct state_array *key = table->subsets[index];
        if (subset->number_of_states != key->number_of_states)
            goto skip;
        uint32_t n = subset->number_of_states;
        for (uint32_t i = 0; i < n; ++i) {
            if (subset->states[i] != key->states[i])
                goto skip;
        }

        // If we got here, we've found the subset we're looking for.  This
        // function adopts the `subset` array, so we need to free it before
        // returning.
        free(subset);
        return index;

    skip:
        index = (index + 1) & mask;

        // We've looped all the way around the hash table.  Abort instead of
        // continuing to loop indefinitely.
        if (index == (hash & mask))
            abort();
    }
}

static int compare_state_ids(const void *aa, const void *bb)
{
    state_id a = *(state_id *)aa;
    state_id b = *(state_id *)bb;
    if (a < b)
        return -1;
    if (a > b)
        return 1;
    return 0;
}

static struct bitset transition_symbols_from_state(struct automaton *a,
 struct subset_table *subsets, state_id state)
{
    struct bitset symbols = bitset_create_empty(a->number_of_symbols);
    struct state_array *subset = subsets->subsets[state];
    for (uint32_t i = 0; i < subset->number_of_states; ++i) {
        struct state s = a->states[subset->states[i]];
        if (!s.accepting)
            continue;
        bitset_add(&symbols, s.transition_symbol);
    }
    return symbols;
}

static int compare_actions(const void *a, const void *b)
{
    return *(const uint16_t *)a - *(const uint16_t *)b;
}

static int compare_bracket_transitions(const void *aa, const void *bb)
{
    struct bitset *a = &((struct bracket_transition *)aa)->transition_symbols;
    struct bitset *b = &((struct bracket_transition *)bb)->transition_symbols;
    return bitset_compare(a, b);
}

static bool equal_bracket_transitions(struct bracket_transitions *a,
 struct bracket_transitions *b)
{
    if (a->number_of_transitions != b->number_of_transitions)
        return false;
    uint32_t n = a->number_of_transitions;
    for (uint32_t i = 0; i < n; ++i) {
        if (compare_bracket_transitions(&a->transitions[i], &b->transitions[i]))
            return false;
    }
    return true;
}
