#include "4-determinize.h"

#include "bitset.h"

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

// Insert or look up the deterministic state id for a subset.  If a new state
// is created, `*next_state` will be incremented and the new state will be added
// to the worklist.
static state_id deterministic_state_for_subset(struct subset_table *table,
 struct worklist *worklist, struct state_array *states, state_id *next_state);

// Find the set of transition symbols corresponding to an accepting
// deterministic state.
static struct bitset transition_symbols_from_state(struct automaton *a,
 struct subset_table *subsets, state_id state);

enum options {
    // We provide an option to ignore the start state in order to determinize
    // reversed automata -- otherwise Brzozowski's algorithm won't work.
    INCLUDE_START_STATE = 0,
    IGNORE_START_STATE = 1,

    // Mark the accepting states of a bracket automaton using the deterministic
    // transition symbols in in_transitions.
    MARK_ACCEPTING_BRACKET_STATES = 2,
};

static void determinize_automaton(struct automaton *a,
 struct automaton *result, struct bracket_transitions in_transitions,
 struct bracket_transitions *out_transitions, symbol_id next_transition_symbol,
 enum options options)
{
    automaton_compute_epsilon_closure(a);

    struct subset_table subsets = {0};
    struct worklist worklist = {0};

    struct bitset bracket_symbols = bitset_create_empty(a->number_of_symbols);
    for (uint32_t i = 0; i < in_transitions.number_of_transitions; ++i) {
        struct bracket_transition transition = in_transitions.transitions[i];
        bitset_union(&bracket_symbols, &transition.transition_symbols);
    }

    state_id next_state = 0;
    struct state_array next_subset = {0};
    if (!(options & IGNORE_START_STATE))
        state_array_push(&next_subset, a->start_state);
    state_array_push_array(&next_subset,
     &a->epsilon_closure_for_state[a->start_state]);
    result->start_state = deterministic_state_for_subset(&subsets, &worklist,
     &next_subset, &next_state);

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

        for (symbol_id symbol = 0; symbol < a->number_of_symbols; ++symbol) {
            if (bitset_contains(&bracket_symbols, symbol)) {
                // We handle bracket symbols separately below.
                continue;
            }
            for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                struct state state = a->states[subset->states[i]];
                for (uint32_t j = 0; j < state.number_of_transitions; ++j) {
                    struct transition transition = state.transitions[j];
                    if (transition.symbol != symbol)
                        continue;
                    state_array_push(&next_subset, transition.target);
                    state_array_push_array(&next_subset,
                     &a->epsilon_closure_for_state[transition.target]);
                }
            }
            if (next_subset.number_of_states == 0)
                continue;
            state_id target = deterministic_state_for_subset(&subsets,
             &worklist, &next_subset, &next_state);
            automaton_add_transition(result, state, target, symbol);
        }

        // FIXME: Unify this code with the code above.
        for (uint32_t n = 0; n < in_transitions.number_of_transitions; ++n) {
            struct bracket_transition t = in_transitions.transitions[n];
            for (uint32_t i = 0; i < subset->number_of_states; ++i) {
                struct state state = a->states[subset->states[i]];
                for (uint32_t j = 0; j < state.number_of_transitions; ++j) {
                    struct transition transition = state.transitions[j];
                    if (transition.symbol == SYMBOL_EPSILON)
                        continue;
                    if (!bitset_contains(&t.transition_symbols,
                     transition.symbol)) {
                        continue;
                    }
                    state_array_push(&next_subset, transition.target);
                    state_array_push_array(&next_subset,
                     &a->epsilon_closure_for_state[transition.target]);
                }
            }
            if (next_subset.number_of_states == 0)
                continue;
            state_id target = deterministic_state_for_subset(&subsets,
             &worklist, &next_subset, &next_state);
            automaton_add_transition(result, state, target,
             t.deterministic_transition_symbol);
        }
    }

    for (uint32_t i = 0; i < subsets.available_size; ++i) {
        if (!subsets.subsets[i])
            continue;
        struct state *state = &result->states[subsets.subset_states[i]];
        if (!state->accepting)
            continue;

        if (out_transitions) {
            struct bracket_transitions *ts = out_transitions;
            uint32_t j = ts->number_of_transitions++;
            ts->transitions = grow_array(ts->transitions,
             &ts->transitions_allocated_bytes, ts->number_of_transitions *
             sizeof(struct bracket_transition));
            state->transition_symbol = next_transition_symbol++;
            ts->transitions[j].deterministic_transition_symbol =
             state->transition_symbol;
            ts->transitions[j].transition_symbols =
             transition_symbols_from_state(a, &subsets, i);
        }
        if (options & MARK_ACCEPTING_BRACKET_STATES) {
            struct bitset s = transition_symbols_from_state(a, &subsets, i);
            uint32_t j;
            for (j = 0; j < in_transitions.number_of_transitions; ++j) {
                struct bracket_transition t = in_transitions.transitions[j];
                if (bitset_compare(&t.transition_symbols, &s))
                    continue;
                state->transition_symbol = t.deterministic_transition_symbol;
                break;
            }
        }
    }

    state_array_destroy(&next_subset);
}

static uint32_t fnv(const void *dataPointer, size_t length);
static int compare_state_ids(const void *aa, const void *bb);
static int compare_bracket_transitions(const void *aa, const void *bb);
static bool equal_bracket_transitions(struct bracket_transitions *a,
 struct bracket_transitions *b);

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

void determinize(struct combined_grammar *grammar,
 struct deterministic_grammar *result, struct bracket_transitions *transitions)
{
    determinize_automaton(&grammar->automaton, &result->automaton, *transitions,
     0, 0, INCLUDE_START_STATE);
    determinize_automaton(&grammar->bracket_automaton,
     &result->bracket_automaton, *transitions, 0, 0,
     INCLUDE_START_STATE | MARK_ACCEPTING_BRACKET_STATES);
}

void determinize_bracket_transitions(struct bracket_transitions *result,
 struct combined_grammar *grammar)
{
    symbol_id next_transition_symbol = grammar->automaton.number_of_symbols;
    if (grammar->bracket_automaton.number_of_symbols > next_transition_symbol)
        next_transition_symbol = grammar->bracket_automaton.number_of_symbols;

    struct automaton a = {0};
    struct bracket_transitions transitions = {0};
    while (true) {
        determinize_automaton(&grammar->bracket_automaton, &a, transitions,
         result, next_transition_symbol, INCLUDE_START_STATE);
        qsort(result->transitions, result->number_of_transitions,
         sizeof(struct bracket_transition), compare_bracket_transitions);
        if (equal_bracket_transitions(&transitions, result))
            break;
#if 1
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

// This is Brzozowski's algorithm.
void determinize_minimize(struct automaton *input, struct automaton *result)
{
    struct automaton reversed = {0};
    struct automaton dfa = {0};
    static const struct bracket_transitions none;
    automaton_reverse(input, &reversed);
    determinize_automaton(&reversed, &dfa, none, 0, 0, IGNORE_START_STATE);
    automaton_clear(&reversed);
    automaton_reverse(&dfa, &reversed);
    determinize_automaton(&reversed, result, none, 0, 0, IGNORE_START_STATE);
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

// This is the 32-bit FNV-1a hash diffusion algorithm.
// http://www.isthe.com/chongo/tech/comp/fnv/index.html

static uint32_t fnvPrime = 0x01000193;
static uint32_t fnvInitial = 0x811c9dc5;

static uint32_t fnv(const void *dataPointer, size_t length)
{
    uint32_t hash = fnvInitial;
    const unsigned char *data = dataPointer;
    for (size_t i = 0; i < length; ++i) {
        hash ^= data[i];
        hash *= fnvPrime;
    }
    return hash;
}
