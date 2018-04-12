#include "5-check-for-ambiguity.h"

#include "fnv.h"
#include <assert.h>
// pair -> shortest ambiguity-exercising path to that pair

// output: list of actions and symbols

// forward:
// - search over triples (a, b, e)
// - start from (start, start, 'allow')
// - to get successors:
//  - iterate over symbol successors (when the symbols are the same)
//   - successors are (s(a), s(b), 'allow')
//  - iterate over bracket successors (when the symbols are in the same subset)
//   - successors are (s(a), s(b), 'allow')
//  - when e is 'allow', iterate over epsilon closures
//   - successors are (s(a), s(b), 'disallow')
//   - when the epsilon closure entry itself has an ambiguity, mark the
//     successor state (s(a), s(b), 'disallow') as possibly ambiguous
// - if we find a state where a != b, mark the state as possibly ambiguous
// - when we mark a state as possibly ambiguous, we stop searching its
//   successors (because if they are co-reachable then so is the original state)
// backward:
// - search in a similar way, over triples (a, b, e)
// - start from (end, end, 'allow') AND (end, end, 'disallow') for each end
// - to get predecessors:
//  - when e is 'allow',
//   - iterate over symbol predecessors (when the symbols are the same)
//   - iterate over bracket predecessors (when the symbols are in the same subset)
//   - predecessors are (s(a), s(b), 'allow') AND (s(a), s(b), 'disallow')
//  - when e is 'disallow', iterate over epsilon closures
//   - predecessors are (s(a), s(b), 'allow')
// - if we find a state previously marked as possibly ambiguous, it's now
//   definitely ambiguous -- report it and stop searching the predecessors
//   (no point any more)

// This is the "epsilon filtering" technique from [1], simplified a little bit
// because we follow an entire path at once.
enum epsilon_state {
    ALLOW_EPSILON_SUCCESSORS,
    DISALLOW_EPSILON_SUCCESSORS,

    // Use this enum for some other exceptional cases.  Pairs with these states
    // should never appear in the actual table.
    INVALID,
    BOUNDARY,
};
struct state_pair {
    state_id a;
    state_id b;
    enum epsilon_state epsilon_state;
};
static struct state_pair invalid_state_pair =
 { UINT32_MAX, UINT32_MAX, INVALID };
static struct state_pair boundary_state_pair =
 { UINT32_MAX, UINT32_MAX, BOUNDARY };

// TODO: Explain this.
enum state_pair_mark {
    EMPTY,
    FORWARD_VISITED,
    BACKWARD_VISITED,
    FORWARD_MARKED,
    BACKWARD_MARKED,
};
enum mark_type {
    VISITED,
    MARKED,
};
enum direction {
    FORWARD,
    BACKWARD,
};
struct state_pair_table {
    struct state_pair *pairs;
    struct state_pair *preceding_pairs;
    struct state_pair *succeeding_pairs;
    // add for preceding/succeeding transition:
    // - primary actions
    // - secondary actions (if ambiguity is here)
    // - does this transition swap the action lists?
    uint32_t *pair_hashes;
    // These are really of type `enum state_pair_mark`, but we represent them as
    // uint8_t to make scanning the table faster.
    uint8_t *mark;

    uint32_t available_size;
    uint32_t used_size;
};

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static uint32_t state_pair_table_lookup(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);

// Normalize state pairs so a is always less than or equal to b.
static struct state_pair state_pair_make(state_id a, state_id b,
 enum epsilon_state epsilon_state)
{
    struct state_pair p = { .a = a, .b = b, .epsilon_state = epsilon_state };
    if (a > b) {
        p.a = b;
        p.b = a;
    }
    return p;
}

struct state_pair_array {
    struct state_pair *pairs;
    uint32_t pairs_allocated_bytes;
    uint32_t number_of_pairs;
};

static void state_pair_array_push(struct state_pair_array *array,
 struct state_pair pair)
{
    assert(pair.epsilon_state == ALLOW_EPSILON_SUCCESSORS ||
     pair.epsilon_state == DISALLOW_EPSILON_SUCCESSORS);
    uint32_t i = array->number_of_pairs++;
    array->pairs = grow_array(array->pairs, &array->pairs_allocated_bytes,
     array->number_of_pairs * sizeof(struct state_pair));
    array->pairs[i] = pair;
}

static void state_pair_array_destroy(struct state_pair_array *array)
{
    free(array->pairs);
    array->pairs = 0;
    array->pairs_allocated_bytes = 0;
    array->number_of_pairs = 0;
}

struct context {
    struct state_pair_table table;
    struct combined_grammar *grammar;
    struct bracket_transitions *determinized_transitions;
};

static void check_automaton_for_ambiguity(struct automaton *automaton,
 struct context *context);

static void state_pair_search(struct automaton *automaton,
 struct context *context, enum direction direction);

static void follow_state_pair_transition(struct state_pair_table *table,
 struct state_pair_array *worklist, struct state_pair previous, state_id a,
 state_id b, enum epsilon_state epsilon_state, enum direction direction,
 bool mark);

static bool symbols_are_compatible(struct context *context, symbol_id a,
 symbol_id b);

static void output_preceding_pairs(struct state_pair_table *table, uint32_t i)
{
    struct state_pair p = table->preceding_pairs[i];
    if (p.epsilon_state == BOUNDARY)
        return;
    uint32_t hash = fnv(&p, sizeof(p));
    uint32_t j = state_pair_table_lookup(table, p, hash);
    // TODO: recursion lul
    if (table->mark[j] != EMPTY)
        output_preceding_pairs(table, j);
    printf("%u %u\n", p.a, p.b);
}

static void output_succeeding_pairs(struct state_pair_table *table, uint32_t i)
{
    struct state_pair p = table->succeeding_pairs[i];
    if (p.epsilon_state == BOUNDARY)
        return;
    uint32_t hash = fnv(&p, sizeof(p));
    uint32_t j = state_pair_table_lookup(table, p, hash);
    printf("%u %u\n", p.a, p.b);
    if (table->mark[j] != EMPTY)
        output_succeeding_pairs(table, j);
}

static void print_ambiguity(struct context context)
{
    for (uint32_t i = 0; i < context.table.available_size; ++i) {
//        if (context.table.mark[i] != BACKWARD_MARKED)
//            continue;
//        output_preceding_pairs(&context.table, i);
        printf("(%u) %u %u %u -> %u %u %u -> %u %u %u\n",
         context.table.mark[i],
         context.table.preceding_pairs[i].a,
         context.table.preceding_pairs[i].b,
         context.table.preceding_pairs[i].epsilon_state,
         context.table.pairs[i].a, context.table.pairs[i].b,
         context.table.pairs[i].epsilon_state,
         context.table.succeeding_pairs[i].a,
         context.table.succeeding_pairs[i].b,
         context.table.succeeding_pairs[i].epsilon_state);
//        output_succeeding_pairs(&context.table, i);
    }
}

void check_for_ambiguity(struct combined_grammar *combined,
 struct bracket_transitions *determinized_transitions)
{
    printf("- bracket -\n");
    struct context bracket_context = {
        .grammar = combined,
        .determinized_transitions = determinized_transitions,
    };
    check_automaton_for_ambiguity(&combined->bracket_automaton,
     &bracket_context);
    print_ambiguity(bracket_context);
    printf("- normal -\n");
    struct context context = {
        .grammar = combined,
        .determinized_transitions = determinized_transitions,
    };
    check_automaton_for_ambiguity(&combined->automaton, &context);
    print_ambiguity(context);
}

static void check_automaton_for_ambiguity(struct automaton *automaton,
 struct context *context)
{
    struct automaton reverse = {0};
    automaton_reverse(automaton, &reverse);
    state_pair_search(automaton, context, FORWARD);
    printf("-- reverse --\n");
    automaton_print(&reverse);
    state_pair_search(&reverse, context, BACKWARD);
    automaton_destroy(&reverse);
}

static void state_pair_search(struct automaton *automaton,
 struct context *context, enum direction direction)
{
    automaton_compute_epsilon_closure(automaton, FOLLOW_ACTION_TRANSITIONS);
    // TODO: Epsilon filtering and cycle detection
    struct state_pair_array worklist = {0};
    if (direction == FORWARD) {
        follow_state_pair_transition(&context->table, &worklist,
         boundary_state_pair, automaton->start_state, automaton->start_state,
         ALLOW_EPSILON_SUCCESSORS, direction, false);
    } else {
        struct state s = automaton->states[automaton->start_state];
        for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
            follow_state_pair_transition(&context->table, &worklist,
             boundary_state_pair, s.transitions[i].target,
             s.transitions[i].target, ALLOW_EPSILON_SUCCESSORS, direction,
             false);
            follow_state_pair_transition(&context->table, &worklist,
             boundary_state_pair, s.transitions[i].target,
             s.transitions[i].target, DISALLOW_EPSILON_SUCCESSORS, direction,
             false);
        }
    }
    // TODO: Make worklist a min-heap to find a minimal (according to some metric) ambiguous path?
    while (worklist.number_of_pairs > 0) {
//        printf("worklist:\n");
//        for (uint32_t i = 0; i < worklist.number_of_pairs; ++i) {
//            printf("%u %u %d\n", worklist.pairs[i].a, worklist.pairs[i].b, worklist.pairs[i].epsilon_state);
//        }
//        printf("end worklist\n");
        struct state_pair s = worklist.pairs[--worklist.number_of_pairs];
        struct state a = automaton->states[s.a];
        struct state b = automaton->states[s.b];
        bool follow_epsilons;
        bool follow_symbols;
        if (direction == FORWARD) {
            follow_epsilons = s.epsilon_state == ALLOW_EPSILON_SUCCESSORS;
            follow_symbols = true;
        } else {
            follow_epsilons = s.epsilon_state == DISALLOW_EPSILON_SUCCESSORS;
            follow_symbols = s.epsilon_state == ALLOW_EPSILON_SUCCESSORS;
        }
        if (follow_symbols) {
            for (uint32_t i = 0; i < a.number_of_transitions; ++i) {
                struct transition at = a.transitions[i];
                if (at.symbol == SYMBOL_EPSILON)
                    continue;
                for (uint32_t j = 0; j < b.number_of_transitions; ++j) {
                    struct transition bt = b.transitions[j];
                    if (!symbols_are_compatible(context, at.symbol, bt.symbol))
                        continue;
                    follow_state_pair_transition(&context->table, &worklist, s,
                     at.target, bt.target, ALLOW_EPSILON_SUCCESSORS, direction,
                     at.target != bt.target || at.symbol != bt.symbol);
                    if (direction == BACKWARD) {
                        follow_state_pair_transition(&context->table, &worklist,
                         s, at.target, bt.target, DISALLOW_EPSILON_SUCCESSORS,
                         direction, at.target != bt.target ||
                         at.symbol != bt.symbol);
                    }
                }
            }
        }
        if (follow_epsilons) {
            struct state_array ae;
            ae = automaton->epsilon_closure_for_state[s.a].reachable;
            for (uint32_t i = 0; i < ae.number_of_states; ++i) {
                struct state_array be;
                be = automaton->epsilon_closure_for_state[s.b].reachable;
                for (uint32_t j = 0; j < be.number_of_states; ++j) {
                    follow_state_pair_transition(&context->table, &worklist, s,
                     ae.states[i], be.states[j], direction == FORWARD ?
                     DISALLOW_EPSILON_SUCCESSORS : ALLOW_EPSILON_SUCCESSORS,
                     direction, ae.states[i] != be.states[j] ||
                     automaton->epsilon_closure_for_state[s.a].ambiguous_action_indexes[i] != UINT32_MAX ||
                     automaton->epsilon_closure_for_state[s.b].ambiguous_action_indexes[j] != UINT32_MAX);
                }
            }
        }
    }
    state_pair_array_destroy(&worklist);
}

static bool should_follow_mark(struct state_pair_table *table, uint32_t index,
 enum direction direction)
{
    if (table->mark[index] == EMPTY)
        return true;
    if (direction == BACKWARD && table->mark[index] == FORWARD_VISITED)
        return true;
    return false;
}

static void apply_mark(struct state_pair_table *table, uint32_t index,
 enum direction direction, enum mark_type mark_type)
{
    enum state_pair_mark next_mark;
    if (direction == FORWARD)
        next_mark = mark_type == MARKED ? FORWARD_MARKED : FORWARD_VISITED;
    else {
        next_mark = BACKWARD_VISITED;
        if (table->mark[index] == FORWARD_MARKED)
            next_mark = BACKWARD_MARKED;
    }
    if (next_mark > table->mark[index])
        table->mark[index] = next_mark;
}

static void follow_state_pair_transition(struct state_pair_table *table,
 struct state_pair_array *worklist, struct state_pair previous, state_id a,
 state_id b, enum epsilon_state epsilon_state, enum direction direction,
 bool mark)
{
    struct state_pair pair = state_pair_make(a, b, epsilon_state);
    bool swapped = (pair.a != a);
    uint32_t hash = fnv(&pair, sizeof(pair));
    uint32_t index = state_pair_table_add(table, pair, hash);
    // TODO: don't just mark the preceding pair; track the action list as well
    // TODO: if there are two conflicting action lists, mark the state even if a == b
    // TODO: we may need more checks to ensure there are no cycles at the beginning of the path
    if (direction == FORWARD &&
     table->preceding_pairs[index].epsilon_state == INVALID)
        table->preceding_pairs[index] = previous;
    if (direction == BACKWARD &&
     table->succeeding_pairs[index].epsilon_state == INVALID)
        table->succeeding_pairs[index] = previous;
    if (should_follow_mark(table, index, direction))
        state_pair_array_push(worklist, pair);
    // TODO: Clean up this mismatch between bool and enum
    apply_mark(table, index, direction, mark ? MARKED : VISITED);
}

static bool symbols_are_compatible(struct context *context, symbol_id a,
 symbol_id b)
{
    if (a == b)
        return true;
    if (a == SYMBOL_EPSILON || b == SYMBOL_EPSILON)
        return false;
    if (a < context->grammar->number_of_tokens ||
     b < context->grammar->number_of_tokens)
        return false;
    struct bracket_transitions *ts = context->determinized_transitions;
    for (uint32_t k = 0; k < ts->number_of_transitions; ++k) {
        struct bitset *symbols = &ts->transitions[k].transition_symbols;
        if (bitset_contains(symbols, a) && bitset_contains(symbols, b))
            return true;
    }
    return false;
}

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash)
{
    assert(pair.epsilon_state == ALLOW_EPSILON_SUCCESSORS ||
     pair.epsilon_state == DISALLOW_EPSILON_SUCCESSORS);
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        struct state_pair_table old = *table;
        uint32_t n = old.available_size * 2;
        if (n == 0)
            n = 4;
        table->pairs = calloc(n, sizeof(struct state_pair));
        table->preceding_pairs = calloc(n, sizeof(struct state_pair));
        table->succeeding_pairs = calloc(n, sizeof(struct state_pair));
        table->pair_hashes = calloc(n, sizeof(uint32_t));
        table->mark = calloc(n, sizeof(uint8_t));
        table->available_size = n;
        table->used_size = 0;
        for (uint32_t i = 0; i < old.available_size; ++i) {
            if (old.mark[i] == EMPTY)
                continue;
            uint32_t index = state_pair_table_add(table, old.pairs[i],
             old.pair_hashes[i]);
            table->preceding_pairs[index] = old.preceding_pairs[i];
            table->succeeding_pairs[index] = old.succeeding_pairs[i];
            table->mark[index] = old.mark[i];
        }
        free(old.pairs);
        free(old.preceding_pairs);
        free(old.succeeding_pairs);
        free(old.pair_hashes);
        free(old.mark);
    }
    uint32_t index = state_pair_table_lookup(table, pair, hash);
    if (table->mark[index] == EMPTY) {
        table->pairs[index] = pair;
        table->preceding_pairs[index] = invalid_state_pair;
        table->succeeding_pairs[index] = invalid_state_pair;
        table->pair_hashes[index] = hash;
        table->used_size++;
    }
    return index;
}

static uint32_t state_pair_table_lookup(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash)
{
    uint32_t mask = table->available_size - 1;
    uint32_t index = hash & mask;
    while (true) {
        if (table->mark[index] == EMPTY)
            return index;
        if (table->pairs[index].a == pair.a && table->pairs[index].b == pair.b
         && table->pairs[index].epsilon_state == pair.epsilon_state)
            return index;
        index = (index + 1) & mask;
        if (index == (hash & mask))
            abort();
    }
}

