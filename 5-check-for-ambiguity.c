#include "5-check-for-ambiguity.h"

#include "bitset.h"
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
};
struct state_pair {
    state_id a;
    state_id b;
    enum epsilon_state epsilon_state;
};

enum state_path_node_type {
    EMPTY_NODE,
    SYMBOL_NODE,
    ACTION_NODE,
    BRACKET_PATH_NODE,
};
enum state_path_node_flags {
    REVERSED = 1 << 0,
    SWAPPED = 1 << 1,
};
struct state_path_node {
    struct state_path_node *next;
    enum state_path_node_type type;
    enum state_path_node_flags flags;
    union {
        uint16_t *actions[2];
        symbol_id symbol;
        struct state_path_node *bracket_path;
    };
};

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
struct state_pair_edge;
struct state_pair_table {
    struct state_pair *pairs;
    struct state_pair_edge *in_edges;
    struct state_pair_edge *out_edges;
    uint32_t *pair_hashes;
    // These are really of type `enum state_pair_mark`, but we represent them as
    // uint8_t to make scanning the table faster.
    uint8_t *mark;

    uint32_t available_size;
    uint32_t used_size;
};
enum state_pair_edge_type {
    INVALID,
    BOUNDARY,
    NORMAL,
};
struct state_pair_edge {
    enum state_pair_edge_type type;
    // One side of the edge; the other is already available in the state pair
    // table.
    struct state_pair pair;

    // These nodes don't have the next pointer filled in.  They should be copied
    // into the "real" node for the path.
    struct state_path_node node;
};
static struct state_pair_edge invalid_state_pair_edge = {
    .type = INVALID, .pair = { UINT32_MAX, UINT32_MAX },
};
static struct state_pair_edge boundary_state_pair_edge = {
    .type = BOUNDARY, .pair = { UINT32_MAX, UINT32_MAX },
};

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static uint32_t state_pair_table_lookup(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static void state_pair_table_clear(struct state_pair_table *table);

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
    struct combined_grammar *grammar;
    struct bracket_transitions *determinized_transitions;

    struct state_path_node **bracket_paths;
    struct state_path_node **bracket_ambiguous_paths;
};

#define FROM_START_STATE 0
#define FROM_ALL_ACCEPTING_STATES 1
#define FROM_ACCEPTING_STATE(state) (2 + ((uint64_t)(state) << 2))
static void state_pair_search(struct automaton *automaton,
 struct state_pair_table *table, struct context *context,
 uint64_t starting_point);

static void follow_state_pair_transition(struct state_pair_table *table,
 struct state_pair_array *worklist, struct state_pair pair,
 struct state_pair_edge edge, enum direction direction);

static bool symbols_are_compatible(struct context *context, symbol_id a,
 symbol_id b);

static void print_ambiguity(struct state_pair_table *table)
{
//    printf("table size: %u\n", table->available_size);
//    return;
    for (uint32_t i = 0; i < table->available_size; ++i) {
        printf("(%u) %u %u %u -> %u %u %u -> %u %u %u\n",
         table->mark[i],
         table->in_edges[i].pair.a,
         table->in_edges[i].pair.b,
         table->in_edges[i].pair.epsilon_state,
         table->pairs[i].a, table->pairs[i].b,
         table->pairs[i].epsilon_state,
         table->out_edges[i].pair.a,
         table->out_edges[i].pair.b,
         table->out_edges[i].pair.epsilon_state);
    }
}

static struct state_path_node *find_path_through(struct state_pair_table *table,
 uint32_t table_pair_index);
static struct state_path_node *find_ambiguous_path(struct state_pair_table *t);

void check_for_ambiguity(struct combined_grammar *combined,
 struct bracket_transitions *determinized_transitions,
 struct ambiguities *ambiguities)
{
    uint32_t tokens = combined->number_of_tokens;
    uint32_t number_of_bracket_transitions = 0;
    if (combined->automaton.number_of_symbols > tokens) {
        number_of_bracket_transitions =
         combined->automaton.number_of_symbols - tokens;
    }
    struct context context = {
        .grammar = combined,
        .determinized_transitions = determinized_transitions,
        .bracket_paths = calloc(number_of_bracket_transitions,
          sizeof(struct state_path_node *)),
        .bracket_ambiguous_paths = calloc(number_of_bracket_transitions,
          sizeof(struct state_path_node *)),
    };
    struct automaton bracket_reversed = {0};
    automaton_reverse(&combined->bracket_automaton, &bracket_reversed);
    bool changed;
    do {
        changed = false;
        for (uint32_t i = 0; i < number_of_bracket_transitions; ++i) {
            if (context.bracket_ambiguous_paths[i])
                continue;
            struct state_pair_table table = {0};
            symbol_id transition_symbol = tokens + i;
            struct state s;
            s = bracket_reversed.states[bracket_reversed.start_state];
            state_id end_state = UINT32_MAX;
            for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
                state_id state = s.transitions[i].target;
                if (transition_symbol !=
                 bracket_reversed.states[state].transition_symbol)
                    continue;
                end_state = state;
                break;
            }
            assert(end_state != UINT32_MAX);
            state_pair_search(&combined->bracket_automaton, &table, &context,
             FROM_START_STATE);
            state_pair_search(&bracket_reversed, &table, &context,
             FROM_ACCEPTING_STATE(end_state));
            struct state_pair end_pair =
             state_pair_make(end_state, end_state, DISALLOW_EPSILON_SUCCESSORS);
            if (!context.bracket_paths[i]) {
                uint32_t index = state_pair_table_lookup(&table, end_pair,
                 fnv(&end_pair, sizeof(end_pair)));
                context.bracket_paths[i] = find_path_through(&table, index);
                if (context.bracket_paths[i])
                    changed = true;
            }
            context.bracket_ambiguous_paths[i] = find_ambiguous_path(&table);
            if (context.bracket_ambiguous_paths[i])
                changed = true;
        }
    } while (changed);

    // - if there's an ambiguity inside of a bracket, we need to find a path
    //   which exhibits it
    // - if there's a path through a bracket transition, we need to find a path
    //   to substitute in
    // so maybe we need two paths: an exemplar path and an ambiguity path...

    struct automaton reversed = {0};
    automaton_reverse(&combined->automaton, &reversed);
    struct state_pair_table table = {0};
    state_pair_search(&combined->automaton, &table, &context, FROM_START_STATE);
    state_pair_search(&reversed, &table, &context, FROM_ALL_ACCEPTING_STATES);

    struct state_path_node *path = find_ambiguous_path(&table);
    if (path)
        ambiguities->has_ambiguity = true;
    bool swap = false;
    while (path) {
        printf("-\n");
        for (int i = 0; i < 2; ++i) {
            struct ambiguity_path *out = &ambiguities->paths[swap ? 1 - i : i];
            switch (path->type) {
            case SYMBOL_NODE: {
                uint32_t token = out->number_of_tokens++;
                out->tokens = grow_array(out->tokens,
                 &out->tokens_allocated_bytes,
                 out->number_of_tokens * sizeof(symbol_id));
                out->tokens[token] = path->symbol;
                break;
            }
            case ACTION_NODE: {
                uint32_t n = 0;
                for (uint16_t *a = path->actions[i]; a && *a; ++a)
                    ++n;
                for (uint32_t j = 0; j < n; ++j) {
                    uint16_t *a = (path->flags & REVERSED) ?
                     &path->actions[i][n - j - 1] : &path->actions[i][j];
                    uint32_t action = out->number_of_actions++;
                    out->actions = grow_array(out->actions,
                     &out->actions_allocated_bytes,
                     out->number_of_actions * sizeof(uint16_t));
                    printf("adding action %u %u to %p\n", (((*a) >> 12) & 0xf), *a & 0xfff, out);
                    out->actions[action] = *a;
                    out->offsets = grow_array(out->offsets,
                     &out->offsets_allocated_bytes,
                     out->number_of_actions * sizeof(size_t));
                    out->offsets[action] = out->number_of_tokens;
                }
                break;
            }
            default:
                break;
            }
        }
        if (path->flags & SWAPPED)
            swap = !swap;
        path = path->next;
    }

//    print_ambiguity(&table);

    automaton_destroy(&bracket_reversed);
    automaton_destroy(&reversed);
}

static void state_pair_search(struct automaton *automaton,
 struct state_pair_table *table, struct context *context,
 uint64_t starting_point)
{
    enum direction direction = BACKWARD;
    if (starting_point == FROM_START_STATE)
        direction = FORWARD;
    automaton_compute_epsilon_closure(automaton, FOLLOW_ACTION_TRANSITIONS);
    struct state_pair_array worklist = {0};
    if (direction == FORWARD) {
        follow_state_pair_transition(table, &worklist,
         state_pair_make(automaton->start_state, automaton->start_state,
         ALLOW_EPSILON_SUCCESSORS), boundary_state_pair_edge, direction);
    } else {
        struct state s = automaton->states[automaton->start_state];
        for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
            if ((starting_point & 0x3) == FROM_ACCEPTING_STATE(0)) {
                state_id state = (symbol_id)(starting_point >> 2);
                if (state != s.transitions[i].target)
                    continue;
            }
            follow_state_pair_transition(table, &worklist,
             state_pair_make(s.transitions[i].target, s.transitions[i].target,
             DISALLOW_EPSILON_SUCCESSORS), boundary_state_pair_edge, direction);
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
            follow_symbols = s.epsilon_state == DISALLOW_EPSILON_SUCCESSORS;
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
                    // If two different bracket symbols are going to the same
                    // state, we're in trouble...
                    // If at.symbol != bt.symbol, we need to mark the state as a
                    // possible ambiguity
                    // We also need to actually find the paths....
                    // This is kinda analogous to the epsilon transition case
                    // Need to track which symbol was followed along an edge
                    // as well?
//                    if (context->bracket_tables[at.symbol - context->grammar->number_of_tokens].has_ambiguity)
                    struct state_pair to = state_pair_make(at.target, bt.target,
                     direction == FORWARD ? ALLOW_EPSILON_SUCCESSORS :
                     DISALLOW_EPSILON_SUCCESSORS);
                    struct state_pair_edge edge = {
                        .type = NORMAL,
                        .pair = s,
                        .node = {
                            .type = SYMBOL_NODE,
                            .symbol = at.symbol, // TODO: Bracket symbols
                            .flags = to.a == at.target ? SWAPPED : 0,
                        },
                    };
                    follow_state_pair_transition(table, &worklist, to, edge,
                     direction);
                }
            }
        }
        if (follow_epsilons) {
            // TODO: Clean this up.
            struct epsilon_closure ac, bc;
            ac = automaton->epsilon_closure_for_state[s.a];
            bc = automaton->epsilon_closure_for_state[s.b];
            for (uint32_t i = 0; i < ac.reachable.number_of_states; ++i) {
                for (uint32_t j = 0; j < bc.reachable.number_of_states; ++j) {
                    struct state_pair to;
                    to = state_pair_make(ac.reachable.states[i],
                     bc.reachable.states[j], direction == FORWARD ?
                     DISALLOW_EPSILON_SUCCESSORS : ALLOW_EPSILON_SUCCESSORS);
                    struct state_pair_edge edge = {
                        .type = NORMAL,
                        .pair = s,
                        .node = {
                            .type = ACTION_NODE,
                            .actions = {
                                ac.actions + ac.action_indexes[i],
                                bc.actions + bc.action_indexes[j],
                            },
                            .flags = to.a == ac.reachable.states[i] ? SWAPPED : 0,
                        },
                    };
                    follow_state_pair_transition(table, &worklist, to, edge,
                     direction);
                }
                struct state_pair to;
                to = state_pair_make(ac.reachable.states[i], s.b,
                 direction == FORWARD ? DISALLOW_EPSILON_SUCCESSORS :
                 ALLOW_EPSILON_SUCCESSORS);
                struct state_pair_edge edge = {
                    .type = NORMAL,
                    .pair = s,
                    .node = {
                        .type = ACTION_NODE,
                        .actions = { ac.actions + ac.action_indexes[i], 0 },
                        .flags = to.b == s.b ? SWAPPED : 0,
                    },
                };
                follow_state_pair_transition(table, &worklist, to, edge,
                 direction);
            }
            for (uint32_t j = 0; j < bc.reachable.number_of_states; ++j) {
                struct state_pair to;
                to = state_pair_make(s.a, bc.reachable.states[j],
                 direction == FORWARD ? DISALLOW_EPSILON_SUCCESSORS :
                 ALLOW_EPSILON_SUCCESSORS);
                struct state_pair_edge edge = {
                    .type = NORMAL,
                    .pair = s,
                    .node = {
                        .type = ACTION_NODE,
                        .actions = { 0, bc.actions + bc.action_indexes[j] },
                        .flags = to.a == s.a ? SWAPPED : 0,
                    },
                };
                follow_state_pair_transition(table, &worklist, to, edge,
                 direction);
            }
            struct state_pair to;
            to = state_pair_make(s.a, s.b, direction == FORWARD ?
             DISALLOW_EPSILON_SUCCESSORS : ALLOW_EPSILON_SUCCESSORS);
            struct state_pair_edge edge = {
                .type = NORMAL,
                .pair = s,
            };
            follow_state_pair_transition(table, &worklist, to, edge, direction);
        }
    }
    state_pair_array_destroy(&worklist);
}

static void follow_state_pair_transition(struct state_pair_table *table,
 struct state_pair_array *worklist, struct state_pair pair,
 struct state_pair_edge edge, enum direction direction)
{
    uint32_t hash = fnv(&pair, sizeof(pair));
    uint32_t index = state_pair_table_add(table, pair, hash);

    // Form two trees: one reaching forward from the start state, and one
    // reaching backward from the accepting states.  When these trees both touch
    // a "marked" node that induces ambiguity, we use this edge information to
    // follow the trees back to their root.
    if (direction == FORWARD && table->in_edges[index].type == INVALID)
        table->in_edges[index] = edge;
    if (direction == BACKWARD && table->out_edges[index].type == INVALID)
        table->out_edges[index] = edge;

    // Update the mark for this state pair.
    // TODO: We don't need these marks; we can just use the presence of edges to
    // track whether a state has been visited yet.
    enum state_pair_mark next_mark;
    if (direction == FORWARD) {
        next_mark = FORWARD_VISITED;
        if (pair.a != pair.b)
            next_mark = FORWARD_MARKED;
    } else {
        next_mark = BACKWARD_VISITED;
        if (table->mark[index] == FORWARD_MARKED)
            next_mark = BACKWARD_MARKED;
    }

    // In the forward pass, only progress through "empty" state pairs.  In the
    // backward pass, we can progress through the same states we visited in the
    // forward pass.
    // FIXME: In principle, we could stop here if we find an ambiguity in the
    // forward pass, but we'd need to make a special case for the bracket
    // forward table.
    if (table->mark[index] == EMPTY || (direction == BACKWARD &&
     table->mark[index] == FORWARD_VISITED))
        state_pair_array_push(worklist, pair);

    if (next_mark > table->mark[index])
        table->mark[index] = next_mark;
}

static struct state_path_node *find_path_through(struct state_pair_table *table,
 uint32_t table_pair_index)
{
    uint32_t index = table_pair_index;
    struct state_path_node *result = 0;
    struct state_path_node **next = &result;
    struct state_pair_edge *edge_list = table->out_edges;
    bool reverse = false;
    while (table->mark[index] != EMPTY) {
        if (edge_list[index].type == BOUNDARY) {
            if (edge_list == table->out_edges) {
                edge_list = table->in_edges;
                index = table_pair_index;
                reverse = true;
                printf("flip\n");
                continue;
            } else
                return result;
        }
        if (edge_list[index].node.type != EMPTY_NODE) {
            struct state_path_node *n = malloc(sizeof(struct state_path_node));
            *n = edge_list[index].node;
            // TODO: reverse the reversedness?
            if (reverse)
                n->flags |= REVERSED;
            if (edge_list == table->out_edges) {
                *next = n;
                next = &n->next;
            } else {
                n->next = result;
                result = n;
            }
        }
        struct state_pair p = edge_list[index].pair;
#if 0
        printf("pair: %u %u -> %u %u  node: %u (%u, %p %p)\n", table->pairs[index].a, table->pairs[index].b, p.a, p.b, edge_list[index].node.type, edge_list[index].node.symbol, edge_list[index].node.actions[0], edge_list[index].node.actions[1]);
        if (edge_list[index].node.type == ACTION_NODE) {
            for (uint16_t *a = edge_list[index].node.actions[0]; a && *a; ++a) {
                printf("a[0] = %u %u\n", (((*a) >> 12) & 0xf), *a & 0xfff);
            }
            for (uint16_t *a = edge_list[index].node.actions[1]; a && *a; ++a) {
                printf("a[1] = %u %u\n", (((*a) >> 12) & 0xf), *a & 0xfff);
            }
        }
#endif
        index = state_pair_table_lookup(table, p, fnv(&p, sizeof(p)));
    }
    // We encountered an empty pair, which means a path doesn't exist.
    while (result) {
        struct state_path_node *next = result->next;
        free(result);
        result = next;
    }
    return 0;
}

static struct state_path_node *find_ambiguous_path(struct state_pair_table *t)
{
    for (uint32_t i = 0; i < t->available_size; ++i) {
        if (t->mark[i] != BACKWARD_MARKED)
            continue;
        struct state_path_node *path = find_path_through(t, i);
        if (path)
            return path;
    }
    return 0;
}

static bool symbols_are_compatible(struct context *context, symbol_id a,
 symbol_id b)
{
    if (a == b)
        return true;
    if (a == SYMBOL_EPSILON || b == SYMBOL_EPSILON)
        return false;
    uint32_t tokens = context->grammar->number_of_tokens;
    if (a < tokens || b < tokens)
        return false;
    struct bracket_transitions *ts = context->determinized_transitions;
    for (uint32_t i = 0; i < ts->number_of_transitions; ++i) {
        struct bitset *symbols = &ts->transitions[i].transition_symbols;
        if (bitset_contains(symbols, a) && bitset_contains(symbols, b))
            return true;
    }
    return false;
}

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash)
{
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        struct state_pair_table old = *table;
        uint32_t n = old.available_size * 2;
        if (n == 0)
            n = 4;
        table->pairs = calloc(n, sizeof(struct state_pair));
        table->in_edges = calloc(n, sizeof(struct state_pair_edge));
        table->out_edges = calloc(n, sizeof(struct state_pair_edge));
        table->pair_hashes = calloc(n, sizeof(uint32_t));
        table->mark = calloc(n, sizeof(uint8_t));
        table->available_size = n;
        table->used_size = 0;
        for (uint32_t i = 0; i < old.available_size; ++i) {
            if (old.mark[i] == EMPTY)
                continue;
            uint32_t index = state_pair_table_add(table, old.pairs[i],
             old.pair_hashes[i]);
            table->in_edges[index] = old.in_edges[i];
            table->out_edges[index] = old.out_edges[i];
            table->mark[index] = old.mark[i];
        }
        free(old.pairs);
        free(old.in_edges);
        free(old.out_edges);
        free(old.pair_hashes);
        free(old.mark);
    }
    uint32_t index = state_pair_table_lookup(table, pair, hash);
    if (table->mark[index] == EMPTY) {
        table->pairs[index] = pair;
        table->in_edges[index] = invalid_state_pair_edge;
        table->out_edges[index] = invalid_state_pair_edge;
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

static void state_pair_table_clear(struct state_pair_table *table)
{
    free(table->pairs);
    free(table->in_edges);
    free(table->out_edges);
    free(table->pair_hashes);
    free(table->mark);
    memset(table, 0, sizeof(*table));
}
