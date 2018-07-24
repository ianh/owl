#include "4-check-for-ambiguity.h"

#include "alloc.h"
#include "bitset.h"
#include "fnv.h"
#include <assert.h>

// The goal of ambiguity checking is to find one of two things:
// 1. An "intrinsically ambiguous" pair of paths between two states.  That is,
//  - two different epsilon-transition paths between the same two states,
//  - two different bracket transitions whose corresponding accepting states are
//    reachable along paths with the same symbols, or
//  - a single bracket transition whose accepting state is itself part of an
//    ambiguous path.
// or
// 2. A pair of distinct states which can be reached along paths following the
//    same input.
//
// We search the product automaton, looking for either of these two cases.  The
// "disambiguation" procedure we did in the combine step ensures that different
// paths through the automaton have distinguishable sequences of actions.  If we
// find a path in the product automaton with a pair of distinct states, we have
// two different paths in the original automaton, and therefore two
// distinguishable sequences of actions for the same input -- an ambiguity.

enum epsilon_state {
    ALLOW_EPSILON_SUCCESSORS,
    DISALLOW_EPSILON_SUCCESSORS,
};
// The `epsilon_state` field implements the "epsilon filtering" technique by
// Allauzen et al. (2011), simplified a little bit because we follow an entire
// path at once.
//
//  Allauzen, C., Mohri, M., & Rastogi, A. (2011). General Algorithms for
//  Testing the Ambiguity of Finite Automata and the Double-Tape Ambiguity of
//  Finite-State Transducers. Int. J. Found. Comput. Sci., 22, 883-904.
struct state_pair {
    state_id b;
    state_id a:31;
    unsigned epsilon_state:1;
};

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

enum {
    // Types of node data.
    INVALID_NODE = 0,
    BOUNDARY_NODE,
    ACTION_NODE,
    SYMBOL_NODE,
    BRACKET_SYMBOL_NODE,
    // Join nodes have one path going left and one going right.
    JOIN_NODE,
};
enum {
    // There's an intrinsic ambiguity along this transition.
    AMBIGUOUS_NODE = 1 << 0,

    // Uses next pointer rather than next_pair.
    COPIED_PATH = 1 << 1,

    // The paths swap places through this node due to pair normalization.
    SWAPPED_PATH = 1 << 2,

    // The paths swap places through this bracket transition due to pair
    // normalization.
    SWAPPED_BRACKET_PATH = 1 << 3,
};
struct path_offset {
    uint32_t symbols;
    uint32_t actions[2];
};
struct path_node {
    union {
        struct path_node *next;
        struct state_pair next_pair;
    };
    union {
        uint16_t *actions[2];
        symbol_id symbol;
        struct state_pair bracket_pair;
        struct path_node *join[2];
    };
    struct path_offset offset;
    uint8_t type;
    uint8_t flags;
};
static struct path_node boundary_node = { .type = BOUNDARY_NODE };

enum state_pair_status {
    EMPTY = 0,
    USED,

    // Locked state pairs shouldn't be modified.  They're used to keep track of
    // bracket automaton paths.
    LOCKED,
};
struct state_pair_table {
    struct state_pair *pairs;
    struct path_node *in_paths;
    struct path_node *out_paths;
    // "ain"-paths are ambiguous in-paths.  We need to store these separately to
    // keep track of them without creating cycles (an ain-path may need to
    // include the same node pair twice -- if there are epsilon-cycles, for
    // example).
    struct path_node *ain_paths;
    uint32_t *in_weights;
    uint8_t *status;

    // A state pair along an ambiguous path, if one has been found.
    bool has_ambiguity;
    struct state_pair ambiguity;
    uint32_t ambiguity_weight;

    uint32_t available_size;
    uint32_t used_size;
};

struct work_item {
    struct path_node node;
    struct state_pair pair;
    uint32_t weight;
};
struct worklist {
    struct work_item *items;
    uint32_t items_allocated_bytes;
    uint32_t number_of_items;
};

enum direction {
    FORWARD,
    BACKWARD,

    // Search from only accepting states which don't have an ambiguous path yet.
    BACKWARD_UNRESOLVED,
};

struct context {
    struct combined_grammar *combined;

    // The paths are stored in the table under pairs of accepting states.
    struct state_pair_table bracket_paths;

    // These are both arrays with one element for each bracket transition.
    struct path_node *ambiguous_bracket_paths;
    state_id *bracket_states;
};

static void search_state_pairs(struct context *context,
 struct state_pair_table *table, struct automaton *automaton,
 enum direction direction);
static void follow_state_pair_transition(struct path_node node, state_id a,
 state_id b, uint32_t weight, enum direction direction,
 struct state_pair_table *table, struct worklist *worklist);

static void build_ambiguity_path(struct context *context,
 struct ambiguity *ambiguity, struct path_offset offset, struct path_node *node,
 int32_t direction, bool swapped);

// Copying a node fills in the offset as well.
static void path_node_copy(struct context *context,
 struct state_pair_table *table, struct path_node *which_paths,
 struct path_node *node);
// Destroy a copied path node.
static void path_node_destroy(struct path_node *node);

static struct path_node *ambiguous_bracket_path(struct context *context,
 symbol_id symbol);

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static uint32_t state_pair_table_lookup(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static void state_pair_table_clear(struct state_pair_table *table);
static void state_pair_table_destroy(struct state_pair_table *table);
static void state_pair_table_rehash(struct state_pair_table *table,
 uint32_t new_size);

void check_for_ambiguity(struct combined_grammar *combined,
 struct ambiguity *ambiguity)
{
    struct context context = {
        .combined = combined,
    };
    context.ambiguous_bracket_paths =
     calloc(combined->number_of_bracket_symbols, sizeof(struct path_node));
    context.bracket_states =
     calloc(combined->number_of_bracket_symbols, sizeof(state_id));
    struct automaton *bracket_automaton = &combined->bracket_automaton;
    for (state_id i = 0; i < bracket_automaton->number_of_states; ++i) {
        struct state s = bracket_automaton->states[i];
        if (s.accepting) {
            context.bracket_states[s.transition_symbol -
             combined->number_of_tokens] = i;
        }
    }

    // Phase one: collect paths through the bracket product automaton
    // (ambiguous or not).
    bool changed;
    do {
        changed = false;
        struct state_pair_table *table = &context.bracket_paths;
        search_state_pairs(&context, table, bracket_automaton, FORWARD);
        for (uint32_t i = 0; i < table->available_size; ++i) {
            if (table->status[i] == EMPTY || table->status[i] == LOCKED)
                continue;
            struct state_pair p = table->pairs[i];
            if (p.epsilon_state != DISALLOW_EPSILON_SUCCESSORS)
                continue;
            if (!bracket_automaton->states[p.a].accepting ||
             !bracket_automaton->states[p.b].accepting)
                continue;
            path_node_copy(&context, table, table->in_paths,
             &table->in_paths[i]);
            table->status[i] = LOCKED;
            changed = true;
        }
        for (uint32_t i = 0; i < table->available_size; ++i) {
            if (table->status[i] == EMPTY || table->status[i] == LOCKED)
                continue;
            table->status[i] = EMPTY;
            table->used_size--;
        }
        // We re-hash here after deleting entries to avoid gaps in the table.
        state_pair_table_rehash(table, table->available_size);
    } while (changed);

    // Phase two: look for ambiguity.
    struct automaton reversed = {0};
    automaton_reverse(&combined->automaton, &reversed);
    struct automaton bracket_reversed = {0};
    automaton_reverse(bracket_automaton, &bracket_reversed);
    struct state_pair_table table = {0};
    while (true) {
        // Look for ambiguities in the bracket product automaton (potentially
        // propagating ambiguities outward as we discover new ambiguous
        // transitions).
        state_pair_table_clear(&table);
        search_state_pairs(&context, &table, bracket_automaton, FORWARD);
        search_state_pairs(&context, &table, &bracket_reversed,
         BACKWARD_UNRESOLVED);
        if (!table.has_ambiguity) {
            // No more bracket paths exist.  Now we can look for the final path
            // in the main automaton.
            state_pair_table_clear(&table);
            search_state_pairs(&context, &table, &combined->automaton, FORWARD);
            search_state_pairs(&context, &table, &reversed, BACKWARD);
            break;
        }

        // We found an ambiguity in the bracket product automaton.
        uint32_t path_index = state_pair_table_lookup(&table, table.ambiguity,
         fnv(&table.ambiguity, sizeof(table.ambiguity)));
        uint32_t i = path_index;
        while (table.out_paths[i].type != BOUNDARY_NODE) {
            // Follow the path until we reach a pair of accepting states.  The
            // search needs to be able to find this ambiguous path using the
            // transition symbols of its final, accepting pair.
            struct state_pair p = table.out_paths[i].next_pair;
            i = state_pair_table_lookup(&table, p, fnv(&p, sizeof(p)));
        }
        struct state_pair p = table.pairs[i];
        assert(p.a == p.b);
        struct state s = combined->bracket_automaton.states[p.a];
        assert(s.accepting);

        // Copy the path and add it to the `ambiguous_bracket_paths` array as a
        // `JOIN_NODE`.
        struct path_node *in = malloc(sizeof(struct path_node));
        struct path_node *out = malloc(sizeof(struct path_node));
        *in = table.in_paths[path_index];
        *out = table.out_paths[path_index];
        if (table.ain_paths[path_index].type != INVALID_NODE)
            *in = table.ain_paths[path_index];
        path_node_copy(&context, &table, table.in_paths, in);
        path_node_copy(&context, &table, table.out_paths, out);
        *ambiguous_bracket_path(&context, s.transition_symbol) =
         (struct path_node){
            .type = JOIN_NODE,
            .flags = AMBIGUOUS_NODE,
            .join = { in, out },
         };

        // Clear the table and keep looping until we don't find any more
        // ambiguities in the bracket product automaton.
        state_pair_table_clear(&table);
    }

    ambiguity->has_ambiguity = table.has_ambiguity;
    if (table.has_ambiguity) {
        // We found an ambiguous path through the root automaton.  Write it to
        // the output ambiguity struct.
        uint32_t path_index = state_pair_table_lookup(&table, table.ambiguity,
         fnv(&table.ambiguity, sizeof(table.ambiguity)));
        struct path_node in = table.in_paths[path_index];
        struct path_node out = table.out_paths[path_index];
        if (table.ain_paths[path_index].type != INVALID_NODE)
            in = table.ain_paths[path_index];
        path_node_copy(&context, &table, table.in_paths, &in);
        path_node_copy(&context, &table, table.out_paths, &out);
        ambiguity->number_of_tokens = in.offset.symbols + out.offset.symbols;
        ambiguity->tokens = calloc(ambiguity->number_of_tokens,
         sizeof(symbol_id));
        for (int i = 0; i < 2; ++i) {
            uint32_t n = in.offset.actions[i] + out.offset.actions[i];
            ambiguity->paths[i].number_of_actions = n;
            ambiguity->paths[i].actions = calloc(n, sizeof(uint16_t));
            ambiguity->paths[i].offsets = calloc(n, sizeof(uint32_t));
        }
        build_ambiguity_path(&context, ambiguity, in.offset, &out, 1, false);
        build_ambiguity_path(&context, ambiguity, in.offset, &in, -1, false);
        path_node_destroy(in.next);
        path_node_destroy(out.next);
    }

    // Clean up.
    automaton_destroy(&reversed);
    automaton_destroy(&bracket_reversed);
    state_pair_table_destroy(&table);
    for (uint32_t i = 0; i < context.bracket_paths.available_size; ++i) {
        if (context.bracket_paths.status[i] == EMPTY)
            continue;
        if (context.bracket_paths.in_paths[i].flags & COPIED_PATH)
            path_node_destroy(context.bracket_paths.in_paths[i].next);
    }
    state_pair_table_destroy(&context.bracket_paths);
    for (uint32_t i = 0; i < combined->number_of_bracket_symbols; ++i) {
        path_node_destroy(context.ambiguous_bracket_paths[i].join[0]);
        path_node_destroy(context.ambiguous_bracket_paths[i].join[1]);
    }
    free(context.ambiguous_bracket_paths);
    free(context.bracket_states);
}

static void search_state_pairs(struct context *context,
 struct state_pair_table *table, struct automaton *automaton,
 enum direction direction)
{
    table->has_ambiguity = false;
    automaton_compute_epsilon_closure(automaton, FOLLOW_ACTION_TRANSITIONS);
    // The worklist is a binary min-heap of state pairs.  The heap is ordered by
    // a weight (currently, the number of symbols it takes to reach the state
    // pair), which lets us produce minimal paths which focus on the ambiguity
    // itself.
    struct worklist worklist = {0};
    if (direction == FORWARD) {
        follow_state_pair_transition(boundary_node, automaton->start_state,
         automaton->start_state, 0, direction, table, &worklist);
    } else {
        struct state s = automaton->states[automaton->start_state];
        for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
            state_id target = s.transitions[i].target;
            // The BACKWARD_UNRESOLVED direction means we ignore accepting
            // states we already have an ambiguous path for.
            if (direction == BACKWARD_UNRESOLVED &&
             ambiguous_bracket_path(context, automaton->states[target].
             transition_symbol)->type != INVALID_NODE)
                continue;
            follow_state_pair_transition(boundary_node, target, target, 0,
             BACKWARD, table, &worklist);
        }
        // The logic above is the only place where the distinction between
        // BACKWARD_UNRESOLVED and BACKWARD matters.  Simplify `direction` to
        // BACKWARD for the rest of the function so we don't have to deal with
        // both.
        direction = BACKWARD;
    }
    while (worklist.number_of_items > 0) {
        struct work_item item = worklist.items[0];
        struct state_pair s = item.pair;

        // Rebalance the heap to ensure the minimum-weight item is at the top.
        struct work_item last = worklist.items[--worklist.number_of_items];
        uint32_t i = 0;
        while (2*i+1 < worklist.number_of_items) {
            uint32_t j = 2*i+1;
            if (2*i+2 < worklist.number_of_items &&
             worklist.items[2*i+2].weight < worklist.items[j].weight)
                j = 2*i+2;
            if (last.weight > worklist.items[j].weight)
                worklist.items[i] = worklist.items[j];
            else
                break;
            i = j;
        }
        worklist.items[i] = last;

        // Check if we've already visited this state pair and set the path field
        // if necessary (`in_paths` for FORWARD, `out_paths` for BACKWARD).
        uint32_t hash = fnv(&s, sizeof(s));
        uint32_t index = state_pair_table_add(table, s, hash);
        if (table->status[index] == LOCKED)
            continue;
        if (direction == FORWARD) {
            bool set_in_path = false;
            if (table->in_paths[index].type == INVALID_NODE) {
                table->in_paths[index] = item.node;
                table->in_weights[index] = item.weight;
                set_in_path = true;
            }
            if ((item.node.flags & AMBIGUOUS_NODE) &&
             table->ain_paths[index].type == INVALID_NODE) {
                table->ain_paths[index] = item.node;
                table->in_weights[index] = item.weight;
            }
            if (!set_in_path)
                continue;
        }
        if (direction == BACKWARD) {
            if (table->out_paths[index].type == INVALID_NODE) {
                table->out_paths[index] = item.node;
                // Check to see if either of our requirements are satisfied:
                // - a path that contains two distinct states, or
                // - a path that contains an intrinsically ambiguous edge.
                bool contains_distinct_states = s.a != s.b &&
                 table->in_paths[index].type != INVALID_NODE;
                bool contains_intrinsic_ambiguity =
                 table->ain_paths[index].type != INVALID_NODE;
                // If this is the lowest-weighted path that satisfies the
                // requirements, track the current node as the new ambiguity.
                if ((contains_distinct_states || contains_intrinsic_ambiguity)
                 && (!table->has_ambiguity || item.weight +
                 table->in_weights[index] < table->ambiguity_weight)) {
                    table->has_ambiguity = true;
                    table->ambiguity = s;
                    table->ambiguity_weight = item.weight +
                     table->in_weights[index];
                }
            } else
                continue;
        }

        // Add each successor of this state pair to the worklist.
        uint32_t w = item.weight;
        struct state a = automaton->states[s.a];
        struct state b = automaton->states[s.b];
        bool follow_epsilons = s.epsilon_state == ALLOW_EPSILON_SUCCESSORS;
        if (direction == BACKWARD)
            follow_epsilons = !follow_epsilons;
        if (follow_epsilons) {
            // Add action transitions by visiting the two states' epsilon
            // closures.  Since states aren't stored in their own epsilon
            // closures, this is a bit more code than it might otherwise be.
            struct epsilon_closure ac, bc;
            ac = automaton->epsilon_closure_for_state[s.a];
            bc = automaton->epsilon_closure_for_state[s.b];
            for (uint32_t i = 0; i < ac.reachable.number_of_states; ++i) {
                for (uint32_t j = 0; j < bc.reachable.number_of_states; ++j) {
                    follow_state_pair_transition((struct path_node){
                        .type = ACTION_NODE,
                        .next_pair = s,
                        .actions[0] = ac.actions + ac.action_indexes[i],
                        .actions[1] = bc.actions + bc.action_indexes[j],
                    }, ac.reachable.states[i], bc.reachable.states[j], w,
                     direction, table, &worklist);
                }
                follow_state_pair_transition((struct path_node){
                    .type = ACTION_NODE,
                    .next_pair = s,
                    .actions[0] = ac.actions + ac.action_indexes[i],
                }, ac.reachable.states[i], s.b, w, direction, table, &worklist);
                // Check `ambiguous_action_indexes` to see if there's a second
                // path between these two states.  If there is, we create a node
                // with both action lists and with the AMBIGUOUS_NODE flag set--
                // `follow_state_pair_transition` will add this node to
                // `ain_nodes` as an intrinsic ambiguity.
                if (direction == FORWARD && s.a == s.b &&
                 ac.ambiguous_action_indexes[i] != UINT32_MAX) {
                    follow_state_pair_transition((struct path_node){
                        .type = ACTION_NODE,
                        .next_pair = s,
                        .flags = AMBIGUOUS_NODE,
                        .actions[0] = ac.actions + ac.action_indexes[i],
                        .actions[1] = ac.actions +
                         ac.ambiguous_action_indexes[i],
                    }, ac.reachable.states[i], ac.reachable.states[i], w,
                     direction, table, &worklist);
                }
            }
            for (uint32_t j = 0; j < bc.reachable.number_of_states; ++j) {
                follow_state_pair_transition((struct path_node){
                    .type = ACTION_NODE,
                    .next_pair = s,
                    .actions[1] = bc.actions + bc.action_indexes[j],
                }, s.a, bc.reachable.states[j], w, direction, table, &worklist);
            }
            follow_state_pair_transition((struct path_node){
                .next_pair = s,
                .type = ACTION_NODE,
            }, s.a, s.b, w, direction, table, &worklist);
        } else {
            // Add successors of symbol and bracket transitions.
            for (uint32_t i = 0; i < a.number_of_transitions; ++i) {
                struct transition at = a.transitions[i];
                if (at.symbol == SYMBOL_EPSILON)
                    continue;
                bool bracket = at.symbol >= context->combined->number_of_tokens;
                for (uint32_t j = 0; j < b.number_of_transitions; ++j) {
                    struct transition bt = b.transitions[j];
                    if (!bracket) {
                        // This is a normal symbol transition.
                        if (at.symbol != bt.symbol)
                            continue;
                        follow_state_pair_transition((struct path_node){
                            .type = SYMBOL_NODE,
                            .next_pair = s,
                            .symbol = at.symbol,
                        }, at.target, bt.target, w + 1, direction, table,
                         &worklist);
                        continue;
                    }
                    if (bt.symbol == SYMBOL_EPSILON)
                        continue;
                    if (bt.symbol < context->combined->number_of_tokens)
                        continue;
                    // This is a bracket transition.

                    // Check the `bracket_paths` table to see if the accepting
                    // state pair corresponding to these two bracket transition
                    // symbols has an associated path.
                    state_id sa = context->bracket_states[at.symbol -
                     context->combined->number_of_tokens];
                    state_id sb = context->bracket_states[bt.symbol -
                     context->combined->number_of_tokens];
                    struct state_pair p = state_pair_make(sa, sb,
                     DISALLOW_EPSILON_SUCCESSORS);
                    uint32_t k;
                    k = state_pair_table_lookup(&context->bracket_paths, p,
                     fnv(&p, sizeof(p)));
                    if (context->bracket_paths.status[k] == LOCKED) {
                        // The "locked" flag indicates there's a path here.
                        // If we're moving along two different transition
                        // symbols, this is an intrinsic ambiguity -- mark it
                        // using the AMBIGUOUS_NODE flag.
                        follow_state_pair_transition((struct path_node){
                            .type = BRACKET_SYMBOL_NODE,
                            .next_pair = s,
                            .bracket_pair = p,
                            .flags = (at.symbol == bt.symbol ? 0 :
                             AMBIGUOUS_NODE) | (sa == p.a ? 0 :
                             SWAPPED_BRACKET_PATH),
                        }, at.target, bt.target, w +
                         context->bracket_paths.in_paths[k].offset.symbols,
                         direction, table, &worklist);
                    }

                    if (at.symbol == bt.symbol) {
                        // If this bracket transition already has an intrinsic
                        // ambiguity, track it using the JOIN_NODE node we
                        // stored in the `ambiguous_bracket_paths` array.
                        struct path_node n = *ambiguous_bracket_path(context,
                         at.symbol);
                        if (n.type != INVALID_NODE) {
                            n.next_pair = s;
                            follow_state_pair_transition(n, at.target,
                             bt.target, w + n.join[0]->offset.symbols +
                             n.join[1]->offset.symbols, direction, table,
                             &worklist);
                        }
                    }
                }
            }
        }
    }
    free(worklist.items);
}

static void follow_state_pair_transition(struct path_node node, state_id a,
 state_id b, uint32_t weight, enum direction direction,
 struct state_pair_table *table, struct worklist *worklist)
{
    struct state_pair pair;
    // Set the `epsilon_state` to ensure the path can't have two sequential
    // epsilon transitions.
    if (node.type == ACTION_NODE) {
        pair = state_pair_make(a, b, direction == FORWARD ?
         DISALLOW_EPSILON_SUCCESSORS : ALLOW_EPSILON_SUCCESSORS);
    } else {
        pair = state_pair_make(a, b, direction == FORWARD ?
         ALLOW_EPSILON_SUCCESSORS : DISALLOW_EPSILON_SUCCESSORS);
    }
    if (a != pair.a)
        node.flags |= SWAPPED_PATH;

    // Add the next pair to the worklist.
    uint32_t i = worklist->number_of_items++;
    if (i == UINT32_MAX)
        abort();
    worklist->items = grow_array(worklist->items,
     &worklist->items_allocated_bytes,
     sizeof(struct work_item) * worklist->number_of_items);
    // Setting i = (i-1)/2 moves up the binary min-heap.
    for(; i > 0 && weight < worklist->items[(i-1)/2].weight; i = (i-1)/2)
        worklist->items[i] = worklist->items[(i-1)/2];
    worklist->items[i] = (struct work_item){
        .node = node,
        .pair = pair,
        .weight = weight,
    };
}

static void build_ambiguity_path(struct context *context,
 struct ambiguity *ambiguity, struct path_offset offset, struct path_node *node,
 int32_t direction, bool swapped)
{
    // `build_ambiguity_path` fills in the ambiguity struct according to the
    // path described in `node`.  The offsets computed in `path_node_copy` are
    // used to figure out where to put each action and symbol.
    for (; node && node->type != BOUNDARY_NODE; node = node->next) {
        if (node->flags & SWAPPED_PATH)
            swapped = !swapped;
        switch (node->type) {
        case ACTION_NODE:
            for (int i = 0; i < 2; ++i) {
                uint16_t *actions = node->actions[swapped ? 1 - i : i];
                struct ambiguity_path *p = &ambiguity->paths[i];
                for (; actions && *actions; ++actions) {
                    if (direction == -1)
                        offset.actions[i]--;
                    p->actions[offset.actions[i]] = *actions;
                    p->offsets[offset.actions[i]] = offset.symbols;
                    if (direction == 1)
                        offset.actions[i]++;
                }
            }
            break;
        case SYMBOL_NODE:
            if (direction == -1)
                offset.symbols--;
            ambiguity->tokens[offset.symbols] = node->symbol;
            if (direction == 1)
                offset.symbols++;
            break;
        case BRACKET_SYMBOL_NODE: {
            struct state_pair p = node->bracket_pair;
            uint32_t i = state_pair_table_lookup(&context->bracket_paths, p,
             fnv(&p, sizeof(p)));
            assert(context->bracket_paths.status[i] == LOCKED);
            struct path_node in = context->bracket_paths.in_paths[i];
            assert(in.flags & COPIED_PATH);
            bool parity = swapped == !(node->flags & SWAPPED_BRACKET_PATH);
            if (direction == 1) {
                offset.symbols += in.offset.symbols;
                for (int i = 0; i < 2; ++i)
                    offset.actions[parity ? 1 - i : i] += in.offset.actions[i];
            }
            build_ambiguity_path(context, ambiguity, offset, &in, -1,
             node->flags & SWAPPED_BRACKET_PATH ? !swapped : swapped);
            if (direction == -1) {
                offset.symbols -= in.offset.symbols;
                for (int i = 0; i < 2; ++i)
                    offset.actions[parity ? 1 - i : i] -= in.offset.actions[i];
            }
            break;
        }
        case JOIN_NODE: {
            int j = direction == 1 ? 0 : 1;
            offset.symbols += node->join[j]->offset.symbols * direction;
            for (int i = 0; i < 2; ++i) {
                offset.actions[i] += direction *
                 node->join[j]->offset.actions[swapped ? 1 - i : i];
            }
            build_ambiguity_path(context, ambiguity, offset, node->join[1], 1,
             swapped);
            build_ambiguity_path(context, ambiguity, offset, node->join[0], -1,
             swapped);
            offset.symbols += node->join[1 - j]->offset.symbols * direction;
            for (int i = 0; i < 2; ++i) {
                offset.actions[i] += direction *
                 node->join[1 - j]->offset.actions[swapped ? 1 - i : i];
            }
            break;
        }
        default:
            break;
        }
    }
}

static void path_node_copy(struct context *context,
 struct state_pair_table *table, struct path_node *which_paths,
 struct path_node *node)
{
    // Copies a path from table storage into heap storage, filling in the
    // offset field according to how many actions and symbols the path contains.
    // The `build_ambiguity_path` function writes actions and symbols into the
    // final ambiguity path using this offset field as a guide -- that's why
    // these two functions have such a similar structure.
    if (node->flags & COPIED_PATH)
        return;
    struct path_offset *offset = &node->offset;
    bool swapped = false;
    for (; node && node->type != BOUNDARY_NODE; node = node->next) {
        if (node->flags & SWAPPED_PATH)
            swapped = !swapped;
        switch (node->type) {
        case ACTION_NODE:
            for (int i = 0; i < 2; ++i) {
                uint16_t *actions = node->actions[swapped ? 1 - i : i];
                for (; actions && *actions; ++actions)
                    offset->actions[i]++;
            }
            break;
        case SYMBOL_NODE:
            offset->symbols++;
            break;
        case BRACKET_SYMBOL_NODE: {
            struct state_pair p = node->bracket_pair;
            uint32_t i = state_pair_table_lookup(&context->bracket_paths, p,
             fnv(&p, sizeof(p)));
            assert(context->bracket_paths.status[i] == LOCKED);
            struct path_node in = context->bracket_paths.in_paths[i];
            assert(in.flags & COPIED_PATH);
            offset->symbols += in.offset.symbols;
            bool parity = swapped == !(node->flags & SWAPPED_BRACKET_PATH);
            for (int i = 0; i < 2; ++i)
                offset->actions[parity ? 1 - i : i] += in.offset.actions[i];
            break;
        }
        case JOIN_NODE:
            offset->symbols += node->join[0]->offset.symbols;
            offset->symbols += node->join[1]->offset.symbols;
            for (int i = 0; i < 2; ++i) {
                offset->actions[swapped ? 1 - i : i] +=
                 node->join[0]->offset.actions[i] +
                 node->join[1]->offset.actions[i];
            }
            break;
        default:
            break;
        }
        if (node->flags & COPIED_PATH)
            continue;
        struct state_pair p = node->next_pair;
        uint32_t i = state_pair_table_lookup(table, p, fnv(&p, sizeof(p)));
        if (table->status[i] == EMPTY || which_paths[i].type == BOUNDARY_NODE)
            node->next = 0;
        else {
            node->next = malloc(sizeof(struct path_node));
            *node->next = which_paths[i];
        }
        node->flags |= COPIED_PATH;
    }
}

static void path_node_destroy(struct path_node *node)
{
    while (node && (node->flags & COPIED_PATH)) {
        struct path_node *next = node->next;
        free(node);
        node = next;
    }
}

static struct path_node *ambiguous_bracket_path(struct context *context,
 symbol_id symbol)
{
    return &context->ambiguous_bracket_paths[symbol -
     context->combined->number_of_tokens];
}

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash)
{
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        uint32_t n = table->available_size * 2;
        if (n <= 128)
            n = 128;
        state_pair_table_rehash(table, n);
    }
    uint32_t index = state_pair_table_lookup(table, pair, hash);
    if (table->status[index] == EMPTY) {
        table->status[index] = USED;
        table->pairs[index] = pair;
        table->in_paths[index].type = INVALID_NODE;
        table->out_paths[index].type = INVALID_NODE;
        table->ain_paths[index].type = INVALID_NODE;
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
        if (table->status[index] == EMPTY)
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
    memset(table->status, 0, sizeof(uint8_t) * table->available_size);
    table->used_size = 0;
}

static void state_pair_table_destroy(struct state_pair_table *table)
{
    free(table->pairs);
    free(table->in_paths);
    free(table->out_paths);
    free(table->ain_paths);
    free(table->in_weights);
    free(table->status);
    memset(table, 0, sizeof(*table));
}

static void state_pair_table_rehash(struct state_pair_table *table,
 uint32_t new_size)
{
    struct state_pair_table old = *table;
    table->pairs = calloc(new_size, sizeof(struct state_pair));
    table->in_paths = calloc(new_size, sizeof(struct path_node));
    table->out_paths = calloc(new_size, sizeof(struct path_node));
    table->ain_paths = calloc(new_size, sizeof(struct path_node));
    table->in_weights = calloc(new_size, sizeof(uint32_t));
    table->status = calloc(new_size, sizeof(uint8_t));
    table->available_size = new_size;
    table->used_size = 0;
    for (uint32_t i = 0; i < old.available_size; ++i) {
        if (old.status[i] == EMPTY)
            continue;
        uint32_t hash = fnv(&old.pairs[i], sizeof(old.pairs[i]));
        uint32_t index = state_pair_table_add(table, old.pairs[i], hash);
        table->in_paths[index] = old.in_paths[i];
        table->out_paths[index] = old.out_paths[i];
        table->ain_paths[index] = old.ain_paths[i];
        table->in_weights[index] = old.in_weights[i];
        table->status[index] = old.status[i];
    }
    free(old.pairs);
    free(old.in_paths);
    free(old.out_paths);
    free(old.ain_paths);
    free(old.in_weights);
    free(old.status);
}
