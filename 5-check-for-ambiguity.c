#include "5-check-for-ambiguity.h"

#include "bitset.h"
#include "fnv.h"
#include <assert.h>

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
    BRACKET_TRANSITION_NODE,
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
static struct path_node invalid_node = { .type = INVALID_NODE };
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
    // keep track of them without creating cycles.
    struct path_node *ain_paths;
    uint32_t *pair_hashes;
    uint8_t *status;

    // A state pair along an ambiguous path, if one has been found.
    bool has_ambiguity;
    struct state_pair ambiguity;

    uint32_t available_size;
    uint32_t used_size;
};

struct worklist {
    struct state_pair *pairs;
    uint32_t pairs_allocated_bytes;
    uint32_t number_of_pairs;
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

static void build_ambiguity_path(struct context *context,
 struct ambiguity *ambiguity, struct path_offset offset, struct path_node *node,
 int32_t direction);

static void search_state_pairs(struct context *context,
 struct state_pair_table *table, struct automaton *automaton,
 enum direction direction);
static void follow_state_pair_transition(struct path_node node, state_id a,
 state_id b, enum direction direction, struct state_pair_table *table,
 struct worklist *worklist);

// Copying a node fills in the offset as well.
static void path_node_copy(struct context *context,
 struct state_pair_table *table, struct path_node *which_paths,
 struct path_node *node);

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static uint32_t state_pair_table_lookup(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash);
static void state_pair_table_clear(struct state_pair_table *table);
static void state_pair_table_rehash(struct state_pair_table *table,
 uint32_t new_size);

void check_for_ambiguity(struct combined_grammar *combined,
 struct ambiguity *ambiguity)
{
    struct context context = {
        .combined = combined,
    };
    uint32_t tokens = combined->number_of_tokens;
    uint32_t number_of_bracket_transitions = 0;
    if (combined->automaton.number_of_symbols > tokens) {
        number_of_bracket_transitions =
         combined->automaton.number_of_symbols - tokens;
    }
    context.ambiguous_bracket_paths =
     calloc(number_of_bracket_transitions, sizeof(struct path_node));
    context.bracket_states =
     calloc(number_of_bracket_transitions, sizeof(state_id));
    struct automaton *bracket_automaton = &combined->bracket_automaton;
    for (state_id i = 0; i < bracket_automaton->number_of_states; ++i) {
        struct state s = bracket_automaton->states[i];
        if (s.accepting) {
            context.bracket_states[s.transition_symbol -
             combined->number_of_tokens] = i;
        }
    }

    // First pass: collect paths through the bracket automaton.
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

    // Second pass: look for ambiguity.
    struct automaton reversed = {0};
    automaton_reverse(&combined->automaton, &reversed);
    struct automaton bracket_reversed = {0};
    automaton_reverse(bracket_automaton, &bracket_reversed);
    struct state_pair_table table = {0};
    while (true) {
        search_state_pairs(&context, &table, &combined->automaton, FORWARD);
        search_state_pairs(&context, &table, &reversed, BACKWARD);
        if (table.has_ambiguity)
            break;
        state_pair_table_clear(&table);
        search_state_pairs(&context, &table, bracket_automaton, FORWARD);
        search_state_pairs(&context, &table, &bracket_reversed,
         BACKWARD_UNRESOLVED);
        if (table.has_ambiguity) {
            uint32_t path_index = state_pair_table_lookup(&table,
             table.ambiguity, fnv(&table.ambiguity, sizeof(table.ambiguity)));
            uint32_t i = path_index;
            while (table.out_paths[i].type != BOUNDARY_NODE) {
                struct state_pair p = table.out_paths[i].next_pair;
                i = state_pair_table_lookup(&table, p, fnv(&p, sizeof(p)));
            }
            struct state_pair p = table.pairs[i];
            assert(p.a == p.b);
            struct state s = combined->bracket_automaton.states[p.a];
            assert(s.accepting);
            struct path_node *in = malloc(sizeof(struct path_node));
            struct path_node *out = malloc(sizeof(struct path_node));
            *in = table.in_paths[path_index];
            *out = table.out_paths[path_index];
            if (table.ain_paths[path_index].type != INVALID_NODE)
                *in = table.ain_paths[path_index];
            path_node_copy(&context, &table, table.in_paths, in);
            path_node_copy(&context, &table, table.out_paths, out);
            context.ambiguous_bracket_paths[s.transition_symbol -
             combined->number_of_tokens] = (struct path_node) {
                .type = JOIN_NODE,
                .flags = AMBIGUOUS_NODE,
                .join = { in, out },
            };
        } else {
            // No more progress can be made toward finding an ambiguous path.
            // That means we're done here.
            ambiguity->has_ambiguity = false;
            return;
        }
        state_pair_table_clear(&table);
    }

    // We found an ambiguous path through the automaton.
    ambiguity->has_ambiguity = true;
    uint32_t path_index = state_pair_table_lookup(&table, table.ambiguity,
     fnv(&table.ambiguity, sizeof(table.ambiguity)));
    struct path_node in = table.in_paths[path_index];
    struct path_node out = table.out_paths[path_index];
    if (table.ain_paths[path_index].type != INVALID_NODE)
        in = table.ain_paths[path_index];
    path_node_copy(&context, &table, table.in_paths, &in);
    path_node_copy(&context, &table, table.out_paths, &out);
    ambiguity->number_of_tokens = in.offset.symbols + out.offset.symbols;
    ambiguity->tokens = calloc(ambiguity->number_of_tokens, sizeof(symbol_id));
    for (int i = 0; i < 2; ++i) {
        uint32_t n = in.offset.actions[i] + out.offset.actions[i];
        ambiguity->paths[i].number_of_actions = n;
        ambiguity->paths[i].actions = calloc(n, sizeof(uint16_t));
        ambiguity->paths[i].offsets = calloc(n, sizeof(uint32_t));
    }
    build_ambiguity_path(&context, ambiguity, in.offset, &out, 1);
    build_ambiguity_path(&context, ambiguity, in.offset, &in, -1);
}

static void build_ambiguity_path(struct context *context,
 struct ambiguity *ambiguity, struct path_offset offset, struct path_node *node,
 int32_t direction)
{
    bool swapped = false;
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
        case BRACKET_TRANSITION_NODE: {
            struct state_pair p = node->bracket_pair;
            uint32_t i = state_pair_table_lookup(&context->bracket_paths, p,
             fnv(&p, sizeof(p)));
            assert(context->bracket_paths.status[i] == LOCKED);
            struct path_node in = context->bracket_paths.in_paths[i];
            assert(in.flags & COPIED_PATH);
            if (direction == 1) {
                offset.symbols += in.offset.symbols;
                for (int i = 0; i < 2; ++i)
                    offset.actions[swapped ? 1 - i : i] += in.offset.actions[i];
            }
            build_ambiguity_path(context, ambiguity, offset, &in, -1);
            if (direction == -1) {
                offset.symbols -= in.offset.symbols;
                for (int i = 0; i < 2; ++i)
                    offset.actions[swapped ? 1 - i : i] -= in.offset.actions[i];
            }
            break;
        }
        case JOIN_NODE: {
            int j = direction == 1 ? 0 : 1;
            offset.symbols += node->join[j]->offset.symbols * direction;
            for (int i = 0; i < 2; ++i) {
                offset.actions[i] += node->join[j]->offset.actions[i] *
                 direction;
            }
            build_ambiguity_path(context, ambiguity, offset, node->join[1], 1);
            build_ambiguity_path(context, ambiguity, offset, node->join[0], -1);
            offset.symbols += node->join[1 - j]->offset.symbols * direction;
            for (int i = 0; i < 2; ++i) {
                offset.actions[i] += node->join[1 - j]->offset.actions[i] *
                 direction;
            }
            break;
        }
        default:
            break;
        }
    }
}

static void search_state_pairs(struct context *context,
 struct state_pair_table *table, struct automaton *automaton,
 enum direction direction)
{
    table->has_ambiguity = false;
    automaton_compute_epsilon_closure(automaton, FOLLOW_ACTION_TRANSITIONS);
    struct worklist worklist = {0};
    if (direction == FORWARD) {
        follow_state_pair_transition(boundary_node, automaton->start_state,
         automaton->start_state, direction, table, &worklist);
    } else {
        struct state s = automaton->states[automaton->start_state];
        for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
            state_id target = s.transitions[i].target;
            if (direction == BACKWARD_UNRESOLVED && context->ambiguous_bracket_paths[automaton->states[target].transition_symbol - context->combined->number_of_tokens].type != INVALID_NODE) {
                continue;
            }
            follow_state_pair_transition(boundary_node, target, target,
             BACKWARD, table, &worklist);
        }
        // This is the only place where the distinction between
        // BACKWARD_UNRESOLVED and BACKWARD matters.  Simplify `direction` to
        // BACKWARD for the rest of the function so we don't have to deal with
        // both.
        direction = BACKWARD;
    }
    while (worklist.number_of_pairs > 0 && !table->has_ambiguity) {
        struct state_pair s = worklist.pairs[--worklist.number_of_pairs];
        struct state a = automaton->states[s.a];
        struct state b = automaton->states[s.b];
        bool follow_epsilons = s.epsilon_state == ALLOW_EPSILON_SUCCESSORS;
        if (direction == BACKWARD)
            follow_epsilons = !follow_epsilons;
        if (follow_epsilons) {
            // Search through action transitions.
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
                    }, ac.reachable.states[i], bc.reachable.states[j],
                     direction, table, &worklist);
                }
                follow_state_pair_transition((struct path_node){
                    .type = ACTION_NODE,
                    .next_pair = s,
                    .actions[0] = ac.actions + ac.action_indexes[i],
                }, ac.reachable.states[i], s.b, direction, table, &worklist);
                if (direction == FORWARD && s.a == s.b &&
                 ac.ambiguous_action_indexes[i] != UINT32_MAX) {
                    follow_state_pair_transition((struct path_node){
                        .type = ACTION_NODE,
                        .next_pair = s,
                        .flags = AMBIGUOUS_NODE,
                        .actions[0] = ac.actions + ac.action_indexes[i],
                        .actions[1] = ac.actions +
                         ac.ambiguous_action_indexes[i],
                    }, ac.reachable.states[i], ac.reachable.states[i],
                     direction, table, &worklist);
                }
            }
            for (uint32_t j = 0; j < bc.reachable.number_of_states; ++j) {
                follow_state_pair_transition((struct path_node){
                    .type = ACTION_NODE,
                    .next_pair = s,
                    .actions[1] = bc.actions + bc.action_indexes[j],
                }, s.a, bc.reachable.states[j], direction, table, &worklist);
            }
            follow_state_pair_transition((struct path_node){
                .next_pair = s,
                .type = ACTION_NODE,
            }, s.a, s.b, direction, table, &worklist);
        } else {
            // Search through symbol and bracket transitions.
            for (uint32_t i = 0; i < a.number_of_transitions; ++i) {
                struct transition at = a.transitions[i];
                if (at.symbol == SYMBOL_EPSILON)
                    continue;
                bool bracket = at.symbol >= context->combined->number_of_tokens;
                for (uint32_t j = 0; j < b.number_of_transitions; ++j) {
                    struct transition bt = b.transitions[j];
                    if (!bracket) {
                        if (at.symbol != bt.symbol)
                            continue;
                        follow_state_pair_transition((struct path_node){
                            .type = SYMBOL_NODE,
                            .next_pair = s,
                            .symbol = at.symbol,
                        }, at.target, bt.target, direction, table, &worklist);
                        continue;
                    }
                    if (bt.symbol == SYMBOL_EPSILON)
                        continue;
                    if (bt.symbol < context->combined->number_of_tokens)
                        continue;
                    struct state_pair p = state_pair_make(context->bracket_states[at.symbol - context->combined->number_of_tokens], context->bracket_states[bt.symbol - context->combined->number_of_tokens], DISALLOW_EPSILON_SUCCESSORS);
                    uint32_t k = state_pair_table_lookup(&context->bracket_paths, p, fnv(&p, sizeof(p)));
                    if (context->bracket_paths.status[k] != EMPTY) {
                        follow_state_pair_transition((struct path_node){
                            .type = BRACKET_TRANSITION_NODE,
                            .next_pair = s,
                            .bracket_pair = p,
                            .flags = at.symbol == bt.symbol ? 0 :
                             AMBIGUOUS_NODE,
                        }, at.target, bt.target, direction, table, &worklist);
                    }
                    if (at.symbol == bt.symbol) {
                        struct path_node n;
                        n = context->ambiguous_bracket_paths[at.symbol -
                         context->combined->number_of_tokens];
                        if (n.type != INVALID_NODE) {
                            n.next_pair = s;
                            follow_state_pair_transition(n, at.target,
                             bt.target, direction, table, &worklist);
                        }
                    }
                }
            }
        }
    }
}

static void follow_state_pair_transition(struct path_node node, state_id a,
 state_id b, enum direction direction, struct state_pair_table *table,
 struct worklist *worklist)
{
    struct state_pair pair;
    if (node.type == ACTION_NODE) {
        pair = state_pair_make(a, b, direction == FORWARD ?
         DISALLOW_EPSILON_SUCCESSORS : ALLOW_EPSILON_SUCCESSORS);
    } else {
        pair = state_pair_make(a, b, direction == FORWARD ?
         ALLOW_EPSILON_SUCCESSORS : DISALLOW_EPSILON_SUCCESSORS);
    }
    if (a != pair.a)
        node.flags |= SWAPPED_PATH;

    uint32_t hash = fnv(&pair, sizeof(pair));
    uint32_t index = state_pair_table_add(table, pair, hash);
    if (table->status[index] == LOCKED)
        return;

    bool continue_following = false;
    if (direction == FORWARD && table->in_paths[index].type == INVALID_NODE) {
        table->in_paths[index] = node;
        continue_following = true;
    }
    if (direction == BACKWARD && table->out_paths[index].type == INVALID_NODE) {
        table->out_paths[index] = node;
        if ((pair.a != pair.b && table->in_paths[index].type != INVALID_NODE) ||
         table->ain_paths[index].type != INVALID_NODE) {
            table->has_ambiguity = true;
            table->ambiguity = pair;
            return;
        }
        continue_following = true;
    }
    if (direction == FORWARD && (node.flags & AMBIGUOUS_NODE) &&
     table->ain_paths[index].type == INVALID_NODE)
        table->ain_paths[index] = node;
    if (!continue_following)
        return;
    uint32_t windex = worklist->number_of_pairs++;
    worklist->pairs = grow_array(worklist->pairs,
     &worklist->pairs_allocated_bytes,
     worklist->number_of_pairs * sizeof(struct state_pair));
    worklist->pairs[windex] = pair;
}

static void path_node_copy(struct context *context,
 struct state_pair_table *table, struct path_node *which_paths,
 struct path_node *node)
{
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
        case BRACKET_TRANSITION_NODE: {
            struct state_pair p = node->bracket_pair;
            uint32_t i = state_pair_table_lookup(&context->bracket_paths, p,
             fnv(&p, sizeof(p)));
            assert(context->bracket_paths.status[i] == LOCKED);
            struct path_node in = context->bracket_paths.in_paths[i];
            assert(in.flags & COPIED_PATH);
            offset->symbols += in.offset.symbols;
            for (int i = 0; i < 2; ++i)
                offset->actions[swapped ? 1 - i : i] += in.offset.actions[i];
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
        if (table->status[i] == EMPTY)
            node->next = 0;
        else {
            node->next = malloc(sizeof(struct path_node));
            *node->next = which_paths[i];
        }
        node->flags |= COPIED_PATH;
    }
}

static uint32_t state_pair_table_add(struct state_pair_table *table,
 struct state_pair pair, uint32_t hash)
{
    if (3 * table->available_size <= 4 * (table->used_size + 1)) {
        uint32_t n = table->available_size * 2;
        if (n == 0)
            n = 4;
        state_pair_table_rehash(table, n);
    }
    uint32_t index = state_pair_table_lookup(table, pair, hash);
    if (table->status[index] == EMPTY) {
        table->status[index] = USED;
        table->pairs[index] = pair;
        table->in_paths[index] = invalid_node;
        table->out_paths[index] = invalid_node;
        table->ain_paths[index] = invalid_node;
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
    free(table->pairs);
    free(table->in_paths);
    free(table->out_paths);
    free(table->ain_paths);
    free(table->pair_hashes);
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
    table->pair_hashes = calloc(new_size, sizeof(uint32_t));
    table->status = calloc(new_size, sizeof(uint8_t));
    table->available_size = new_size;
    table->used_size = 0;
    for (uint32_t i = 0; i < old.available_size; ++i) {
        if (old.status[i] == EMPTY)
            continue;
        uint32_t index = state_pair_table_add(table, old.pairs[i],
         old.pair_hashes[i]);
        table->in_paths[index] = old.in_paths[i];
        table->out_paths[index] = old.out_paths[i];
        table->ain_paths[index] = old.ain_paths[i];
        table->status[index] = old.status[i];
    }
    free(old.pairs);
    free(old.in_paths);
    free(old.out_paths);
    free(old.ain_paths);
    free(old.pair_hashes);
    free(old.status);
}
