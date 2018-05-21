#include "6b-interpret.h"

#include "fancy-tree-output.h"
#include "x-construct-actions.h"
#include <assert.h>
#include <stdio.h>

#define READ_KEYWORD_TOKEN read_keyword_token

#define WRITE_NUMBER_TOKEN write_number_token
#define WRITE_IDENTIFIER_TOKEN write_identifier_token
#define WRITE_STRING_TOKEN write_string_token

#define ALLOW_DASHES_IN_IDENTIFIERS(info) \
 (((struct tokenizer_info *)info)->allow_dashes_in_identifiers)

#define IDENTIFIER_TOKEN \
 (((struct tokenizer_info *)tokenizer->info)->identifier_symbol)
#define NUMBER_TOKEN (((struct tokenizer_info *)tokenizer->info)->number_symbol)
#define STRING_TOKEN (((struct tokenizer_info *)tokenizer->info)->string_symbol)
#define BRACKET_TRANSITION_TOKEN 0xffffffff

static size_t read_keyword_token(uint32_t *token, bool *end_token,
 const char *text, void *info);
static void write_identifier_token(size_t offset, size_t length, void *info);
static void write_string_token(size_t offset, size_t length,
 size_t content_offset, size_t content_length, void *info);
static void write_number_token(size_t offset, size_t length, double number,
 void *info);

struct interpret_context;
struct tokenizer_info {
    struct interpret_context *context;
    symbol_id identifier_symbol;
    symbol_id number_symbol;
    symbol_id string_symbol;
    bool allow_dashes_in_identifiers;
};

struct interpret_node;
#define FINISHED_NODE_T struct interpret_node *
#define FINISH_NODE finish_node
#define FINISH_TOKEN finish_token

static struct interpret_node *finish_node(uint32_t rule, uint32_t choice,
 struct interpret_node *next_sibling, struct interpret_node **slots,
 size_t start_location, size_t end_location, struct interpret_context *context);
static struct interpret_node *finish_token(uint32_t rule,
 struct interpret_node *next_sibling, struct interpret_context *context);

#define RULE_T uint32_t
#define RULE_LOOKUP rule_lookup
#define ROOT_RULE root_rule
#define FIXITY_ASSOCIATIVITY_PRECEDENCE_LOOKUP(fixity_associativity, \
 precedence, rule, choice, context) do { \
    fixity_associativity = fixity_associativity_lookup(rule, choice, context); \
    precedence = precedence_lookup(rule, choice, context); \
 } while (0)
#define NUMBER_OF_SLOTS_LOOKUP number_of_slots_lookup
#define LEFT_RIGHT_OPERAND_SLOTS_LOOKUP(rule, left, right, operand, info) \
 (left_right_operand_slots_lookup(rule, &(left), &(right), &(operand), info))

static uint32_t rule_lookup(uint32_t parent, uint32_t slot,
 struct interpret_context *context);
static uint32_t root_rule(struct interpret_context *context);
static int fixity_associativity_lookup(uint32_t rule, uint32_t choice,
 struct interpret_context *context);
static int precedence_lookup(uint32_t rule, uint32_t choice,
 struct interpret_context *context);
static size_t number_of_slots_lookup(uint32_t rule,
 struct interpret_context *context);
static void left_right_operand_slots_lookup(uint32_t rule, uint32_t *left,
 uint32_t *right, uint32_t *operand, struct interpret_context *context);

#include "x-tokenize.h"
#include "x-construct-parse-tree.h"

struct interpret_context {
    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;
    struct bracket_transitions *transitions;

    struct state_array stack;
    state_id state;

    struct bluebird_default_tokenizer *tokenizer;
    struct construct_state construct_state;
    struct interpret_node *tokens;

    // Table mapping abstract offsets to concrete offsets.
    size_t *offset_table;
    uint32_t offset_table_allocated_bytes;
    uint32_t next_action_offset;
};

enum interpret_node_type {
    NODE_RULE,
    NODE_IDENTIFIER_TOKEN,
    NODE_NUMBER_TOKEN,
    NODE_STRING_TOKEN,
};

struct interpret_node {
    enum interpret_node_type type;
    struct interpret_node *next_sibling;

    size_t start_location;
    size_t end_location;

    // For rules.
    uint32_t rule_index;
    uint32_t choice_index;
    bool is_operator;

    size_t number_of_slots;
    struct interpret_node **slots;

    // For tokens.
    union {
        struct {
            const char *name;
            size_t length;
        } identifier;
        double number;
        struct {
            const char *string;
            size_t length;
        } string;
    };
};

static void fill_run_states(struct interpret_context *ctx,
 struct bluebird_token_run *run);
static struct interpret_node *build_parse_tree(struct interpret_context *ctx,
 struct bluebird_token_run *run);

static symbol_id token_symbol(struct combined_grammar *grammar, const char *s);
static bool follow_transition(struct automaton *a, state_id *state,
 symbol_id symbol);
static void follow_transition_reversed(struct interpret_context *ctx,
 state_id *last_nfa_state, uint32_t state, uint32_t token, size_t start,
 size_t end);

static uint32_t push_action_offset(struct interpret_context *, size_t offset);

#if 1
static void print_token_runs(struct interpret_context *ctx,
 struct bluebird_token_run *token_run)
{
    if (!token_run)
        return;
    print_token_runs(ctx, token_run->prev);
    for (uint16_t i = 0; i < token_run->number_of_tokens; ++i) {
        if (token_run->states[i] >= (1UL << 31)) {
            struct state s = ctx->deterministic->bracket_automaton.states[token_run->states[i] - (1UL << 31)];
            if (s.accepting && s.transition_symbol)
                printf("%02x -> %u* (%x)\n", token_run->tokens[i], token_run->states[i] - (1U << 31), s.transition_symbol);
            else
                printf("%02x -> %u*\n", token_run->tokens[i], token_run->states[i] - (1U << 31));
        } else
            printf("%02x -> %u\n", token_run->tokens[i], token_run->states[i]);
    }
}

static void print_parse_tree(struct interpret_context *ctx,
 struct interpret_node *node, struct slot *slot, int indent)
{
    if (!node)
        return;
    for (int i = 0; i < indent; ++i)
        printf(" ");
    if (node->type != NODE_RULE) {
        switch (node->type) {
        case NODE_IDENTIFIER_TOKEN:
            printf("%.*s", (int)node->identifier.length, node->identifier.name);
            break;
        case NODE_NUMBER_TOKEN:
            printf("%f", node->number);
            break;
        case NODE_STRING_TOKEN:
            printf("%.*s", (int)node->string.length, node->string.string);
            break;
        default:
            break;
        }
        // FIXME: We need to fill in the correct rule index for this to work.
//        if (slot && (slot->name_length != rule->name_length ||
//         memcmp(slot->name, rule->name, rule->name_length)))
//            printf("@%.*s", (int)slot->name_length, slot->name);
        printf("\n");
        print_parse_tree(ctx, node->next_sibling, slot, indent);
        return;
    }
    struct rule *rule = &ctx->grammar->rules[node->rule_index];
    printf("%.*s", (int)rule->name_length, rule->name);
    if (slot && (slot->name_length != rule->name_length ||
     memcmp(slot->name, rule->name, rule->name_length)))
        printf("@%.*s", (int)slot->name_length, slot->name);
    if (rule->number_of_choices) {
        if (node->choice_index >= rule->number_of_choices) {
            struct operator *op = &rule->operators[node->choice_index -
             rule->number_of_choices];
            printf(" : %.*s", (int)op->name_length, op->name);
        } else {
            struct choice *choice = &rule->choices[node->choice_index];
            printf(" : %.*s", (int)choice->name_length, choice->name);
        }
    }
    printf("  %lu - %lu", node->start_location, node->end_location);
    printf("\n");
    for (uint32_t i = 0; i < rule->number_of_slots; ++i) {
        struct slot *slot = &rule->slots[i];
        if (!node->slots[i])
            continue;
        print_parse_tree(ctx, node->slots[i], slot, indent + 1);
    }
    print_parse_tree(ctx, node->next_sibling, slot, indent);

}
#endif

// XXX begin terrible code
uint32_t max_tree_depth(struct interpret_node *node)
{
    if (!node)
        return 0;
    if (node->type != NODE_RULE)
        return 0;
    uint32_t max_depth = 1;
    for (uint32_t i = 0; i < node->number_of_slots; ++i) {
        uint32_t depth = max_tree_depth(node->slots[i]);
        if (depth + 1 > max_depth)
            max_depth = depth + 1;
    }
    return max_depth;
}
void find_number_of_labels(struct interpret_node *node, uint32_t depth,
 struct document *document)
{
    if (!node)
        return;
    if (node->type != NODE_RULE)
        return;
    if (depth == 0)
        return;
    for (uint32_t i = 0; i < node->number_of_slots; ++i) {
        struct interpret_node *slot = node->slots[i];
        while (slot) {
            if (slot->type != NODE_RULE) {
                slot = slot->next_sibling;
                continue;
            }
            find_number_of_labels(slot, depth - 1, document);
            document->rows[depth].number_of_labels++;
            slot = slot->next_sibling;
        }
    }
}
static void append_string(char **string, uint32_t *length, uint32_t *bytes,
 const char *append, size_t append_length)
{
    uint32_t index = *length;
    *length += (uint32_t)append_length;
    *string = grow_array(*string, bytes, *length);
    memcpy(*string + index, append, append_length);
}
static void fill_rows(struct interpret_context *ctx, struct interpret_node *node,
 struct rule *rule, uint32_t depth, uint32_t n, struct document *document)
{
    if (!node)
        return;
    if (node->type != NODE_RULE)
        return;
    if (depth == 0)
        return;
    for (uint32_t i = 0; i < node->number_of_slots; ++i) {
        struct interpret_node *slot = node->slots[i];
        struct slot *s = &rule->slots[i];
        while (slot) {
            if (slot->type != NODE_RULE) {
                slot = slot->next_sibling;
                continue;
            }
            struct rule *rule = &ctx->grammar->rules[slot->rule_index];
            uint32_t j = document->rows[depth].number_of_labels++;

            char *str = 0;
            uint32_t len = 0;
            uint32_t bytes = 0;
            append_string(&str, &len, &bytes, rule->name, rule->name_length);
//            if (s && (s->name_length != rule->name_length ||
//             memcmp(s->name, rule->name, rule->name_length))) {
//                append_string(&str, &len, &bytes, "@", 1);
//                append_string(&str, &len, &bytes, s->name, s->name_length);
//            }
            if (rule->number_of_choices) {
                append_string(&str, &len, &bytes, ":", 1);
                if (slot->choice_index >= rule->number_of_choices) {
                    struct operator *op = &rule->operators[slot->choice_index -
                     rule->number_of_choices];
                    append_string(&str, &len, &bytes, op->name,
                     op->name_length);
                } else {
                    struct choice *choice = &rule->choices[slot->choice_index];
                    append_string(&str, &len, &bytes, choice->name,
                     choice->name_length);
                }
            }

            document->rows[depth].labels[j] = (struct label){
                .text = str, // TODO: No leak!
                .length = len,
                .start = n - (uint32_t)(SIZE_MAX - slot->start_location) - 1,
                .end = n - (uint32_t)(SIZE_MAX - slot->end_location) - 1,
            };
            fill_rows(ctx, slot, rule, depth - 1, n, document);
            slot = slot->next_sibling;
        }
    }
}
static int compare_labels(const void *a, const void *b)
{
    struct label la = *(struct label *)a;
    struct label lb = *(struct label *)b;
    if (la.start < lb.start)
        return -1;
    else if (la.start > lb.start)
        return 1;
    else
        return 0;
}
// XXX end terrible code

void interpret(struct grammar *grammar, struct combined_grammar *combined,
 struct bracket_transitions *transitions,
 struct deterministic_grammar *deterministic, const char *text)
{
    if (deterministic->automaton.number_of_states > (1UL << 31) ||
     deterministic->bracket_automaton.number_of_states > (1UL << 31)) {
        fprintf(stderr, "error: automaton has too many states.\n");
        exit(-1);
    }
    struct bluebird_token_run *token_run = 0;
    struct tokenizer_info info = {
        .identifier_symbol = token_symbol(combined, "identifier"),
        .number_symbol = token_symbol(combined, "number"),
        .string_symbol = token_symbol(combined, "string"),
        .allow_dashes_in_identifiers =
         SHOULD_ALLOW_DASHES_IN_IDENTIFIERS(combined),
    };
    struct bluebird_default_tokenizer tokenizer = {
        .text = text,
        .info = &info,
    };
    struct interpret_context context = {
        .grammar = grammar,
        .combined = combined,
        .deterministic = deterministic,
        .transitions = transitions,
        .tokenizer = &tokenizer,
    };
    info.context = &context;
    while (bluebird_default_tokenizer_advance(&tokenizer, &token_run))
        fill_run_states(&context, token_run);
    if (text[tokenizer.offset] != '\0') {
        // TODO: Better error message
        fprintf(stderr, "error: tokenizing failed.\n");
        exit(-1);
    }
    print_token_runs(&context, token_run);
    struct interpret_node *root = build_parse_tree(&context, token_run);
    // TODO: Error handling.
    print_parse_tree(&context, root, 0, 0);
    for (uint32_t i = 0; i < context.next_action_offset; ++i) {
        printf("%u. %lu\n", i, context.offset_table[i]);
    }
    uint32_t depth = max_tree_depth(root);
    struct document document = {
        .rows = calloc(depth, sizeof(struct row)),
        .number_of_rows = depth,
        .number_of_columns = 80,
    };
    uint32_t row0_allocated_bytes = 0;
    uint32_t n = context.next_action_offset;
    for (uint32_t i = 0; i < n; ++i) {
        if (n - i - 1 <= i)
            break;
        size_t t = context.offset_table[n - i - 1];
        context.offset_table[n - i - 1] = context.offset_table[i];
        context.offset_table[i] = t;
    }
    for (uint32_t i = 1; i < n; ++i) {
        if (context.offset_table[i] != context.offset_table[i - 1]) {
            uint32_t index = document.rows[0].number_of_labels++;
            document.rows[0].labels = grow_array(document.rows[0].labels,
             &row0_allocated_bytes, document.rows[0].number_of_labels *
             sizeof(struct label));
            document.rows[0].labels[index] = (struct label){
                .text = context.tokenizer->text + context.offset_table[i - 1],
                .length = context.offset_table[i] - context.offset_table[i - 1],
                .start = i - 1,
                .end = i,
            };
        }
    }
    uint32_t index = document.rows[0].number_of_labels++;
    document.rows[0].labels = grow_array(document.rows[0].labels,
     &row0_allocated_bytes, document.rows[0].number_of_labels *
     sizeof(struct label));
    document.rows[0].labels[index] = (struct label){
        .text = "",
        .length = 0,
        .start = n - 1,
        .end = n,
    };
    find_number_of_labels(root, depth - 1, &document);
    for (uint32_t i = 1; i < document.number_of_rows; ++i) {
        document.rows[i].labels = calloc(document.rows[i].number_of_labels,
         sizeof(struct label));
        document.rows[i].number_of_labels = 0;
    }
    fill_rows(&context, root, &context.grammar->rules[context.grammar->root_rule], depth - 1, n, &document);
    for (uint32_t i = 0; i < document.number_of_rows; ++i) {
        qsort(document.rows[i].labels, document.rows[i].number_of_labels,
         sizeof(struct label), compare_labels);
        printf("row %u:\n", i);
        for (uint32_t j = 0; j < document.rows[i].number_of_labels; ++j) {
            struct label l = document.rows[i].labels[j];
            printf(" %.*s %u - %u\n", (int)l.length, l.text, l.start, l.end);
        }
    }
    if (getenv("TERM")) {
        document.reset_color_code = "\033[0m";
        document.line_indicator_color_code = "\033[90m";
        document.color_codes = (const char *[]){
            "\033[38;5;168m",
            "\033[38;5;113m",
            "\033[38;5;68m",
            "\033[38;5;214m",
            "\033[38;5;97m",
        };
        document.number_of_color_codes = 5;
    }
    output_document(&document);
}

static void fill_run_states(struct interpret_context *ctx,
 struct bluebird_token_run *run)
{
    struct state_array *stack = &ctx->stack;
    state_id state = ctx->state;
    struct automaton *a = stack->number_of_states > 0 ?
     &ctx->deterministic->bracket_automaton : &ctx->deterministic->automaton;
    for (uint16_t i = 0; i < run->number_of_tokens; ++i) {
        symbol_id symbol = run->tokens[i];
        run->states[i] = state + (stack->number_of_states > 0 ? 1UL << 31 : 0);
        if (a->states[state].accepting && stack->number_of_states > 0) {
            // We've reached the end token for a guard bracket.
            assert(symbol == BRACKET_TRANSITION_TOKEN);
            symbol = a->states[state].transition_symbol;
            run->tokens[i] = symbol;
            state = state_array_pop(stack);
            if (stack->number_of_states == 0)
                a = &ctx->deterministic->automaton;
        } else if (follow_transition(a, &state, symbol)) {
            // This is just a normal token.
            continue;
        } else {
            // Maybe this is the start token for a guard bracket.
            // TODO: Look for start symbols/tokens explicitly?
            state_array_push(stack, state);
            a = &ctx->deterministic->bracket_automaton;
            state = a->start_state;
        }
        run->states[i] = state + (stack->number_of_states > 0 ? 1UL << 31 : 0);
        if (!follow_transition(a, &state, symbol)) {
            // TODO: Better error message here.
            fprintf(stderr, "error: unexpected token %u at %u.\n", symbol, i);
            exit(-1);
            break;
        }
    }
    ctx->state = state;
}

static struct interpret_node *build_parse_tree(struct interpret_context *ctx,
 struct bluebird_token_run *run)
{
    ctx->construct_state.info = ctx;
    state_id nfa_state = ctx->combined->final_nfa_state;
    if (ctx->combined->root_rule_is_expression)
        construct_begin(&ctx->construct_state, CONSTRUCT_EXPRESSION_ROOT);
    else
        construct_begin(&ctx->construct_state, CONSTRUCT_NORMAL_ROOT);
    size_t whitespace = ctx->tokenizer->whitespace;
    size_t offset = ctx->tokenizer->offset - whitespace;
    while (run) {
        uint16_t length_offset = run->lengths_size - 1;
        uint16_t n = run->number_of_tokens;
        for (uint16_t i = n - 1; i < n; i--) {
            size_t end = offset;
            size_t len = 0;
            if (run->tokens[i] < ctx->combined->number_of_tokens)
                len = decode_token_length(run, &length_offset, &offset);
            follow_transition_reversed(ctx, &nfa_state, run->states[i],
             run->tokens[i], end, end + whitespace);
            if (len > 0) {
                push_action_offset(ctx, end);
                push_action_offset(ctx, end - len);
            }
            whitespace = end - offset - len;
        }
        run = run->prev;
    }
    // TODO: is this correct?
    follow_transition_reversed(ctx, &nfa_state, UINT32_MAX, UINT32_MAX,
     offset, offset + whitespace);
    return construct_finish(&ctx->construct_state);
}

static symbol_id token_symbol(struct combined_grammar *grammar, const char *s)
{
    size_t length = strlen(s);
    for (uint32_t i = grammar->number_of_keyword_tokens;
     i < grammar->number_of_tokens; ++i) {
        struct token *token = &grammar->tokens[i];
        if (token->length != length)
            continue;
        if (memcmp(token->string, s, length))
            continue;
        return i;
    }
    return SYMBOL_EPSILON;
}

static bool follow_transition(struct automaton *a, state_id *state,
 symbol_id symbol)
{
    struct state s = a->states[*state];
    for (uint32_t i = 0; i < s.number_of_transitions; ++i) {
        if (s.transitions[i].symbol != symbol)
            continue;
        *state = s.transitions[i].target;
        return true;
    }
    return false;
}

#define CONSTRUCT_ACTION_NAME(name) PRINT_CONSTRUCT_ACTION_ ## name,
enum { CONSTRUCT_ACTIONS };
#undef CONSTRUCT_ACTION_NAME
static void print_action(uint16_t action, size_t offset)
{
    uint16_t slot = CONSTRUCT_ACTION_GET_SLOT(action);
    switch (CONSTRUCT_ACTION_GET_TYPE(action)) {
#define CONSTRUCT_ACTION_NAME(name) case PRINT_CONSTRUCT_ACTION_ ## name : printf(#name " %u at %lu\n", slot, offset); break;
CONSTRUCT_ACTIONS
#undef CONSTRUCT_ACTION_NAME
    }
}

static void follow_transition_reversed(struct interpret_context *ctx,
 state_id *last_nfa_state, uint32_t state, uint32_t token, size_t start,
 size_t end)
{
    struct action_map *map = &ctx->deterministic->action_map;
    bool bracket_automaton = false;
    if (state >= (1UL << 31) && state != UINT32_MAX) {
        state -= (1UL << 31);
        map = &ctx->deterministic->bracket_action_map;
        bracket_automaton = true;
    }
    // TODO: Include this info in a token table.
    bool bracket_transition = false;
    for (uint32_t j = 0; j < ctx->transitions->number_of_transitions; ++j) {
        struct bracket_transition t = ctx->transitions->transitions[j];
        if (token != t.deterministic_transition_symbol)
            continue;
        bracket_transition = true;
        break;
    }
    struct action_map_entry *entry;
    state_id nfa_state = *last_nfa_state;
    entry = action_map_find(map, nfa_state, state, token);
    if (!entry) {
        fprintf(stderr, "internal error (%u %u %x)\n", state, nfa_state, token);
        exit(-1);
    }
    nfa_state = entry->nfa_state;
    if (bracket_transition) {
        state_array_push(&ctx->stack, nfa_state);
        // TODO: Use a table.
        for (state_id k = 0; k <
         ctx->combined->bracket_automaton.number_of_states; ++k) {
            if (ctx->combined->bracket_automaton.states[k].transition_symbol
             != entry->nfa_symbol)
                continue;
            nfa_state = k;
            break;
        }
    }
    size_t offset = end;
    for (uint32_t k = entry->action_index; k < map->number_of_actions; k++) {
        if (map->actions[k] == 0)
            break;
        uint32_t action_offset = ctx->next_action_offset;
        switch (CONSTRUCT_ACTION_GET_TYPE(map->actions[k])) {
        case CONSTRUCT_ACTION_SET_SLOT_CHOICE:
        case CONSTRUCT_ACTION_TOKEN_SLOT:
            break;
        case CONSTRUCT_ACTION_BEGIN_SLOT:
        case CONSTRUCT_ACTION_BEGIN_EXPRESSION_SLOT:
        case CONSTRUCT_ACTION_BEGIN_OPERAND:
        case CONSTRUCT_ACTION_BEGIN_OPERATOR:
            action_offset = push_action_offset(ctx, offset);
            break;
        case CONSTRUCT_ACTION_END_SLOT:
        case CONSTRUCT_ACTION_END_EXPRESSION_SLOT:
        case CONSTRUCT_ACTION_END_OPERAND:
        case CONSTRUCT_ACTION_END_OPERATOR:
            if (offset != start) {
                // Add offsets for the whitespace.
                // TODO: Detect newlines.
                push_action_offset(ctx, offset);
                offset = start;
                push_action_offset(ctx, offset);
                printf("whitespace: %u - %u\n", start, end);
            }
            action_offset = push_action_offset(ctx, offset);
            break;
        }
        printf("action: %u\n", offset);
        print_action(map->actions[k], action_offset);
        construct_action_apply(&ctx->construct_state, map->actions[k],
         SIZE_MAX - action_offset);
    }
    if (offset != start) {
        // Add offsets for the whitespace.
        // TODO: Detect newlines.
        // TODO: Don't copy and paste this.
        push_action_offset(ctx, offset);
        offset = start;
        push_action_offset(ctx, offset);
    }
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state) {
        nfa_state = state_array_pop(&ctx->stack);
    }
    *last_nfa_state = nfa_state;
}

static uint32_t push_action_offset(struct interpret_context *ctx, size_t offset)
{
    uint32_t action_offset = ctx->next_action_offset++;
    ctx->offset_table = grow_array(ctx->offset_table,
     &ctx->offset_table_allocated_bytes,
     ctx->next_action_offset * sizeof(size_t));
    ctx->offset_table[action_offset] = offset;
    return action_offset;
}

static size_t read_keyword_token(uint32_t *token, bool *end_token,
 const char *text, void *info)
{
    struct combined_grammar *combined =
     ((struct tokenizer_info *)info)->context->combined;
    symbol_id symbol = SYMBOL_EPSILON;
    size_t max_len = 0;
    bool end = false;
    for (uint32_t i = 0; i < combined->number_of_keyword_tokens; ++i) {
        struct token token = combined->tokens[i];
        if (token.length > max_len && !strncmp((const char *)text, token.string,
         token.length)) {
            max_len = token.length;
            symbol = i;
            end = token.type == TOKEN_END;
        }
    }
    *end_token = end;
    *token = symbol;
    return max_len;
}

static void write_identifier_token(size_t offset, size_t length, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_IDENTIFIER_TOKEN;
    node->identifier.name = ctx->tokenizer->text + offset;
    node->identifier.length = length;
    ctx->tokens = node;
}

static void write_string_token(size_t offset, size_t length,
 size_t content_offset, size_t content_length, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_STRING_TOKEN;
    // TODO: String escape sequences.
    node->string.string = ctx->tokenizer->text + offset;
    node->string.length = length;
    ctx->tokens = node;
}

static void write_number_token(size_t offset, size_t length, double number,
 void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_NUMBER_TOKEN;
    node->number = number;
    ctx->tokens = node;
}

static struct interpret_node *finish_node(uint32_t rule, uint32_t choice,
 struct interpret_node *next_sibling, struct interpret_node **slots,
 size_t start_location, size_t end_location, struct interpret_context *context)
{
    // TODO: We don't need to construct this intermediate data structure.
    // - If we're building JSON, we can just build the JSON string as a linked
    //   list.
    // - If we're doing fancy output, we can write nodes directly into the rows
    //   of the document.
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->rule_index = rule;
    node->choice_index = choice;
    node->next_sibling = next_sibling;
    node->start_location = start_location;
    node->end_location = end_location;
    node->number_of_slots = context->grammar->rules[rule].number_of_slots;
    node->slots = calloc(node->number_of_slots,
     sizeof(struct interpret_node *));
    memcpy(node->slots, slots,
     sizeof(struct interpret_node *) * node->number_of_slots);
    return node;
}

static struct interpret_node *finish_token(uint32_t rule,
 struct interpret_node *next_sibling, struct interpret_context *context)
{
    struct interpret_node *token = context->tokens;
    if (!token)
        abort();
    context->tokens = token->next_sibling;
    token->next_sibling = 0;
    return token;
}

static uint32_t rule_lookup(uint32_t parent, uint32_t slot,
 struct interpret_context *context)
{
    return context->grammar->rules[parent].slots[slot].rule_index;
}

static uint32_t root_rule(struct interpret_context *context)
{
    return context->grammar->root_rule;
}

static int fixity_associativity_lookup(uint32_t rule_index, uint32_t choice,
 struct interpret_context *context)
{
    struct rule *rule = &context->grammar->rules[rule_index];
    assert(choice >= rule->number_of_choices);
    struct operator op = rule->operators[choice - rule->number_of_choices];
    switch (op.fixity) {
    case PREFIX:
        return CONSTRUCT_PREFIX;
    case POSTFIX:
        return CONSTRUCT_POSTFIX;
    case INFIX:
        switch (op.associativity) {
        case FLAT:
            return CONSTRUCT_INFIX_FLAT;
        case NONASSOC:
        case LEFT:
            return CONSTRUCT_INFIX_LEFT;
        case RIGHT:
            return CONSTRUCT_INFIX_RIGHT;
        }
    }
}

static int precedence_lookup(uint32_t rule_index, uint32_t choice,
 struct interpret_context *context)
{
    struct rule *rule = &context->grammar->rules[rule_index];
    assert(choice >= rule->number_of_choices);
    return rule->operators[choice - rule->number_of_choices].precedence;
}

static size_t number_of_slots_lookup(uint32_t rule,
 struct interpret_context *context)
{
    return context->grammar->rules[rule].number_of_slots;
}

static void left_right_operand_slots_lookup(uint32_t rule_index, uint32_t *left,
 uint32_t *right, uint32_t *operand, struct interpret_context *context)
{
    struct rule *rule = &context->grammar->rules[rule_index];
    *left = rule->left_slot_index;
    *right = rule->right_slot_index;
    *operand = rule->operand_slot_index;
}
