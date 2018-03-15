#include "6b-interpret.h"

#include "x-construct-actions.h"
#include <assert.h>
#include <stdio.h>

#define READ_KEYWORD_TOKEN read_keyword_token

#define WRITE_NUMBER_TOKEN write_number_token
#define WRITE_IDENTIFIER_TOKEN write_identifier_token
#define WRITE_STRING_TOKEN write_string_token

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
};

struct interpret_node;
#define FINISHED_NODE_T struct interpret_node *
#define FINISH_NODE finish_node
#define FINISH_TOKEN finish_token

static struct interpret_node *finish_node(uint32_t rule, uint32_t choice,
 struct interpret_node *next_sibling, struct interpret_node **slots,
 struct interpret_context *context);
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
 state_id *last_nfa_state, uint32_t state, uint32_t token);

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
        // TODO: Indicate more explicitly that this is an operator?
        if (node->choice_index > rule->number_of_choices) {
            struct operator *op = &rule->operators[node->choice_index -
             rule->number_of_choices];
            printf(" : %.*s", (int)op->name_length, op->name);
        } else {
            struct choice *choice = &rule->choices[node->choice_index];
            printf(" : %.*s", (int)choice->name_length, choice->name);
        }
    }
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
    while (run) {
        uint16_t n = run->number_of_tokens;
        for (uint16_t i = n - 1; i < n; i--) {
            follow_transition_reversed(ctx, &nfa_state, run->states[i],
             run->tokens[i]);
        }
        run = run->prev;
    }
    follow_transition_reversed(ctx, &nfa_state, UINT32_MAX, UINT32_MAX);
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
static void print_action(uint16_t action)
{
    uint16_t slot = CONSTRUCT_ACTION_GET_SLOT(action);
    switch (CONSTRUCT_ACTION_GET_TYPE(action)) {
#define CONSTRUCT_ACTION_NAME(name) case PRINT_CONSTRUCT_ACTION_ ## name : printf(#name " %u\n", slot); break;
CONSTRUCT_ACTIONS
#undef CONSTRUCT_ACTION_NAME
    }
}

static void follow_transition_reversed(struct interpret_context *ctx,
 state_id *last_nfa_state, uint32_t state, uint32_t token)
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
    for (uint32_t k = entry->action_index; k < map->number_of_actions; k++) {
        if (map->actions[k] == 0)
            break;
        print_action(map->actions[k]);
        construct_action_apply(&ctx->construct_state, map->actions[k]);
    }
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state) {
        nfa_state = state_array_pop(&ctx->stack);
    }
    *last_nfa_state = nfa_state;
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
 struct interpret_context *context)
{
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->rule_index = rule;
    node->choice_index = choice;
    node->next_sibling = next_sibling;
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
