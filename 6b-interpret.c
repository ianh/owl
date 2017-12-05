#include "6b-interpret.h"

#include <stdio.h>

// TODO: Review style for this file.

#define READ_KEYWORD_TOKEN read_keyword_token_bootstrap

#define WRITE_NUMBER_TOKEN write_number_token
#define WRITE_IDENTIFIER_TOKEN write_identifier_token
#define WRITE_STRING_TOKEN write_string_token

#define IDENTIFIER_TOKEN \
 (((struct tokenizer_info *)tokenizer->info)->identifier_symbol)
#define NUMBER_TOKEN (((struct tokenizer_info *)tokenizer->info)->number_symbol)
#define STRING_TOKEN (((struct tokenizer_info *)tokenizer->info)->string_symbol)
#define BRACKET_TRANSITION_TOKEN 0xffffffff

static size_t read_keyword_token_bootstrap(uint32_t *token, bool *end_token,
 const unsigned char *text, void *info);
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

#include "tokenize.h"

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

enum parse_tree_node_type {
    NODE_IDENTIFIER,
    NODE_NUMBER,
    NODE_STRING,
    NODE_RULE,
};

struct parse_tree_node {
    struct parse_tree_node *parent;
    struct parse_tree_node *next_sibling;
    size_t start;
    size_t length;
    uint16_t rename;

    enum parse_tree_node_type type;
    union {
        struct {
            rule_id id;
            struct parse_tree_node *first_child;
        } rule;
        struct {
            const unsigned char *name;
            size_t length;
        } identifier;
        double number;
        struct {
            const unsigned char *string;
            size_t length;
        } string;
    };
};

struct operator_precedence_stack {
    struct operator_precedence_stack *parent;

    struct parse_tree_node *first_operator;
    struct parse_tree_node *first_value;
};

struct interpret_context {
    struct grammar *grammar;
    struct combined_grammar *combined;
    struct bracket_transitions *transitions;
    struct deterministic_grammar *deterministic;
    struct bluebird_tree *tree;
    struct bluebird_default_tokenizer *tokenizer;
    struct bluebird_token_run **run;

    struct state_array stack;
    state_id state;

    struct parse_tree_node *root;
    struct parse_tree_node *under_construction;
    struct parse_tree_node *token_nodes;
    struct operator_precedence_stack *op_stack;
    uint16_t next_rename;

    int indent;
};

static void fill_run_states(struct interpret_context *ctx)
{
    struct bluebird_token_run *run = *ctx->run;
    struct state_array *stack = &ctx->stack;
    state_id state = ctx->state;
    struct automaton *a = stack->number_of_states > 0 ?
     &ctx->deterministic->bracket_automaton : &ctx->deterministic->automaton;
    for (uint16_t i = 0; i < run->number_of_tokens; ++i) {
        symbol_id symbol = run->tokens[i];
        run->states[i] = state + (stack->number_of_states > 0 ? 1UL << 31 : 0);
        if (a->states[state].accepting && stack->number_of_states > 0) {
            // We've reached the end token for a guard bracket.
            if (symbol != BRACKET_TRANSITION_TOKEN) {
                fprintf(stderr, "shame: please email ian@ianhenderson.org and "
                 "let me know that \"a BRACKET_TRANSITION_TOKEN was left out "
                 "of the token stream\".\n");
                exit(-1);
            }
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
            fprintf(stderr, "error: unexpected token at %u.\n", i);
            break;
        }
    }
    ctx->state = state;
}

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
                printf("%02x -> %u* (%x)\n", token_run->tokens[i], token_run->states[i] - (1UL << 31), s.transition_symbol);
            else
                printf("%02x -> %u*\n", token_run->tokens[i], token_run->states[i] - (1UL << 31));
        } else
            printf("%02x -> %u\n", token_run->tokens[i], token_run->states[i]);
    }
}

static bool should_reduce(struct interpret_context *ctx, struct rule *rule)
{
    struct parse_tree_node *top = ctx->op_stack->first_operator;
    if (!top)
        return false;
    struct rule *top_rule = &ctx->grammar->rules[top->rule.id];
    return rule->precedence < top_rule->precedence ||
     (rule->precedence == top_rule->precedence && rule->associativity == RIGHT);
}

static void reduce(struct interpret_context *ctx)
{
    struct parse_tree_node *op = ctx->op_stack->first_operator;
    rule_id op_rule_id = op->rule.id;
    struct rule *op_rule = &ctx->grammar->rules[op_rule_id];
    // TODO: Don't indiscriminately overwrite `op->rule.first_child`.
    if (op_rule->fixity == INFIX && op_rule->associativity == FLAT) {
        struct parse_tree_node *first_value = ctx->op_stack->first_value;
        struct parse_tree_node *last_value = first_value;
        struct parse_tree_node *last_op = op;
        while (last_op && last_op->rule.id == op_rule_id) {
            last_op = last_op->next_sibling;
            last_value = last_value->next_sibling;
            // TODO: Combine ops together.
        }
        ctx->op_stack->first_operator = last_op;
        ctx->op_stack->first_value = op;
        op->next_sibling = last_value->next_sibling;
        op->rule.first_child = first_value;
        last_value->next_sibling = 0;
    } else if (op_rule->fixity == INFIX) {
        ctx->op_stack->first_operator = op->next_sibling;
        struct parse_tree_node *left = ctx->op_stack->first_value;
        struct parse_tree_node *right = left->next_sibling;
        op->next_sibling = right->next_sibling;
        ctx->op_stack->first_value = op;

        right->next_sibling = op->rule.first_child;
        op->rule.first_child = left;
        left->next_sibling = right;
    } else {
        ctx->op_stack->first_operator = op->next_sibling;
        struct parse_tree_node *value = ctx->op_stack->first_value;
        op->next_sibling = value->next_sibling;
        ctx->op_stack->first_value = op;

        value->next_sibling = op->rule.first_child;
        op->rule.first_child = value;
    }
}

static void perform_action(struct interpret_context *ctx, uint16_t action)
{
    if (action & ACTION_END_RULE) {
        rule_id id = action & ~ACTION_END_RULE;
        struct rule *rule = &ctx->grammar->rules[id];
        if (rule->type == RULE_WITH_OPERATORS) {
            struct operator_precedence_stack *stack;
            stack = calloc(1, sizeof(struct operator_precedence_stack));
            stack->parent = ctx->op_stack;
            ctx->op_stack = stack;
        }
        struct parse_tree_node *n = calloc(1, sizeof(struct parse_tree_node));
        n->type = NODE_RULE;
        n->rename = ctx->next_rename;
        n->rule.id = id;
        struct rule *parent = 0;
        if (ctx->under_construction) {
            rule_id parent_id = ctx->under_construction->rule.id;
            parent = &ctx->grammar->rules[parent_id];
        }
        if (parent && parent->type == RULE_WITH_OPERATORS) {
            if (rule->type == CHOICE_RULE) {
                n->next_sibling = ctx->op_stack->first_value;
                ctx->op_stack->first_value = n;
            } else if (rule->type == OPERATOR_RULE) {
                while (should_reduce(ctx, rule))
                    reduce(ctx);
                n->next_sibling = ctx->op_stack->first_operator;
                ctx->op_stack->first_operator = n;
                if (rule->fixity == PREFIX)
                    reduce(ctx);
            } else
                abort();
        } else {
            if (ctx->under_construction) {
                n->next_sibling = ctx->under_construction->rule.first_child;
                ctx->under_construction->rule.first_child = n;
            } else
                ctx->root = n;
        }
        n->parent = ctx->under_construction;
        ctx->under_construction = n;
    } else if (action == ACTION_BEGIN) {
        rule_id id = ctx->under_construction->rule.id;
        if (ctx->grammar->rules[id].type == RULE_WITH_OPERATORS) {
            while (ctx->op_stack->first_operator)
                reduce(ctx);
            ctx->under_construction->rule.first_child = ctx->op_stack->first_value;
            struct operator_precedence_stack *stack = ctx->op_stack;
            ctx->op_stack = ctx->op_stack->parent;
            free(stack);
        }
        ctx->under_construction = ctx->under_construction->parent;
    } else if (action & ACTION_RENAME) {
        ctx->next_rename = action & ~ACTION_RENAME;
        rule_id id = ctx->under_construction->rule.id;
        if (ctx->grammar->rules[id].number_of_renames <= ctx->next_rename)
            abort();
    } else if (action == ACTION_WRITE_TOKEN) {
        struct parse_tree_node *n = ctx->token_nodes;
        ctx->token_nodes = n->next_sibling;
        n->next_sibling = 0;
        n->rename = ctx->next_rename;
        if (ctx->under_construction) {
            n->parent = ctx->under_construction;
            n->next_sibling = ctx->under_construction->rule.first_child;
            ctx->under_construction->rule.first_child = n;
        } else
            ctx->root = n;
    } else
        abort();

    if (!(action & ACTION_RENAME))
        ctx->next_rename = 0xffff;

    if (action == ACTION_BEGIN)
        ctx->indent--;
    for (int i = 0; i < ctx->indent; ++i)
        printf("  ");
    if (action & ACTION_END_RULE) {
        struct rule *r = &ctx->grammar->rules[action & ~ACTION_END_RULE];
        printf("action end rule: %.*s / %.*s\n", (int)r->name_length, r->name, (int)r->choice_name_length, r->choice_name);
        ctx->indent++;
    } else if (action & ACTION_RENAME) {
        printf("action rename: %u\n", action & ~ACTION_RENAME);
    } else {
        printf("action: %u\n", action);
    }
}

static void print_parse_tree(struct interpret_context *ctx,
 struct parse_tree_node *node, int indent)
{
    for (int i = 0; i < indent; ++i)
        printf("  ");
    if (node->rename < MAX_NUMBER_OF_RENAMES) {
        struct rule *p = &ctx->grammar->rules[node->parent->rule.id];
        struct rename r = p->renames[node->rename];
        printf("(as ");
        fwrite(r.name, r.name_length, 1, stdout);
        printf(") ");
    }
    switch (node->type) {
    case NODE_IDENTIFIER:
        fwrite(node->identifier.name, node->identifier.length, 1, stdout);
        printf("\n");
        return;
    case NODE_NUMBER:
        printf("%g\n", node->number);
        return;
    case NODE_STRING:
        printf("\"%.*s\"\n", (int)node->string.length, node->string.string);
        return;
    default:
        break;
    }
    struct rule *r = &ctx->grammar->rules[node->rule.id];
    fwrite(r->name, r->name_length, 1, stdout);
    if (r->choice_name) {
        printf(" / ");
        fwrite(r->choice_name, r->choice_name_length, 1, stdout);
    }
    if (node->rule.first_child)
        printf(":\n");
    else
        printf("\n");
    struct parse_tree_node *n;
    for (n = node->rule.first_child; n; n = n->next_sibling)
        print_parse_tree(ctx, n, indent + 1);
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
    // TODO: Binary search.
    state_id nfa_state = *last_nfa_state;
    bool found = false;
    for (uint32_t j = 0; j < map->number_of_entries; ++j) {
        struct action_map_entry entry = map->entries[j];
        if (entry.dfa_state != state)
            continue;
        if (entry.target_nfa_state != nfa_state)
            continue;
        if (entry.dfa_symbol != token)
            continue;
        nfa_state = entry.nfa_state;
        if (bracket_transition) {
            state_array_push(&ctx->stack, nfa_state);
            // TODO: Use a table.
            for (state_id k = 0; k < ctx->combined->bracket_automaton.number_of_states; ++k) {
                if (ctx->combined->bracket_automaton.states[k].transition_symbol != entry.nfa_symbol)
                    continue;
                nfa_state = k;
                break;
            }
        }
        for (uint32_t k = entry.action_index; k < map->number_of_actions; k++) {
            if (map->actions[k] == 0)
                break;
            perform_action(ctx, map->actions[k]);
        }
        found = true;
        break;
    }
    if (!found) {
        fprintf(stderr, "internal error (%u %u %x)\n", state, nfa_state, token);
        exit(-1);
    }
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state) {
        nfa_state = state_array_pop(&ctx->stack);
    }
    *last_nfa_state = nfa_state;
}

static void build_parse_tree(struct interpret_context *ctx)
{
    state_id nfa_state = ctx->combined->final_nfa_state;
    while (*ctx->run) {
        struct bluebird_token_run *run = *ctx->run;
        uint16_t n = run->number_of_tokens;
        for (uint16_t i = n - 1; i < n; i--) {
            follow_transition_reversed(ctx, &nfa_state, run->states[i],
             run->tokens[i]);
        }
        *ctx->run = run->prev;
    }
    follow_transition_reversed(ctx, &nfa_state, UINT32_MAX, UINT32_MAX);
}

static symbol_id find_token(struct grammar *grammar, const char *str)
{
    size_t length = strlen(str);
    for (uint32_t i = 0; i < grammar->number_of_tokens; ++i) {
        struct token other = grammar->tokens[i];
        if (other.keyword)
            continue;
        if (other.length != length)
            continue;
        if (memcmp(other.string, str, length))
            continue;
        return other.symbol;
    }
    return 0xffffffff;
}

static size_t read_keyword_token_bootstrap(uint32_t *token, bool *end_token,
 const unsigned char *text, void *info)
{
    struct grammar *grammar = ((struct tokenizer_info *)info)->context->grammar;
    symbol_id symbol = SYMBOL_EPSILON;
    size_t max_len = 0;
    bool end = false;
    for (uint32_t i = 0; i < grammar->number_of_tokens; ++i) {
        struct token k = grammar->tokens[i];
        if (k.keyword && k.length > max_len &&
         !strncmp((const char *)text, k.string, k.length)) {
            max_len = k.length;
            symbol = k.symbol;
            end = k.type == TOKEN_END;
        }
    }
    *end_token = end;
    *token = symbol;
    return max_len;
}

static void write_identifier_token(size_t offset, size_t length, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct parse_tree_node *n = calloc(1, sizeof(struct parse_tree_node));
    n->next_sibling = ctx->token_nodes;
    n->type = NODE_IDENTIFIER;
    n->identifier.name = ctx->tokenizer->text + offset;
    n->identifier.length = length;
    ctx->token_nodes = n;
}

static void write_string_token(size_t offset, size_t length,
 size_t content_offset, size_t content_length, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct parse_tree_node *n = calloc(1, sizeof(struct parse_tree_node));
    n->next_sibling = ctx->token_nodes;
    n->type = NODE_STRING;
    n->string.string = ctx->tokenizer->text + content_offset;
    n->string.length = content_length;
    ctx->token_nodes = n;
}

static void write_number_token(size_t offset, size_t length, double number,
 void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct parse_tree_node *n = calloc(1, sizeof(struct parse_tree_node));
    n->next_sibling = ctx->token_nodes;
    n->type = NODE_NUMBER;
    n->number = number;
    ctx->token_nodes = n;
}

void interpret(struct grammar *grammar, struct combined_grammar *combined,
 struct bracket_transitions *transitions,
 struct deterministic_grammar *deterministic, const unsigned char *text)
{
    if (deterministic->automaton.number_of_states > (1UL << 31) ||
     deterministic->bracket_automaton.number_of_states > (1UL << 31)) {
        fprintf(stderr, "error: automaton has too many states.\n");
        exit(-1);
    }
    struct bluebird_token_run *token_run = 0;
    struct tokenizer_info info = {
        .identifier_symbol = find_token(grammar, "identifier"),
        .number_symbol = find_token(grammar, "number"),
        .string_symbol = find_token(grammar, "string"),
    };
    struct bluebird_default_tokenizer tokenizer = {
        .text = text,
        .info = &info,
    };
    struct interpret_context context = {
        .grammar = grammar,
        .combined = combined,
        .transitions = transitions,
        .deterministic = deterministic,
        .tokenizer = &tokenizer,
        .run = &token_run,

        .state = deterministic->automaton.start_state,

        .next_rename = 0xffff,
    };
    info.context = &context;
    while (bluebird_default_tokenizer_advance(&tokenizer, &token_run))
        fill_run_states(&context);
    // During tokenization, create tree nodes for all the tokens and link them
    // together.
    // During tree building, every time we see an ACTION_WRITE_TOKEN, pop a
    // token node off and put it into the tree.
    if (text[tokenizer.offset] != '\0') {
        fprintf(stderr, "error: tokenizing failed.\n");
        exit(-1);
    }
    print_token_runs(&context, token_run);
    build_parse_tree(&context);
    if (context.token_nodes)
        abort();
    print_parse_tree(&context, context.root, 0);
}
