#include "6b-interpret.h"

#include <stdio.h>

// TODO: Review style for this file.

#define READ_KEYWORD_TOKEN read_keyword_token_bootstrap

#define IDENTIFIER_TOKEN \
 (((struct grammar *)tokenizer->info)->identifier_symbol)
#define NUMBER_TOKEN (((struct grammar *)tokenizer->info)->number_symbol)
#define STRING_TOKEN (((struct grammar *)tokenizer->info)->string_symbol)
#define BRACKET_TRANSITION_TOKEN 0xffffffff

static size_t read_keyword_token_bootstrap(uint32_t *token, bool *end_token,
 const unsigned char *text, size_t length, void *info)
{
    struct grammar *grammar = (struct grammar *)info;
    symbol_id symbol = SYMBOL_EPSILON;
    size_t max_len = 0;
    bool end = false;
    for (uint32_t i = 0; i < grammar->number_of_keywords; ++i) {
        struct keyword k = grammar->keywords[i];
        if (length >= k.length && k.length > max_len &&
         !memcmp(text, k.keyword, k.length)) {
            max_len = k.length;
            symbol = k.symbol;
            end = k.type == KEYWORD_END;
        }
    }
    *end_token = end;
    *token = symbol;
    return max_len;
}

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

static void perform_action(struct interpret_context *ctx, uint16_t action)
{
    if (action == ACTION_BEGIN)
        ctx->indent--;
    for (int i = 0; i < ctx->indent; ++i)
        printf("  ");
    if (action & ACTION_END_RULE) {
        struct rule *r = &ctx->grammar->rules[action & ~ACTION_END_RULE];
        size_t name_length;
        const char *name = bluebird_tree_get_identifier(ctx->tree, r->name, &name_length);
        size_t choice_length = 0;
        const char *choice = "";
        if (r->choice_name) {
            choice = bluebird_tree_get_identifier(ctx->tree, r->choice_name, &choice_length);
        }
        printf("action end rule: %.*s / %.*s\n", (int)name_length, name, (int)choice_length, choice);
        ctx->indent++;
    } else if (action & ACTION_RENAME) {
        printf("action rename: %u\n", action & ~ACTION_RENAME);
    } else {
        printf("action: %u\n", action);
    }
}

static void follow_transition_reversed(struct interpret_context *ctx,
 state_id *last_nfa_state, uint32_t state, uint32_t token)
{
    struct action_map *map = &ctx->deterministic->action_map;
    bool bracket_automaton = false;
    if (state >= (1UL << 31)) {
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
        break;
    }
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state) {
        nfa_state = state_array_pop(&ctx->stack);
    }
    *last_nfa_state = nfa_state;
}

static void build_parse_tree(struct interpret_context *ctx)
{
    state_id nfa_state = ctx->deterministic->final_nfa_state;
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

void interpret(struct grammar *grammar, struct combined_grammar *combined,
 struct bracket_transitions *transitions,
 struct deterministic_grammar *deterministic, struct bluebird_tree *tree,
 const unsigned char *text, size_t text_len)
{
    if (deterministic->automaton.number_of_states > (1UL << 31) ||
     deterministic->bracket_automaton.number_of_states > (1UL << 31)) {
        fprintf(stderr, "error: automaton has too many states.\n");
        exit(-1);
    }
    struct bluebird_token_run *token_run = 0;
    struct bluebird_default_tokenizer tokenizer = {
        .text = text,
        .length = text_len,
        .info = grammar,
    };
    struct interpret_context context = {
        .grammar = grammar,
        .combined = combined,
        .transitions = transitions,
        .deterministic = deterministic,
        .tree = tree,
        .tokenizer = &tokenizer,
        .run = &token_run,

        .state = deterministic->automaton.start_state,
    };
    while (bluebird_default_tokenizer_advance(&tokenizer, &token_run))
        fill_run_states(&context);
    if (tokenizer.offset < tokenizer.length) {
        fprintf(stderr, "error: tokenizing failed.\n");
        exit(-1);
    }
    print_token_runs(&context, token_run);
    build_parse_tree(&context);
}
