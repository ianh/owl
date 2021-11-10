#include "6b-interpret.h"

#include "alloc.h"
#include "construct-actions.h"
#include <assert.h>
#include <stdio.h>

#define READ_WHITESPACE read_whitespace
#define READ_KEYWORD_TOKEN read_keyword_token
#define READ_CUSTOM_TOKEN read_custom_token

#define CUSTOM_TOKEN_DATA(identifier) void *identifier = 0

#define WRITE_NUMBER_TOKEN write_number_token
#define WRITE_IDENTIFIER_TOKEN write_identifier_token
#define WRITE_INTEGER_TOKEN write_integer_token
#define WRITE_STRING_TOKEN write_string_token
#define WRITE_CUSTOM_TOKEN write_custom_token

#define ALLOW_DASHES_IN_IDENTIFIERS(info) \
 (((struct tokenizer_info *)info)->allow_dashes_in_identifiers)

#define ESCAPE_CHAR(c, info) \
 (((struct tokenizer_info *)info)->single_char_escapes ? \
  ESCAPE_CHAR_SINGLE(c, info) : (c))

#define IDENTIFIER_TOKEN \
 (((struct tokenizer_info *)tokenizer->info)->identifier_symbol)
#define INTEGER_TOKEN (((struct tokenizer_info *)tokenizer->info)->integer_symbol)
#define NUMBER_TOKEN (((struct tokenizer_info *)tokenizer->info)->number_symbol)
#define STRING_TOKEN (((struct tokenizer_info *)tokenizer->info)->string_symbol)
#define BRACKET_SYMBOL_TOKEN 0xffffffff
#define COMMENT_TOKEN 0xffffffff

#define IF_NUMBER_TOKEN(cond, ...) \
 if ((((struct tokenizer_info *)tokenizer->info)->number_symbol \
 != SYMBOL_EPSILON) && (cond)) __VA_ARGS__
#define IF_INTEGER_TOKEN(cond, ...) \
 if ((((struct tokenizer_info *)tokenizer->info)->integer_symbol \
 != SYMBOL_EPSILON) && (cond)) __VA_ARGS__
#define IF_STRING_TOKEN(cond, ...) \
 if ((((struct tokenizer_info *)tokenizer->info)->string_symbol \
 != SYMBOL_EPSILON) && (cond)) __VA_ARGS__
#define IF_IDENTIFIER_TOKEN(cond, ...) \
 if ((((struct tokenizer_info *)tokenizer->info)->identifier_symbol \
 != SYMBOL_EPSILON) && (cond)) __VA_ARGS__

static size_t read_whitespace(const char *text, void *info);
static size_t read_keyword_token(uint32_t *token, bool *end_token,
 const char *text, void *info);
static bool read_custom_token(uint32_t *token, size_t *token_length,
 const char *text, bool *whitespace, void **data, void *info);
static void write_identifier_token(size_t offset, size_t length, void *info);
static void write_integer_token(size_t offset, size_t length, uint64_t integer,
 void *info);
static void write_string_token(size_t offset, size_t length,
 const char *string, size_t string_length, bool has_escapes, void *info);
static void write_number_token(size_t offset, size_t length, double number,
 void *info);
static void write_custom_token(size_t offset, size_t length, uint32_t token,
 void *data, void *info);

struct interpret_context;
struct tokenizer_info {
    struct interpret_context *context;
    symbol_id identifier_symbol;
    symbol_id integer_symbol;
    symbol_id number_symbol;
    symbol_id string_symbol;
    bool allow_dashes_in_identifiers;
    bool single_char_escapes;
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

struct saved_state {
    struct bitset bracket_reachability;
    struct automaton *automaton;
    state_id state;
    bool in_bracket;
};

struct interpret_context {
    struct grammar *grammar;
    struct combined_grammar *combined;
    struct deterministic_grammar *deterministic;

    struct saved_state *stack;
    uint32_t stack_allocated_bytes;
    uint32_t stack_depth;

    // This array has an element for every symbol that appears in the automata.
    // If the symbol isn't a bracket transition symbol, the array has UINT32_T
    // in its position.
    uint32_t *bracket_transition_for_symbol;
    size_t bracket_transitions_length;

    struct state_array nfa_stack;

    struct owl_default_tokenizer *tokenizer;
    struct construct_state construct_state;
    struct interpret_node *tokens;

    // Set if we're outputting the result of ambiguity checking (where there are
    // no real tokens).
    bool no_tokens;

    struct document document;

    // Table mapping abstract offsets to concrete offsets.
    size_t *offset_table;
    uint32_t offset_table_allocated_bytes;
    uint32_t next_action_offset;

    // Used to keep nodes in a consistent order.
    size_t next_node_order;
};

enum interpret_node_type {
    NODE_RULE,
    NODE_IDENTIFIER_TOKEN,
    NODE_INTEGER_TOKEN,
    NODE_NUMBER_TOKEN,
    NODE_STRING_TOKEN,
    NODE_CUSTOM_TOKEN,
};

struct interpret_node {
    enum interpret_node_type type;
    struct interpret_node *next_sibling;

    size_t start_location;
    size_t end_location;

    // For rules and custom tokens.
    uint32_t rule_index;
    // For rules.
    uint32_t choice_index;
    bool is_operator;

    size_t number_of_slots;
    struct interpret_node **slots;

    // Used for formatted output:
    // Sorted by start_location.  Only includes children of type NODE_RULE.
    size_t number_of_children;
    struct interpret_node **children;
    // How deep this subtree is (longest path of NODE_RULE children).
    uint32_t depth;
    // Used to establish order for nodes that appear at the same offset.
    size_t order;
    // Which slot was this in the parent rule? (can be null if the slot isn't
    // shown or doesn't exist)
    struct slot *slot;

    // For tokens.
    union {
        struct {
            const char *name;
            size_t length;
        } identifier;
        uint64_t integer;
        double number;
        struct {
            const char *string;
            size_t length;
            bool has_escapes;
        } string;
    };
};

static void fill_run_states(struct interpret_context *ctx,
 struct owl_token_run *run);
static struct interpret_node *build_parse_tree(struct interpret_context *ctx,
 struct owl_token_run *run);
static void destroy_parse_tree(struct interpret_node *tree);

static symbol_id token_symbol(struct combined_grammar *combined,
 struct grammar *grammar, enum rule_token_type type);
static bool follow_transition(struct automaton *a, state_id *state,
 symbol_id symbol);
static void follow_transition_reversed(struct interpret_context *ctx,
 state_id *last_nfa_state, uint32_t state, uint32_t token, size_t start,
 size_t end);

static size_t push_action_offsets(struct interpret_context *ctx, size_t start,
 size_t end);

#if 0
static void print_action(uint16_t action, size_t offset)
{
    uint16_t slot = CONSTRUCT_ACTION_GET_SLOT(action);
    printf("%s %u at %lu\n", action_name(action), slot, offset);
}
#endif

// We use `root_mode` to hide the root node when it's not necessary to
// understand the parse tree.
enum root_mode { PRINT_ROOT_NODE, HIDE_ROOT_NODE };
static void initialize_document(struct interpret_context *ctx,
 struct interpret_node *root, uint32_t number_of_token_labels,
 enum root_mode root_mode);
static void destroy_document(struct document *document);

static uint32_t offset_labels(struct document *document, uint32_t start,
 uint32_t end, uint32_t offset, uint32_t color);
static void fill_rows(struct interpret_context *ctx,
 struct interpret_node *node, uint32_t depth, uint32_t *offset);

static void output_ambiguity_path(struct interpreter *interpreter,
 struct ambiguity *ambiguity, int which_path, struct label *token_labels,
 uint32_t number_of_token_labels, uint32_t *row_count, FILE *output)
{
    struct interpret_context context = {
        .grammar = interpreter->grammar,
        .combined = interpreter->combined,
        .deterministic = interpreter->deterministic,

        .no_tokens = true,
    };
    context.construct_state.info = &context;
    struct ambiguity_path *path = &ambiguity->paths[which_path];

    size_t end_location = 0;
    if (ambiguity->number_of_tokens > 0)
        end_location = ambiguity->number_of_tokens * 4 - 2;
    construct_begin(&context.construct_state, end_location,
     context.combined->root_rule_is_expression ?
     CONSTRUCT_EXPRESSION_ROOT : CONSTRUCT_NORMAL_ROOT);
    uint32_t n = path->number_of_actions;
#if 0
    for (uint32_t i = n - 1; i < n; --i) {
        print_action(path->actions[i], path->offsets[i] * 4);
    }
#endif
    size_t offset = SIZE_MAX;
    for (uint32_t i = n - 1; i < n; --i) {
        size_t next_offset = path->offsets[i] * 4;
        if (CONSTRUCT_IS_END_ACTION(path->actions[i]) && next_offset > 0)
            next_offset -= 2;
        if (next_offset < offset)
            offset = next_offset;
        construct_action_apply(&context.construct_state, path->actions[i],
         offset);
    }
    struct interpret_node *root = construct_finish(&context.construct_state, 0);

    initialize_document(&context, root, number_of_token_labels,
     PRINT_ROOT_NODE);
    memcpy(context.document.rows[0].labels, token_labels,
     number_of_token_labels * sizeof(struct label));

    uint32_t end_offset = 0;
    fill_rows(&context, root, root->depth - 1, &end_offset);
    offset_labels(&context.document, (uint32_t)root->end_location,
     2 * number_of_token_labels, end_offset, 0);
#if 0
    for (uint32_t i = 0; i < context.document.number_of_rows; ++i) {
        printf("row %u:\n", i);
        for (uint32_t j = 0; j < context.document.rows[i].number_of_labels; ++j) {
            struct label l = context.document.rows[i].labels[j];
            printf(" %.*s %u - %u\n", (int)l.length, l.text, l.start, l.end);
        }
    }
#endif
    if (*row_count != UINT32_MAX) {
        context.document.color_offset = (int32_t)*row_count -
         (int32_t)context.document.number_of_rows;
    }
    output_document(output, &context.document, interpreter->terminal_info);
    *row_count = context.document.number_of_rows;
    destroy_document(&context.document);
    destroy_parse_tree(root);
}

void output_ambiguity(struct interpreter *interpreter,
 struct ambiguity *ambiguity, FILE *output)
{
    // Find a suitable whitespace token.
    struct token whitespace_token = {0};
    uint32_t number_of_whitespace_tokens =
     interpreter->grammar->number_of_whitespace_tokens;
    // Look for a ' ' token.
    uint32_t token_index = find_token(interpreter->grammar->whitespace_tokens,
     number_of_whitespace_tokens, " ", 1, TOKEN_DONT_CARE, 0);
    if (token_index < number_of_whitespace_tokens)
        whitespace_token = interpreter->grammar->whitespace_tokens[token_index];
    else if (number_of_whitespace_tokens > 0)
        whitespace_token = interpreter->grammar->whitespace_tokens[0];
    else {
        errorf("no whitespace specified - ambiguities may be inaccurate");
        error.level = WARNING;
        print_error();
        error = (struct error){0};
    }

    // Used to determine whether numbers should be shown as '1.0' or just '1'.
    bool has_integers = token_symbol(interpreter->combined,
     interpreter->grammar, RULE_TOKEN_INTEGER) != SYMBOL_EPSILON;

    struct label *token_labels = calloc(ambiguity->number_of_tokens * 2 + 1,
     sizeof(struct label));
    struct combined_grammar *combined = interpreter->combined;
    uint32_t identifier_iterator = 0;
    uint32_t number_iterator = 0;
    uint32_t string_iterator = 0;
    uint32_t custom_iterator = 0;
    for (uint32_t i = 0; i < ambiguity->number_of_tokens; ++i) {
        token_labels[i * 2] = (struct label){
            .start = i * 4,
            .end = i * 4 + 1,
            .color = DEFAULT_COLOR,
        };
        struct token token = combined->tokens[ambiguity->tokens[i]];
        if (ambiguity->tokens[i] < combined->number_of_keyword_tokens) {
            token_labels[i * 2].text = token.string;
            token_labels[i * 2].length = token.length;
        } else {
            struct rule *rule = interpreter->grammar->rules[token.rule_index];
            char *text = 0;
            size_t length = 0;
            do {
                // This whole business is to avoid adding tokens that will
                // parse as keywords.  We don't want to confuse anyone by
                // producing a token that won't actually parse to the right
                // thing.
                switch (rule->token_type) {
                case RULE_TOKEN_IDENTIFIER: {
                    size_t underscores = identifier_iterator / 26;
                    size_t letter = identifier_iterator % 26;
                    text = realloc(text, underscores + 1);
                    for (size_t j = 0; j < underscores; ++j)
                        text[j] = '_';
                    text[underscores] = 'a' + letter;
                    length = underscores + 1;
                    identifier_iterator++;
                    break;
                }
                case RULE_TOKEN_INTEGER:
                case RULE_TOKEN_NUMBER:
                    if (rule->token_type == RULE_TOKEN_NUMBER && has_integers) {
                        length = snprintf(0, 0, "%u.0", number_iterator + 1);
                        text = realloc(text, length + 1);
                        sprintf(text, "%u.0", number_iterator + 1);
                    } else {
                        length = snprintf(0, 0, "%u", number_iterator + 1);
                        text = realloc(text, length + 1);
                        sprintf(text, "%u", number_iterator + 1);
                    }
                    number_iterator++;
                    break;
                case RULE_TOKEN_STRING: {
                    size_t copies = 1 + (string_iterator / 26);
                    size_t letter = string_iterator % 26;
                    text = realloc(text, copies + 2);
                    for (size_t j = 0; j < copies; ++j)
                        text[j + 1] = 'a' + letter;
                    text[0] = '"';
                    text[copies + 1] = '"';
                    length = copies + 2;
                    string_iterator++;
                    break;
                }
                case RULE_TOKEN_CUSTOM: {
                    const char *original_text = 0;
                    if (rule->number_of_token_exemplars == 0) {
                        original_text = token.string;
                        length = token.length;
                    } else {
                        uint32_t index = custom_iterator %
                         rule->number_of_token_exemplars;
                        struct token *exemplar = &rule->token_exemplars[index];
                        original_text = exemplar->string;
                        length = exemplar->length;
                    }
                    text = malloc(length);
                    memcpy(text, original_text, length);
                    custom_iterator++;
                    break;
                }
                }
            } while (find_token(combined->tokens,
             combined->number_of_keyword_tokens, text, length, TOKEN_DONT_CARE,
             0) < combined->number_of_keyword_tokens);
            token_labels[i * 2].text = text;
            token_labels[i * 2].length = length;
        }
        token_labels[i * 2 + 1] = (struct label){
            .text = whitespace_token.string,
            .length = whitespace_token.length,
            .start = i * 4 + 2,
            .end = i * 4 + 3,
        };
    }
    token_labels[ambiguity->number_of_tokens * 2] = (struct label){
        .text = "",
        .length = 0,
        .start = ambiguity->number_of_tokens * 4,
        .end = ambiguity->number_of_tokens * 4 + 1,
    };

    errorf("this grammar is ambiguous");
    print_error();
    fputs("\n", output);
    struct row token_row = {
        .labels = token_labels,
        .number_of_labels = ambiguity->number_of_tokens * 2 + 1,
    };
    struct document token_document = {
        .rows = &token_row,
        .number_of_rows = 1,
    };
    output_document(output, &token_document, interpreter->terminal_info);
    fputs("\n  can be parsed in two different ways: as\n\n", output);
    uint32_t row_count = UINT32_MAX;
    output_ambiguity_path(interpreter, ambiguity, 0, token_labels,
     token_row.number_of_labels, &row_count, output);
    fputs("\n  or as\n\n", output);
    output_ambiguity_path(interpreter, ambiguity, 1, token_labels,
     token_row.number_of_labels, &row_count, output);
    fputs("\n", output);

    for (uint32_t i = 0; i < ambiguity->number_of_tokens; ++i) {
        if (ambiguity->tokens[i] >= combined->number_of_keyword_tokens)
            free((void *)token_labels[i * 2].text);
    }
    free(token_labels);
}

static void count_row_labels(struct interpret_context *ctx,
 struct interpret_node *node, uint32_t depth)
{
    if (!node || node->type != NODE_RULE)
        return;
    if (depth + 1 < ctx->document.number_of_rows)
        ctx->document.rows[depth + 1].number_of_labels++;
    for (uint32_t i = 0; i < node->number_of_children; ++i)
        count_row_labels(ctx, node->children[i], depth - 1);
}

static void adjust_locations(struct interpret_context *ctx,
 struct interpret_node *node)
{
    uint32_t n = ctx->next_action_offset;
    node->start_location = n - (SIZE_MAX - node->start_location) - 1;
    node->end_location = n - (SIZE_MAX - node->end_location) - 1;
    for (uint32_t i = 0; i < node->number_of_children; ++i)
        adjust_locations(ctx, node->children[i]);
}

static void append_string(char **string, uint32_t *length, uint32_t *bytes,
 const char *append, size_t append_length)
{
    uint32_t index = *length;
    if (append_length > UINT32_MAX ||
     *length + (uint32_t)append_length < *length)
        abort();
    *length += (uint32_t)append_length;
    *string = grow_array(*string, bytes, *length);
    memcpy(*string + index, append, append_length);
}

static uint32_t offset_labels(struct document *document, uint32_t start,
 uint32_t end, uint32_t offset, uint32_t color)
{
    uint32_t n = start;
    while (n < end) {
        struct label *l = &document->rows[0].labels[n / 2];
        l->start = n + offset;
        l->end = n + offset + 1;
        l->color = color;
        n += 2;
    }
    return n;
}

static void fill_rows(struct interpret_context *ctx,
 struct interpret_node *node, uint32_t depth, uint32_t *offset)
{
    if (!node || node->type != NODE_RULE)
        return;
    uint32_t location_cursor = (uint32_t)node->start_location;
    uint32_t start = location_cursor + *offset;
    for (size_t i = 0; i < node->number_of_children; ++i) {
        struct interpret_node *child = node->children[i];
        offset_labels(&ctx->document, location_cursor,
         (uint32_t)child->start_location, *offset, depth + 1);
        if (depth == 0)
            abort();
        fill_rows(ctx, child, depth - 1, offset);
        location_cursor = (uint32_t)child->end_location;
    }
    location_cursor = offset_labels(&ctx->document, location_cursor,
     (uint32_t)node->end_location, *offset, depth + 1);
    if (depth + 1 >= ctx->document.number_of_rows)
        return;
    (*offset)++;
    if (node->start_location == node->end_location) {
        // Avoid empty ranges which confuse the layout algorithm.
        (*offset)++;
    }
    struct slot *s = node->slot;
    struct rule *rule = ctx->grammar->rules[node->rule_index];
    uint32_t j = ctx->document.rows[depth + 1].number_of_labels++;

    char *str = 0;
    uint32_t len = 0;
    uint32_t bytes = 0;
    append_string(&str, &len, &bytes, rule->name, rule->name_length);
    if (rule->number_of_choices) {
        append_string(&str, &len, &bytes, ":", 1);
        struct choice *choice = &rule->choices[node->choice_index];
        append_string(&str, &len, &bytes, choice->name, choice->name_length);
    }
    if (s && (s->name_length != rule->name_length ||
     memcmp(s->name, rule->name, rule->name_length))) {
        append_string(&str, &len, &bytes, "@", 1);
        append_string(&str, &len, &bytes, s->name, s->name_length);
    }
    ctx->document.rows[depth + 1].labels[j] = (struct label){
        .text = str,
        .length = len,
        .start = start,
        .end = location_cursor + *offset - 1,
    };
}

static void initialize_document(struct interpret_context *ctx,
 struct interpret_node *root, uint32_t number_of_token_labels,
 enum root_mode root_mode)
{
    uint32_t number_of_rows = root->depth;
    if (root_mode == PRINT_ROOT_NODE)
        number_of_rows++;
    ctx->document = (struct document){
        .rows = calloc(number_of_rows, sizeof(struct row)),
        .number_of_rows = number_of_rows,
    };
    ctx->document.rows[0].number_of_labels = number_of_token_labels;
    count_row_labels(ctx, root, root->depth - 1);
    for (uint32_t i = 0; i < ctx->document.number_of_rows; ++i) {
        ctx->document.rows[i].labels =
         calloc(ctx->document.rows[i].number_of_labels, sizeof(struct label));
        if (i > 0) {
            // We use `number_of_labels` to keep track of the next label index
            // for non-zero rows.
            ctx->document.rows[i].number_of_labels = 0;
        }
    }
}

static void destroy_document(struct document *document)
{
    for (uint32_t i = 0; i < document->number_of_rows; ++i) {
        struct row row = document->rows[i];
        if (i != 0) {
            for (uint32_t j = 0; j < row.number_of_labels; ++j)
                free((void *)row.labels[j].text);
        }
        free(row.labels);
    }
    free(document->rows);
}

void interpret(struct interpreter *interpreter, const char *text, FILE *output)
{
    struct grammar *grammar = interpreter->grammar;
    struct combined_grammar *combined = interpreter->combined;
    struct deterministic_grammar *deterministic = interpreter->deterministic;

    struct owl_token_run *token_run = 0;
    struct tokenizer_info info = {
        .identifier_symbol = token_symbol(combined, grammar,
         RULE_TOKEN_IDENTIFIER),
        .integer_symbol = token_symbol(combined, grammar, RULE_TOKEN_INTEGER),
        .number_symbol = token_symbol(combined, grammar, RULE_TOKEN_NUMBER),
        .string_symbol = token_symbol(combined, grammar, RULE_TOKEN_STRING),
        .allow_dashes_in_identifiers =
         SHOULD_ALLOW_DASHES_IN_IDENTIFIERS(combined),
        .single_char_escapes = version_capable(interpreter->version,
         SINGLE_CHAR_ESCAPES),
    };
    struct owl_default_tokenizer tokenizer = {
        .text = text,
        .info = &info,
    };
    struct interpret_context context = {
        .grammar = grammar,
        .combined = combined,
        .deterministic = deterministic,
        .tokenizer = &tokenizer,
    };
    info.context = &context;
    context.stack_depth = 1;
    context.stack = grow_array(context.stack, &context.stack_allocated_bytes,
     sizeof(struct saved_state));
    context.stack[0].state = deterministic->automaton.start_state;
    context.stack[0].automaton = &deterministic->automaton;
    while (owl_default_tokenizer_advance(&tokenizer, &token_run))
        fill_run_states(&context, token_run);
    if (text[tokenizer.offset] != '\0') {
        estimate_next_token_range(&tokenizer, &error.ranges[0].start,
         &error.ranges[0].end);
        exit_with_errorf("the text '%.*s' doesn't match any token",
         (int)(error.ranges[0].end - error.ranges[0].start),
         text + error.ranges[0].start);
    }
    if (context.stack_depth != 1 ||
     !deterministic->automaton.states[context.stack[0].state].accepting) {
        find_end_range(&tokenizer, &error.ranges[0].start,
         &error.ranges[0].end);
        exit_with_errorf("expected more text after the last token");
    }
    push_action_offsets(&context, 0, 0);
    struct interpret_node *root = build_parse_tree(&context, token_run);
#if 0
    for (uint32_t i = 0; i < context.next_action_offset; ++i)
        printf("%u. %lu\n", i, context.offset_table[i]);
#endif
    adjust_locations(&context, root);
    enum root_mode root_mode = root->depth > 1 &&
     grammar->rules[grammar->root_rule]->number_of_choices == 0 ?
     HIDE_ROOT_NODE : PRINT_ROOT_NODE;
    uint32_t n = context.next_action_offset;
    initialize_document(&context, root, n / 2, root_mode);
    for (uint32_t i = 0; i < n; ++i) {
        // Reverse all the offsets.  We could change how we access the table
        // instead, but that would make the code harder to read (and it's
        // already bad enough as it is).
        if (n - i - 1 <= i)
            break;
        size_t t = context.offset_table[n - i - 1];
        context.offset_table[n - i - 1] = context.offset_table[i];
        context.offset_table[i] = t;
    }
    for (uint32_t i = 0; i < n / 2; ++i) {
        uint32_t index = i * 2;

        // Find the last newline in this token, so we know where to break lines.
        size_t newline_offset = context.offset_table[index];
        for (size_t j = context.offset_table[index + 1];
         j > context.offset_table[index]; --j) {
            if (context.tokenizer->text[j - 1] == '\n') {
                newline_offset = j;
                break;
            }
        }
        if (i >= n / 2 - 2 && newline_offset != context.offset_table[index]) {
            // Don't print a trailing newline.  The "n / 2 - 2" (instead of
            // "n / 2 - 1") is because there's a "dummy" token at the end of the
            // first row.
            context.document.rows[0].labels[i].start = index;
            context.document.rows[0].labels[i].end = index + 1;
            continue;
        }
        context.document.rows[0].labels[i] = (struct label){
            .text = context.tokenizer->text + newline_offset,
            .length = context.offset_table[index + 1] - newline_offset,
            .start = index,
            .end = index + 1,
            .starts_with_newline =
             newline_offset != context.offset_table[index],
        };
    }
    uint32_t offset = 0;
    fill_rows(&context, root, root->depth - 1, &offset);
    uint32_t location_cursor = (uint32_t)root->end_location;
    offset_labels(&context.document, location_cursor, n, offset, 0);
#if 0
    for (uint32_t i = 0; i < context.document.number_of_rows; ++i) {
        printf("row %u:\n", i);
        for (uint32_t j = 0; j < context.document.rows[i].number_of_labels; ++j) {
            struct label l = context.document.rows[i].labels[j];
            printf(" %.*s %u - %u\n", (int)l.length, l.text, l.start, l.end);
        }
    }
#endif
    output_document(output, &context.document, interpreter->terminal_info);
    destroy_document(&context.document);
    destroy_parse_tree(root);
    free(context.stack);
    free(context.offset_table);
    free(context.bracket_transition_for_symbol);
}

static bool valid_state(struct interpret_context *ctx, struct saved_state *s,
 state_id state)
{
    if (!s->in_bracket)
        return true;
    return bitset_intersects(&ctx->deterministic->bracket_reachability[state],
     &s->bracket_reachability);
}

static void fill_bracket_transitions_for_symbols(struct interpret_context *ctx)
{
    if (ctx->bracket_transition_for_symbol)
        return;
    struct deterministic_grammar *d = ctx->deterministic;
    size_t len = ctx->combined->number_of_tokens;
    for (uint32_t i = 0; i < d->transitions.number_of_transitions; ++i) {
        symbol_id symbol =
         d->transitions.transitions[i].deterministic_transition_symbol;
        if (symbol + 1 > len)
            len = symbol + 1;
    }
    ctx->bracket_transition_for_symbol = malloc(len * sizeof(uint32_t));
    ctx->bracket_transitions_length = len;
    memset(ctx->bracket_transition_for_symbol, 0xff, len * sizeof(uint32_t));
    for (uint32_t i = 0; i < d->transitions.number_of_transitions; ++i) {
        symbol_id symbol =
         d->transitions.transitions[i].deterministic_transition_symbol;
        ctx->bracket_transition_for_symbol[symbol] = i;
    }
}

static void fill_run_states(struct interpret_context *ctx,
 struct owl_token_run *run)
{
    fill_bracket_transitions_for_symbols(ctx);
    struct saved_state *top = &ctx->stack[ctx->stack_depth - 1];
    for (uint16_t i = 0; i < run->number_of_tokens; ++i) {
        symbol_id symbol = run->tokens[i];
        run->states[i] = top->state + (top->in_bracket ? 1UL << 31 : 0);
        struct state s = top->automaton->states[top->state];
        if (s.accepting && top->in_bracket) {
            // We've reached the end token for a guard bracket.
            assert(symbol == BRACKET_SYMBOL_TOKEN);
            symbol = s.transition_symbol;
            run->tokens[i] = symbol;
            bitset_destroy(&top->bracket_reachability);
            ctx->stack_depth--;
            if (ctx->stack_depth < 1)
                abort();
            top = &ctx->stack[ctx->stack_depth - 1];
        } else if (follow_transition(top->automaton, &top->state, symbol)) {
            // This is just a normal token.
            if (!valid_state(ctx, top, top->state))
                goto unexpected_token;
            continue;
        } else {
            // Maybe this is the start token for a guard bracket.
            struct state s = top->automaton->states[top->state];
            struct bitset reachability = bitset_create_empty(
             ctx->deterministic->transitions.number_of_transitions);
            for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                if (!valid_state(ctx, top, s.transitions[j].target))
                    continue;
                symbol_id symbol = s.transitions[j].symbol;
                uint32_t k = UINT32_MAX;
                if (symbol < ctx->bracket_transitions_length)
                    k = ctx->bracket_transition_for_symbol[symbol];
                if (k != UINT32_MAX)
                    bitset_add(&reachability, k);
            }
            if (ctx->stack_depth == UINT32_MAX)
                abort();
            ctx->stack = grow_array(ctx->stack, &ctx->stack_allocated_bytes,
             sizeof(struct saved_state) * ++ctx->stack_depth);
            top = &ctx->stack[ctx->stack_depth - 1];
            top->in_bracket = true;
            top->automaton = &ctx->deterministic->bracket_automaton;
            top->state = top->automaton->start_state;
            top->bracket_reachability = bitset_move(&reachability);
        }
        run->states[i] = top->state + (top->in_bracket ? 1UL << 31 : 0);
        if (follow_transition(top->automaton, &top->state, symbol) &&
         valid_state(ctx, top, top->state))
            continue;
unexpected_token:
        {
            find_token_range(ctx->tokenizer, run, i, &error.ranges[0].start,
             &error.ranges[0].end);
            for (uint32_t j = 0; j < ctx->combined->number_of_tokens; ++j) {
                struct token token = ctx->combined->tokens[j];
                if (token.symbol != symbol)
                    continue;
                if (j < ctx->combined->number_of_keyword_tokens) {
                    exit_with_errorf("unexpected token '%.*s'",
                     (int)token.length, token.string);
                } else {
                    exit_with_errorf("unexpected %.*s", (int)token.length,
                     token.string);
                }
            }
            abort();
        }
    }
}

static struct interpret_node *build_parse_tree(struct interpret_context *ctx,
 struct owl_token_run *run)
{
    fill_bracket_transitions_for_symbols(ctx);
    ctx->construct_state.info = ctx;
    construct_begin(&ctx->construct_state, SIZE_MAX,
     ctx->combined->root_rule_is_expression ?
     CONSTRUCT_EXPRESSION_ROOT : CONSTRUCT_NORMAL_ROOT);
    state_id nfa_state = ctx->combined->final_nfa_state;
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
            if (len > 0)
                push_action_offsets(ctx, end, end - len);
            whitespace = end - offset - len;
        }
        struct owl_token_run *old = run;
        run = run->prev;
        free(old);
    }
    state_array_destroy(&ctx->nfa_stack);
    follow_transition_reversed(ctx, &nfa_state, UINT32_MAX, UINT32_MAX,
     offset, offset + whitespace);
    return construct_finish(&ctx->construct_state,
     SIZE_MAX - ctx->next_action_offset + 1);
}

static symbol_id token_symbol(struct combined_grammar *combined,
 struct grammar *grammar, enum rule_token_type type)
{
    for (uint32_t i = combined->number_of_keyword_tokens;
     i < combined->number_of_tokens; ++i) {
        struct rule *rule = grammar->rules[combined->tokens[i].rule_index];
        if (rule->token_type == type)
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
    bool bracket_transition = false;
    if (token != UINT32_MAX) {
        bracket_transition =
         ctx->bracket_transition_for_symbol[token] != UINT32_MAX;
    }
    struct action_map_entry *entry;
    state_id nfa_state = *last_nfa_state;
    entry = action_map_find(map, nfa_state, state, token);
    if (!entry) {
        fprintf(stderr, "internal error (%u %u %x)\n", state, nfa_state, token);
        abort();
    }
    nfa_state = entry->nfa_state;
    if (bracket_transition) {
        state_array_push(&ctx->nfa_stack, nfa_state);
        // FIXME: Use a table instead of doing this linear search.
        for (state_id i = 0; i <
         ctx->combined->bracket_automaton.number_of_states; ++i) {
            struct state s = ctx->combined->bracket_automaton.states[i];
            if (!s.accepting || s.transition_symbol != entry->nfa_symbol)
                continue;
            nfa_state = i;
            break;
        }
    }
    size_t offset = end;
    for (uint16_t *a = entry->actions; a && *a; a++) {
        if (CONSTRUCT_IS_END_ACTION(*a) && offset != start)
            offset = push_action_offsets(ctx, offset, start);
        uint32_t action_offset = ctx->next_action_offset - 1;
#if 0
        printf("action: %u\n", offset);
        print_action(*a, action_offset);
#endif
        construct_action_apply(&ctx->construct_state, *a,
         SIZE_MAX - action_offset);
    }
    if (offset != start)
        push_action_offsets(ctx, offset, start);
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state)
        nfa_state = state_array_pop(&ctx->nfa_stack);
    *last_nfa_state = nfa_state;
}

static void push_action_offset(struct interpret_context *ctx, size_t offset)
{
    if (ctx->next_action_offset == UINT32_MAX)
        abort();
    uint32_t action_offset = ctx->next_action_offset++;
    ctx->offset_table = grow_array(ctx->offset_table,
     &ctx->offset_table_allocated_bytes,
     ctx->next_action_offset * sizeof(size_t));
    ctx->offset_table[action_offset] = offset;
}

static size_t push_action_offsets(struct interpret_context *ctx, size_t start,
 size_t end)
{
    push_action_offset(ctx, start);
    push_action_offset(ctx, end);
    return end;
}

static void destroy_parse_tree(struct interpret_node *tree)
{
    while (tree) {
        struct interpret_node *next = tree->next_sibling;
        for (size_t i = 0; i < tree->number_of_slots; ++i)
            destroy_parse_tree(tree->slots[i]);
        if (tree->type == NODE_STRING_TOKEN && tree->string.has_escapes)
            free((void *)tree->string.string);
        free(tree->slots);
        free(tree->children);
        free(tree);
        tree = next;
    }
}

static size_t read_whitespace(const char *text, void *info)
{
    struct grammar *grammar = ((struct tokenizer_info *)info)->context->grammar;
    size_t max_len = 0;
    for (uint32_t i = 0; i < grammar->number_of_whitespace_tokens; ++i) {
        struct token token = grammar->whitespace_tokens[i];
        if (token.length > max_len && !strncmp((const char *)text, token.string,
         token.length))
            max_len = token.length;
    }
    return max_len;
}

static size_t read_keyword_token(uint32_t *token, bool *end_token,
 const char *text, void *info)
{
    struct grammar *grammar = ((struct tokenizer_info *)info)->context->grammar;
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
    for (uint32_t i = 0; i < grammar->number_of_comment_tokens; ++i) {
        struct token token = grammar->comment_tokens[i];
        if (token.length > max_len && !strncmp((const char *)text, token.string,
         token.length)) {
            max_len = token.length;
            symbol = COMMENT_TOKEN;
            end = false;
        }
    }
    *end_token = end;
    *token = symbol;
    return max_len;
}

static bool read_custom_token(uint32_t *token, size_t *token_length,
 const char *text, bool *whitespace, void **data, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct combined_grammar *combined = ctx->combined;
    bool matched = false;
    for (uint32_t i = combined->number_of_keyword_tokens;
     i < combined->number_of_tokens; ++i) {
        struct rule *r = ctx->grammar->rules[combined->tokens[i].rule_index];
        if (r->token_type != RULE_TOKEN_CUSTOM)
            continue;
        if (r->number_of_token_exemplars > 0) {
            for (uint32_t j = 0; j < r->number_of_token_exemplars; ++j) {
                struct token e = r->token_exemplars[j];
                if (e.length > *token_length &&
                 !strncmp(text, e.string, e.length)) {
                    *token_length = e.length;
                    *token = combined->tokens[i].symbol;
                    matched = true;
                }
            }
        } else if (r->name_length > *token_length &&
         !strncmp(text, r->name, r->name_length)) {
            *token_length = r->name_length;
            *token = combined->tokens[i].symbol;
            matched = true;
        }
    }
    *whitespace = false;
    return matched;
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

static void write_integer_token(size_t offset, size_t length, uint64_t integer,
 void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_INTEGER_TOKEN;
    node->integer = integer;
    ctx->tokens = node;
}

static void write_string_token(size_t offset, size_t length,
 const char *string, size_t string_length, bool has_escapes, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_STRING_TOKEN;
    node->string.string = string;
    node->string.length = string_length;
    node->string.has_escapes = has_escapes;
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

static void write_custom_token(size_t offset, size_t length, uint32_t token,
 void *data, void *info)
{
    struct interpret_context *ctx = ((struct tokenizer_info *)info)->context;
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->next_sibling = ctx->tokens;
    node->type = NODE_CUSTOM_TOKEN;
    node->rule_index = ctx->combined->tokens[token].rule_index;
    ctx->tokens = node;
}

static int compare_locations(const void *a, const void *b)
{
    struct interpret_node *na = *(struct interpret_node **)a;
    struct interpret_node *nb = *(struct interpret_node **)b;
    if (na->start_location < nb->start_location)
        return -1;
    if (na->start_location > nb->start_location)
        return 1;
    if (na->end_location < nb->end_location)
        return -1;
    if (na->end_location > nb->end_location)
        return 1;
    if (na->order > nb->order)
        return -1;
    if (na->order < nb->order)
        return 1;
    return 0;
}

static struct interpret_node *finish_node(uint32_t rule, uint32_t choice,
 struct interpret_node *next_sibling, struct interpret_node **slots,
 size_t start_location, size_t end_location, struct interpret_context *context)
{
    struct interpret_node *node = calloc(1, sizeof(struct interpret_node));
    node->type = NODE_RULE;
    node->rule_index = rule;
    node->choice_index = choice;
    node->next_sibling = next_sibling;
    node->start_location = start_location;
    node->end_location = end_location;
    node->order = context->next_node_order++;
    node->number_of_slots = context->grammar->rules[rule]->number_of_slots;
    node->slots = calloc(node->number_of_slots,
     sizeof(struct interpret_node *));
    memcpy(node->slots, slots,
     sizeof(struct interpret_node *) * node->number_of_slots);
    // Compute the depth and children for the output functions to use.
    uint32_t max_depth = 0;
    for (size_t i = 0; i < node->number_of_slots; ++i) {
        struct interpret_node *slot = node->slots[i];
        for (; slot; slot = slot->next_sibling) {
            if (slot->type != NODE_RULE)
                continue;
            struct rule *r = context->grammar->rules[rule];
            if (i != r->operand_slot_index && i != r->left_slot_index &&
             i != r->right_slot_index)
                slot->slot = &r->slots[i];
            node->number_of_children++;
            if (slot->depth > max_depth)
                max_depth = slot->depth;
        }
    }
    node->depth = max_depth + 1;
    node->children = calloc(node->number_of_children,
     sizeof(struct interpret_node *));
    size_t index = 0;
    for (size_t i = 0; i < node->number_of_slots; ++i) {
        struct interpret_node *slot = node->slots[i];
        for (; slot; slot = slot->next_sibling) {
            if (slot->type != NODE_RULE)
                continue;
            node->children[index] = slot;
            index++;
        }
    }
    qsort(node->children, node->number_of_children,
     sizeof(struct interpret_node *), compare_locations);
    return node;
}

static struct interpret_node *finish_token(uint32_t rule,
 struct interpret_node *next_sibling, struct interpret_context *context)
{
    if (context->no_tokens)
        return next_sibling;
    struct interpret_node *token = context->tokens;
    if (!token)
        abort();
    context->tokens = token->next_sibling;
    token->next_sibling = next_sibling;
    return token;
}

static uint32_t rule_lookup(uint32_t parent, uint32_t slot,
 struct interpret_context *context)
{
    if (slot >= context->grammar->rules[parent]->number_of_slots)
        abort();
    return context->grammar->rules[parent]->slots[slot].rule_index;
}

static uint32_t root_rule(struct interpret_context *context)
{
    return context->grammar->root_rule;
}

static int fixity_associativity_lookup(uint32_t rule_index, uint32_t choice,
 struct interpret_context *context)
{
    struct rule *rule = context->grammar->rules[rule_index];
    assert(choice >= rule->first_operator_choice);
    struct choice op = rule->choices[choice];
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
    abort();
}

static int precedence_lookup(uint32_t rule_index, uint32_t choice,
 struct interpret_context *context)
{
    struct rule *rule = context->grammar->rules[rule_index];
    assert(choice >= rule->first_operator_choice);
    return rule->choices[choice].precedence;
}

static size_t number_of_slots_lookup(uint32_t rule,
 struct interpret_context *context)
{
    return context->grammar->rules[rule]->number_of_slots;
}

static void left_right_operand_slots_lookup(uint32_t rule_index, uint32_t *left,
 uint32_t *right, uint32_t *operand, struct interpret_context *context)
{
    struct rule *rule = context->grammar->rules[rule_index];
    *left = rule->left_slot_index;
    *right = rule->right_slot_index;
    *operand = rule->operand_slot_index;
}
