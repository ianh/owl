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

    struct state_array nfa_stack;

    struct bluebird_default_tokenizer *tokenizer;
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

    // Used for fancy tree output:
    // Sorted by start_location.  Only includes children of type NODE_RULE.
    size_t number_of_children;
    struct interpret_node **children;
    // How deep this subtree is (longest path of NODE_RULE children).
    uint32_t depth;
    // Which slot was this in the parent rule? (can be null if the slot isn't
    // shown or doesn't exist)
    struct slot *slot;

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

static size_t push_action_offsets(struct interpret_context *ctx, size_t start,
 size_t end);
static bool is_end_action(uint16_t action);

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
    printf("  %u - %u", SIZE_MAX - node->start_location, SIZE_MAX - node->end_location);
    printf("\n");
    for (uint32_t i = 0; i < rule->number_of_slots; ++i) {
        struct slot *slot = &rule->slots[i];
        if (!node->slots[i])
            continue;
        print_parse_tree(ctx, node->slots[i], slot, indent + 1);
    }
    print_parse_tree(ctx, node->next_sibling, slot, indent);

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
#endif

static void initialize_document(struct interpret_context *ctx,
 struct interpret_node *root, uint32_t number_of_token_labels);

static uint32_t offset_labels(struct document *document, uint32_t start,
 uint32_t end, uint32_t offset, uint32_t color);
static void fill_rows(struct interpret_context *ctx,
 struct interpret_node *node, uint32_t depth, uint32_t *offset);

static void output_ambiguity_path(struct interpreter *interpreter,
 struct ambiguity *ambiguity, int which_path, struct label *token_labels,
 uint32_t *row_count, FILE *output)
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
        if (is_end_action(path->actions[i]) && next_offset > 0)
            next_offset -= 2;
        if (next_offset < offset)
            offset = next_offset;
        construct_action_apply(&context.construct_state, path->actions[i],
         offset);
    }
    struct interpret_node *root = construct_finish(&context.construct_state, 0);

    initialize_document(&context, root, ambiguity->number_of_tokens * 2);
    memcpy(context.document.rows[0].labels, token_labels,
     ambiguity->number_of_tokens * 2 * sizeof(struct label));

    uint32_t end_offset = 0;
    fill_rows(&context, root, root->depth - 1, &end_offset);
    offset_labels(&context.document, (uint32_t)root->end_location,
     ambiguity->number_of_tokens * 4, end_offset, 0);
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
}

void output_ambiguity(struct interpreter *interpreter,
 struct ambiguity *ambiguity, FILE *output)
{
    struct label *token_labels = calloc(ambiguity->number_of_tokens * 2,
     sizeof(struct label));
    struct combined_grammar *combined = interpreter->combined;
    uint32_t identifier_iterator = 0;
    uint32_t number_iterator = 0;
    uint32_t string_iterator = 0;
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
            bool is_identifier = token.length == strlen("identifier") &&
             !memcmp(token.string, "identifier", strlen("identifier"));
            bool is_number = token.length == strlen("number") &&
             !memcmp(token.string, "number", strlen("number"));
            bool is_string = token.length == strlen("string") &&
             !memcmp(token.string, "string", strlen("string"));
            char *text = 0;
            size_t length = 0;
            do {
                // This whole business is to avoid adding tokens that will
                // parse as keywords.  We don't want to confuse anyone by
                // producing a string that won't actually parse to the right
                // thing.
                if (is_identifier) {
                    size_t underscores = identifier_iterator / 26;
                    size_t letter = identifier_iterator % 26;
                    text = realloc(text, underscores + 1);
                    if (!text)
                        abort();
                    for (size_t j = 0; j < underscores; ++j)
                        text[j] = '_';
                    text[underscores] = 'a' + letter;
                    length = underscores + 1;
                    identifier_iterator++;
                } else if (is_number) {
                    length = snprintf(0, 0, "%u", number_iterator + 1);
                    text = realloc(text, length + 1);
                    if (!text)
                        abort();
                    sprintf(text, "%u", number_iterator + 1);
                    number_iterator++;
                } else if (is_string) {
                    size_t copies = 1 + (string_iterator / 26);
                    size_t letter = string_iterator % 26;
                    text = realloc(text, copies + 2);
                    if (!text)
                        abort();
                    for (size_t j = 0; j < copies; ++j)
                        text[j + 1] = 'a' + letter;
                    text[0] = '"';
                    text[copies + 1] = '"';
                    length = copies + 2;
                    string_iterator++;
                }
            } while (find_token(combined->tokens, combined->number_of_tokens,
             text, length, TOKEN_DONT_CARE, 0) < combined->number_of_tokens);
            token_labels[i * 2].text = text;
            token_labels[i * 2].length = length;
        }
        token_labels[i * 2 + 1] = (struct label){
            .text = " ",
            .length = 1,
            .start = i * 4 + 2,
            .end = i * 4 + 3,
        };
    }

    // TODO: Adjust colors to help maintain association between output rows.
    errorf("this grammar is ambiguous");
    print_error();
    fputs("\n", output);
    struct row token_row = {
        .labels = token_labels,
        .number_of_labels = ambiguity->number_of_tokens * 2,
    };
    struct document token_document = {
        .rows = &token_row,
        .number_of_rows = 1,
    };
    output_document(output, &token_document, interpreter->terminal_info);
    fputs("\n  can be parsed in two different ways: as\n\n", output);
    uint32_t row_count = UINT32_MAX;
    output_ambiguity_path(interpreter, ambiguity, 0, token_labels, &row_count,
     output);
    fputs("\n  or as\n\n", output);
    output_ambiguity_path(interpreter, ambiguity, 1, token_labels, &row_count,
     output);
    fputs("\n", output);
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
        location_cursor = offset_labels(&ctx->document, location_cursor,
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
    struct rule *rule = &ctx->grammar->rules[node->rule_index];
    uint32_t j = ctx->document.rows[depth + 1].number_of_labels++;

    char *str = 0;
    uint32_t len = 0;
    uint32_t bytes = 0;
    append_string(&str, &len, &bytes, rule->name, rule->name_length);
    if (rule->number_of_choices) {
        append_string(&str, &len, &bytes, ":", 1);
        if (node->choice_index >= rule->number_of_choices) {
            struct operator *op = &rule->operators[node->choice_index -
             rule->number_of_choices];
            append_string(&str, &len, &bytes, op->name,
             op->name_length);
        } else {
            struct choice *choice = &rule->choices[node->choice_index];
            append_string(&str, &len, &bytes, choice->name,
             choice->name_length);
        }
    }
    if (s && (s->name_length != rule->name_length ||
     memcmp(s->name, rule->name, rule->name_length))) {
        append_string(&str, &len, &bytes, "@", 1);
        append_string(&str, &len, &bytes, s->name, s->name_length);
    }
    ctx->document.rows[depth + 1].labels[j] = (struct label){
        .text = str, // TODO: No leak!
        .length = len,
        .start = start,
        .end = location_cursor + *offset - 1,
    };
}

static void initialize_document(struct interpret_context *ctx,
 struct interpret_node *root, uint32_t number_of_token_labels)
{
    uint32_t number_of_rows = root->depth;
//    if (root->depth == 1 ||
//     ctx->grammar->rules[ctx->grammar->root_rule].number_of_choices > 0) {
        // If the root rule is the only rule that matched, or if it has choices,
        // then show the root node in the tree.  Otherwise, we hide the root to
        // avoid an extra nesting level across the entire output.
        // TODO: replace this rule with one based on the root rule being covered
        // by its child rules.
        number_of_rows++;
//    }
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

void interpret(struct interpreter *interpreter, const char *text, FILE *output)
{
    struct grammar *grammar = interpreter->grammar;
    struct combined_grammar *combined = interpreter->combined;
    struct deterministic_grammar *deterministic = interpreter->deterministic;

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
        .tokenizer = &tokenizer,
    };
    info.context = &context;
    context.stack_depth = 1;
    context.stack = grow_array(context.stack, &context.stack_allocated_bytes,
     sizeof(struct saved_state));
    context.stack[0].state = deterministic->automaton.start_state;
    context.stack[0].automaton = &deterministic->automaton;
    while (bluebird_default_tokenizer_advance(&tokenizer, &token_run))
        fill_run_states(&context, token_run);
    if (text[tokenizer.offset] != '\0') {
        // TODO: Better error message
        fprintf(stderr, "error: tokenizing failed.\n");
        exit(-1);
    }
    if (context.stack_depth != 1 ||
     !deterministic->automaton.states[context.stack[0].state].accepting) {
        // TODO: Better error message
        fprintf(stderr, "error: unexpected end of file.\n");
        exit(-1);
    }
//    print_token_runs(&context, token_run);
    push_action_offsets(&context, 0, 0);
    struct interpret_node *root = build_parse_tree(&context, token_run);
    // TODO: Error handling.
//    print_parse_tree(&context, root, 0, 0);
#if 0
    for (uint32_t i = 0; i < context.next_action_offset; ++i)
        printf("%u. %lu\n", i, context.offset_table[i]);
#endif
    adjust_locations(&context, root);
    uint32_t n = context.next_action_offset;
    initialize_document(&context, root, n / 2);
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
}

// on enter bracket:
// - find all valid successors that have bracket transitions
// - fill in a bitset based on those transitions
// - push this bitset onto a stack
// on leave bracket:
// - pop the stack
// on normal transition:
// - check that the successor state is valid

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
    size_t len = d->automaton.number_of_symbols * sizeof(uint32_t);
    ctx->bracket_transition_for_symbol = malloc(len);
    memset(ctx->bracket_transition_for_symbol, 0xff, len);
    for (uint32_t i = 0; i < d->transitions.number_of_transitions; ++i) {
        symbol_id symbol =
         d->transitions.transitions[i].deterministic_transition_symbol;
        ctx->bracket_transition_for_symbol[symbol] = i;
    }
}

static void fill_run_states(struct interpret_context *ctx,
 struct bluebird_token_run *run)
{
    fill_bracket_transitions_for_symbols(ctx);
    struct saved_state *top = &ctx->stack[ctx->stack_depth - 1];
    for (uint16_t i = 0; i < run->number_of_tokens; ++i) {
        symbol_id symbol = run->tokens[i];
        run->states[i] = top->state + (top->in_bracket ? 1UL << 31 : 0);
        struct state s = top->automaton->states[top->state];
        if (s.accepting && top->in_bracket) {
            // We've reached the end token for a guard bracket.
            assert(symbol == BRACKET_TRANSITION_TOKEN);
            symbol = s.transition_symbol;
            run->tokens[i] = symbol;
            ctx->stack_depth--;
            if (ctx->stack_depth < 1) {
                fprintf(stderr, "internal error (underflow)\n");
                abort();
            }
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
                uint32_t k = ctx->bracket_transition_for_symbol[symbol];
                if (k != UINT32_MAX)
                    bitset_add(&reachability, k);
            }
            // TODO: if nothing is reachable, give up?
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
            size_t offset = ctx->tokenizer->offset - ctx->tokenizer->whitespace;
            size_t last_offset = offset;
            size_t len = 0;
            uint16_t length_offset = run->lengths_size - 1;
            for (uint32_t j = i; j < run->number_of_tokens; ++j) {
                if (run->tokens[j] >= ctx->combined->number_of_tokens)
                    continue;
                last_offset = offset;
                len = decode_token_length(run, &length_offset, &offset);
            }
            error.ranges[0].start = last_offset - len;
            error.ranges[0].end = last_offset;
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
 struct bluebird_token_run *run)
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
        run = run->prev;
    }
    follow_transition_reversed(ctx, &nfa_state, UINT32_MAX, UINT32_MAX,
     offset, offset + whitespace);
    return construct_finish(&ctx->construct_state,
     SIZE_MAX - ctx->next_action_offset + 1);
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
        // TODO: Use a table.
        for (state_id i = 0; i <
         ctx->combined->bracket_automaton.number_of_states; ++i) {
            if (ctx->combined->bracket_automaton.states[i].transition_symbol
             != entry->nfa_symbol)
                continue;
            nfa_state = i;
            break;
        }
    }
    size_t offset = end;
    for (uint32_t i = entry->action_index; i < map->number_of_actions; i++) {
        if (map->actions[i] == 0)
            break;
        uint32_t action_offset = ctx->next_action_offset;
        if (is_end_action(map->actions[i]) && offset != start)
            offset = push_action_offsets(ctx, offset, start);
        action_offset = ctx->next_action_offset - 1;
        // TODO: make sure the start of two different things at the same level
        // can't be the same (to avoid problems with sorting/stability/etc)
#if 0
        printf("action: %u\n", offset);
        print_action(map->actions[k], action_offset);
#endif
        construct_action_apply(&ctx->construct_state, map->actions[i],
         SIZE_MAX - action_offset);
    }
    if (offset != start)
        offset = push_action_offsets(ctx, offset, start);
    if (bracket_automaton && state ==
     ctx->deterministic->bracket_automaton.start_state)
        nfa_state = state_array_pop(&ctx->nfa_stack);
    *last_nfa_state = nfa_state;
}

static void push_action_offset(struct interpret_context *ctx, size_t offset)
{
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

static bool is_end_action(uint16_t action)
{
    switch (CONSTRUCT_ACTION_GET_TYPE(action)) {
    case CONSTRUCT_ACTION_END_SLOT:
    case CONSTRUCT_ACTION_END_EXPRESSION_SLOT:
    case CONSTRUCT_ACTION_END_OPERAND:
    case CONSTRUCT_ACTION_END_OPERATOR:
        return true;
    default:
        return false;
    }
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

static int compare_start_locations(const void *a, const void *b)
{
    struct interpret_node *na = *(struct interpret_node **)a;
    struct interpret_node *nb = *(struct interpret_node **)b;
    if (na->start_location < nb->start_location)
        return -1;
    else if (na->start_location > nb->start_location)
        return 1;
    else
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
    node->number_of_slots = context->grammar->rules[rule].number_of_slots;
    node->slots = calloc(node->number_of_slots,
     sizeof(struct interpret_node *));
    if (!node->slots)
        abort();
    memcpy(node->slots, slots,
     sizeof(struct interpret_node *) * node->number_of_slots);
    // Compute the depth and children for the fancy tree functions to use.
    uint32_t max_depth = 0;
    for (size_t i = 0; i < node->number_of_slots; ++i) {
        struct interpret_node *slot = node->slots[i];
        for (; slot; slot = slot->next_sibling) {
            if (slot->type != NODE_RULE)
                continue;
            struct rule *r = &context->grammar->rules[rule];
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
    if (!node->children)
        abort();
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
    // TODO: offset start locations for multiple actions in the same place?
    qsort(node->children, node->number_of_children,
     sizeof(struct interpret_node *), compare_start_locations);
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
    token->next_sibling = 0;
    return token;
}

static uint32_t rule_lookup(uint32_t parent, uint32_t slot,
 struct interpret_context *context)
{
    if (slot >= context->grammar->rules[parent].number_of_slots)
        abort();
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
