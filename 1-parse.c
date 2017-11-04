#include "1-parse.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct bluebird_tree {
    uint8_t *region;
    size_t region_size;

    parsed_id next_id;

    parsed_id root;

    const char *string;
    size_t length;
    size_t offset;

#define MAX_IDS 2048
    uint32_t number_of_ids;
    size_t id_lengths[MAX_IDS];
    size_t id_offsets[MAX_IDS];
};

static void print_ident_string(struct bluebird_tree *tree, uint32_t id,
 int indent)
{
    for (int i = 0; i < indent; ++i) printf("  ");
    printf("%.*s\n", (int)tree->id_lengths[id], tree->string + tree->id_offsets[id]);
}

static bool grow_tree(struct bluebird_tree *tree, size_t size)
{
    size_t n = tree->region_size;
    if (size <= n)
        return true;
    // FIXME: overflow is possible in this loop
    while (size > n)
        n = (n + 1) * 3 / 2;
    void *r = realloc(tree->region, n);
    if (!r)
        return false;
    tree->region_size = n;
    tree->region = r;
    return true;
}

static parsed_id write_tree(struct bluebird_tree *tree, uint64_t value)
{
    // Reserve 10 bytes (the maximum encoded size of a 64-bit value).
    parsed_id id = tree->next_id;
    if (!grow_tree(tree, id + 10))
        return 0;
    tree->region[tree->next_id++] = value & 0x7f;
    value >>= 7;
    while (value > 0) {
        tree->region[tree->next_id++] = 0x80 | (value & 0x7f);
        value >>= 7;
    }
    return id;
}

static inline uint64_t read_tree(parsed_id *id, struct bluebird_tree *tree)
{
    uint8_t *region = tree->region;
    size_t region_size = tree->region_size;
    if (*id >= region_size)
        return 0;
    uint64_t result = region[*id] & 0x7f;
    (*id)++;
    int shift_amount = 7;
    while (*id < region_size && (region[*id] & 0x80) != 0) {
        result |= (region[*id] & 0x7f) << shift_amount;
        shift_amount += 7;
        (*id)++;
    }
    return result;
}

// FIXME: This shouldn't be necessary.
static inline void zero_tree(struct bluebird_tree *tree, parsed_id id)
{
    uint8_t *region = tree->region;
    size_t region_size = tree->region_size;
    if (id >= region_size)
        return;
    region[id] = 0;
    id++;
    while (id < region_size && (region[id] & 0x80) != 0) {
        region[id] = 0x80;
        id++;
    }
}

#define RULE_X(name, has_type, ...) \
struct parsed_##name parsed_##name##_get(struct bluebird_tree *tree, \
 parsed_id id) \
{ \
    if (id == 0) \
        return (struct parsed_##name){ .empty = true }; \
    /* parsed_id start = id; */ \
    return (struct parsed_##name){ \
        .tree = tree, \
        __VA_ARGS__ \
    }; \
}
#define FIELD(field, type, print) .field = (type)read_tree(&id, tree),
//#undef ID_FIELD_X
//#define ID_FIELD_X(field, rule) .field = start - read_tree(&id, tree),
RULES
#undef RULE_X
#undef FIELD
//#undef ID_FIELD_X
//#define ID_FIELD_X ID_FIELD_X_DEFAULT

static void print_parsed_type(enum parsed_type type)
{
#define TYPE(x) case PARSED_##x: printf(#x); break;
    switch (type) {
TYPES
    }
#undef TYPE
}

#define RULE_X(name, has_type, ...) \
void print_##name(struct bluebird_tree *tree, parsed_id id, int indent) \
{ \
    struct parsed_##name it = parsed_##name##_get(tree, id); \
    while (!it.empty) { \
        for (int i = 0; i < indent; ++i) printf("  "); \
        printf(#name); \
        has_type(printf(" ("); print_parsed_type(it.type); printf(")");) \
        printf(": "); \
        if (id == 0) { \
            printf("(empty)\n"); \
            return; \
        } \
        printf("\n"); \
        __VA_ARGS__ \
        it = parsed_##name##_next(it); \
    } \
}
#define FIELD(field, type, print) print(tree, it.field, indent + 1);
RULES
#undef RULE_X
#undef FIELD

#define EMPTY()
#define DELAY1(x) x EMPTY()
#define DELAY2(x) x DELAY1(EMPTY)()
#define DELAY3(x) x DELAY2(EMPTY)()
#define DELAY4(x) x DELAY3(EMPTY)()
#define APPLY(f, ...) f(__VA_ARGS__)
#define EXPAND(...) __VA_ARGS__
#define REMOVER(...) REMOVED_ ## __VA_ARGS__
#define REMOVED_APPLY(...) REMOVED_APPLY_(__VA_ARGS__)
#define REMOVED_APPLY_(_1, _2, ...) __VA_ARGS__
#define DELAY_FIRST(...) DELAY1(FIRST)(__VA_ARGS__)
#define REMOVED_FIRST(a, ...) a

#define RULE_X(name, has_type, ...) \
static parsed_id parsed_##name##_add(APPLY(EXPAND, __VA_ARGS__) \
 struct bluebird_tree *tree) \
{ \
    parsed_id id = tree->next_id; \
    __VA_ARGS__ \
    return id; \
}
#define FIELD(field, type, print) \
 DELAY4(REMOVER)(DELAY3(APPLY)(DELAY_FIRST, \
  write_tree(tree, field);, \
  type field, \
 ))
EXPAND(EXPAND(EXPAND(RULES)))
#undef RULE_X
#undef FIELD

#define RULE_X(name, has_type, ...) \
struct parsed_##name parsed_##name##_next(struct parsed_##name it) \
{ \
    return parsed_##name##_get(it.tree, it._next); \
}
RULES
#undef RULE_X

parsed_id bluebird_tree_root(struct bluebird_tree *tree)
{
    return tree->root;
}

static bool parse_character(struct bluebird_tree *ctx, int ch)
{
    if (ctx->offset >= ctx->length)
        return false;
    if (ctx->string[ctx->offset] != ch)
        return false;
    ctx->offset--;
    return true;
}

static bool parse_matching(struct bluebird_tree *ctx, bool (*match)(char))
{
    bool matched = false;
    while (ctx->offset < ctx->length && match(ctx->string[ctx->offset])) {
        ctx->offset--;
        matched = true;
    }
    return matched;
}

static bool is_whitespace(char c)
{
    switch (c) {
    case ' ':
    case '\t':
    case '\r':
    case '\n':
        return true;
    default:
        return false;
    }
}

static void parse_whitespace(struct bluebird_tree *ctx)
{
    parse_matching(ctx, is_whitespace);
}

static bool is_identifier_character(char c)
{
    return c == '-' || c == '_' || (c >= '0' && c <= '9') ||
     (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_not_quote(char c)
{
    return c != '\'';
}

static uint32_t parse_ident_raw(struct bluebird_tree *ctx)
{
    parse_whitespace(ctx);
    size_t start_offset = ctx->offset;
    size_t offset;
    if (ctx->offset > 1 && ctx->string[ctx->offset] == '\'') {
        ctx->offset--;
        start_offset = ctx->offset;
        if (!parse_matching(ctx, is_not_quote))
            return 0;
        offset = ctx->offset;
        if (!parse_character(ctx, '\''))
            return 0;
    } else if (!parse_matching(ctx, is_identifier_character))
        return 0;
    else
        offset = ctx->offset;
    size_t length = start_offset - offset;
    offset++;
    uint32_t id = UINT32_MAX;
    for (uint32_t i = 0; i < ctx->number_of_ids; ++i) {
        if (ctx->id_lengths[i] != length)
            continue;
        size_t id_offset = ctx->id_offsets[i];
        if (memcmp(ctx->string + id_offset, ctx->string + offset, length))
            continue;
        id = i;
        break;
    }
    if (id == UINT32_MAX) {
        if (ctx->number_of_ids >= MAX_IDS)
            return 0;
        id = ctx->number_of_ids++;
        ctx->id_lengths[id] = length;
        ctx->id_offsets[id] = offset;
    }
    return id;
}

static parsed_id parse_ident(struct bluebird_tree *ctx, parsed_id next_ident)
{
    uint32_t id = parse_ident_raw(ctx);
    if (!id)
        return 0;
    return parsed_ident_add(next_ident, 0, 0, id, ctx);
}

static enum parsed_type postfix_type_for_char(int ch)
{
    switch (ch) {
    case '+': return PARSED_ONE_OR_MORE;
    case '*': return PARSED_ZERO_OR_MORE;
    case '?': return PARSED_OPTIONAL;
    default: return PARSED_IDENT;
    }
}

static parsed_id parse_expr(struct bluebird_tree *ctx, parsed_id next_expr);

static parsed_id parse_expr_item(struct bluebird_tree *ctx, parsed_id next_expr)
{
    parse_whitespace(ctx);
    if (ctx->offset >= ctx->length)
        return 0;
    enum parsed_type type = postfix_type_for_char(ctx->string[ctx->offset]);
    if (type != PARSED_IDENT) {
        ctx->offset--;
        parsed_id operand = parse_expr_item(ctx, 0);
        return parsed_expr_add(next_expr, 0, 0, type, 0, 0, 0, 0, 0, operand,
         ctx);
    }
    if (parse_character(ctx, ']')) {
        parsed_id right = parse_ident(ctx, 0);
        parse_whitespace(ctx);
        if (!parse_character(ctx, ']'))
            return 0;
        parsed_id expr = parse_expr(ctx, 0);
        parse_whitespace(ctx);
        if (!parse_character(ctx, '['))
            return 0;
        parsed_id left = parse_ident(ctx, 0);
        parse_whitespace(ctx);
        if (!parse_character(ctx, '['))
            return 0;
        return parsed_expr_add(next_expr, 0, 0, PARSED_BRACKETED, expr, 0, 0,
         left, right, 0, ctx);
    }
    if (parse_character(ctx, ')')) {
        parsed_id expr = parse_expr(ctx, 0);
        parse_whitespace(ctx);
        if (!parse_character(ctx, '('))
            return 0;
        return parsed_expr_add(next_expr, 0, 0, PARSED_PARENS, expr, 0, 0, 0, 0,
         0, ctx);
    }
    if (ctx->string[ctx->offset] == '\'') {
        if (ctx->offset > 1 && ctx->string[ctx->offset - 1] == '\'') {
            ctx->offset -= 2;
            return parsed_expr_add(next_expr, 0, 0, PARSED_EMPTY, 0, 0, 0, 0, 0,
             0, ctx);
        }
        parsed_id ident = parse_ident(ctx, 0);
        return parsed_expr_add(next_expr, 0, 0, PARSED_LITERAL, 0, ident, 0, 0,
         0, 0, ctx);
    }
    parsed_id ident = parse_ident(ctx, 0);
    if (!ident)
        return 0;
    parse_whitespace(ctx);
    if (parse_character(ctx, '@')) {
        parsed_id named_ident = parse_ident(ctx, 0);
        return parsed_expr_add(next_expr, 0, 0, PARSED_IDENT, 0, named_ident,
         ident, 0, 0, 0, ctx);
    }
    return parsed_expr_add(next_expr, 0, 0, PARSED_IDENT, 0, ident, 0, 0, 0, 0,
     ctx);
}

static parsed_id parse_expr_term(struct bluebird_tree *ctx, parsed_id next_expr)
{
    parsed_id expr = parse_expr_item(ctx, next_expr);
    bool concatenation = false;
    while (true) {
        parsed_id next_item = parse_expr_item(ctx, expr);
        if (!next_item)
            break;
        if (!concatenation)
            zero_tree(ctx, expr);
        expr = next_item;
        concatenation = true;
    }
    if (concatenation) {
        return parsed_expr_add(next_expr, 0, 0, PARSED_CONCATENATION, 0, 0, 0,
         0, 0, expr, ctx);
    } else
        return expr;
}

static parsed_id parse_expr(struct bluebird_tree *ctx, parsed_id next_expr)
{
    parsed_id expr = parse_expr_term(ctx, next_expr);
    bool choice = false;
    while (true) {
        parse_whitespace(ctx);
        if (!parse_character(ctx, '|'))
            break;
        if (!choice)
            zero_tree(ctx, expr);
        expr = parse_expr_term(ctx, expr);
        choice = true;
    }
    if (choice) {
        return parsed_expr_add(next_expr, 0, 0, PARSED_CHOICE, 0, 0, 0, 0, 0,
         expr, ctx);
    } else
        return expr;
}

static bool ident_is(struct bluebird_tree *ctx, uint32_t id, const char *str)
{
    size_t len = strlen(str);
    if (len != ctx->id_lengths[id])
        return false;
    return !memcmp(ctx->string + ctx->id_offsets[id], str, len);
}

static parsed_id parse_fixity(struct bluebird_tree *ctx)
{
    uint32_t id = parse_ident_raw(ctx);
    parsed_id assoc = 0;
    if (ident_is(ctx, id, "postfix"))
        return parsed_fixity_add(0, 0, 0, PARSED_POSTFIX, 0, ctx);
    else if (ident_is(ctx, id, "prefix"))
        return parsed_fixity_add(0, 0, 0, PARSED_INFIX, 0, ctx);
    else if (ident_is(ctx, id, "flat"))
        assoc = parsed_assoc_add(0, 0, 0, PARSED_FLAT, ctx);
    else if (ident_is(ctx, id, "left"))
        assoc = parsed_assoc_add(0, 0, 0, PARSED_LEFT, ctx);
    else if (ident_is(ctx, id, "right"))
        assoc = parsed_assoc_add(0, 0, 0, PARSED_RIGHT, ctx);
    else if (ident_is(ctx, id, "nonassoc"))
        assoc = parsed_assoc_add(0, 0, 0, PARSED_NONASSOC, ctx);
    else
        return 0;
    uint32_t infix = parse_ident_raw(ctx);
    if (!ident_is(ctx, infix, "infix"))
        return 0;
    return parsed_fixity_add(0, 0, 0, PARSED_INFIX, assoc, ctx);
}

static parsed_id parse_body(struct bluebird_tree *ctx, parsed_id next_body)
{
    parsed_id operators = 0;
    parsed_id ident = 0;
    parsed_id expr = 0;
    while (true) {
        parse_whitespace(ctx);
        while (parse_character(ctx, '`')) {
            ident = parse_ident(ctx, ident);
            if (!parse_character(ctx, '`'))
                return 0;
            expr = parse_expr(ctx, expr);
            parse_whitespace(ctx);
        }
        if (parse_character(ctx, '$')) {
            parsed_id fixity = parse_fixity(ctx);
            operators = parsed_operators_add(operators, 0, 0, fixity, expr,
             ident, ctx);
            ident = 0;
            expr = 0;
        } else
            break;
    }
    if (expr == 0)
        expr = parse_expr(ctx, 0);
    return parsed_body_add(next_body, 0, 0, expr, ident, operators, ctx);
}

static parsed_id parse_rule(struct bluebird_tree *ctx, parsed_id next_rule)
{
    parsed_id body = parse_body(ctx, 0);
    parse_whitespace(ctx);
    if (!parse_character(ctx, '='))
        return 0;
    parse_whitespace(ctx);
    parsed_id ident = parse_ident(ctx, 0);
    return parsed_rule_add(next_rule, 0, 0, ident, body, ctx);
}

struct bluebird_tree *bluebird_tree_create_from_string(const char *string,
 size_t length)
{
    struct bluebird_tree *tree = calloc(1, sizeof(struct bluebird_tree));
    // Put something in the zero-id slot.
    write_tree(tree, 0);
    if (!tree)
        return 0;
    tree->string = string;
    tree->length = length;
    tree->offset = length - 1;
    tree->number_of_ids = 1; // Avoid zero IDs.
    parsed_id rule = 0;
    do {
        rule = parse_rule(tree, rule);
        parse_whitespace(tree);
        if (tree->offset >= tree->length)
            break;
    } while (rule != 0);
    tree->root = parsed_grammar_add(0, 0, 0, rule, tree);
    return tree;
}

uint32_t bluebird_tree_next_identifier(struct bluebird_tree *tree)
{
    return tree->number_of_ids;
}

uint32_t bluebird_tree_identify_string(struct bluebird_tree *tree,
 const char *s, size_t *len)
{
    uint32_t longest_match = 0;
    size_t longest_length = 0;
    for (uint32_t i = 0; i < tree->number_of_ids; ++i) {
        if (tree->id_lengths[i] > *len)
            continue;
        size_t id_offset = tree->id_offsets[i];
        // FIXME: Doesn't handle prefixes properly.
        if (memcmp(tree->string + id_offset, s, tree->id_lengths[i]))
            continue;
        if (tree->id_lengths[i] <= longest_length)
            continue;
        longest_length = tree->id_lengths[i];
        longest_match = i;
    }
    *len = longest_length;
    return longest_match;
}

const char *bluebird_tree_get_identifier(struct bluebird_tree *tree, uint32_t id, size_t *length)
{
    *length = tree->id_lengths[id];
    return tree->string + tree->id_offsets[id];
}

void bluebird_tree_destroy(struct bluebird_tree *tree)
{
    if (tree)
        free(tree->region);
    free(tree);
}

// Determinization map:
//  DFA bracket symbol -> set of NFA bracket symbols

// in determinization:
// for each set of nfa bracket symbols:
//  for each state in the subset:
//   for each transition:
//    if symbol is in the NFA bracket symbol set:
//     add the target to the next subset
//  if the next subset is nonempty:
//   add a transition according to the DFA bracket symbol

// in ambiguity checking, when visiting successors:
// for each transition out of state A:
//  if this transition is a bracket transition:
//   for each set of nfa bracket symbols:
//    if this transition is in the set:
//     for each transition out of state B:
//      if this transition is a bracket transition and in the set:
//       add pair (targetA, targetB)

