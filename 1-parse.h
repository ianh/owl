#ifndef _1_PARSE_H_
#define _1_PARSE_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef size_t parsed_id;
struct bluebird_tree;

#define TYPES \
TYPE(IDENT) \
TYPE(LITERAL) \
TYPE(PARENS) \
TYPE(BRACKETED) \
TYPE(ZERO_OR_MORE) \
TYPE(ONE_OR_MORE) \
TYPE(OPTIONAL) \
TYPE(CHOICE) \
TYPE(CONCATENATION) \
TYPE(POSTFIX) \
TYPE(PREFIX) \
TYPE(INFIX) \
TYPE(FLAT) \
TYPE(LEFT) \
TYPE(RIGHT) \
TYPE(NONASSOC)

enum parsed_type {
#define TYPE(x) PARSED_##x,
TYPES
#undef TYPE
};

#define DISABLE_PRINT(...)
#define print_DISABLE_PRINT(...)

#define HAS_TYPE(...) __VA_ARGS__
#define NO_TYPE(...)

#define RULE(name, has_type, fields) \
RULE_X(name, has_type, \
    ID_FIELD_X(_next, DISABLE_PRINT) \
    FIELD(string_offset, size_t, DISABLE_PRINT) \
    FIELD(string_length, size_t, DISABLE_PRINT) \
    has_type(FIELD(type, enum parsed_type, DISABLE_PRINT)) \
    fields \
)

#define ID_FIELD_X_DEFAULT(field, rule) FIELD(field, parsed_id, print_##rule)
#define ID_FIELD_X ID_FIELD_X_DEFAULT
#define ID_FIELD(field) ID_FIELD_X(field, field)

#define RULES \
RULE(grammar, NO_TYPE, \
    ID_FIELD(rule) \
) \
RULE(rule, NO_TYPE, \
    ID_FIELD(ident) \
    ID_FIELD(body) \
) \
RULE(ident, NO_TYPE, \
    FIELD(identifier, uint32_t, print_ident_string) \
) \
RULE(body, NO_TYPE, \
    ID_FIELD(expr) \
    ID_FIELD(ident) \
    ID_FIELD(operators) \
) \
RULE(expr, HAS_TYPE, \
    ID_FIELD(expr) \
    ID_FIELD(ident) \
    ID_FIELD_X(name, ident) \
    ID_FIELD_X(left, ident) \
    ID_FIELD_X(right, ident) \
    ID_FIELD_X(operand, expr) \
) \
RULE(operators, NO_TYPE, \
    ID_FIELD(fixity) \
    ID_FIELD(expr) \
    ID_FIELD(ident) \
) \
RULE(fixity, HAS_TYPE, \
    ID_FIELD(assoc) \
) \
RULE(assoc, HAS_TYPE, )

#define RULE_X(name, has_type, ...) \
struct parsed_##name; \
struct parsed_##name parsed_##name##_get(struct bluebird_tree *, parsed_id); \
struct parsed_##name parsed_##name##_next(struct parsed_##name); \
void print_##name(struct bluebird_tree *, parsed_id, int indent); \
struct parsed_##name { \
    struct bluebird_tree *tree; \
    bool empty; \
    __VA_ARGS__ \
};
#define FIELD(field, type, print) type field;
RULES
#undef RULE_X
#undef FIELD

/*
struct bluebird_token {
    int type;
    union {
        int64_t integer;
        double real;
        struct {
            const unsigned char *bytes;
            size_t length;
        } string;
        uint32_t identifier;
    } data;
};
*/

// Using the default tokenizer.
struct bluebird_tree *bluebird_tree_create_from_string(const char *string,
 size_t length);
//struct bluebird_tree *bluebird_tree_create_from_file(FILE *file);

parsed_id bluebird_tree_root(struct bluebird_tree *tree);

// TODO: Remove this and do lexing separately.
uint32_t bluebird_tree_next_identifier(struct bluebird_tree *tree);

void bluebird_tree_destroy(struct bluebird_tree *tree);

// struct bluebird_tree *bluebird_tree_create_empty();
// bool bluebird_tree_add_token(struct bluebird_tree *tree, struct token);

#endif
