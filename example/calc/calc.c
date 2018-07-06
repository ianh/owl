#define BLUEBIRD_PARSER_IMPLEMENTATION
#include "parser.h"

#include <readline/history.h>
#include <readline/readline.h>
#include <stdio.h>

static double eval(struct bluebird_ref ref);

struct binding {
    struct binding *next;
    char *name;
    size_t name_length;
    double value;
};
static struct binding *bindings;
static struct binding *lookup(struct parsed_identifier id);

int main()
{
    while (true) {
        char *s = readline("> ");
        if (!s)
            break;
        add_history(s);
        struct bluebird_tree *tree = bluebird_tree_create_from_string(s);
        if (bluebird_tree_get_error(tree, NULL) != ERROR_NONE) {
            printf("parse error.\n");
            bluebird_tree_destroy(tree);
            continue;
        }
        struct parsed_command cmd = bluebird_tree_get_parsed_command(tree);
        double n = eval(cmd.expression);
        if (cmd.type == PARSED_ASSIGN) {
            struct parsed_identifier id = parsed_identifier_get(cmd.identifier);
            struct binding *b = lookup(id);
            if (!b) {
                b = malloc(sizeof(struct binding));
                b->name = malloc(id.length);
                memcpy(b->name, id.identifier, id.length);
                b->name_length = id.length;
                b->next = bindings;
                bindings = b;
            }
            b->value = n;
        }
        printf("%g\n", n);
        bluebird_tree_destroy(tree);
    }
    return 0;
}

static double eval(struct bluebird_ref ref)
{
    struct parsed_expression expr = parsed_expression_get(ref);
    switch (expr.type) {
    case PARSED_VARIABLE: {
        struct parsed_identifier id = parsed_identifier_get(expr.identifier);
        struct binding *b = lookup(id);
        if (b)
            return b->value;
        else
            printf("variable '%.*s' not set\n", (int)id.length, id.identifier);
        return 0;
    }
    case PARSED_NUMBER:
        return parsed_number_get(expr.number).number;
    case PARSED_PARENS:
        return eval(expr.expression);
    case PARSED_NEGATE:
        return -eval(expr.operand);
    case PARSED_MULTIPLY:
        return eval(expr.left) * eval(expr.right);
    case PARSED_DIVIDE:
        return eval(expr.left) / eval(expr.right);
    case PARSED_ADD:
        return eval(expr.left) + eval(expr.right);
    case PARSED_SUBTRACT:
        return eval(expr.left) - eval(expr.right);
    default:
        return 0;
    }
}

static struct binding *lookup(struct parsed_identifier id)
{
    struct binding *b = bindings;
    for (; b; b = b->next) {
        if (b->name_length == id.length &&
         !memcmp(b->name, id.identifier, id.length))
            break;
    }
    return b;
}
