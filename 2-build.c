#include "2-build.h"

#include "4-determinize.h"
#include "grow-array.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct context {
    struct grammar *grammar;
    struct bluebird_tree *tree;

    struct rule *rule;
    symbol_id next_symbol;
    state_id next_state;
};

struct boundary_states;
static void build_body_automaton(struct context *ctx,
 struct automaton *automaton, struct parsed_expr *expr);
static void build_body_expression(struct context *ctx,
 struct automaton *automaton, struct parsed_expr *expr,
 struct boundary_states boundary);
static struct boundary_states connect_expression(struct context *ctx,
 struct automaton *a, struct parsed_expr *expr, struct boundary_states outer);

static uint32_t add_slot(struct context *ctx, struct rule *rule,
 const char *slot_name, size_t slot_name_length,
 uint32_t referenced_rule_index);

static symbol_id add_keyword_token(struct context *ctx, struct rule *rule,
 parsed_id id, enum token_type type);

static uint32_t add_rule(struct context *ctx, const char *name, size_t len);
static void add_token_rule(struct context *ctx, const char *name, size_t len);
static uint32_t find_rule(struct context *ctx, const char *name, size_t len);

void build(struct grammar *grammar, struct bluebird_tree *tree)
{
    struct context context = {
        .grammar = grammar,
        .tree = tree,
    };
    parsed_id root = bluebird_tree_root_id(tree);
    struct parsed_grammar g = parsed_grammar_get(tree, root);

    // First, create rule structs for each rule in the grammar.  We have to do
    // this in a separate pass so we can look up names before they appear in the
    // parse tree.
    grammar->root_rule = 0;
    struct parsed_rule parsed_rule = parsed_rule_get(tree, g.rule);
    while (!parsed_rule.empty) {
        struct parsed_identifier name =
         parsed_identifier_get(tree, parsed_rule.identifier);
        uint32_t index = add_rule(&context, name.identifier, name.length);
        if (index == UINT32_MAX) {
            fprintf(stderr, "error: there are multiple rules named '%.*s'\n",
             (int)name.length, name.identifier);
            exit(-1);
        }
        parsed_rule = parsed_rule_next(parsed_rule);
    }
    if (grammar->number_of_rules == 0) {
        fprintf(stderr, "error: a bluebird grammar needs at least one rule of "
         "the form 'rule_name = ...'.\n");
        exit(-1);
    }

    // Add rules for all the kinds of tokens we support.
    add_token_rule(&context, "identifier", strlen("identifier"));
    add_token_rule(&context, "number", strlen("number"));
    add_token_rule(&context, "string", strlen("string"));

    // Now fill in the rules according to the contents of each parsed rule.
    for (parsed_rule = parsed_rule_get(tree, g.rule); !parsed_rule.empty;
     parsed_rule = parsed_rule_next(parsed_rule)) {
        struct parsed_identifier name =
         parsed_identifier_get(tree, parsed_rule.identifier);

        // Look up rules by name (instead of just counting up by index) because
        // it's less likely to break in a confusing way.
        uint32_t rule_index = find_rule(&context, name.identifier, name.length);
        if (rule_index == UINT32_MAX)
            abort();
        struct rule *rule = &grammar->rules[rule_index];

        // Store the rule into our context object so we don't have to pass it
        // around everywhere while we're building the rule's automata.
        context.rule = rule;
        context.next_symbol = 0;

        struct parsed_body body = parsed_body_get(tree, parsed_rule.body);
        if (!body.identifier) {
            // This is a simple rule with no choices.  Create the automaton
            // directly.
            struct parsed_expr expr = parsed_expr_get(tree, body.expr);
            build_body_automaton(&context, &rule->automaton, &expr);
            continue;
        }

        // This rule has multiple choices.  Add them to the rule as choice
        // structs.
        struct parsed_expr expr = parsed_expr_get(tree, body.expr);
        struct parsed_identifier choice_identifier;
        choice_identifier = parsed_identifier_get(tree, body.identifier);
        while (!expr.empty) {
            if (rule->number_of_choices >= MAX_NUMBER_OF_CHOICES) {
                // TODO: Show location in original grammar text.
                fprintf(stderr, "error: rules with more than %u choice clauses "
                 "are currently unsupported.\n", MAX_NUMBER_OF_CHOICES);
                exit(-1);
            }
            uint32_t choice_index = rule->number_of_choices++;
            rule->choices = grow_array(rule->choices,
             &rule->choices_allocated_bytes,
             sizeof(struct choice) * rule->number_of_choices);
            struct choice *choice = &rule->choices[choice_index];
            choice->name = choice_identifier.identifier;
            choice->name_length = choice_identifier.length;
            build_body_automaton(&context, &choice->automaton, &expr);
            expr = parsed_expr_next(expr);
            choice_identifier = parsed_identifier_next(choice_identifier);
        }

        // Create operator structs from each operator.
        struct parsed_operators ops;
        ops = parsed_operators_get(tree, body.operators);
        int32_t precedence = -1;
        while (!ops.empty) {
            if (rule->number_of_operators + rule->number_of_choices
             >= MAX_NUMBER_OF_CHOICES) {
                // TODO: Show location in original grammar text.
                fprintf(stderr, "error: rules with more than %u combined "
                 "choice and operator clauses are currently unsupported.\n",
                 MAX_NUMBER_OF_CHOICES);
                exit(-1);
            }
            // First, unpack the fixity and associativity from the parse tree.
            struct parsed_fixity fixity = parsed_fixity_get(tree, ops.fixity);
            enum fixity rule_fixity;
            switch (fixity.type) {
            case PARSED_PREFIX_OP:
                rule_fixity = PREFIX; break;
            case PARSED_POSTFIX_OP:
                rule_fixity = POSTFIX; break;
            case PARSED_INFIX_OP:
                rule_fixity = INFIX; break;
            default:
                abort();
            }
            struct parsed_assoc assoc = parsed_assoc_get(tree, fixity.assoc);
            enum associativity rule_associativity = 0;
            if (!assoc.empty) {
                switch (assoc.type) {
                case PARSED_LEFT_OP:
                    rule_associativity = LEFT; break;
                case PARSED_RIGHT_OP:
                    rule_associativity = RIGHT; break;
                case PARSED_FLAT_OP:
                    rule_associativity = FLAT; break;
                case PARSED_NONASSOC_OP:
                    rule_associativity = NONASSOC; break;
                default:
                    abort();
                }
            }
            // Then add each operator at this precedence to the rule.
            struct parsed_operator op = parsed_operator_get(tree, ops.operator);
            while (!op.empty) {
                struct parsed_expr op_expr = parsed_expr_get(tree, op.expr);
                struct parsed_identifier op_choice =
                 parsed_identifier_get(tree, op.identifier);
                uint32_t op_index = rule->number_of_operators++;
                rule->operators = grow_array(rule->operators,
                 &rule->operators_allocated_bytes,
                 sizeof(struct operator) * rule->number_of_operators);
                struct operator *operator = &rule->operators[op_index];
                operator->name = op_choice.identifier;
                operator->name_length = op_choice.length;
                operator->fixity = rule_fixity;
                operator->associativity = rule_associativity;
                operator->precedence = precedence;
                build_body_automaton(&context, &operator->automaton,
                 &op_expr);
                op = parsed_operator_next(op);
            }
            // Each new 'operators' section has a lower precedence than the
            // previous one.
            precedence--;
            ops = parsed_operators_next(ops);
        }

        // Add slots for operands -- 'left'/'right' for infix operators, and
        // 'operand' for prefix and postfix operators.
        for (uint32_t i = 0; i < rule->number_of_operators; ++i) {
            struct operator *operator = &rule->operators[i];
            if (operator->fixity == INFIX && operator->associativity != FLAT) {
                rule->left_slot_index = add_slot(&context, rule, "left",
                 strlen("left"), rule_index);
                rule->right_slot_index = add_slot(&context, rule, "right",
                 strlen("right"), rule_index);
                if (rule->left_slot_index == UINT32_MAX ||
                 rule->right_slot_index == UINT32_MAX) {
                    fprintf(stderr, "error: the 'left' and 'right' slots are "
                     "reserved for the left and right operands of the '%.*s' "
                     "operator.\n", (int)operator->name_length,
                     operator->name);
                    exit(-1);
                }
            } else {
                rule->operand_slot_index = add_slot(&context, rule, "operand",
                 strlen("operand"), rule_index);
                if (rule->operand_slot_index == UINT32_MAX) {
                    fprintf(stderr, "error: the 'operand' slot is reserved for "
                     "the operand of the '%.*s' operator.\n",
                     (int)operator->name_length, operator->name);
                    exit(-1);
                }
            }
        }
    }
}

struct boundary_states {
    state_id entry;
    state_id exit;
};

static void build_body_automaton(struct context *ctx,
 struct automaton *out_automaton, struct parsed_expr *expr)
{
    struct automaton automaton = {0};
    struct boundary_states boundary = { .entry = 0, .exit = 1 };
    automaton_set_start_state(&automaton, boundary.entry);
    automaton_mark_accepting_state(&automaton, boundary.exit);

    struct context saved_context = *ctx;
    ctx->next_state = 2;
    build_body_expression(ctx, &automaton, expr, boundary);
    ctx->next_state = saved_context.next_state;

    determinize_minimize(&automaton, out_automaton);
    automaton_destroy(&automaton);
}

static void build_body_expression(struct context *ctx,
 struct automaton *automaton, struct parsed_expr *expr,
 struct boundary_states b)
{
    struct bluebird_tree *tree = ctx->tree;
    struct rule *rule = ctx->rule;
    switch (expr->type) {
    case PARSED_CHOICE: {
        struct parsed_expr choice = parsed_expr_get(tree, expr->operand);
        while (!choice.empty) {
            connect_expression(ctx, automaton, &choice, b);
            choice = parsed_expr_next(choice);
        }
        break;
    }
    case PARSED_CONCATENATION: {
        struct parsed_expr term = parsed_expr_get(tree, expr->operand);
        struct boundary_states inner = { .entry = ctx->next_state++ };
        automaton_add_transition(automaton, b.entry, inner.entry,
         SYMBOL_EPSILON);
        while (!term.empty) {
            inner.exit = ctx->next_state++;
            build_body_expression(ctx, automaton, &term, inner);
            inner.entry = inner.exit;
            term = parsed_expr_next(term);
        }
        automaton_add_transition(automaton, inner.exit, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_IDENT: {
        struct parsed_identifier ident =
         parsed_identifier_get(tree, expr->identifier);
        struct parsed_identifier name =
         parsed_identifier_get(tree, expr->rename);
        const char *rule_name = ident.identifier;
        size_t rule_name_length = ident.length;
        const char *slot_name = rule_name;
        size_t slot_name_length = rule_name_length;
        if (!name.empty) {
            slot_name = name.identifier;
            slot_name_length = name.length;
        }
        uint32_t rule_index = find_rule(ctx, rule_name, rule_name_length);
        if (rule_index == UINT32_MAX) {
            fprintf(stderr, "error: unknown rule or token '%.*s'.\n",
             (int)rule_name_length, rule_name);
            exit(-1);
        }
        uint32_t slot_index = add_slot(ctx, rule, slot_name, slot_name_length,
         rule_index);
        if (slot_index == UINT32_MAX)
            exit(-1);
        automaton_add_transition(automaton, b.entry, b.exit,
         rule->slots[slot_index].symbol);
        break;
    }
    case PARSED_LITERAL: {
        automaton_add_transition(automaton, b.entry, b.exit,
         add_keyword_token(ctx, rule, expr->string, TOKEN_NORMAL));
        break;
    }
    case PARSED_PARENS: {
        struct parsed_expr parens = parsed_expr_get(tree, expr->expr);
        build_body_expression(ctx, automaton, &parens, b);
        break;
    }
    case PARSED_BRACKETED: {
        struct parsed_expr bracket_expr = parsed_expr_get(tree, expr->expr);
        uint32_t bracket_index = rule->number_of_brackets++;
        rule->brackets = grow_array(rule->brackets,
         &rule->brackets_allocated_bytes,
         sizeof(struct bracket) * rule->number_of_brackets);
        // We can't write directly into the automaton in the bracket struct
        // because more brackets could be created inside build_body_automaton,
        // invalidating the pointer.  Write into a stack value instead, then
        // move its contents into place.
        struct bracket *bracket = &rule->brackets[bracket_index];
        if (bracket_expr.empty) {
            automaton_set_start_state(&bracket->automaton, 0);
            automaton_mark_accepting_state(&bracket->automaton, 0);
        } else {
            struct automaton bracket_automaton = {0};
            build_body_automaton(ctx, &bracket_automaton, &bracket_expr);
            automaton_move(&bracket_automaton, &bracket->automaton);
            automaton_destroy(&bracket_automaton);
        }
        bracket->symbol = ctx->next_symbol++;
        bracket->start_symbol = add_keyword_token(ctx, rule, expr->begin_token,
         TOKEN_START);
        bracket->end_symbol = add_keyword_token(ctx, rule, expr->end_token,
         TOKEN_END);
        automaton_add_transition(automaton, b.entry, b.exit, bracket->symbol);
        break;
    }
    case PARSED_ZERO_OR_MORE: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        struct boundary_states in;
        in = connect_expression(ctx, automaton, &operand, b);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_ONE_OR_MORE: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        struct boundary_states in;
        in = connect_expression(ctx, automaton, &operand, b);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        break;
    }
    case PARSED_OPTIONAL: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        connect_expression(ctx, automaton, &operand, b);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    default:
        abort();
    }
}

static struct boundary_states connect_expression(struct context *ctx,
 struct automaton *a, struct parsed_expr *expr, struct boundary_states outer)
{
    struct boundary_states inner;
    inner.entry = ctx->next_state++;
    inner.exit = ctx->next_state++;
    build_body_expression(ctx, a, expr, inner);
    automaton_add_transition(a, outer.entry, inner.entry, SYMBOL_EPSILON);
    automaton_add_transition(a, inner.exit, outer.exit, SYMBOL_EPSILON);
    return inner;
}

static uint32_t add_slot(struct context *ctx, struct rule *rule,
 const char *slot_name, size_t slot_name_length, uint32_t referenced_rule_index)
{
    uint32_t slot_index = 0;
    for (; slot_index < rule->number_of_slots; ++slot_index) {
        struct slot *slot = &rule->slots[slot_index];
        if (slot->name_length != slot_name_length)
            continue;
        if (memcmp(slot->name, slot_name, slot_name_length))
            continue;
        if (slot->rule_index != referenced_rule_index) {
            fprintf(stderr, "error: in the rule '%.*s', the name '%.*s' "
             "could refer to either the rule '%.*s' or the rule '%.*s'.\n",
             (int)rule->name_length, rule->name,
             (int)slot_name_length, slot_name,
             (int)ctx->grammar->rules[slot->rule_index].name_length,
             ctx->grammar->rules[slot->rule_index].name,
             (int)ctx->grammar->rules[referenced_rule_index].name_length,
             ctx->grammar->rules[referenced_rule_index].name);
            return UINT32_MAX;
        }
        break;
    }
    if (slot_index >= rule->number_of_slots) {
        symbol_id symbol = ctx->next_symbol++;
        if (rule->number_of_slots >= MAX_NUMBER_OF_SLOTS) {
            fprintf(stderr, "error: rules with more than %u references to "
             "other rules or tokens are currently unsupported.\n",
             MAX_NUMBER_OF_SLOTS);
            exit(-1);
        }
        rule->number_of_slots = slot_index + 1;
        rule->slots = grow_array(rule->slots, &rule->slots_allocated_bytes,
         rule->number_of_slots * sizeof(struct slot));
        struct slot *slot = &rule->slots[slot_index];
        slot->symbol = symbol;
        slot->name = slot_name;
        slot->name_length = slot_name_length;
        slot->rule_index = referenced_rule_index;
    }
    return slot_index;
}

static const char *token_type_string(enum token_type type)
{
    switch (type) {
    case TOKEN_NORMAL:
        return "a normal";
    case TOKEN_START:
        return "a start";
    case TOKEN_END:
        return "an end";
    default:
        return "";
    }
}

uint32_t find_token(struct token *tokens, uint32_t number_of_tokens,
 const char *string, size_t length, enum token_type type)
{
    uint32_t token_index = 0;
    for (; token_index < number_of_tokens; ++token_index) {
        struct token *token = &tokens[token_index];
        if (token->length != length)
            continue;
        if (memcmp(token->string, string, length))
            continue;
        if (type != TOKEN_DONT_CARE && token->type != type) {
            // TODO: Show location in original grammar text.
            fprintf(stderr, "error: token '%.*s' can't be used as both %s and "
             "%s keyword\n", (int)length, string,
             token_type_string(token->type), token_type_string(type));
            exit(-1);
        }
        break;
    }
    return token_index;
}

static symbol_id add_keyword_token(struct context *ctx, struct rule *rule,
 parsed_id id, enum token_type type)
{
    struct parsed_string keyword = parsed_string_get(ctx->tree, id);
    if (keyword.length <= 2) {
        // Zero-length keywords are treated as epsilons.
        return SYMBOL_EPSILON;
    }
    uint32_t token_index = find_token(rule->keyword_tokens,
     rule->number_of_keyword_tokens, keyword.string + 1, keyword.length - 2, type);
    if (token_index >= rule->number_of_keyword_tokens) {
        rule->number_of_keyword_tokens = token_index + 1;
        rule->keyword_tokens = grow_array(rule->keyword_tokens,
         &rule->keyword_tokens_allocated_bytes,
         sizeof(struct token) * rule->number_of_keyword_tokens);
        rule->keyword_tokens[token_index].string = keyword.string + 1;
        rule->keyword_tokens[token_index].length = keyword.length - 2;
        rule->keyword_tokens[token_index].type = type;
        rule->keyword_tokens[token_index].symbol = ctx->next_symbol++;
    }
    return rule->keyword_tokens[token_index].symbol;
}

static uint32_t add_rule(struct context *ctx, const char *name, size_t len)
{
    // Disallow duplicate rules.
    if (find_rule(ctx, name, len) < ctx->grammar->number_of_rules)
        return UINT32_MAX;
    uint32_t index = ctx->grammar->number_of_rules++;
    ctx->grammar->rules = grow_array(ctx->grammar->rules,
     &ctx->grammar->rules_allocated_bytes,
     sizeof(struct rule) * ctx->grammar->number_of_rules);
    ctx->grammar->rules[index].name = name;
    ctx->grammar->rules[index].name_length = len;
    ctx->grammar->rules[index].operand_slot_index = UINT32_MAX;
    ctx->grammar->rules[index].left_slot_index = UINT32_MAX;
    ctx->grammar->rules[index].right_slot_index = UINT32_MAX;
    return index;
}

static void add_token_rule(struct context *ctx, const char *name, size_t len)
{
    uint32_t index = add_rule(ctx, name, len);
    if (index == UINT32_MAX) {
        // Just skip this token rule if there's already an explicit rule with
        // that name.  If the user wants to name a rule 'identifier', there's no
        // reason to stop them from doing it.
        return;
    }
    ctx->grammar->rules[index].is_token = true;
}

static uint32_t find_rule(struct context *ctx, const char *name, size_t len)
{
    for (uint32_t i = 0; i < ctx->grammar->number_of_rules; ++i) {
        struct rule *rule = &ctx->grammar->rules[i];
        if (rule->name_length != len)
            continue;
        if (memcmp(rule->name, name, len))
            continue;
        return i;
    }
    return UINT32_MAX;
}
