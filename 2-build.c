#include "2-build.h"

#include "4-determinize.h"
#include "grow-array.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct boundary_states;

struct context {
    struct grammar *grammar;
    struct bluebird_tree *tree;

    symbol_id next_symbol;
};

static rule_id add_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier, struct parsed_expr *expr,
 uint32_t syntactic_rule_or_0xffff);
static rule_id add_empty_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier);
static void determinize_minimize_rule(struct rule *rule);

static void build_body_expression(struct context *ctx, rule_id rule,
 struct parsed_expr *expr, struct boundary_states boundary,
 state_id *next_state_id);
static symbol_id add_binding(struct context *ctx, const char *str, size_t len);
static symbol_id find_binding(struct context *ctx, const char *str, size_t len);
static symbol_id add_keyword_token(struct context *ctx, uint32_t identifier,
 enum token_type type);
static struct boundary_states connect_expression(struct context *ctx,
 rule_id rule, struct parsed_expr *expr, struct boundary_states outer,
 state_id *next_state_id);

struct boundary_states {
    state_id entry;
    state_id exit;
};

void build(struct grammar *grammar, struct bluebird_tree *tree)
{
    struct context context = {
        .grammar = grammar,
        .tree = tree,
        // TODO: choose this symbol in a more reasonable way (counting down
        // doesn't work because bitsets)
        .next_symbol = bluebird_tree_next_identifier(tree) + 100,
    };
    parsed_id root = bluebird_tree_root(tree);
    struct parsed_grammar g = parsed_grammar_get(tree, root);

    // First, we set up bindings for every syntactic rule.
    struct parsed_rule rule = parsed_rule_get(tree, g.rule);
    while (!rule.empty) {
        uint32_t identifier = parsed_ident_get(tree, rule.ident).identifier;
        size_t name_length = 0;
        const char *name = bluebird_tree_get_identifier(tree, identifier,
         &name_length);
        add_binding(&context, name, name_length);
        rule = parsed_rule_next(rule);
    }
    // ...and every named token type.
    grammar->first_token_binding = grammar->number_of_bindings;
    add_binding(&context, "identifier", strlen("identifier"));
    add_binding(&context, "number", strlen("number"));
    add_binding(&context, "string", strlen("string"));
    // Any further bindings will be keyword tokens.
    grammar->first_keyword_token_binding = grammar->number_of_bindings;

    // Now we do another pass to create each rule.
    rule = parsed_rule_get(tree, g.rule);
    while (!rule.empty) {
        symbol_id ident = parsed_ident_get(tree, rule.ident).identifier;
        size_t name_length = 0;
        const char *name = bluebird_tree_get_identifier(tree, ident,
         &name_length);

        symbol_id identifier = find_binding(&context, name, name_length);

        struct parsed_body body = parsed_body_get(tree, rule.body);
        if (!body.ident) {
            // This is a single-choice rule, so we just add it directly.
            struct parsed_expr expr = parsed_expr_get(tree, body.expr);
            rule_id id;
            id = add_rule(&context, SIMPLE_RULE, identifier, &expr, 0xffff);
            grammar->bindings[identifier].rule = id;
            grammar->rules[id].name = name;
            grammar->rules[id].name_length = name_length;
            rule = parsed_rule_next(rule);
            continue;
        }

        // This is a multiple-choice rule.  We make separate rules for each
        // choice, them combine them into a single named rule.
        rule_id syntactic_rule = add_empty_rule(&context,
         body.operators ? RULE_WITH_OPERATORS : RULE_WITH_CHOICES, identifier);
        grammar->bindings[identifier].rule = syntactic_rule;
        grammar->rules[syntactic_rule].name = name;
        grammar->rules[syntactic_rule].name_length = name_length;
        uint32_t number_of_choices = 0;
        struct automaton *combined_automaton;
        combined_automaton = grammar->rules[syntactic_rule].automaton;
        struct boundary_states combined_boundary = { .entry = 0, .exit = 1 };
        automaton_set_start_state(combined_automaton, combined_boundary.entry);

        // Create a rule for each choice.
        struct parsed_expr expr = parsed_expr_get(tree, body.expr);
        struct parsed_ident choice = parsed_ident_get(tree, body.ident);
        while (!expr.empty) {
            symbol_id sym = context.next_symbol++;
            rule_id id;
            id = add_rule(&context, CHOICE_RULE, sym, &expr, syntactic_rule);
            grammar->rules[id].name = name;
            grammar->rules[id].name_length = name_length;
            grammar->rules[id].choice_name = bluebird_tree_get_identifier(tree,
             choice.identifier, &grammar->rules[id].choice_name_length);
            if (number_of_choices >= MAX_NUMBER_OF_CHOICES) {
                fprintf(stderr, "error: rules with more than %u choices and "
                 "operators are currently unsupported.\n",
                 MAX_NUMBER_OF_CHOICES);
                exit(-1);
            }
            grammar->rules[id].choice_index = number_of_choices++;
            grammar->rules[id].syntactic_rule = syntactic_rule;
            automaton_add_transition(combined_automaton,
             combined_boundary.entry, combined_boundary.exit, sym);
            expr = parsed_expr_next(expr);
            choice = parsed_ident_next(choice);
        }

        // Create rules from each operator clause.
        struct parsed_operators ops;
        ops = parsed_operators_get(tree, body.operators);
        int32_t precedence = -1;
        while (!ops.empty) {

            // First, unpack the fixity and associativity from the parse tree.
            struct parsed_fixity fixity = parsed_fixity_get(tree, ops.fixity);
            enum fixity rule_fixity;
            switch (fixity.type) {
            case PARSED_PREFIX:
                rule_fixity = PREFIX; break;
            case PARSED_POSTFIX:
                rule_fixity = POSTFIX; break;
            case PARSED_INFIX:
                rule_fixity = INFIX; break;
            default:
                abort();
            }
            struct parsed_assoc assoc = parsed_assoc_get(tree, fixity.assoc);
            enum associativity rule_associativity = 0;
            if (!assoc.empty) {
                switch (assoc.type) {
                case PARSED_LEFT:
                    rule_associativity = LEFT; break;
                case PARSED_RIGHT:
                    rule_associativity = RIGHT; break;
                case PARSED_FLAT:
                    rule_associativity = FLAT; break;
                case PARSED_NONASSOC:
                    rule_associativity = NONASSOC; break;
                default:
                    abort();
                }
            }

            // Then add each operator at this precedence as a rule.
            struct parsed_expr op_expr = parsed_expr_get(tree, ops.expr);
            struct parsed_ident op_choice = parsed_ident_get(tree, ops.ident);
            while (!op_expr.empty) {
                symbol_id op_id = context.next_symbol++;
                rule_id i = add_rule(&context, OPERATOR_RULE, op_id, &op_expr,
                 syntactic_rule);
                struct rule *r = &grammar->rules[i];
                r->name = name;
                r->name_length = name_length;
                r->choice_name = bluebird_tree_get_identifier(tree,
                 op_choice.identifier, &r->choice_name_length);
                if (number_of_choices >= MAX_NUMBER_OF_CHOICES) {
                    fprintf(stderr, "error: rules with more than %u choices "
                     "and operators are currently unsupported.\n",
                     MAX_NUMBER_OF_CHOICES);
                    exit(-1);
                }
                r->choice_index = number_of_choices++;
                r->fixity = rule_fixity;
                r->associativity = rule_associativity;
                r->precedence = precedence;
                r->syntactic_rule = syntactic_rule;
                if (rule_fixity == PREFIX) {
                    // Prefix operators are a transition from the start state
                    // to itself.
                    automaton_add_transition(combined_automaton,
                     combined_boundary.entry, combined_boundary.entry, op_id);
                } else if (rule_fixity == POSTFIX) {
                    // Postfix operators are a transition from the end state
                    // to itself.
                    automaton_add_transition(combined_automaton,
                     combined_boundary.exit, combined_boundary.exit, op_id);
                } else if (rule_associativity == NONASSOC) {
                    // Non-associative operators match either exactly two
                    // operands (if the operator is present) or exactly one
                    // operand (if it isn't): that means we have to duplicate
                    // all the states here, unlike the other kinds of operators.
                    uint32_t n = combined_automaton->number_of_states;
                    for (state_id i = 0; i < n; ++i) {
                        struct state s = combined_automaton->states[i];
                        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
                            struct transition t = s.transitions[j];
                            automaton_add_transition(combined_automaton, i + n,
                             t.target + n, t.symbol);
                        }
                    }
                    automaton_add_transition(combined_automaton,
                     combined_boundary.exit, combined_boundary.entry + n,
                     op_id);
                    automaton_add_transition(combined_automaton,
                     combined_boundary.exit, combined_boundary.exit + n,
                     SYMBOL_EPSILON);
                    combined_boundary.exit = combined_boundary.exit + n;
                } else {
                    // Infix operators are a transition from the end state back
                    // to the start state.
                    automaton_add_transition(combined_automaton,
                     combined_boundary.exit, combined_boundary.entry, op_id);
                }
                op_expr = parsed_expr_next(op_expr);
                op_choice = parsed_ident_next(op_choice);
            }
            precedence--;
            ops = parsed_operators_next(ops);
        }

        grammar->rules[syntactic_rule].number_of_choices = number_of_choices;
        automaton_mark_accepting_state(combined_automaton,
         combined_boundary.exit);
        determinize_minimize_rule(&grammar->rules[syntactic_rule]);

        rule = parsed_rule_next(rule);
    }
}

static rule_id add_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier, struct parsed_expr *expr,
 uint32_t syntactic_rule_or_0xffff)
{
    struct grammar *grammar = ctx->grammar;
    rule_id i = add_empty_rule(ctx, type, identifier);
    if (syntactic_rule_or_0xffff == 0xffff)
        grammar->rules[i].syntactic_rule = i;
    else
        grammar->rules[i].syntactic_rule = syntactic_rule_or_0xffff;

    struct boundary_states boundary = { .entry = 0, .exit = 1 };
    automaton_set_start_state(grammar->rules[i].automaton, boundary.entry);
    automaton_mark_accepting_state(grammar->rules[i].automaton, boundary.exit);
    state_id next_state_id = 2;

    build_body_expression(ctx, i, expr, boundary, &next_state_id);
    determinize_minimize_rule(&grammar->rules[i]);

    return i;
}

static rule_id add_empty_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier)
{
    struct grammar *grammar = ctx->grammar;
    rule_id i = grammar->number_of_rules++;
    grammar->rules = grow_array(grammar->rules,
     &grammar->rules_allocated_bytes,
     grammar->number_of_rules * sizeof(struct rule));
    grammar->rules[i].type = type;
    grammar->rules[i].identifier = identifier;
    grammar->rules[i].choice_index = 0xffff;
    grammar->rules[i].automaton = calloc(1, sizeof(struct automaton));
    return i;
}

static void determinize_minimize_rule(struct rule *rule)
{
    struct automaton *nfa = rule->automaton;
    rule->automaton = calloc(1, sizeof(struct automaton));
    determinize_minimize(nfa, rule->automaton);
    free(nfa);
}

static void build_body_expression(struct context *ctx, rule_id rule,
 struct parsed_expr *expr, struct boundary_states b, state_id *next_state_id)
{
    struct automaton *automaton = ctx->grammar->rules[rule].automaton;
    struct bluebird_tree *tree = ctx->tree;
    switch (expr->type) {
    case PARSED_CHOICE: {
        struct parsed_expr choice = parsed_expr_get(tree, expr->operand);
        while (!choice.empty) {
            connect_expression(ctx, rule, &choice, b, next_state_id);
            choice = parsed_expr_next(choice);
        }
        break;
    }
    case PARSED_CONCATENATION: {
        struct parsed_expr term = parsed_expr_get(tree, expr->operand);
        struct boundary_states inner = { .entry = (*next_state_id)++ };
        automaton_add_transition(automaton, b.entry, inner.entry,
         SYMBOL_EPSILON);
        while (!term.empty) {
            inner.exit = (*next_state_id)++;
            build_body_expression(ctx, rule, &term, inner, next_state_id);
            inner.entry = inner.exit;
            term = parsed_expr_next(term);
        }
        automaton_add_transition(automaton, inner.exit, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_IDENT: {
        struct parsed_ident ident = parsed_ident_get(tree, expr->ident);
        struct parsed_ident name = parsed_ident_get(tree, expr->name);
        symbol_id symbol = ctx->next_symbol++;
        struct rule *r = &ctx->grammar->rules[rule];
        r = &ctx->grammar->rules[r->syntactic_rule];
        if (r->number_of_slots >= MAX_NUMBER_OF_SLOTS) {
            fprintf(stderr, "error: rules with more than %u references to "
             "other rules or tokens are currently unsupported.\n",
             MAX_NUMBER_OF_SLOTS);
            exit(-1);
        }
        uint32_t slot = r->number_of_slots++;
        r->slots = grow_array(r->slots, &r->slots_allocated_bytes,
         r->number_of_slots * sizeof(struct slot));
        r->slots[slot].identifier = symbol;

        size_t n;
        const char *id = bluebird_tree_get_identifier(tree,
         ident.identifier, &n);
        r->slots[slot].binding = find_binding(ctx, id, n);
        if (!name.empty) {
            r->slots[slot].name = bluebird_tree_get_identifier(tree,
             name.identifier, &r->slots[slot].name_length);
        } else {
            r->slots[slot].name = id;
            r->slots[slot].name_length = n;
        }
        automaton_add_transition(automaton, b.entry, b.exit, symbol);
        break;
    }
    case PARSED_LITERAL: {
        struct parsed_ident ident = parsed_ident_get(tree, expr->ident);
        symbol_id symbol = add_keyword_token(ctx, ident.identifier,
         TOKEN_NORMAL);
        automaton_add_transition(automaton, b.entry, b.exit, symbol);
        break;
    }
    case PARSED_EMPTY: {
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_PARENS: {
        struct parsed_expr parens = parsed_expr_get(tree, expr->expr);
        build_body_expression(ctx, rule, &parens, b, next_state_id);
        break;
    }
    case PARSED_BRACKETED: {
        struct parsed_expr bracket = parsed_expr_get(tree, expr->expr);
        symbol_id id = ctx->next_symbol++;
        rule_id bracketed = add_rule(ctx, BRACKETED_RULE, id, &bracket,
         ctx->grammar->rules[rule].syntactic_rule);
        struct rule *r = &ctx->grammar->rules[bracketed];
        r->name = ctx->grammar->rules[rule].name;
        // TODO: This should also be reworked to use an explicit token rule.
        // Or a semantic rather than syntactic restriction?
        r->start_symbol = add_keyword_token(ctx,
         parsed_ident_get(tree, expr->left).identifier, TOKEN_START);
        r->end_symbol = add_keyword_token(ctx,
         parsed_ident_get(tree, expr->right).identifier, TOKEN_END);
        automaton_add_transition(automaton, b.entry, b.exit, id);
        break;
    }
    case PARSED_ZERO_OR_MORE: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        struct boundary_states in = connect_expression(ctx, rule, &operand, b,
         next_state_id);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_ONE_OR_MORE: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        struct boundary_states in = connect_expression(ctx, rule, &operand, b,
         next_state_id);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        break;
    }
    case PARSED_OPTIONAL: {
        struct parsed_expr operand = parsed_expr_get(tree, expr->operand);
        connect_expression(ctx, rule, &operand, b, next_state_id);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    default:
        abort();
    }
}

static symbol_id add_binding(struct context *ctx, const char *str, size_t len)
{
    symbol_id id = ctx->grammar->number_of_bindings++;
    ctx->grammar->bindings = grow_array(ctx->grammar->bindings,
     &ctx->grammar->bindings_allocated_bytes,
     ctx->grammar->number_of_bindings * sizeof(struct binding));
    ctx->grammar->bindings[id].name = str;
    ctx->grammar->bindings[id].length = len;
    return id;
}

static symbol_id find_binding(struct context *ctx, const char *str, size_t len)
{
    // Don't return keyword tokens.  We have another function to find those.
    for (uint32_t i = 0; i < ctx->grammar->first_keyword_token_binding; ++i) {
        struct binding *binding = &ctx->grammar->bindings[i];
        if (binding->length != len)
            continue;
        if (memcmp(binding->name, str, len))
            continue;
        return i;
    }
    return SYMBOL_EPSILON;
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
    }
}

static symbol_id add_keyword_token(struct context *ctx, uint32_t identifier,
 enum token_type type)
{
    size_t keyword_length = 0;
    const char *keyword = bluebird_tree_get_identifier(ctx->tree, identifier,
     &keyword_length);
    // Check whether we already added this token -- if so, return its symbol.
    for (uint32_t i = ctx->grammar->first_keyword_token_binding;
     i < ctx->grammar->number_of_bindings; ++i) {
        struct binding *binding = &ctx->grammar->bindings[i];
        if (binding->length != keyword_length)
            continue;
        if (memcmp(binding->name, keyword, keyword_length))
            continue;
        if (binding->type != type) {
            // TODO: Show location in original grammar text.
            fprintf(stderr, "error: token '%.*s' can't be used both as %s and "
             "%s keyword\n", (int)keyword_length, keyword,
             token_type_string(binding->type), token_type_string(type));
            exit(-1);
        }
        return i;
    }
    symbol_id id = add_binding(ctx, keyword, keyword_length);
    assert(id >= ctx->grammar->first_keyword_token_binding);
    ctx->grammar->bindings[id].type = type;
    return id;
}

static struct boundary_states connect_expression(struct context *ctx,
 rule_id rule, struct parsed_expr *expr, struct boundary_states outer,
 state_id *next_state_id)
{
    struct boundary_states inner;
    inner.entry = (*next_state_id)++;
    inner.exit = (*next_state_id)++;
    build_body_expression(ctx, rule, expr, inner, next_state_id);
    struct automaton *a = ctx->grammar->rules[rule].automaton;
    automaton_add_transition(a, outer.entry, inner.entry, SYMBOL_EPSILON);
    automaton_add_transition(a, inner.exit, outer.exit, SYMBOL_EPSILON);
    return inner;
}
