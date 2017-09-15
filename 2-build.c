#include "2-build.h"

#include "5-determinize.h"
#include "grow-array.h"

#include <stdio.h>
#include <stdlib.h>

struct boundary_states;

struct context {
    struct grammar *grammar;
    struct bluebird_tree *tree;

    symbol_id next_symbol;
};

static rule_id add_rule(struct context *ctx, enum rule_type type,
 symbol_id name, struct parsed_expr *expr);

static void build_body_expression(struct context *ctx, rule_id rule,
 struct parsed_expr *expr, struct boundary_states boundary,
 state_id *next_state_id);
static struct boundary_states connect_expression(struct context *ctx,
 rule_id rule, struct parsed_expr *expr, struct boundary_states outer,
 state_id *next_state_id);

static enum fixity fixity_for_parsed_fixity(struct parsed_fixity *);
static enum associativity associativity_for_parsed_assoc(struct parsed_assoc *);

void build(struct grammar *grammar, struct bluebird_tree *tree)
{
    struct context context = {
        .grammar = grammar,
        .tree = tree,
        .next_symbol = bluebird_tree_next_identifier(tree),
    };
    parsed_id root = bluebird_tree_root(tree);
    struct parsed_grammar g = parsed_grammar_get(tree, root);
    struct parsed_rule rule = parsed_rule_get(tree, g.rule);
    while (!rule.empty) {
        if (grammar->number_of_rules >= MAX_NUMBER_OF_RULES) {
            fprintf(stderr, "error: grammars with more than %u rules are "
             "currently unsupported.\n", MAX_NUMBER_OF_RULES);
            exit(-1);
        }
        symbol_id name = parsed_ident_get(tree, rule.ident).identifier;

        // 1. Create rules from the syntactic rule itself.  Each choice creates
        //    a separate rule.
        struct parsed_body body = parsed_body_get(tree, rule.body);
        struct parsed_expr expr = parsed_expr_get(tree, body.expr);
        struct parsed_ident choice = parsed_ident_get(tree, body.ident);
        while (!expr.empty) {
            rule_id i = add_rule(&context, NAMED_RULE, name, &expr);
            if (!choice.empty) {
                grammar->rules[i].choice_name = choice.identifier;
                choice = parsed_ident_next(choice);
            }
            expr = parsed_expr_next(expr);
        }

        // 2. Create rules from each operator clause.
        struct parsed_operators ops;
        ops = parsed_operators_get(tree, body.operators);
        int32_t precedence = -1;
        while (!ops.empty) {
            struct parsed_fixity fixity = parsed_fixity_get(tree, ops.fixity);
            enum fixity rule_fixity = fixity_for_parsed_fixity(&fixity);
            struct parsed_assoc assoc = parsed_assoc_get(tree, fixity.assoc);
            enum associativity rule_associativity = 0;
            if (!assoc.empty)
                rule_associativity = associativity_for_parsed_assoc(&assoc);
            struct parsed_expr op_expr = parsed_expr_get(tree, ops.expr);
            struct parsed_ident op_choice = parsed_ident_get(tree, ops.ident);
            while (!op_expr.empty) {
                rule_id i = add_rule(&context, OPERATOR_RULE, name, &op_expr);
                struct rule *r = &grammar->rules[i];
                r->choice_name = op_choice.identifier;
                r->fixity = rule_fixity;
                r->associativity = rule_associativity;
                r->precedence = precedence;
                op_expr = parsed_expr_next(op_expr);
                op_choice = parsed_ident_next(op_choice);
            }
            precedence--;
            ops = parsed_operators_next(ops);
        }

        rule = parsed_rule_next(rule);
    }
}

struct boundary_states {
    state_id entry;
    state_id exit;
};

static rule_id add_rule(struct context *ctx, enum rule_type type,
 symbol_id name, struct parsed_expr *expr)
{
    struct grammar *grammar = ctx->grammar;
    rule_id i = grammar->number_of_rules++;
    grammar->rules = grow_array(grammar->rules,
     &grammar->rules_allocated_bytes,
     grammar->number_of_rules * sizeof(struct rule));
    grammar->rules[i].type = type;
    grammar->rules[i].name = name;
    struct boundary_states boundary = { .entry = 0, .exit = 1 };
    state_id next_state_id = 2;
    struct automaton nfa = { .start_state = boundary.entry };
    automaton_mark_accepting_state(&nfa, boundary.exit);
    grammar->rules[i].automaton = &nfa;
    build_body_expression(ctx, i, expr, boundary, &next_state_id);
    grammar->rules[i].automaton = calloc(1, sizeof(struct automaton));
    determinize_minimize(&nfa, grammar->rules[i].automaton);
    return i;
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
        symbol_id symbol = ident.identifier;
        if (!name.empty) {
            struct rule *r = &ctx->grammar->rules[rule];
            uint32_t rename_index = r->number_of_renames++;
            r->renames = grow_array(r->renames, &r->renames_allocated_bytes,
             r->number_of_renames);
            r->renames[rename_index].name = name.identifier;
            r->renames[rename_index].original_name = ident.identifier;
            symbol = name.identifier;
        }
        automaton_add_transition(automaton, b.entry, b.exit, symbol);
        break;
    }
    case PARSED_LITERAL: {
        struct parsed_ident ident = parsed_ident_get(tree, expr->ident);
        automaton_add_transition(automaton, b.entry, b.exit, ident.identifier);
        break;
    }
    case PARSED_PARENS: {
        struct parsed_expr parens = parsed_expr_get(tree, expr->expr);
        build_body_expression(ctx, rule, &parens, b, next_state_id);
        break;
    }
    case PARSED_BRACKETED: {
        struct parsed_expr bracket = parsed_expr_get(tree, expr->expr);
        symbol_id name = ctx->next_symbol++;
        rule_id bracketed = add_rule(ctx, BRACKETED_RULE, name, &bracket);
        struct rule *r = &ctx->grammar->rules[bracketed];
        r->start_token = parsed_ident_get(tree, expr->left).identifier;
        r->end_token = parsed_ident_get(tree, expr->right).identifier;
        automaton_add_transition(automaton, b.entry, b.exit, name);
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

static enum fixity fixity_for_parsed_fixity(struct parsed_fixity *f)
{
    switch (f->type) {
    case PARSED_PREFIX:
        return PREFIX;
    case PARSED_POSTFIX:
        return POSTFIX;
    case PARSED_INFIX:
        return INFIX;
    default:
        abort();
    }
}

static enum associativity associativity_for_parsed_assoc(struct parsed_assoc *a)
{
    switch (a->type) {
    case PARSED_LEFT:
        return LEFT;
    case PARSED_RIGHT:
        return RIGHT;
    case PARSED_FLAT:
        return FLAT;
    case PARSED_NONASSOC:
        return NONASSOC;
    default:
        abort();
    }
}
