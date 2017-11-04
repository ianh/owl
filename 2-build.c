#include "2-build.h"

#include "4-determinize.h"
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
 symbol_id identifier, struct parsed_expr *expr);
static rule_id add_empty_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier);
static void determinize_minimize_rule(struct rule *rule);

static void build_body_expression(struct context *ctx, rule_id rule,
 struct parsed_expr *expr, struct boundary_states boundary,
 state_id *next_state_id);
static symbol_id add_keyword_token(struct context *ctx, uint32_t identifier,
 enum keyword_type type);
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
        .next_symbol = bluebird_tree_next_identifier(tree),
    };
    grammar->identifier_symbol = context.next_symbol++;
    grammar->number_symbol = context.next_symbol++;
    grammar->string_symbol = context.next_symbol++;
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
        struct parsed_body body = parsed_body_get(tree, rule.body);
        if (!body.ident) {
            // This is a single-choice rule, so we just add it directly.
            struct parsed_expr expr = parsed_expr_get(tree, body.expr);
            rule_id i = add_rule(&context, NAMED_RULE, name, &expr);
            grammar->rules[i].name = name;
            rule = parsed_rule_next(rule);
            continue;
        }

        // This is a multiple-choice rule.  We make separate rules for each
        // choice, them combine them into a single named rule.
        rule_id combined_rule = add_empty_rule(&context, NAMED_RULE, name);
        grammar->rules[combined_rule].name = name;
        struct automaton *combined_automaton;
        combined_automaton = grammar->rules[combined_rule].automaton;
        struct boundary_states combined_boundary = { .entry = 0, .exit = 1 };
        automaton_set_start_state(combined_automaton, combined_boundary.entry);

        // Create a rule for each choice.
        struct parsed_expr expr = parsed_expr_get(tree, body.expr);
        struct parsed_ident choice = parsed_ident_get(tree, body.ident);
        while (!expr.empty) {
            symbol_id id = context.next_symbol++;
            rule_id i = add_rule(&context, CHOICE_RULE, id, &expr);
            grammar->rules[i].name = name;
            grammar->rules[i].choice_name = choice.identifier;
            automaton_add_transition(combined_automaton,
             combined_boundary.entry, combined_boundary.exit, id);
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
                rule_id i = add_rule(&context, OPERATOR_RULE, op_id, &op_expr);
                struct rule *r = &grammar->rules[i];
                r->name = name;
                r->choice_name = op_choice.identifier;
                r->fixity = rule_fixity;
                r->associativity = rule_associativity;
                r->precedence = precedence;
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

        automaton_mark_accepting_state(combined_automaton,
         combined_boundary.exit);
        determinize_minimize_rule(&grammar->rules[combined_rule]);

        rule = parsed_rule_next(rule);
    }
}

static rule_id add_rule(struct context *ctx, enum rule_type type,
 symbol_id identifier, struct parsed_expr *expr)
{
    struct grammar *grammar = ctx->grammar;
    rule_id i = add_empty_rule(ctx, type, identifier);

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
        symbol_id symbol = ident.identifier;
        // TODO: Identifying what kind of token this is should be done at parse
        // time (with identifier, number, etc. as keywords), not as a post-pass.
        // Two grammar rules: token (as a separate rule) and keyword (as a
        // choice in expr).
        size_t n;
        const char *id = bluebird_tree_get_identifier(tree, symbol, &n);
        if (n == strlen("identifier") && !memcmp(id, "identifier", n))
            symbol = ctx->grammar->identifier_symbol;
        else if (n == strlen("number") && !memcmp(id, "number", n))
            symbol = ctx->grammar->number_symbol;
        else if (n == strlen("string") && !memcmp(id, "string", n))
            symbol = ctx->grammar->string_symbol;
        if (!name.empty) {
            struct rule *r = &ctx->grammar->rules[rule];
            if (r->number_of_renames >= MAX_NUMBER_OF_RENAMES) {
                fprintf(stderr, "error: rules with more than %u references of "
                 "the form 'rule@name' are currently unsupported.\n",
                 MAX_NUMBER_OF_RENAMES);
                exit(-1);
            }
            uint32_t rename_index = r->number_of_renames++;
            r->renames = grow_array(r->renames, &r->renames_allocated_bytes,
             r->number_of_renames * sizeof(struct rename));
            r->renames[rename_index].name = name.identifier;
            r->renames[rename_index].original_name = symbol;
            symbol = name.identifier;
        }
        automaton_add_transition(automaton, b.entry, b.exit, symbol);
        break;
    }
    case PARSED_LITERAL: {
        struct parsed_ident ident = parsed_ident_get(tree, expr->ident);
        symbol_id symbol = add_keyword_token(ctx, ident.identifier,
         KEYWORD_NORMAL);
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
        rule_id bracketed = add_rule(ctx, BRACKETED_RULE, id, &bracket);
        struct rule *r = &ctx->grammar->rules[bracketed];
        r->name = ctx->grammar->rules[rule].name;
        // TODO: This should also be reworked to use an explicit token rule.
        // Or a semantic rather than syntactic restriction?
        r->start_symbol = add_keyword_token(ctx,
         parsed_ident_get(tree, expr->left).identifier, KEYWORD_START);
        r->end_symbol = add_keyword_token(ctx,
         parsed_ident_get(tree, expr->right).identifier, KEYWORD_END);
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

static const char *keyword_type_string(enum keyword_type type)
{
    switch (type) {
    case KEYWORD_NORMAL:
        return "a normal";
    case KEYWORD_START:
        return "a start";
    case KEYWORD_END:
        return "an end";
    }
}

static symbol_id add_keyword_token(struct context *ctx, uint32_t identifier,
 enum keyword_type type)
{
    size_t keyword_length = 0;
    const char *keyword = bluebird_tree_get_identifier(ctx->tree, identifier,
     &keyword_length);
    // Check whether we already added this token -- if so, return its symbol.
    for (uint32_t i = 0; i < ctx->grammar->number_of_keywords; ++i) {
        struct keyword other = ctx->grammar->keywords[i];
        if (other.length != keyword_length)
            continue;
        if (memcmp(other.keyword, keyword, keyword_length))
            continue;
        if (other.type != type) {
            // TODO: Show location in original grammar text.
            fprintf(stderr, "error: token '%.*s' can't be used both as %s and "
             "%s keyword\n", (int)keyword_length, keyword,
             keyword_type_string(other.type), keyword_type_string(type));
            exit(-1);
        }
        return other.symbol;
    }
    uint32_t id = ctx->grammar->number_of_keywords++;
    ctx->grammar->keywords = grow_array(ctx->grammar->keywords,
     &ctx->grammar->keywords_allocated_bytes,
     ctx->grammar->number_of_keywords * sizeof(struct keyword));
    ctx->grammar->keywords[id].symbol = ctx->next_symbol++;
    ctx->grammar->keywords[id].keyword = keyword;
    ctx->grammar->keywords[id].length = keyword_length;
    ctx->grammar->keywords[id].type = type;
    return ctx->grammar->keywords[id].symbol;
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
