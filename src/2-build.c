#include "2-build.h"

#include "5-determinize.h"
#include "alloc.h"
#include "grow-array.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct context {
    struct grammar *grammar;
    struct grammar_version version;
    struct owl_tree *tree;

    uint32_t rule_index;
    symbol_id next_symbol;
    state_id next_state;

    size_t bracket_nesting;
    size_t expression_nesting;
};

// Limit nesting depth to avoid stack overflow.
#define MAX_EXPRESSION_NESTING 1000

struct boundary_states;
static void build_body_automaton(struct context *ctx,
 struct automaton *automaton, struct owl_ref expr_ref);
static void build_body_expression(struct context *ctx,
 struct automaton *automaton, struct owl_ref expr_ref,
 struct boundary_states boundary);
static struct boundary_states connect_expression(struct context *ctx,
 struct automaton *a, struct owl_ref ref, struct boundary_states outer);

static uint32_t add_choice(struct context *ctx, struct rule *rule,
 struct owl_ref identifier, struct source_range expr_range);

static uint32_t add_slot(struct context *ctx, struct rule *rule,
 const char *slot_name, size_t slot_name_length, uint32_t referenced_rule_index,
 struct source_range range, const char *error_reason);

static symbol_id add_keyword_token(struct context *ctx, struct rule *rule,
 struct owl_ref string_ref, enum token_type type);

static uint32_t add_rule(struct context *ctx, const char *name, size_t len);
static bool add_token_rule(struct context *ctx, enum rule_token_type type,
 uint32_t *index, struct parsed_identifier ident);
static uint32_t find_rule(struct context *ctx, const char *name, size_t len);

static void check_version(struct grammar_version version,
 enum version_capability capability, struct source_range range);

void build(struct grammar *grammar, struct owl_tree *tree,
 struct grammar_version version)
{
    struct context context = {
        .grammar = grammar,
        .version = version,
        .tree = tree,
    };
    struct parsed_grammar g = owl_tree_get_parsed_grammar(tree);

    // First, create rule structs for each rule in the grammar.  We have to do
    // this in a separate pass so we can look up names before they appear in the
    // parse tree.
    grammar->root_rule = 0;
    struct owl_ref r;
    for (r = g.rule; !r.empty; r = owl_next(r)) {
        struct parsed_rule parsed_rule = parsed_rule_get(r);
        struct parsed_identifier name =
         parsed_identifier_get(parsed_rule.identifier);
        uint32_t index = add_rule(&context, name.identifier, name.length);
        if (index == UINT32_MAX) {
            errorf("there are multiple rules named '%.*s'", (int)name.length,
             name.identifier);
            uint32_t other = find_rule(&context, name.identifier, name.length);
            error.ranges[0] = grammar->rules[other]->name_range;
            error.ranges[1] = name.range;
            exit_with_error();
        }
        grammar->rules[index]->name_range = name.range;
    }
    if (grammar->number_of_rules == 0) {
        errorf("an owl grammar needs at least one rule of the form "
         "'rule_name = ...'");
        exit_with_error();
    }

    // Add custom token rules.
    for (struct owl_ref t = g.custom_token; !t.empty; t = owl_next(t)) {
        struct parsed_custom_token token = parsed_custom_token_get(t);
        check_version(version, CUSTOM_TOKENS, token.range);
        struct parsed_identifier name = parsed_identifier_get(token.identifier);
        uint32_t index = add_rule(&context, name.identifier, name.length);
        if (index == UINT32_MAX) {
            errorf("the rule '%.*s' has the same name as a token",
             (int)name.length, name.identifier);
            uint32_t other = find_rule(&context, name.identifier, name.length);
            error.ranges[0] = grammar->rules[other]->name_range;
            error.ranges[1] = name.range;
            exit_with_error();
        }
        struct rule *rule = context.grammar->rules[index];
        rule->is_token = true;
        rule->token_type = RULE_TOKEN_CUSTOM;
        rule->name_range = name.range;
        for (struct owl_ref s = token.string; !s.empty; s = owl_next(s)) {
            struct parsed_string string = parsed_string_get(s);
            if (string.length == 0) {
                error.ranges[0] = string.range;
                exit_with_errorf("token exemplar strings can't be empty");
            }
            uint32_t index = rule->number_of_token_exemplars++;
            if (index == UINT32_MAX)
                abort();
            rule->token_exemplars = grow_array(rule->token_exemplars,
             &rule->token_exemplars_allocated_bytes,
             rule->number_of_token_exemplars * sizeof(struct token));
            rule->token_exemplars[index].string = string.string;
            rule->token_exemplars[index].length = string.length;
            rule->token_exemplars[index].range = string.range;
        }
    }

    // Now we fill in the choices for each rule.  We need to do this in a
    // separate pass in case there are "exception" specifiers which exclude
    // certain choices.
    uint32_t rule_index = 0;
    for (r = g.rule; !r.empty; r = owl_next(r), rule_index++) {
        struct parsed_rule parsed_rule = parsed_rule_get(r);
        assert(rule_index < grammar->number_of_rules);
        struct rule *rule = grammar->rules[rule_index];
        struct parsed_body body = parsed_body_get(parsed_rule.body);
        if (body.identifier.empty) {
            // This is a simple rule with no choices -- there's nothing we need
            // to do here.
            continue;
        }

        // This rule has multiple choices.  Add them to the rule as choice
        // structs.
        while (!body.expr.empty) {
            if (rule->number_of_choices >= MAX_NUMBER_OF_CHOICES) {
                errorf("rules with more than %u choice clauses are currently "
                 "unsupported", MAX_NUMBER_OF_CHOICES);
                error.ranges[0] = rule->name_range;
                exit_with_error();
            }
            struct parsed_expr expr = parsed_expr_get(body.expr);
            add_choice(&context, rule, body.identifier, expr.range);
            body.expr = owl_next(body.expr);
            body.identifier = owl_next(body.identifier);
        }

        // Create operator structs from each operator.
        rule->first_operator_choice = rule->number_of_choices;
        int32_t precedence = -1;
        while (!body.operators.empty) {
            struct parsed_operators ops = parsed_operators_get(body.operators);
            // First, unpack the fixity and associativity from the parse tree.
            struct parsed_fixity fixity = parsed_fixity_get(ops.fixity);
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
            enum associativity rule_associativity = 0;
            if (!fixity.assoc.empty) {
                switch (parsed_assoc_get(fixity.assoc).type) {
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
            while (!ops.operator.empty) {
                struct parsed_operator op = parsed_operator_get(ops.operator);
                if (rule->number_of_choices >= MAX_NUMBER_OF_CHOICES) {
                    errorf("rules with more than %u choice clauses are "
                     "currently unsupported", MAX_NUMBER_OF_CHOICES);
                    error.ranges[0] = rule->name_range;
                    exit_with_error();
                }
                uint32_t operator = add_choice(&context, rule, op.identifier,
                 parsed_expr_get(op.expr).range);
                rule->choices[operator].fixity = rule_fixity;
                rule->choices[operator].associativity = rule_associativity;
                rule->choices[operator].precedence = precedence;
                ops.operator = owl_next(ops.operator);
            }
            // Each new 'operators' section has a lower precedence than the
            // previous one.
            precedence--;
            body.operators = owl_next(body.operators);
        }
    }

    // Now fill in the automata according to the contents of each parsed rule.
    rule_index = 0;
    for (r = g.rule; !r.empty; r = owl_next(r), rule_index++) {
        struct parsed_rule parsed_rule = parsed_rule_get(r);
        struct rule *rule = grammar->rules[rule_index];

        // Store the rule index in our context object so we don't have to pass
        // it around everywhere while we're building the rule's automata.
        context.rule_index = rule_index;
        context.next_symbol = 0;

        struct parsed_body body = parsed_body_get(parsed_rule.body);
        if (body.identifier.empty) {
            // This is a simple rule with no choices.  Create the automaton
            // directly.
            build_body_automaton(&context, &rule->automaton, body.expr);
            continue;
        }

        // Fill in the choice automata...
        uint32_t choice_index = 0;
        for (; !body.expr.empty; body.expr = owl_next(body.expr)) {
            struct choice *choice = &rule->choices[choice_index++];
            build_body_automaton(&context, &choice->automaton, body.expr);
        }

        // ...and the operator automata.
        while (!body.operators.empty) {
            struct parsed_operators ops = parsed_operators_get(body.operators);
            while (!ops.operator.empty) {
                struct parsed_operator op = parsed_operator_get(ops.operator);
                struct choice *operator = &rule->choices[choice_index++];
                build_body_automaton(&context, &operator->automaton, op.expr);
                ops.operator = owl_next(ops.operator);
            }
            body.operators = owl_next(body.operators);
        }

        // Add slots for operands -- 'left'/'right' for infix operators, and
        // 'operand' for prefix and postfix operators.
        uint32_t i = rule->first_operator_choice;
        for (; i < rule->number_of_choices; ++i) {
            struct choice *operator = &rule->choices[i];
            char buf[256];
            if (operator->fixity == INFIX && operator->associativity != FLAT) {
                snprintf(buf, sizeof(buf), "is reserved for the left operand "
                 "of operator '%.*s'", (int)operator->name_length,
                 operator->name);
                rule->left_slot_index = add_slot(&context, rule, "left",
                 strlen("left"), rule_index, (struct source_range){0}, buf);
                snprintf(buf, sizeof(buf), "is reserved for the right operand "
                 "of operator '%.*s'", (int)operator->name_length,
                 operator->name);
                rule->right_slot_index = add_slot(&context, rule, "right",
                 strlen("right"), rule_index, (struct source_range){0}, buf);
            } else {
                snprintf(buf, sizeof(buf), "is reserved for the operand of "
                 "operator '%.*s'", (int)operator->name_length, operator->name);
                rule->operand_slot_index = add_slot(&context, rule, "operand",
                 strlen("operand"), rule_index, (struct source_range){0}, buf);
            }
        }
    }

    // Add whitespace tokens.
    bool specified_whitespace = false;
    while (!g.whitespace.empty) {
        specified_whitespace = true;
        struct parsed_whitespace w = parsed_whitespace_get(g.whitespace);
        check_version(version, WHITESPACE, w.range);
        while (!w.string.empty) {
            struct parsed_string s = parsed_string_get(w.string);
            if (s.length == 0) {
                error.ranges[0] = s.range;
                exit_with_errorf("whitespace can't be empty");
            }
            w.string = owl_next(w.string);
            uint32_t token_index = find_token(grammar->whitespace_tokens,
             grammar->number_of_whitespace_tokens, s.string, s.length,
             TOKEN_WHITESPACE, &s.range);
            if (token_index >= grammar->number_of_whitespace_tokens) {
                if (token_index == UINT32_MAX)
                    abort();
                grammar->number_of_whitespace_tokens = token_index + 1;
                grammar->whitespace_tokens =
                 grow_array(grammar->whitespace_tokens,
                 &grammar->whitespace_tokens_allocated_bytes,
                 sizeof(struct token) * grammar->number_of_whitespace_tokens);
                grammar->whitespace_tokens[token_index] = (struct token){
                    .string = s.string,
                    .length = s.length,
                    .type = TOKEN_WHITESPACE,
                    .range = s.range
                };
            } else {
                error.ranges[0] = grammar->whitespace_tokens[token_index].range;
                error.ranges[1] = s.range;
                exit_with_errorf("this whitespace was specified twice");
            }
        }
        g.whitespace = owl_next(g.whitespace);
    }

    // Use default whitespace if none is specified.
    if (!specified_whitespace) {
        struct token tokens[] = {
            { .string = " ", .length = 1, .type = TOKEN_WHITESPACE },
            { .string = "\t", .length = 1, .type = TOKEN_WHITESPACE },
            { .string = "\n", .length = 1, .type = TOKEN_WHITESPACE },
            { .string = "\r", .length = 1, .type = TOKEN_WHITESPACE },
        };
        grammar->whitespace_tokens = grow_array(grammar->whitespace_tokens,
         &grammar->whitespace_tokens_allocated_bytes, sizeof(tokens));
        memcpy(grammar->whitespace_tokens, tokens, sizeof(tokens));
        grammar->number_of_whitespace_tokens = sizeof(tokens)/sizeof(tokens[0]);
    }

    // Finally, add any comment tokens to the grammar.
    while (!g.comment_token.empty) {
        struct parsed_comment_token t =
         parsed_comment_token_get(g.comment_token);
        struct owl_ref str = t.string;
        if (!strcmp(version.string, "owl.v1")) {
            if (t.comment_token_v1.empty) {
                error.ranges[0] = t.range;
                error.ranges[1] = version.range;
                exit_with_errorf(".line-comment-token was added in owl.v2");
            }
            str = parsed_comment_token_v1_get(t.comment_token_v1).string;
        } else if (str.empty) {
            error.ranges[0] = t.range;
            error.ranges[1] = version.range;
            exit_with_errorf("line-comment-token has been removed; use "
             ".line-comment-token instead");
        }
        struct parsed_string s = parsed_string_get(str);
        if (s.length == 0) {
            error.ranges[0] = s.range;
            exit_with_errorf("comment tokens can't be empty");
        }
        uint32_t token_index = find_token(grammar->comment_tokens,
         grammar->number_of_comment_tokens, s.string, s.length,
         TOKEN_START_LINE_COMMENT, &s.range);
        if (token_index >= grammar->number_of_comment_tokens) {
            if (token_index == UINT32_MAX)
                abort();
            grammar->number_of_comment_tokens = token_index + 1;
            grammar->comment_tokens = grow_array(grammar->comment_tokens,
             &grammar->comment_tokens_allocated_bytes,
             sizeof(struct token) * grammar->number_of_comment_tokens);
            grammar->comment_tokens[token_index] = (struct token){
                .string = s.string,
                .length = s.length,
                .type = TOKEN_START_LINE_COMMENT,
                .range = s.range
            };
        } else {
            error.ranges[0] = grammar->comment_tokens[token_index].range;
            error.ranges[1] = s.range;
            exit_with_errorf("the same comment token was specified twice");
        }
        g.comment_token = owl_next(g.comment_token);
    }
}

struct boundary_states {
    state_id entry;
    state_id exit;
};

static void build_body_automaton(struct context *ctx,
 struct automaton *out_automaton, struct owl_ref expr_ref)
{
    struct automaton automaton = {0};
    struct boundary_states boundary = { .entry = 0, .exit = 1 };
    automaton_set_start_state(&automaton, boundary.entry);
    automaton_mark_accepting_state(&automaton, boundary.exit);

    struct context saved_context = *ctx;
    ctx->next_state = 2;
    build_body_expression(ctx, &automaton, expr_ref, boundary);
    ctx->next_state = saved_context.next_state;

    determinize_minimize(&automaton, out_automaton);
    automaton_destroy(&automaton);
}

static void build_body_expression(struct context *ctx,
 struct automaton *automaton, struct owl_ref expr_ref,
 struct boundary_states b)
{
    struct parsed_expr expr = parsed_expr_get(expr_ref);
    if (ctx->expression_nesting++ > MAX_EXPRESSION_NESTING) {
        error.ranges[0] = expr.range;
        exit_with_errorf("operators are nested too deeply");
    }
    struct rule *rule = ctx->grammar->rules[ctx->rule_index];
    switch (expr.type) {
    case PARSED_CHOICE:
        for (; !expr.operand.empty; expr.operand = owl_next(expr.operand))
            connect_expression(ctx, automaton, expr.operand, b);
        break;
    case PARSED_CONCATENATION: {
        struct boundary_states inner = { .entry = ctx->next_state++ };
        automaton_add_transition(automaton, b.entry, inner.entry,
         SYMBOL_EPSILON);
        for (;!expr.operand.empty; expr.operand = owl_next(expr.operand)) {
            inner.exit = ctx->next_state++;
            build_body_expression(ctx, automaton, expr.operand, inner);
            inner.entry = inner.exit;
        }
        automaton_add_transition(automaton, inner.exit, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_IDENT: {
        struct parsed_identifier ident = parsed_identifier_get(expr.identifier);
        const char *rule_name = ident.identifier;
        size_t rule_name_length = ident.length;
        const char *slot_name = rule_name;
        size_t slot_name_length = rule_name_length;
        if (!expr.rename.empty) {
            struct parsed_identifier name = parsed_identifier_get(expr.rename);
            slot_name = name.identifier;
            slot_name_length = name.length;
        }
        uint32_t rule_index = find_rule(ctx, rule_name, rule_name_length);
        if (rule_index == UINT32_MAX) {
            if (!add_token_rule(ctx, RULE_TOKEN_IDENTIFIER, &rule_index, ident)
             && !add_token_rule(ctx, RULE_TOKEN_INTEGER, &rule_index, ident)
             && !add_token_rule(ctx, RULE_TOKEN_NUMBER, &rule_index, ident)
             && !add_token_rule(ctx, RULE_TOKEN_STRING, &rule_index, ident)) {
                errorf("unknown rule or token");
                error.ranges[0] = ident.range;
                exit_with_error();
            }
        }
        struct rule *referent = ctx->grammar->rules[rule_index];
        if (ctx->bracket_nesting == 0 && rule_index <= ctx->rule_index) {
            if (rule_index == ctx->rule_index) {
                errorf("outside of guard brackets [ ], the rule '%.*s' cannot "
                 "refer to itself", (int)rule->name_length, rule->name);
            } else {
                errorf("outside of guard brackets [ ], the rule '%.*s' cannot "
                 "refer to the earlier rule '%.*s'", (int)rule->name_length,
                 rule->name, (int)rule_name_length, rule_name);
            }
            error.ranges[0] = referent->name_range;
            error.ranges[1] = ident.range;
            exit_with_error();
        }
        uint32_t slot_index = add_slot(ctx, rule, slot_name, slot_name_length,
         rule_index, expr.range, "could refer to two different rules");

        // Collect exceptions (if applicable) and find the proper choice set.
        struct bitset excluded =
         bitset_create_empty(referent->number_of_choices);
        while (!expr.exception.empty) {
            struct parsed_identifier exception =
             parsed_identifier_get(expr.exception);
            bool found = false;
            for (uint32_t i = 0; i < referent->number_of_choices; ++i) {
                if (exception.length == referent->choices[i].name_length &&
                 !memcmp(exception.identifier, referent->choices[i].name,
                 exception.length)) {
                     bitset_add(&excluded, i);
                     found = true;
                     break;
                }
            }
            if (!found) {
                error.ranges[0] = exception.range;
                exit_with_errorf("'%.*s' is not the name of a choice clause "
                 "for '%.*s'", (int)exception.length, exception.identifier,
                 (int)referent->name_length, referent->name);
            }
            expr.exception = owl_next(expr.exception);
        }
        struct slot *slot = &rule->slots[slot_index];
        uint32_t i = 0;
        for (; i < slot->number_of_choice_sets; ++i) {
            if (bitset_compare(&excluded,
             &slot->choice_sets[i].excluded_choices) == 0)
                break;
        }
        if (i >= slot->number_of_choice_sets) {
            if (i == UINT32_MAX)
                abort();
            slot->number_of_choice_sets = i + 1;
            slot->choice_sets = grow_array(slot->choice_sets,
             &slot->choice_sets_allocated_bytes,
             sizeof(struct slot_choice_set) * slot->number_of_choice_sets);
            slot->choice_sets[i].excluded_choices = bitset_move(&excluded);
            slot->choice_sets[i].symbol = ctx->next_symbol++;
        } else
            bitset_destroy(&excluded);
        automaton_add_transition(automaton, b.entry, b.exit,
         rule->slots[slot_index].choice_sets[i].symbol);
        break;
    }
    case PARSED_LITERAL:
        automaton_add_transition(automaton, b.entry, b.exit,
         add_keyword_token(ctx, rule, expr.string, TOKEN_NORMAL));
        break;
    case PARSED_PARENS:
        build_body_expression(ctx, automaton, expr.expr, b);
        break;
    case PARSED_BRACKETED: {
        if (rule->number_of_brackets == UINT32_MAX)
            abort();
        uint32_t bracket_index = rule->number_of_brackets++;
        rule->brackets = grow_array(rule->brackets,
         &rule->brackets_allocated_bytes,
         sizeof(struct bracket) * rule->number_of_brackets);
        // We can't write directly into the automaton in the bracket struct
        // because more brackets could be created inside build_body_automaton,
        // invalidating the pointer.  Write into a stack value instead, then
        // move its contents into place.
        struct bracket *bracket = &rule->brackets[bracket_index];
        if (expr.expr.empty) {
            automaton_set_start_state(&bracket->automaton, 0);
            automaton_mark_accepting_state(&bracket->automaton, 0);
        } else {
            struct automaton bracket_automaton = {0};
            ctx->bracket_nesting++;
            build_body_automaton(ctx, &bracket_automaton, expr.expr);
            ctx->bracket_nesting--;
            // Update the bracket pointer in case it was invalidated.
            bracket = &rule->brackets[bracket_index];
            automaton_move(&bracket_automaton, &bracket->automaton);
            automaton_destroy(&bracket_automaton);
        }
        bracket->symbol = ctx->next_symbol++;
        bracket->start_symbol = add_keyword_token(ctx, rule, expr.begin_token,
         TOKEN_START);
        bracket->end_symbol = add_keyword_token(ctx, rule, expr.end_token,
         TOKEN_END);
        if (bracket->start_symbol == SYMBOL_EPSILON) {
            errorf("'' is not a valid start keyword");
            error.ranges[0] = parsed_string_get(expr.begin_token).range;
            exit_with_error();
        }
        if (bracket->end_symbol == SYMBOL_EPSILON) {
            errorf("'' is not a valid end keyword");
            error.ranges[0] = parsed_string_get(expr.end_token).range;
            exit_with_error();
        }
        automaton_add_transition(automaton, b.entry, b.exit, bracket->symbol);
        break;
    }
    case PARSED_ZERO_OR_MORE: {
        struct boundary_states in =
         connect_expression(ctx, automaton, expr.operand, b);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    }
    case PARSED_ONE_OR_MORE: {
        struct boundary_states in
         = connect_expression(ctx, automaton, expr.operand, b);
        automaton_add_transition(automaton, in.exit, in.entry, SYMBOL_EPSILON);
        break;
    }
    case PARSED_OPTIONAL:
        connect_expression(ctx, automaton, expr.operand, b);
        automaton_add_transition(automaton, b.entry, b.exit, SYMBOL_EPSILON);
        break;
    case PARSED_REPETITION: {
        check_version(ctx->version, EXPLICIT_REPETITION, expr.range);
        uint64_t min = 0;
        uint64_t max = 1;
        bool infinite = true;
        if (!expr.repetition.empty) {
            struct parsed_repetition repetition;
            repetition = parsed_repetition_get(expr.repetition);
            min = parsed_integer_get(repetition.begin).integer;
            switch (repetition.type) {
            case PARSED_EXACT:
                max = min;
                infinite = false;
                break;
            case PARSED_RANGE:
                max = parsed_integer_get(repetition.end).integer;
                if (max < min) {
                   error.ranges[0] = parsed_integer_get(repetition.begin).range;
                   error.ranges[1] = parsed_integer_get(repetition.end).range;
                   exit_with_errorf("the maximum number of repetitions must "
                    "be greater than the minimum");
                }
                infinite = false;
                break;
            case PARSED_AT_LEAST:
                max = min + 1;
                break;
            default:
                abort();
            }
        }
        if (infinite && max < 2 && !expr.expr.empty) {
            // For expressions like list-item{','}, we need at least one loop
            // iteration with i > 0 in order to match delimiters.
            max = 2;
        }
        struct boundary_states inner = { .entry = ctx->next_state++ };
        automaton_add_transition(automaton, b.entry, inner.entry,
         SYMBOL_EPSILON);
        for (uint64_t i = 0; i < max; ++i) {
            state_id entry = inner.entry;
            if (i > 0 && !expr.expr.empty) {
                inner.exit = ctx->next_state++;
                build_body_expression(ctx, automaton, expr.expr, inner);
                inner.entry = inner.exit;
            }
            inner.exit = ctx->next_state++;
            build_body_expression(ctx, automaton, expr.operand, inner);
            inner.entry = inner.exit;
            if (i >= min) {
                automaton_add_transition(automaton, entry, b.exit,
                 SYMBOL_EPSILON);
            }
            if (infinite && i == max - 1) {
                automaton_add_transition(automaton, inner.exit, entry,
                 SYMBOL_EPSILON);
            }
        }
        automaton_add_transition(automaton, inner.exit, b.exit, SYMBOL_EPSILON);
        break;
    }
    default:
        abort();
    }
    ctx->expression_nesting--;
}

static struct boundary_states connect_expression(struct context *ctx,
 struct automaton *a, struct owl_ref ref, struct boundary_states outer)
{
    struct boundary_states inner;
    inner.entry = ctx->next_state++;
    inner.exit = ctx->next_state++;
    build_body_expression(ctx, a, ref, inner);
    automaton_add_transition(a, outer.entry, inner.entry, SYMBOL_EPSILON);
    automaton_add_transition(a, inner.exit, outer.exit, SYMBOL_EPSILON);
    return inner;
}

static uint32_t add_choice(struct context *ctx, struct rule *rule,
 struct owl_ref identifier, struct source_range expr_range)
{
    struct parsed_identifier ident = parsed_identifier_get(identifier);
    for (uint32_t i = 0; i < rule->number_of_choices; ++i) {
        if (rule->choices[i].name_length != ident.length)
            continue;
        if (memcmp(rule->choices[i].name, ident.identifier, ident.length))
            continue;
        errorf("there's already a choice named '%.*s'", (int)ident.length,
         ident.identifier);
        error.ranges[0] = rule->choices[i].expr_range;
        error.ranges[1] = rule->choices[i].name_range;
        error.ranges[2] = ident.range;
        exit_with_error();
    }
    uint32_t choice_index = rule->number_of_choices++;
    rule->choices = grow_array(rule->choices, &rule->choices_allocated_bytes,
     sizeof(struct choice) * rule->number_of_choices);
    struct choice *choice = &rule->choices[choice_index];
    choice->name = ident.identifier;
    choice->name_length = ident.length;
    choice->name_range = ident.range;
    choice->expr_range = expr_range;
    return choice_index;
}

static uint32_t add_slot(struct context *ctx, struct rule *rule,
 const char *slot_name, size_t slot_name_length, uint32_t referenced_rule_index,
 struct source_range range, const char *error_reason)
{
    static const char * const reserved[] = {
        // By the C standard.
        "auto", "else", "long", "switch", "break", "enum", "register",
        "typedef", "case", "extern", "return", "union", "char", "float",
        "short", "unsigned", "const", "for", "signed", "void", "continue",
        "goto", "sizeof", "volatile", "default", "if", "static", "while", "do",
        "int", "struct", "double", 0,

        // By Owl itself.
        "range", "type",
    };
    bool c_keyword = true;
    for (int i = 0; i < sizeof(reserved) / sizeof(reserved[0]); ++i) {
        if (reserved[i] == 0) {
            c_keyword = false;
            continue;
        }
        if (strncmp(reserved[i], slot_name, slot_name_length))
            continue;
        if (reserved[i][slot_name_length] != '\0')
            continue;
        errorf("the name '%s' is a reserved %s and cannot be used to refer to "
         "a rule or token", reserved[i], c_keyword ? "C keyword" :
         "field name");
        error.ranges[0] = range;
        exit_with_error();
    }
    uint32_t slot_index = 0;
    for (; slot_index < rule->number_of_slots; ++slot_index) {
        struct slot *slot = &rule->slots[slot_index];
        if (slot->name_length != slot_name_length)
            continue;
        if (memcmp(slot->name, slot_name, slot_name_length))
            continue;
        if (slot->rule_index != referenced_rule_index) {
            errorf("in the rule '%.*s', the name '%.*s' %s",
             (int)rule->name_length, rule->name, (int)slot_name_length,
             slot_name, error_reason);
            error.ranges[0] = slot->range;
            error.ranges[1] = range;
            exit_with_error();
        }
        break;
    }
    if (slot_index >= rule->number_of_slots) {
        symbol_id symbol = ctx->next_symbol++;
        if (rule->number_of_slots >= MAX_NUMBER_OF_SLOTS) {
            errorf("rules with more than %u references to other rules or "
             "tokens are currently unsupported", MAX_NUMBER_OF_SLOTS);
            error.ranges[0] = rule->name_range;
            error.ranges[1] = range;
            exit_with_error();
        }
        rule->number_of_slots = slot_index + 1;
        rule->slots = grow_array(rule->slots, &rule->slots_allocated_bytes,
         sizeof(struct slot) * rule->number_of_slots);
        struct slot *slot = &rule->slots[slot_index];
        slot->name = slot_name;
        slot->name_length = slot_name_length;
        slot->rule_index = referenced_rule_index;
        slot->range = range;
        slot->number_of_choice_sets = 1;
        slot->choice_sets = grow_array(slot->choice_sets,
         &slot->choice_sets_allocated_bytes,
         sizeof(struct slot_choice_set) * slot->number_of_choice_sets);
        slot->choice_sets[0].symbol = symbol;
        slot->choice_sets[0].excluded_choices = bitset_create_empty(ctx->
         grammar->rules[referenced_rule_index]->number_of_choices);
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
    case TOKEN_START_LINE_COMMENT:
        return "a comment";
    case TOKEN_WHITESPACE:
        return "a whitespace";
    default:
        return "";
    }
}

uint32_t find_token(struct token *tokens, uint32_t number_of_tokens,
 const char *string, size_t length, enum token_type type,
 struct source_range *range)
{
    uint32_t token_index = 0;
    for (; token_index < number_of_tokens; ++token_index) {
        struct token *token = &tokens[token_index];
        if (token->length != length)
            continue;
        if (memcmp(token->string, string, length))
            continue;
        if (type != TOKEN_DONT_CARE && token->type != type) {
            errorf("token '%.*s' can't be used as both %s and %s keyword",
             (int)length, string, token_type_string(token->type),
             token_type_string(type));
            error.ranges[0] = token->range;
            if (range)
                error.ranges[1] = *range;
            exit_with_error();
        }
        break;
    }
    return token_index;
}

static symbol_id add_keyword_token(struct context *ctx, struct rule *rule,
 struct owl_ref string_ref, enum token_type type)
{
    struct parsed_string keyword = parsed_string_get(string_ref);
    if (keyword.length == 0) {
        // Zero-length keywords are treated as epsilons.
        return SYMBOL_EPSILON;
    }
    uint32_t token_index = find_token(rule->keyword_tokens,
     rule->number_of_keyword_tokens, keyword.string, keyword.length, type,
     &keyword.range);
    if (token_index >= rule->number_of_keyword_tokens) {
        if (token_index == UINT32_MAX)
            abort();

        // Check for overlap with custom tokens (so exemplar generation in
        // ambiguity checking works properly).
        for (uint32_t i = 0; i < ctx->grammar->number_of_rules; ++i) {
            struct rule *rule = ctx->grammar->rules[i];
            if (!rule->is_token)
                continue;
            if (rule->token_type != RULE_TOKEN_CUSTOM)
                continue;
            if (rule->number_of_token_exemplars == 0) {
                if (rule->name_length == keyword.length &&
                 !memcmp(rule->name, keyword.string, keyword.length)) {
                    error.ranges[0] = rule->name_range;
                    error.ranges[1] = keyword.range;
                    exit_with_errorf("this user-defined token can't share its "
                     "name with a keyword");
                }
            } else {
                for (uint32_t i = 0; i < rule->number_of_token_exemplars; ++i) {
                    struct token exemplar = rule->token_exemplars[i];
                    if (exemplar.length == keyword.length &&
                     !memcmp(exemplar.string, keyword.string, keyword.length)) {
                        error.ranges[0] = exemplar.range;
                        error.ranges[1] = keyword.range;
                        exit_with_errorf("this exemplar string conflicts with "
                         "a keyword");
                    }
                }
            }
        }

        // Add the new token to the list.
        rule->number_of_keyword_tokens = token_index + 1;
        rule->keyword_tokens = grow_array(rule->keyword_tokens,
         &rule->keyword_tokens_allocated_bytes,
         sizeof(struct token) * rule->number_of_keyword_tokens);
        rule->keyword_tokens[token_index].string = keyword.string;
        rule->keyword_tokens[token_index].length = keyword.length;
        rule->keyword_tokens[token_index].type = type;
        rule->keyword_tokens[token_index].symbol = ctx->next_symbol++;
        rule->keyword_tokens[token_index].range = keyword.range;
    }
    return rule->keyword_tokens[token_index].symbol;
}

static uint32_t add_rule(struct context *ctx, const char *name, size_t len)
{
    // Disallow duplicate rules.
    if (find_rule(ctx, name, len) < ctx->grammar->number_of_rules)
        return UINT32_MAX;
    uint32_t index = ctx->grammar->number_of_rules++;
    if (index == UINT32_MAX)
        abort();
    ctx->grammar->rules = grow_array(ctx->grammar->rules,
     &ctx->grammar->rules_allocated_bytes,
     sizeof(struct rule *) * ctx->grammar->number_of_rules);
    ctx->grammar->rules[index] = calloc(1, sizeof(struct rule));
    ctx->grammar->rules[index]->name = name;
    ctx->grammar->rules[index]->name_length = len;
    ctx->grammar->rules[index]->operand_slot_index = UINT32_MAX;
    ctx->grammar->rules[index]->left_slot_index = UINT32_MAX;
    ctx->grammar->rules[index]->right_slot_index = UINT32_MAX;
    return index;
}

static bool add_token_rule(struct context *ctx, enum rule_token_type type,
 uint32_t *index, struct parsed_identifier ident)
{
    const char *name;
    switch (type) {
    case RULE_TOKEN_IDENTIFIER: name = "identifier"; break;
    case RULE_TOKEN_INTEGER: name = "integer"; break;
    case RULE_TOKEN_NUMBER: name = "number"; break;
    case RULE_TOKEN_STRING: name = "string"; break;
    default: return false;
    }
    size_t len = strlen(name);
    if (ident.length != len || memcmp(ident.identifier, name, len) != 0)
        return false;
    *index = add_rule(ctx, name, len);
    if (*index == UINT32_MAX)
        return false;
    if (type == RULE_TOKEN_INTEGER)
        check_version(ctx->version, INTEGER_TOKENS, ident.range);
    ctx->grammar->rules[*index]->is_token = true;
    ctx->grammar->rules[*index]->token_type = type;
    return true;
}

static uint32_t find_rule(struct context *ctx, const char *name, size_t len)
{
    for (uint32_t i = 0; i < ctx->grammar->number_of_rules; ++i) {
        struct rule *rule = ctx->grammar->rules[i];
        if (rule->name_length != len)
            continue;
        if (memcmp(rule->name, name, len))
            continue;
        return i;
    }
    return UINT32_MAX;
}

bool version_capable(struct grammar_version version,
 enum version_capability capability)
{
    switch (capability) {
    case CUSTOM_TOKENS:
        return strcmp(version.string, "owl.v1") != 0;
    case WHITESPACE:
    case SINGLE_CHAR_ESCAPES:
        return strcmp(version.string, "owl.v1") != 0 &&
         strcmp(version.string, "owl.v2") != 0;
    case INTEGER_TOKENS:
    case EXPLICIT_REPETITION:
        return strcmp(version.string, "owl.v1") != 0 &&
         strcmp(version.string, "owl.v2") != 0 &&
         strcmp(version.string, "owl.v3") != 0;
    default:
        return false;
    }
}

static void check_version(struct grammar_version version,
 enum version_capability capability, struct source_range range)
{
    if (version_capable(version, capability))
        return;
    error.ranges[0] = range;
    error.ranges[1] = version.range;
    switch (capability) {
    case CUSTOM_TOKENS:
        exit_with_errorf("custom tokens are unsupported in versions before "
         "owl.v2");
        break;
    case INTEGER_TOKENS:
        exit_with_errorf("integer tokens are unsupported in versions before "
         "owl.v4");
        break;
    case EXPLICIT_REPETITION:
        exit_with_errorf("explicit repetition is unsupported in versions "
         "before owl.v4");
        break;
    case WHITESPACE:
        exit_with_errorf("specifying whitespace is unsupported in versions "
         "before owl.v3");
        break;
    default:
        break;
    }
}

void grammar_destroy(struct grammar *grammar)
{
    for (uint32_t i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *r = grammar->rules[i];
        for (uint32_t j = 0; j < r->number_of_choices; ++j)
            automaton_destroy(&r->choices[j].automaton);
        free(r->choices);
        for (uint32_t j = 0; j < r->number_of_brackets; ++j)
            automaton_destroy(&r->brackets[j].automaton);
        free(r->brackets);
        for (uint32_t j = 0; j < r->number_of_slots; ++j) {
            for (uint32_t k = 0; k < r->slots[j].number_of_choice_sets; ++k)
                bitset_destroy(&r->slots[j].choice_sets[k].excluded_choices);
            free(r->slots[j].choice_sets);
        }
        free(r->slots);
        free(r->keyword_tokens);
        free(r->token_exemplars);
        automaton_destroy(&r->automaton);
        free(r);
    }
    free(grammar->rules);
    free(grammar->comment_tokens);
    free(grammar->whitespace_tokens);
    memset(grammar, 0, sizeof(*grammar));
}
