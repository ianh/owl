#include "3-combine.h"

#include "5-determinize.h"
#include "alloc.h"
#include "construct-actions.h"
#include <assert.h>
#include <stdio.h>

struct rename {
    symbol_id from;
    symbol_id to;
};

static void substitute_slots(struct grammar *grammar, struct rule *rule,
 uint32_t min_rule_index, struct automaton *a,
 struct automaton *automaton_for_rule, struct rename *renames,
 size_t number_of_renames);

static void update_number_of_symbols(struct automaton *a);
static void equalize_number_of_symbols(struct automaton *a,
 struct automaton *b);
// Returns the start state of the embedded automaton.
static state_id embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action);
static void remove_choice_actions(struct automaton *a, struct bitset *choices);

void combine(struct combined_grammar *result, struct grammar *grammar)
{
    uint32_t n = grammar->number_of_rules;
    struct automaton *automaton_for_rule = calloc(n, sizeof(struct automaton));
    struct rename **renames_for_rule = calloc(n, sizeof(struct rename *));
    symbol_id **bracket_symbols_for_rule = calloc(n, sizeof(symbol_id *));
    uint32_t tokens_allocated_bytes = 0;

    struct rule *root = grammar->rules[grammar->root_rule];
    result->root_rule_is_expression = root->number_of_choices >
     root->first_operator_choice;

    // First pass: collect all the keyword tokens into the `tokens` result
    // array, making sure to combine duplicates using `find_token`.
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = grammar->rules[i];
        // Allocate enough space for keyword renames and bracket renames.
        renames_for_rule[i] = calloc(rule->number_of_keyword_tokens +
         rule->number_of_brackets, sizeof(struct rename));

        for (uint32_t j = 0; j < rule->number_of_keyword_tokens; ++j) {
            struct token token = rule->keyword_tokens[j];
            uint32_t index = find_token(result->tokens,
             result->number_of_tokens, token.string, token.length, token.type,
             &token.range);
            if (index >= result->number_of_tokens) {
                if (index == UINT32_MAX)
                    abort();
                result->number_of_tokens = index + 1;
                result->tokens = grow_array(result->tokens,
                 &tokens_allocated_bytes,
                 sizeof(struct token) * result->number_of_tokens);
                result->tokens[index] = token;
            }
            renames_for_rule[i][j].from = token.symbol;
            renames_for_rule[i][j].to = index;

            // We call `find_token` on comment tokens and whitespace tokens so
            // an error is produced if a token appears as more than one
            // different type.
            find_token(grammar->comment_tokens,
             grammar->number_of_comment_tokens, token.string, token.length,
             token.type, &token.range);
            find_token(grammar->whitespace_tokens,
             grammar->number_of_whitespace_tokens, token.string, token.length,
             token.type, &token.range);
        }
    }
    for (uint32_t i = 0; i < grammar->number_of_comment_tokens; ++i) {
        // We also need to check whether any whitespace token appears as a
        // comment token.
        struct token token = grammar->comment_tokens[i];
        find_token(grammar->whitespace_tokens,
         grammar->number_of_whitespace_tokens, token.string, token.length,
         token.type, &token.range);
    }
    result->number_of_keyword_tokens = result->number_of_tokens;

    // Second pass: add the "token class" tokens which are represented as rules
    // (like identifier, number, and so on) to the `tokens` result array.
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = grammar->rules[i];
        if (!rule->is_token)
            continue;
        symbol_id symbol = result->number_of_tokens++;
        if (symbol == UINT32_MAX)
            abort();
        result->tokens = grow_array(result->tokens, &tokens_allocated_bytes,
         sizeof(struct token) * result->number_of_tokens);
        result->tokens[symbol].string = rule->name;
        result->tokens[symbol].length = rule->name_length;
        result->tokens[symbol].symbol = symbol;
        result->tokens[symbol].rule_index = i;

        // Create an automaton to be substituted into other rules.
        automaton_set_start_state(&automaton_for_rule[i], 0);
        automaton_add_transition(&automaton_for_rule[i], 0, 1, symbol);
        automaton_mark_accepting_state(&automaton_for_rule[i], 1);
    }
    // Bracket symbols come after the tokens.
    symbol_id next_bracket_symbol = result->number_of_tokens;

    // Third pass: build the rule automata.  We build the automata bottom-up by
    // visiting each rule in reverse order.
    for (uint32_t i = n - 1; i < n; --i) {
        struct rule *rule = grammar->rules[i];
        if (rule->is_token)
            continue;
        struct automaton automaton = {0};
        if (rule->number_of_choices == 0) {
            // If the rule is a simple rule without choices, we already built a
            // combined automaton -- just copy it into place.
            automaton_copy(&rule->automaton, &automaton);
        } else {
            // Otherwise, we have to combine all the choices and operators
            // together, using actions to track which choice or operator is
            // matched.
            state_id start = automaton_create_state(&automaton);
            state_id end = automaton_create_state(&automaton);
            uint32_t j = 0;
            for (; j < rule->first_operator_choice; ++j) {
                // Embed each choice into the automaton, tracking it as an
                // "operand" if this is an expression rule with operators.
                struct choice *choice = &rule->choices[j];
                uint16_t start_action = 0;
                uint16_t end_action = 0;
                if (rule->first_operator_choice < rule->number_of_choices) {
                    start_action = CONSTRUCT_ACTION(ACTION_BEGIN_OPERAND, 0);
                    end_action = CONSTRUCT_ACTION(ACTION_END_OPERAND, j);
                } else
                    end_action = CONSTRUCT_ACTION(ACTION_SET_SLOT_CHOICE, j);
                state_id choice_start = embed(&automaton, &choice->automaton,
                 end, SYMBOL_EPSILON, end_action);
                automaton_add_transition_with_action(&automaton, start,
                 choice_start, SYMBOL_EPSILON, start_action);
            }
            while (j < rule->number_of_choices) {
                // Now we add the operator automata.  Operators are a virtual
                // "transition" between a to_state and a from_state -- the
                // fixity determines which states the operator is a transition
                // between.
                struct choice *op = &rule->choices[j];
                state_id to_state;
                state_id from_state;
                if (op->fixity == PREFIX) {
                    // Prefix operators are a transition from the start state to
                    // itself.
                    from_state = start;
                    to_state = start;
                } else if (op->fixity == POSTFIX) {
                    // Postfix operators are a transition from the end state to
                    // itself.
                    from_state = end;
                    to_state = end;
                } else if (op->associativity == NONASSOC) {
                    // Non-associative operators match either exactly two
                    // operands (if the operator is present) or exactly one
                    // operand (if it isn't): that means we have to duplicate
                    // all the states here by embedding the automaton in itself.
                    state_id rhs_end = automaton_create_state(&automaton);
                    // We have to mark the state as accepting so embed works
                    // properly. (this is kind of silly)
                    automaton_mark_accepting_state(&automaton, end);
                    state_id rhs_start = embed(&automaton, &automaton, rhs_end,
                     SYMBOL_EPSILON, 0);
                    automaton.states[end].accepting = false;
                    // Allow the automaton to skip past the nonassoc operator.
                    automaton_add_transition(&automaton, end, rhs_end,
                     SYMBOL_EPSILON);
                    from_state = end;
                    to_state = rhs_start;
                    end = rhs_end;
                } else {
                    // Infix operators are a transition from the end state back
                    // to the start state.
                    from_state = end;
                    to_state = start;
                }
                int precedence = op->precedence;
                for (; j < rule->number_of_choices; ++j) {
                    struct choice *op = &rule->choices[j];
                    if (op->precedence != precedence)
                        break;
                    // Embed each operator automaton and hook it up according
                    // to the rules above.
                    state_id op_start = embed(&automaton, &op->automaton,
                     to_state, SYMBOL_EPSILON,
                     CONSTRUCT_ACTION(ACTION_END_OPERATOR, j));
                    automaton_add_transition_with_action(&automaton, from_state,
                     op_start, SYMBOL_EPSILON,
                     CONSTRUCT_ACTION(ACTION_BEGIN_OPERATOR, 0));

                    // Add a nesting level to match precedence properly.
                    state_id next_start = automaton_create_state(&automaton);
                    state_id next_end = automaton_create_state(&automaton);
                    automaton_add_transition(&automaton, next_start, start,
                     SYMBOL_EPSILON);
                    automaton_add_transition(&automaton, end, next_end,
                     SYMBOL_EPSILON);
                    start = next_start;
                    end = next_end;
                }
            }
            automaton_set_start_state(&automaton, start);
            automaton_mark_accepting_state(&automaton, end);
        }

        // The bracket symbols for each rule are local to that rule's automata.
        // Rename them so they're globally unique across all automata.
        bracket_symbols_for_rule[i] =
         calloc(rule->number_of_brackets, sizeof(symbol_id));
        for (uint32_t j = 0; j < rule->number_of_brackets; ++j) {
            symbol_id symbol = next_bracket_symbol++;
            size_t index = j + rule->number_of_keyword_tokens;
            renames_for_rule[i][index].from = rule->brackets[j].symbol;
            renames_for_rule[i][index].to = symbol;
            bracket_symbols_for_rule[i][j] = symbol;
        }

        // Now that we have a combined automaton, we need to fill in the slots
        // that refer to other rules' automata.  Rules can only refer to later
        // rules, so since we're building the rule automata from last to first,
        // these automata are guaranteed to already have been built.
        substitute_slots(grammar, rule, i + 1, &automaton, automaton_for_rule,
         renames_for_rule[i], rule->number_of_keyword_tokens +
         rule->number_of_brackets);
        disambiguate_minimize(&automaton, &automaton_for_rule[i]);
        automaton_destroy(&automaton);
    }

    // Fourth pass: build and substitute the bracket automata from each rule.
    struct automaton combined_bracket_automaton = {0};
    automaton_set_start_state(&combined_bracket_automaton, 0);
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = grammar->rules[i];
        if (rule->is_token || rule->number_of_brackets == 0)
            continue;

        // Create a combined bracket automaton for this particular rule.  Embed
        // each bracket's automaton with the proper begin and end symbols.
        struct automaton bracket_automaton = {0};
        automaton_set_start_state(&bracket_automaton, 0);
        for (uint32_t j = 0; j < rule->number_of_brackets; ++j) {
            struct bracket *bracket = &rule->brackets[j];
            state_id end = automaton_create_state(&bracket_automaton);
            state_id start = embed(&bracket_automaton, &bracket->automaton,
             end, bracket->end_symbol, 0);
            automaton_mark_accepting_state(&bracket_automaton, end);
            bracket_automaton.states[end].transition_symbol =
             bracket_symbols_for_rule[i][j];
            automaton_add_transition(&bracket_automaton,
             bracket_automaton.start_state, start, bracket->start_symbol);
        }

        // Substitute any slots that appear in this bracket automaton.
        substitute_slots(grammar, rule, 0, &bracket_automaton,
         automaton_for_rule, renames_for_rule[i],
         rule->number_of_keyword_tokens + rule->number_of_brackets);

        // Embed the rule's bracket automaton into the larger result bracket
        // automaton.  We do this by hand instead of using `embed` to make sure
        // we preserve accepting states and their transition symbols.
        uint32_t m = combined_bracket_automaton.number_of_states;
        for (uint32_t j = 0; j < bracket_automaton.number_of_states; ++j) {
            struct state s = bracket_automaton.states[j];
            for (uint32_t k = 0; k < s.number_of_transitions; ++k) {
               automaton_add_transition_with_action(&combined_bracket_automaton,
                j + m, s.transitions[k].target + m, s.transitions[k].symbol,
                s.transitions[k].action);
            }
            if (s.accepting) {
               automaton_mark_accepting_state(&combined_bracket_automaton,
                j + m);
               combined_bracket_automaton.states[j + m].transition_symbol =
                s.transition_symbol;
            }
        }
        automaton_add_transition(&combined_bracket_automaton,
         combined_bracket_automaton.start_state,
         bracket_automaton.start_state + m, SYMBOL_EPSILON);
        automaton_destroy(&bracket_automaton);
    }

    // Before we can call `disambiguate`, we need these automata to have the
    // same `number_of_symbols` so they can share bracket transition bitsets.
    // (This may be a sign that the two automata should be combined into one.)
    equalize_number_of_symbols(&automaton_for_rule[grammar->root_rule],
     &combined_bracket_automaton);

    // Produce the final automata by "disambiguating" the combined automata.
    // This removes redundant epsilon transitions so our ambiguity checking
    // algorithm works properly.
    disambiguate(&automaton_for_rule[grammar->root_rule],
     &combined_bracket_automaton, &result->automaton,
     &result->bracket_automaton, result->number_of_tokens);

    // Make sure we have a single final state so we have somewhere to start when
    // walking backwards and running transition actions.
    result->final_nfa_state = automaton_create_state(&result->automaton);
    for (uint32_t i = 0; i < result->automaton.number_of_states; ++i) {
        if (result->automaton.states[i].accepting) {
            result->automaton.states[i].accepting = false;
            automaton_add_transition(&result->automaton, i,
             result->final_nfa_state, SYMBOL_EPSILON);
        }
    }
    automaton_mark_accepting_state(&result->automaton, result->final_nfa_state);

    // Set the symbol field in the result tokens (even though it probably
    // shouldn't be used) to avoid confusion.  (This may be a sign that we
    // should have a separate type for these tokens without this field.)
    for (uint32_t i = 0; i < result->number_of_tokens; ++i)
        result->tokens[i].symbol = i;

    // Equalize the number of symbols again -- we'll be using bracket transition
    // bitsets again when we fully determinize the automata.
    equalize_number_of_symbols(&result->automaton, &result->bracket_automaton);

    // Compute the number of bracket transition symbols (it may be different
    // from `next_bracket_symbol` due to disambiguation).
    result->number_of_bracket_symbols = 0;
    if (result->automaton.number_of_symbols > result->number_of_tokens) {
        result->number_of_bracket_symbols = result->automaton.number_of_symbols
         - result->number_of_tokens;
    }

    for (uint32_t i = 0; i < n; ++i) {
        free(renames_for_rule[i]);
        free(bracket_symbols_for_rule[i]);
        automaton_destroy(&automaton_for_rule[i]);
    }
    automaton_destroy(&combined_bracket_automaton);
    free(renames_for_rule);
    free(bracket_symbols_for_rule);
    free(automaton_for_rule);
}

// This function substitutes automata into slots while renaming symbols.  We
// have to do all of this at once to avoid name collisions (where the result of
// a substitution is mistakenly substituted a second time).
static void substitute_slots(struct grammar *grammar, struct rule *rule,
 uint32_t min_rule_index, struct automaton *a,
 struct automaton *automaton_for_rule, struct rename *renames,
 size_t number_of_renames)
{
    uint32_t n = a->number_of_states;
    for (uint32_t i = 0; i < n; ++i) {
        for (state_id j = 0; j < a->states[i].number_of_transitions; ++j) {
            symbol_id symbol = a->states[i].transitions[j].symbol;
            bool renamed = false;
            for (size_t k = 0; k < number_of_renames; k++) {
                if (renames[k].from == symbol) {
                    a->states[i].transitions[j].symbol = renames[k].to;
                    renamed = true;
                    break;
                }
            }
            if (renamed) {
                // Slots are never renamed, so there's no need to loop through
                // the slots looking for this symbol.
                continue;
            }
            for (uint32_t k = 0; k < rule->number_of_slots; ++k) {
                struct slot *slot = &rule->slots[k];
                for (uint32_t l = 0; l < slot->number_of_choice_sets; ++l) {
                    if (slot->choice_sets[l].symbol != symbol)
                        continue;
                    if (slot->rule_index < min_rule_index)
                        abort();
                    struct rule *slot_rule = grammar->rules[slot->rule_index];
                    uint16_t begin_action = 0;
                    uint16_t end_action = 0;
                    // Mark this slot in the action list.
                    if (slot_rule->is_token)
                        end_action = CONSTRUCT_ACTION(ACTION_TOKEN_SLOT, k);
                    else if (slot_rule->first_operator_choice <
                     slot_rule->number_of_choices) {
                        // Using '0' instead of 'k' here actually reduces the
                        // size of the automata (if two transitions have the
                        // same action, we can follow both of them
                        // simultaneously while determinizing).
                        begin_action =
                         CONSTRUCT_ACTION(ACTION_BEGIN_EXPRESSION_SLOT, 0);
                        end_action =
                         CONSTRUCT_ACTION(ACTION_END_EXPRESSION_SLOT, k);
                    } else {
                        begin_action = CONSTRUCT_ACTION(ACTION_BEGIN_SLOT, 0);
                        end_action = CONSTRUCT_ACTION(ACTION_END_SLOT, k);
                    }
                    struct automaton *r = &automaton_for_rule[slot->rule_index];
                    struct automaton choice_removed = {0};
                    struct slot_choice_set *choice_set = &slot->choice_sets[l];
                    if (!bitset_is_empty(&choice_set->excluded_choices)) {
                        automaton_copy(r, &choice_removed);
                        remove_choice_actions(&choice_removed,
                         &choice_set->excluded_choices);
                        r = &choice_removed;
                    }
                    state_id start = embed(a, r,
                     a->states[i].transitions[j].target, SYMBOL_EPSILON,
                     end_action);
                    a->states[i].transitions[j].symbol = SYMBOL_EPSILON;
                    a->states[i].transitions[j].action = begin_action;
                    a->states[i].transitions[j].target = start;
                    automaton_destroy(&choice_removed);
                    break;
                }
            }
        }
    }
}

void combined_grammar_destroy(struct combined_grammar *grammar)
{
    automaton_destroy(&grammar->automaton);
    automaton_destroy(&grammar->bracket_automaton);
    free(grammar->tokens);
    memset(grammar, 0, sizeof(*grammar));
}

static void update_number_of_symbols(struct automaton *a)
{
    uint32_t n = a->number_of_states;
    a->number_of_symbols = 0;
    for (uint32_t i = 0; i < n; ++i) {
        for (uint32_t j = 0; j < a->states[i].number_of_transitions; ++j) {
            symbol_id symbol = a->states[i].transitions[j].symbol;
            if (symbol != SYMBOL_EPSILON && symbol >= a->number_of_symbols)
                a->number_of_symbols = symbol + 1;
        }
        if (a->states[i].transition_symbol >= a->number_of_symbols)
            a->number_of_symbols = a->states[i].transition_symbol + 1;
    }
}

static void equalize_number_of_symbols(struct automaton *a,
 struct automaton *bracket)
{
    update_number_of_symbols(a);
    update_number_of_symbols(bracket);
    uint32_t number_of_symbols = a->number_of_symbols;
    if (bracket->number_of_symbols > number_of_symbols)
        number_of_symbols = bracket->number_of_symbols;
    a->number_of_symbols = number_of_symbols;
    bracket->number_of_symbols = number_of_symbols;
}

static state_id embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action)
{
    uint32_t m = from->number_of_states;
    uint32_t n = into->number_of_states;
    for (state_id i = 0; i < m; ++i) {
        struct state s = from->states[i];
        if (s.accepting) {
            automaton_add_transition_with_action(into, i + n, out_state,
             out_symbol, out_action);
        }
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            automaton_add_transition_with_action(into, i + n, t.target + n,
             t.symbol, t.action);
        }
    }
    // If there aren't any transitions to or from the start state, the code
    // above won't create the corresponding state in `into`.  In that case, we
    // create a new state so our return value is actually valid.
    if (from->start_state + n < into->number_of_states)
        return from->start_state + n;
    else
        return automaton_create_state(into);
}

// The `remove_choice_actions` function is used to implement the `\:` operator.
static void remove_choice_actions(struct automaton *a, struct bitset *choices)
{
    for (state_id i = 0; i < a->number_of_states; ++i) {
        struct state s = a->states[i];
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            // These are the actions which embody a choice.
            switch (CONSTRUCT_ACTION_GET_TYPE(s.transitions[j].action)) {
            case ACTION_SET_SLOT_CHOICE:
            case ACTION_END_OPERAND:
            case ACTION_END_OPERATOR:
                break;
            default:
                continue;
            }
            if (bitset_contains(choices,
             CONSTRUCT_ACTION_GET_CHOICE(s.transitions[j].action))) {
                // Remove this transition by making it a self-transition.
                s.transitions[j].action = 0;
                s.transitions[j].target = i;
            }
        }
    }
}
