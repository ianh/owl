#include "3-combine.h"

#include "x-construct-actions.h"
#include <stdio.h>

#define CONSTRUCT_ACTION(action) ACTION_##action,
enum action_type {
CONSTRUCT_ACTIONS
};
#undef CONSTRUCT_ACTION

static void substitute_rule(struct automaton *into, symbol_id symbol,
 uint32_t choice_index_or_0xffff, struct automaton *substitute);
static void substitute_slots(struct automaton *a, struct grammar *grammar,
 struct slot *slots, uint32_t number_of_slots);
static void check_substitutions_in_range(struct automaton *a,
 struct grammar *grammar);
static void update_number_of_symbols(struct automaton *a);

// Returns the start state of the embedded automaton.
static state_id embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action);

void combine(struct combined_grammar *result, struct grammar *grammar)
{
    struct automaton *bracket_automaton = &result->bracket_automaton;
    automaton_set_start_state(bracket_automaton, 0);
    for (rule_id i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *rule = &grammar->rules[i];
        if (rule->type == BRACKETED_RULE) {
            state_id end = bracket_automaton->number_of_states;
            automaton_mark_accepting_state(bracket_automaton, end);
            bracket_automaton->states[end].transition_symbol = rule->identifier;

            state_id start = embed(bracket_automaton, rule->automaton, end,
             rule->end_symbol, 0);
            automaton_add_transition(bracket_automaton,
             bracket_automaton->start_state, start, rule->start_symbol);
            struct rule *syntactic_rule = &grammar->rules[rule->syntactic_rule];
            substitute_slots(bracket_automaton, grammar, syntactic_rule->slots,
             syntactic_rule->number_of_slots);
        }
    }

    automaton_add_transition(&result->automaton, 0, 1,
     grammar->rules[0].identifier);
    automaton_mark_accepting_state(&result->automaton, 1);
    result->final_nfa_state = 1;
    for (rule_id i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *rule = &grammar->rules[i];
        if (rule->type != BRACKETED_RULE) {
            uint32_t choice_index_or_0xffff = 0xffff;
            if (rule->type == CHOICE_RULE || rule->type == OPERATOR_RULE)
                choice_index_or_0xffff = rule->choice_index;
            substitute_rule(&result->automaton, rule->identifier,
             choice_index_or_0xffff, rule->automaton);
            substitute_rule(&result->bracket_automaton, rule->identifier,
             choice_index_or_0xffff, rule->automaton);
            struct rule *syntactic_rule = &grammar->rules[rule->syntactic_rule];
            substitute_slots(&result->automaton, grammar,
             syntactic_rule->slots, syntactic_rule->number_of_slots);
            substitute_slots(&result->bracket_automaton, grammar,
             syntactic_rule->slots, syntactic_rule->number_of_slots);
        }
    }

    check_substitutions_in_range(&result->automaton, grammar);
    check_substitutions_in_range(&result->bracket_automaton, grammar);

    update_number_of_symbols(&result->automaton);
    update_number_of_symbols(&result->bracket_automaton);

    // We need these automata to share the same number_of_symbols so they can
    // share bracket transition bitsets.  This may be a sign that the two
    // automata should be combined into one.
    uint32_t number_of_symbols = result->automaton.number_of_symbols;
    if (result->bracket_automaton.number_of_symbols > number_of_symbols)
        number_of_symbols = result->bracket_automaton.number_of_symbols;
    result->automaton.number_of_symbols = number_of_symbols;
    result->bracket_automaton.number_of_symbols = number_of_symbols;
}

static void substitute_rule(struct automaton *into, symbol_id symbol,
 uint32_t choice_index_or_0xffff, struct automaton *substitute)
{
    uint32_t n = into->number_of_states;
    uint32_t m = substitute->number_of_states;
    uint32_t offset = n;
    for (uint32_t i = 0; i < n; ++i) {
        for (uint32_t j = 0; j < into->states[i].number_of_transitions; ++j) {
            if (into->states[i].transitions[j].symbol != symbol)
                continue;
            state_id end_state = into->states[i].transitions[j].target;
            uint16_t action = 0;
            // TODO
//            if (choice_index_or_0xffff <= MAX_NUMBER_OF_CHOICES)
//                action = ACTION_MARK_CHOICE_INDEX | choice_index_or_0xffff;
            state_id start_state = embed(into, substitute, end_state,
             SYMBOL_EPSILON, action);
            into->states[i].transitions[j].target = start_state;
            into->states[i].transitions[j].symbol = SYMBOL_EPSILON;
            into->states[i].transitions[j].action = 0;
            offset += m;
        }
    }
}

static void substitute_slots(struct automaton *a, struct grammar *grammar,
 struct slot *slots, uint32_t number_of_slots)
{
    state_id next_state = a->number_of_states;
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        for (state_id j = 0; j < a->states[i].number_of_transitions; ++j) {
            for (uint32_t k = 0; k < number_of_slots; ++k) {
                if (slots[k].identifier != a->states[i].transitions[j].symbol)
                    continue;
                bool token = slots[k].binding >= grammar->first_token_binding;
                state_id in = next_state++;
                state_id out = next_state++;
                automaton_add_transition_with_action(a, out,
                 a->states[i].transitions[j].target, SYMBOL_EPSILON,
                 MAKE_CONSTRUCT_ACTION(ACTION_END_SLOT, k, 0));
                automaton_add_transition_with_action(a, in, out,
                 slots[k].binding, 0);
                a->states[i].transitions[j].symbol = SYMBOL_EPSILON;
                a->states[i].transitions[j].action = token ? 0 :
                 MAKE_CONSTRUCT_ACTION(ACTION_BEGIN_SLOT, k, 0);
                a->states[i].transitions[j].target = in;
                break;
            }
        }
    }
}

static void check_substitutions_in_range(struct automaton *a,
 struct grammar *grammar)
{
    uint32_t n = a->number_of_states;
    for (uint32_t i = 0; i < n; ++i) {
        for (uint32_t j = 0; j < a->states[i].number_of_transitions; ++j) {
            symbol_id symbol = a->states[i].transitions[j].symbol;
            if (symbol != SYMBOL_EPSILON &&
             (symbol < grammar->first_token_binding ||
             symbol >= grammar->number_of_bindings)) {
                bool is_bracket_transition = false;
                for (uint32_t k = 0; k < grammar->number_of_rules; ++k) {
                    if (grammar->rules[k].identifier == symbol &&
                     grammar->rules[k].type == BRACKETED_RULE) {
                        is_bracket_transition = true;
                        break;
                    }
                }
                if (is_bracket_transition)
                    continue;
                // TODO: Better error message.
                printf("unsubstituted symbol %x\n", symbol);
                exit(-1);
            }
        }
    }
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
    }
}

static state_id embed(struct automaton *into, struct automaton *from,
 state_id out_state, symbol_id out_symbol, uint16_t out_action)
{
    uint32_t m = from->number_of_states;
    uint32_t n = into->number_of_states;
    for (state_id i = 0; i < m; ++i) {
        struct state s = from->states[i];
        if (s.accepting)
            automaton_add_transition_with_action(into, i + n, out_state,
             out_symbol, out_action);
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            automaton_add_transition(into, i + n, t.target + n, t.symbol);
        }
    }
    return from->start_state + n;
}
