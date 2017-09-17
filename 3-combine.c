#include "3-combine.h"

static void substitute(struct automaton *into, symbol_id symbol, rule_id rule,
 struct automaton *substitute);

// TODO: Renames

void combine(struct combined_grammar *result, struct grammar *grammar)
{
    struct automaton *bracket_automaton = &result->bracket_automaton;
    state_id bracket_accepting_state = 1;
    automaton_mark_accepting_state(bracket_automaton, bracket_accepting_state);
    for (rule_id i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *rule = &grammar->rules[i];
        if (rule->type == BRACKETED_RULE) {
            rule_id start = automaton_embed(bracket_automaton, rule->automaton,
             bracket_accepting_state, rule->end_token, 0);
            automaton_add_transition(bracket_automaton,
             bracket_automaton->start_state, start, rule->start_token);
        }
    }

    automaton_add_transition(&result->automaton, 0, 1,
     grammar->rules[0].identifier);
    automaton_mark_accepting_state(&result->automaton, 1);
    for (rule_id i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *rule = &grammar->rules[i];
        if (rule->type != BRACKETED_RULE) {
            substitute(&result->automaton, rule->identifier, i,
             rule->automaton);
            substitute(&result->bracket_automaton, rule->identifier, i,
             rule->automaton);
        }
    }
}

static void substitute(struct automaton *into, symbol_id symbol, rule_id rule,
 struct automaton *substitute)
{
    uint32_t n = into->number_of_states;
    uint32_t m = substitute->number_of_states;
    uint32_t offset = n;
    for (uint32_t i = 0; i < n; ++i) {
        for (uint32_t j = 0; j < into->states[i].number_of_transitions; ++j) {
            if (into->states[i].transitions[j].symbol != symbol)
                continue;
            state_id end_state = into->states[i].transitions[j].target;
            state_id start_state = automaton_embed(into, substitute, end_state,
             SYMBOL_EPSILON, ACTION_END_RULE | rule);
            into->states[i].transitions[j].target = start_state;
            into->states[i].transitions[j].symbol = SYMBOL_EPSILON;
            into->states[i].transitions[j].action = ACTION_BEGIN;
            offset += m;
        }
    }
}
