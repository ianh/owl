#include "3-combine.h"

static void substitute(struct automaton *into, symbol_id symbol, rule_id rule,
 struct automaton *substitute);
static void apply_renames(struct automaton *a, struct rename *renames,
 uint32_t number_of_renames);
static void add_write_token_actions(struct automaton *a, symbol_id token);

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
            apply_renames(bracket_automaton, rule->renames,
             rule->number_of_renames);
        }
    }

    automaton_add_transition(&result->automaton, 0, 1,
     grammar->rules[0].identifier);
    automaton_mark_accepting_state(&result->automaton, 1);
    result->final_nfa_state = 1;
    for (rule_id i = 0; i < grammar->number_of_rules; ++i) {
        struct rule *rule = &grammar->rules[i];
        if (rule->type != BRACKETED_RULE) {
            substitute(&result->automaton, rule->identifier, i,
             rule->automaton);
            substitute(&result->bracket_automaton, rule->identifier, i,
             rule->automaton);
            apply_renames(&result->automaton, rule->renames,
             rule->number_of_renames);
            apply_renames(&result->bracket_automaton, rule->renames,
             rule->number_of_renames);
        }
    }

    add_write_token_actions(&result->automaton, grammar->identifier_symbol);
    add_write_token_actions(&result->automaton, grammar->number_symbol);
    add_write_token_actions(&result->automaton, grammar->string_symbol);
    add_write_token_actions(bracket_automaton, grammar->identifier_symbol);
    add_write_token_actions(bracket_automaton, grammar->number_symbol);
    add_write_token_actions(bracket_automaton, grammar->string_symbol);
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
            state_id start_state = embed(into, substitute, end_state,
             SYMBOL_EPSILON, ACTION_END_RULE | rule);
            into->states[i].transitions[j].target = start_state;
            into->states[i].transitions[j].symbol = SYMBOL_EPSILON;
            into->states[i].transitions[j].action = ACTION_BEGIN;
            offset += m;
        }
    }
}

static void apply_renames(struct automaton *a, struct rename *renames,
 uint32_t number_of_renames)
{
    state_id next_state = a->number_of_states;
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        for (state_id j = 0; j < a->states[i].number_of_transitions; ++j) {
            for (uint32_t k = 0; k < number_of_renames; ++k) {
                if (renames[k].name != a->states[i].transitions[j].symbol)
                    continue;
                automaton_add_transition_with_action(a, next_state,
                 a->states[i].transitions[j].target, SYMBOL_EPSILON,
                 ACTION_RENAME | k);
                a->states[i].transitions[j].symbol = renames[k].original_name;
                a->states[i].transitions[j].target = next_state;
                next_state++;
                break;
            }
        }
    }
}

static void add_write_token_actions(struct automaton *a, symbol_id token)
{
    state_id next_state = a->number_of_states;
    for (state_id i = 0; i < a->number_of_states; ++i) {
        for (uint32_t j = 0; j < a->states[i].number_of_transitions; ++j) {
            if (a->states[i].transitions[j].symbol != token)
                continue;
            automaton_add_transition_with_action(a, next_state,
             a->states[i].transitions[j].target, SYMBOL_EPSILON,
             ACTION_WRITE_TOKEN);
            a->states[i].transitions[j].target = next_state;
            next_state++;
            break;
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
        if (s.accepting) {
            automaton_add_transition_with_action(into, i + n, out_state,
             out_symbol, out_action);
        }
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            automaton_add_transition(into, i + n, t.target + n, t.symbol);
        }
    }
    return from->start_state + n;
}
