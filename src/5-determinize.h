#ifndef DETERMINIZE_H
#define DETERMINIZE_H

#include "3-combine.h"
#include "bitset.h"

// STEP 5 - DETERMINIZE

struct bracket_transition {
    struct bitset transition_symbols;
    symbol_id deterministic_transition_symbol;
};
struct bracket_transitions {
    // Sorted by transition_symbols.
    struct bracket_transition *transitions;
    uint32_t transitions_allocated_bytes;
    uint32_t number_of_transitions;
};
struct action_map_entry {
    state_id target_nfa_state;
    state_id dfa_state;
    symbol_id dfa_symbol;
    state_id nfa_state;
    symbol_id nfa_symbol;
    uint16_t *actions;
};
struct action_map {
    // Sorted lexicographically by target_nfa_state, then by dfa_state, then by
    // dfa_symbol.  "Initial" entries with no dfa_state or dfa_symbol are
    // encoded using UINT32_MAX for both fields.
    struct action_map_entry *entries;
    uint32_t entries_allocated_bytes;
    uint32_t number_of_entries;
};
struct deterministic_grammar {
    struct automaton automaton;
    struct automaton bracket_automaton;

    struct action_map action_map;
    struct action_map bracket_action_map;

    // Storage for the `actions` in the action maps.
    uint16_t *actions;
    uint32_t number_of_actions;

    // Accepting states of the bracket automaton are determinized, too -- the
    // `transitions` struct tracks which transition symbols of the determinized
    // automaton correspond to which transition symbols of the original, non-
    // deterministic bracket automaton.
    struct bracket_transitions transitions;

    // For each state in the bracket automaton, this array stores the set of
    // bracket transitions which can be reached from this state.  We check this
    // set against the expected transitions as we parse in order to know exactly
    // where the text stops being a valid prefix of the recognized language.
    struct bitset *bracket_reachability;
};

void determinize(struct combined_grammar *grammar,
 struct deterministic_grammar *result);

void deterministic_grammar_destroy(struct deterministic_grammar *grammar);

struct action_map_entry *action_map_find(struct action_map *map,
 state_id target_nfa_state, state_id dfa_state, symbol_id dfa_symbol);

// Step 2 (build) uses this function to determinize and minimize rules.
void determinize_minimize(struct automaton *input, struct automaton *result);

// "Disambiguation" is just determinization where actions are treated like
// symbols.  We use it in step 3 (combine) to remove redundant action
// transitions and guarantee that state-level ambiguity is equivalent to
// action-level ambiguity.
void disambiguate(struct automaton *input, struct automaton *input_bracket,
 struct automaton *result, struct automaton *result_bracket,
 symbol_id first_bracket_transition_symbol);

void disambiguate_minimize(struct automaton *input, struct automaton *result);

#endif
