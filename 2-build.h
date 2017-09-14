#ifndef _2_BUILD_H_
#define _2_BUILD_H_

#include "1-parse.h"
#include "automaton.h"

struct rule;

// STEP 2 - BUILD

// In step 2, we produce a grammar from the parse tree.  This grammar includes
// automata for each rule together with the information we need to combine these
// automata together in step 3.
struct grammar {
    struct rule *rules;
    uint32_t rules_allocated_bytes;
    uint16_t number_of_rules;
};

// Each rule_id is an index into the rules array of our grammar.  In order to
// build a parse tree, we will track the transitions into and out of each rule
// by assigning each transition an action_id computed from the rule_id.  We
// limit rule_ids to a maximum of 0x7fff so they fit into a 16-bit action_id
// with room for one extra tag bit.
typedef uint16_t rule_id;
#define MAX_NUMBER_OF_RULES 0x8000

enum fixity { PREFIX, POSTFIX, INFIX };
enum associativity { FLAT, LEFT, RIGHT, NONASSOC };
enum rule_type { NAMED_RULE, OPERATOR_RULE, BRACKETED_RULE };
struct rename;

// Rules are pieces of the grammar you can refer to from other rules.  We track
// the transitions into and out of each rule in order to create a parse tree.
//
// A single syntactic rule may produce multiple rules in this structural sense.
// Specifically, rules are created for each of the following:
//
// - An entire syntactic rule with no choices
//     a = b
// - A choice within a syntactic rule
//     choice : choice-name
// - An operator within a syntactic rule which uses the `.operators` keyword
//     '+' : add
// - The contents of a guard bracket
//     [ '(' expr ')' ]
struct rule {
    enum rule_type type;

    symbol_id name;
    // For rules with multiple choices (e.g., rule = a : choice-a  b : choice-b)
    symbol_id choice_name;

    // For bracketed rules.
    token_id start_token;
    token_id end_token;

    // For operator rules.
    enum fixity fixity;
    enum associativity associativity;
    int32_t precedence;

    // These are "renames" of the form rule@name.
    struct rename *renames;
    uint32_t renames_allocated_bytes;
    uint32_t number_of_renames;

    // This automaton matches only the contents of the rule.  It may contain
    // transitions referencing other rules, renames, bracketed rules, and so on.
    // These references will be eliminated by substitution in step 3.
    struct automaton *automaton;
};
struct rename {
    symbol_id original_name;
    symbol_id name;
};

// Create a grammar based on the contents of parse tree `tree`.  We assume that
// `grammar` points to a zero-initialized value.
void build(struct grammar *grammar, struct bluebird_tree *tree);

#endif
