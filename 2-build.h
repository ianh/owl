#ifndef _2_BUILD_H_
#define _2_BUILD_H_

#include "1-parse.h"
#include "automaton.h"

struct rule;
struct keyword;

// STEP 2 - BUILD

// In step 2, we produce a grammar from the parse tree.  This grammar includes
// automata for each rule together with the information we need to combine these
// automata together in step 3.
struct grammar {
    struct rule *rules;
    uint32_t rules_allocated_bytes;
    uint16_t number_of_rules;

    // Symbols for tokens.
    symbol_id identifier_symbol;
    symbol_id number_symbol;
    symbol_id string_symbol;
    struct keyword *keywords;
    uint32_t keywords_allocated_bytes;
    uint32_t number_of_keywords;
};

// Each `rule_id` is an index into the rules array of our grammar.  In order to
// build a parse tree, we will track the transitions into and out of each rule
// by assigning each transition an `action_id` computed from the `rule_id`.  We
// limit rule_ids to a maximum of 0x7fff so they fit into a 16-bit `action_id`
// with room for one extra tag bit.
typedef uint16_t rule_id;
#define MAX_NUMBER_OF_RULES 0x8000

// Renames must also be encoded as actions.
#define MAX_NUMBER_OF_RENAMES 0x4000

enum fixity { PREFIX, POSTFIX, INFIX };
enum associativity { FLAT, LEFT, RIGHT, NONASSOC };
enum rule_type { NAMED_RULE, CHOICE_RULE, OPERATOR_RULE, BRACKETED_RULE };
struct rename;

// Rules are pieces of the grammar you can refer to from other rules.  We track
// the transitions into and out of each rule in order to create a parse tree.
//
// A single syntactic rule may produce multiple rules in this structural sense.
// Specifically, rules are created for each of the following:
//
// - An entire syntactic rule (NAMED_RULE)
//     a = b
// - A choice within a syntactic rule (CHOICE_RULE)
//     choice : choice-name
// - An operator under a syntactic rule's `.operators` keyword (OPERATOR_RULE)
//     '+' : add
// - The contents of a guard bracket (BRACKETED_RULE)
//     [ '(' expr ')' ]
struct rule {
    enum rule_type type;

    // A rule's identifier is used to refer to it from other rules.
    symbol_id identifier;

    // The name of this rule (for NAMED_RULES) or the rule in which this rule
    // appears (for other types of rules).
    uint32_t name;

    // For choice rules.
    uint32_t choice_name;

    // For bracketed rules.
    symbol_id start_symbol;
    symbol_id end_symbol;

    // For operator rules.
    enum fixity fixity;
    enum associativity associativity;
    int32_t precedence;

    // These are "renames" of the form rule@name.
    struct rename *renames;
    uint32_t renames_allocated_bytes;
    uint16_t number_of_renames;

    // This automaton matches only the contents of the rule.  It may contain
    // transitions referencing other rules, renames, bracketed rules, and so on.
    // These references will be eliminated by substitution in step 3.
    struct automaton *automaton;
};
struct rename {
    symbol_id original_name;
    symbol_id name;
};

enum keyword_type {
    KEYWORD_NORMAL,
    KEYWORD_START,
    KEYWORD_END,
};

struct keyword {
    symbol_id symbol;

    enum keyword_type type;

    // The `keyword` string is a direct reference to the original parsed text.
    const char *keyword;
    size_t length;
};

// Create a grammar based on the contents of parse tree `tree`.  We assume that
// `grammar` points to a zero-initialized value.
void build(struct grammar *grammar, struct bluebird_tree *tree);

#endif
