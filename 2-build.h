#ifndef _2_BUILD_H_
#define _2_BUILD_H_

#include "1-parse.h"
#include "automaton.h"

// STEP 2 - BUILD

// In step 2, we produce a grammar from the parse tree.
struct rule;
struct grammar {
    struct rule *rules;
    uint32_t rules_allocated_bytes;
    uint32_t number_of_rules;

    // This is the starting "root" rule's index.
    uint32_t root_rule;
};

struct bracket;
struct choice;
struct operator;
struct slot;
struct token;

struct rule {
    // This string is a direct reference to the original parsed text.
    const char *name;
    size_t name_length;

    // Token classes like 'identifier' and 'number' are represented as rules.
    bool is_token;

    // Give names to each possible choice for this rule.  Choice indexes appear
    // in automaton actions.
    struct choice *choices;
    uint32_t choices_allocated_bytes;
    uint32_t number_of_choices;

    // If this symbol has any operators, we generate "expression slot" actions
    // which can be interpreted to form an expression tree.
    struct operator *operators;
    uint32_t operators_allocated_bytes;
    uint32_t number_of_operators;

    // Slots are where children can appear in the parse tree.
    struct slot *slots;
    uint32_t slots_allocated_bytes;
    uint32_t number_of_slots;

    // Brackets are pieces of the rule that are enclosed in guard brackets.
    struct bracket *brackets;
    uint32_t brackets_allocated_bytes;
    uint32_t number_of_brackets;

    // These are the keyword tokens included in this rule.  They'll be unified
    // into a single list during the combine stage.
    struct token *keyword_tokens;
    uint32_t keyword_tokens_allocated_bytes;
    uint32_t number_of_keyword_tokens;

    // For simple rules (for rules with choices, this is an empty automaton).
    struct automaton automaton;
};

// Slots, choices and operators are all encoded using 12 bits in the action ID.
// That means there can't be more than 4096 (2^12) of them.
#define MAX_NUMBER_OF_SLOTS 4096
#define MAX_NUMBER_OF_CHOICES 4096
#define MAX_NUMBER_OF_OPERATORS 4096

struct choice {
    // The name; for example, a choice specified as `'int' : integer` will have
    // the name "integer".  This is a reference to the original parsed text.
    const char *name;
    size_t name_length;

    struct automaton automaton;
};

enum fixity { PREFIX, POSTFIX, INFIX };
enum associativity { FLAT, LEFT, RIGHT, NONASSOC };

struct operator {
    // The name; for example, an operator specified as `'+' : plus` will have
    // the name "plus".  This is a reference to the original parsed text.
    const char *name;
    size_t name_length;

    enum fixity fixity;
    enum associativity associativity;
    int32_t precedence;

    struct automaton automaton;
};

struct slot {
    // Most of the time, the slot name will match the name for the symbol this
    // slot refers to.  For renamed slots, it may differ.  This is a reference
    // to the original parsed text.
    const char *name;
    size_t name_length;

    // The symbol that appears in the containing rule's automata.
    symbol_id symbol;

    // The rule this slot refers to.
    uint32_t rule_index;
};

struct bracket {
    symbol_id symbol;

    symbol_id start_symbol;
    symbol_id end_symbol;

    struct automaton automaton;
};

enum token_type { TOKEN_NORMAL, TOKEN_START, TOKEN_END };
struct token {
    const char *string;
    size_t length;

    enum token_type type;
    symbol_id symbol;
};

// Create a grammar based on the contents of parse tree `tree`.  We assume that
// `grammar` points to a zero-initialized value.
void build(struct grammar *grammar, struct bluebird_tree *tree);

// We share this function with the combine step -- both build and combine need
// to de-duplicate keyword tokens in the same way.
uint32_t find_token(struct token *tokens, uint32_t number_of_tokens,
 const char *string, size_t length, enum token_type type);

#endif
