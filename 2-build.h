#ifndef _2_BUILD_H_
#define _2_BUILD_H_

#include "1-parse.h"
#include "automaton.h"

struct rule;
struct binding;

// STEP 2 - BUILD

// In step 2, we produce a grammar from the parse tree.  This grammar includes
// automata for each rule together with the information we need to combine these
// automata together in step 3.
struct grammar {
    struct rule *rules;
    uint32_t rules_allocated_bytes;
    uint32_t number_of_rules;

    // A binding associates a name with an automaton symbol.  These names can
    // refer to tokens or rules.
    struct binding *bindings;
    uint32_t bindings_allocated_bytes;
    uint32_t number_of_bindings;

    // The index of each binding determines its type.  If the index of a binding
    // is less than `first_token_binding`, it's a rule -- otherwise, it's a
    // token.  The first keyword token can be found at index
    // `first_keyword_token_binding`.
    symbol_id first_token_binding;
    symbol_id first_keyword_token_binding;
};

// Each `rule_id` is an index into the rules array of our grammar.
typedef uint32_t rule_id;

// In order to build a parse tree, we track which elements appear in which
// 'slots' by assigning each transition an `action_id` computed from the slot
// index.  We limit slot indexes to a maximum of 0x7fff so they fit into a
// 16-bit `action_id` with room for one extra tag bit.
// TODO: Rewrite this comment
#define MAX_NUMBER_OF_SLOTS 16

// Similarly, we need to track which choice we took using an `action_id`.  We
// use half of the remaining 16-bit space for these indexes.
#define MAX_NUMBER_OF_CHOICES 0x4000

enum fixity { PREFIX, POSTFIX, INFIX };
enum associativity { FLAT, LEFT, RIGHT, NONASSOC };
enum rule_type {
    SIMPLE_RULE,
    RULE_WITH_CHOICES,
    RULE_WITH_OPERATORS,
    CHOICE_RULE,
    OPERATOR_RULE,
    BRACKETED_RULE,
};
struct slot;

// Rules are pieces of the grammar you can refer to from other rules.  We track
// the transitions into and out of each rule in order to create a parse tree.
//
// A single syntactic rule may produce multiple rules in this structural sense.
// Specifically, rules are created for each of the following:
//
// - An entire syntactic rule (SIMPLE_RULE)
//     a = b
// - A syntactic rule with one or many choices (RULE_WITH_CHOICES)
//     a = choice : choice-name
// - A syntactic rule with an `.operators` section (RULE_WITH_OPERATORS)
//     a = choice : choice-name .operators infix left '+' : add
// - A choice within a syntactic rule (CHOICE_RULE)
//     choice : choice-name
// - An operator under a syntactic rule's `.operators` section (OPERATOR_RULE)
//     '+' : add
// - The contents of a guard bracket (BRACKETED_RULE)
//     [ '(' expr ')' ]
struct rule {
    enum rule_type type;

    // A rule's identifier is used to refer to it from other rules.
    symbol_id identifier;

    // The name of this rule (for syntactic rules) or the syntactic rule in
    // which this rule appears (for other types of rules).
    const char *name;
    size_t name_length;

    // For choice rules.
    const char *choice_name;
    size_t choice_name_length;
    // FIXME: This duplicates the choice index in the binding?
    uint32_t choice_index;
    // For syntactic rules with choices.
    uint32_t number_of_choices;

    // For bracketed rules.
    symbol_id start_symbol;
    symbol_id end_symbol;

    // For operator rules.
    enum fixity fixity;
    enum associativity associativity;
    int32_t precedence;

    // The slots of a (syntactic) rule are the places where children can appear
    // in the parse tree.
    struct slot *slots;
    uint32_t slots_allocated_bytes;
    uint32_t number_of_slots;

    // The index of the containing syntactic rule.
    rule_id syntactic_rule;

    // This automaton matches only the contents of the rule.  It may contain
    // transitions referencing other rules, renames, bracketed rules, and so on.
    // These references will be eliminated by substitution in step 3.
    struct automaton *automaton;
};
struct slot {
    // In the "combine" phase, we rewrite this symbol into the `binding` symbol
    // while adding an action to track which slot we're in.
    symbol_id identifier;

    // This is the binding that determines what will be parsed into the slot.
    symbol_id binding;

    // Most of the time, this will match the name for the token or rule this
    // slot refers to.  For renames, it may differ.
    const char *name;
    size_t name_length;
};

enum token_type { TOKEN_NORMAL, TOKEN_START, TOKEN_END };
// We encode choice and operator rules using a transition action that refers to
// binding choice indexes.  These indexes must be limited to leave us enough
// encoding space in our 16-bit action_id.
#define MAX_CHOICE_INDEX 0x3fff
struct binding {
    // This string is a direct reference to the original parsed text.
    const char *name;
    size_t length;

    // For token bindings.
    enum token_type type;

    // For rule bindings.
    rule_id rule;

    /*
    struct choice *choices;
    uint32_t choices_allocated_bytes;
    uint32_t number_of_choices;

    struct bracket *brackets;
    uint32_t brackets_allocated_bytes;
    uint32_t number_of_brackets;

    struct operator *operators;
    uint32_t operators_allocated_bytes;
    uint32_t number_of_operators;
     */
    // TODO:
    // - list of bracket automata
    // - list of choices
    // - list of operators

    // For choice/operator rule bindings.  This provides an ordering to the
    // choices that we can encode in a transition action.
    rule_id *choices;
    uint32_t choices_allocated_bytes;
    uint32_t number_of_choices;
};

// Create a grammar based on the contents of parse tree `tree`.  We assume that
// `grammar` points to a zero-initialized value.
void build(struct grammar *grammar, struct bluebird_tree *tree);

#endif
