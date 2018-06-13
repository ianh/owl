#ifndef _2_BUILD_H_
#define _2_BUILD_H_

#include "1-parse.h"
#include "automaton.h"
#include "error.h"

// STEP 2 - BUILD

// In step 2, we produce a structured `grammar` object from the raw parse tree.
// This grammar serves as a template for the syntax trees our generated parser
// will construct as well as a collection of scripts (in the form of
// deterministic automata) for recognizing atomic pieces (the individual
// choices, operators, and bracket expressions) of this syntax.

// Step 3 stitches these scripts together, encoding the rule indexes, choice
// indexes and slot indexes as transition actions in one big nondeterministic
// automaton.

struct grammar;
struct rule;

// The main function of this step.  The `grammar` struct should be initialized
// to be full of zeros.
void build(struct grammar *grammar, struct bluebird_tree *tree);

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
    struct source_range name_range;

    // Token classes like 'identifier' and 'number' are represented as rules.
    bool is_token;

    // Choices are named alternatives in a rule.  For example, here's a rule
    // with three choices:
    //
    //   operation =
    //    '+' : plus
    //    '-' : minus
    //    '*' : times
    //
    // The 'plus', 'minus' and 'times' clauses are represented here as choice
    // structs.  A rule without any named choices (that is, a rule with a
    // `number_of_choices` field equal to zero) will store its single automaton
    // in the `automaton` field declared below.
    struct choice *choices;
    uint32_t choices_allocated_bytes;
    uint32_t number_of_choices;

    // Operators are choices that appear after the '.operators' keyword.
    // They're represented separately and include information like fixity,
    // associativity and precedence.
    struct operator *operators;
    uint32_t operators_allocated_bytes;
    uint32_t number_of_operators;

    // Slots are places where children can appear in the parse tree.  Each
    // reference to a rule or named token class creates a slot with that name,
    // and operators create slots for their operands. For example, this rule has
    // six slots:
    //
    //   expression =
    //     identifier : variable
    //     number : literal
    //     [ '(' expression ')' ] : parens
    //    .operators postfix
    //     [ '(' (expression (',' expression)*)? ')' ] : function-call
    //    .operators infix left
    //     '+' : plus
    //     '-' : minus
    //
    // ...'identifier', 'number', and 'expression' for the three rule
    // references, 'left' and 'right' for the infix operators, and 'operand' for
    // the postfix operator.  Note that slots with the same name ('expression')
    // are combined together.
    struct slot *slots;
    uint32_t slots_allocated_bytes;
    uint32_t number_of_slots;

    // For rules with infix operators -- these are the slots for the right and
    // left operands.
    uint32_t right_slot_index;
    uint32_t left_slot_index;

    // For rules with prefix and postfix operators.
    uint32_t operand_slot_index;

    // Brackets are pieces of the rule that are enclosed in guard brackets.
    struct bracket *brackets;
    uint32_t brackets_allocated_bytes;
    uint32_t number_of_brackets;

    // These are the keyword tokens (literal tokens enclosed in 'quotes') that
    // appear in this rule.  They'll be unified into a single list during the
    // combine stage.
    struct token *keyword_tokens;
    uint32_t keyword_tokens_allocated_bytes;
    uint32_t number_of_keyword_tokens;

    // For simple rules with no choices.  In rules with choices, this is an
    // empty automaton.
    struct automaton automaton;
};

// Slots, choices and operators are all encoded using 12 bits in the action ID.
// That means there can't be more than 4096 (2^12) of them.
#define MAX_NUMBER_OF_SLOTS 4096
#define MAX_NUMBER_OF_CHOICES 4096

struct choice {
    // The name; for example, a choice specified as `'int' : integer` will have
    // the name "integer".  This is a reference to the original parsed text.
    const char *name;
    size_t name_length;

    // A deterministic automaton which recognizes this choice.
    struct automaton automaton;

    // For error reporting.
    struct source_range name_range;
    struct source_range expr_range;
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

    // A deterministic automaton which recognizes this operator.
    struct automaton automaton;

    // For error reporting.
    struct source_range name_range;
    struct source_range expr_range;
};

struct slot {
    // Unless you use the '@' operator to rename it, a slot will have the same
    // name as the rule it refers to. The string itself is a reference to the
    // original parsed text.
    const char *name;
    size_t name_length;

    // The symbol representing this slot.  It can appear in any automaton in the
    // containing rule -- we'll substitute out these symbols in step 3.
    symbol_id symbol;

    // The rule this slot refers to.
    uint32_t rule_index;

    // For error reporting.
    struct source_range range;
};

struct bracket {
    // This symbol represents this bracket when it appears in other automata in
    // the rule.
    symbol_id symbol;

    // The start and end symbols for the guard bracket.  Right now, these can
    // only be keyword tokens.
    symbol_id start_symbol;
    symbol_id end_symbol;

    // A deterministic automaton which recognizes the contents of the guard
    // bracket.
    struct automaton automaton;
};

// The `token_type` enum encodes whether this token is a start or end token
// (which can only appear at the start or end of a guard bracket) or a normal
// token (which cannot appear at the start or end of a guard bracket).
// The TOKEN_DONT_CARE option is used to search for tokens of any type.
enum token_type { TOKEN_NORMAL, TOKEN_START, TOKEN_END, TOKEN_DONT_CARE };
struct token {
    const char *string;
    size_t length;
    struct source_range range;

    enum token_type type;

    // For tokens in step 2, this symbol is local to the token's enclosing rule.
    // In step 3, we combine the lists of tokens together, and this symbol is
    // global to the entire combined automaton.
    symbol_id symbol;
};

// We share this function with the combine step -- both build and combine need
// to de-duplicate keyword tokens in the same way.
uint32_t find_token(struct token *tokens, uint32_t number_of_tokens,
 const char *string, size_t length, enum token_type type,
 struct source_range *range);

#endif
