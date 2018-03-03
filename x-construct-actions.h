
// These are the parse tree construction actions in order.  See 3-combine.c and
// x-construct-parse-tree.h for how these are used.

#define CONSTRUCT_ACTIONS \
 CONSTRUCT_ACTION_NAME(NONE) \
 CONSTRUCT_ACTION_NAME(END_SLOT) \
 CONSTRUCT_ACTION_NAME(END_EXPRESSION_SLOT) \
 CONSTRUCT_ACTION_NAME(BEGIN_SLOT) \
 CONSTRUCT_ACTION_NAME(BEGIN_EXPRESSION_SLOT) \
 CONSTRUCT_ACTION_NAME(SET_SLOT_CHOICE) \
 CONSTRUCT_ACTION_NAME(TOKEN_SLOT) \
 CONSTRUCT_ACTION_NAME(END_OPERAND) \
 CONSTRUCT_ACTION_NAME(END_OPERATOR) \
 CONSTRUCT_ACTION_NAME(BEGIN_OPERAND) \
 CONSTRUCT_ACTION_NAME(BEGIN_OPERATOR) \

#define CONSTRUCT_ACTION(type, slot_or_choice) \
 (((type) << 12) | ((slot_or_choice) & 0xfff))

// No action has both a slot and a choice, so we can use the same bits for both.
#define CONSTRUCT_ACTION_GET_TYPE(action) (((action) >> 12) & 0xf)
#define CONSTRUCT_ACTION_GET_SLOT(action) ((action) & 0xfff)
#define CONSTRUCT_ACTION_GET_CHOICE(action) ((action) & 0xfff)
