
// These are the parse tree construction actions in order.  See 3-combine.c and
// x-construct-parse-tree.h for how these are used.

#define CONSTRUCT_ACTIONS \
 CONSTRUCT_ACTION(END_SLOT) \
 CONSTRUCT_ACTION(END_EXPRESSION_SLOT) \
 CONSTRUCT_ACTION(BEGIN_SLOT) \
 CONSTRUCT_ACTION(BEGIN_EXPRESSION_SLOT) \
 CONSTRUCT_ACTION(TOKEN_SLOT) \
 CONSTRUCT_ACTION(END_OPERAND) \
 CONSTRUCT_ACTION(END_OPERATOR) \
 CONSTRUCT_ACTION(BEGIN_OPERAND) \
 CONSTRUCT_ACTION(BEGIN_OPERATOR) \

#define MAKE_CONSTRUCT_ACTION(type, slot, choice) \
 (((type) << 12) | ((slot) << 8) | (choice))

#define CONSTRUCT_ACTION_GET_TYPE(action) (((action) >> 12) & 0xf)
#define CONSTRUCT_ACTION_GET_SLOT(action) (((action) >> 6) & 0x3f)
#define CONSTRUCT_ACTION_GET_CHOICE(action) ((action) & 0x3f)
