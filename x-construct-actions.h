#ifndef _CONSTRUCT_ACTIONS_H_
#define _CONSTRUCT_ACTIONS_H_

// These are the parse tree construction actions in order.  See 3-combine.c and
// x-construct-parse-tree.h for how these are used.

// We use #defines here instead of an enum so the values can be substituted into
// the generated parser.
#define ACTION_NONE 0
#define ACTION_END_SLOT 1
#define ACTION_END_EXPRESSION_SLOT 2
#define ACTION_BEGIN_SLOT 3
#define ACTION_BEGIN_EXPRESSION_SLOT 4
#define ACTION_SET_SLOT_CHOICE 5
#define ACTION_TOKEN_SLOT 6
#define ACTION_END_OPERAND 7
#define ACTION_END_OPERATOR 8
#define ACTION_BEGIN_OPERAND 9
#define ACTION_BEGIN_OPERATOR 10

#define CONSTRUCT_ACTION(type, slot_or_choice) \
 (((type) << 12) | ((slot_or_choice) & 0xfff))

// No action has both a slot and a choice, so we can use the same bits for both.
#define CONSTRUCT_ACTION_GET_TYPE(action) (((action) >> 12) & 0xf)
#define CONSTRUCT_ACTION_GET_SLOT(action) ((action) & 0xfff)
#define CONSTRUCT_ACTION_GET_CHOICE(action) ((action) & 0xfff)

static inline bool is_end_action(uint16_t action)
{
    switch (CONSTRUCT_ACTION_GET_TYPE(action)) {
    case ACTION_END_SLOT:
    case ACTION_END_EXPRESSION_SLOT:
    case ACTION_END_OPERAND:
    case ACTION_END_OPERATOR:
        return true;
    default:
        return false;
    }
}

#endif
