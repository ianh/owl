#ifndef CONSTRUCT_ACTIONS_H
#define CONSTRUCT_ACTIONS_H

// These are the parse tree construction actions in order.  See 3-combine.c and
// x-construct-parse-tree.h for how these are used.

// We use #defines here instead of an enum so the values can be substituted into
// the generated parser.
#define ACTION_NONE 0
#define ACTION_BEGIN_SLOT 1
#define ACTION_BEGIN_EXPRESSION_SLOT 2
#define ACTION_SET_SLOT_CHOICE 3
#define ACTION_TOKEN_SLOT 4
#define ACTION_BEGIN_OPERAND 5
#define ACTION_BEGIN_OPERATOR 6
#define ACTION_END_SLOT 8
#define ACTION_END_EXPRESSION_SLOT 9
#define ACTION_END_OPERAND 10
#define ACTION_END_OPERATOR 11

#define CONSTRUCT_ACTION(type, slot_or_choice) \
 (((type) << 12) | ((slot_or_choice) & 0xfff))

// No action has both a slot and a choice, so we can use the same bits for both.
#define CONSTRUCT_ACTION_GET_TYPE(action) (((action) >> 12) & 0xf)
#define CONSTRUCT_ACTION_GET_SLOT(action) ((action) & 0xfff)
#define CONSTRUCT_ACTION_GET_CHOICE(action) ((action) & 0xfff)

#define CONSTRUCT_IS_END_ACTION(action) (CONSTRUCT_ACTION_GET_TYPE(action) & 8)

static inline const char *action_name(unsigned action)
{
    switch (CONSTRUCT_ACTION_GET_TYPE(action)) {
    case ACTION_NONE:
        return "ACTION_NONE";
    case ACTION_END_SLOT:
        return "ACTION_END_SLOT";
    case ACTION_END_EXPRESSION_SLOT:
        return "ACTION_END_EXPRESSION_SLOT";
    case ACTION_BEGIN_SLOT:
        return "ACTION_BEGIN_SLOT";
    case ACTION_BEGIN_EXPRESSION_SLOT:
        return "ACTION_BEGIN_EXPRESSION_SLOT";
    case ACTION_SET_SLOT_CHOICE:
        return "ACTION_SET_SLOT_CHOICE";
    case ACTION_TOKEN_SLOT:
        return "ACTION_TOKEN_SLOT";
    case ACTION_END_OPERAND:
        return "ACTION_END_OPERAND";
    case ACTION_END_OPERATOR:
        return "ACTION_END_OPERATOR";
    case ACTION_BEGIN_OPERAND:
        return "ACTION_BEGIN_OPERAND";
    case ACTION_BEGIN_OPERATOR:
        return "ACTION_BEGIN_OPERATOR";
    default:
        return "?";
    }
}

#endif
