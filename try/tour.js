tour = {
"start":{"grammar":
`# Welcome to Owl.
# <https://github.com/ianh/owl>

# Type a grammar here, with rules like

plus = number '+' number

# Owl matches the first rule against the
# input in the box below.

# A copy of the Owl tool runs in your
# browser each time the grammar or input
# changes.  Its output is shown in the box
# on the right.

# Try modifying the numbers below to see
# how the output changes.

# If you want to learn more, check out the
# repository at <https://github.com/ianh/owl>
# or take a #tour.

`,"input": 
`123 + 456
`},
"tour":{"grammar":
`# PREV < #start Start Page
# NEXT > #nested Arrays of Arrays

# Welcome to the tour of Owl!

# In this tour, we'll go through a bunch
# of small examples which show off various
# features of Owl.

# Here's a grammar for arrays of numbers:

input = array*
array = '[' ((number ',')* number)? ']'

# To match repetitions, this grammar uses *
# (which matches its operand any number of
# times).  The ? operator (which matches
# either its operand or nothing) is used
# to support empty arrays.

# If you've used regular expressions, this
# syntax should be very familiar!
`,"input":
`[]
[2]
[1, 213]
[32984, 23423423, 10981038, 2, 7, 1231239]
`},
"nested":{"grammar":
`# PREV < #tour A Grammar for Arrays
# NEXT > #ambiguity Detecting Ambiguity

# Instead of numbers, this array contains
# other arrays:

input = array*
array = [ '[' ((array ',')* array)? ']' ]

# Note the use of guard brackets [ ... ]
# around the pattern -- whenever a rule
# refers to itself (or any earlier rule),
# the reference has to appear inside these
# brackets, enclosed by an explicit
# start ('[') and end (']') token.
`,"input":
`[]
[[]]
[[],[]]
[[[]],[[],[]]]
[[[],[[[]],[]]],[],[[],[[]]]]
`},
"ambiguity":{"grammar":
`# PREV < #nested Arrays of Arrays
# NEXT > #named Named Choices

# Why have this restriction on self-
# referential rules?  These more limited
# grammars are easier to analyze.
# In particular, Owl can reliably find
# *ambiguities*, where one input can
# produce two different parse trees:

a = three+ | five+
three = '.' '.' '.'
five = '.' '.' '.' '.' '.'

# Here we use Owl's ambiguity checker to
# find the least common multiple of three
# and five.
`,"input":""},
"named":{"grammar":
`# PREV < #ambiguity Detecting Ambiguity
# NEXT > #expr Operators & Expressions

colors = (modifier? color)*
modifier =
    'light' : light
    'dark' : dark
color =
    'red' : red
    'yellow' : yellow
    'blue' : blue
    'brown' : brown

# These rules can match in several different
# ways; each choice is named using a colon.
# The parse tree on the right shows which
# choice was matched.
`,"input":
`red
dark brown
light blue
`},
"expr":{"grammar":
`# PREV < #named Named Choices
# NEXT > #call Function Calls

# Owl's ".operators" keyword can be used to
# match mathematical expressions.

input = expr\\:negate*
expr =
    number : num
    [ '(' expr ')' ] : parens
  .operators prefix
    '-' : negate
  .operators infix flat
    '*' : times
    '/' : divided-by
  .operators infix flat
    '+' : plus
    '-' : minus

# Operators are named just like choices.
# "Infix" operators are the usual binary
# operators; "prefix" and "postfix" are
# unary and take a single operand.

# Infix operators also have associativity.
# Try changing "flat" to "left" or "right"
# and see how the parse trees for the infix
# operators change.

# The "\\:negate" syntax prevents the :negate
# operator from appearing at the top level.
# Try deleting it and see what happens!
`,"input":
`1
2 + 2
1 + 2 / (3 + 4) * 5
`},
"call":{"grammar":
`# PREV < #expr Operators & Expressions
# NEXT > #stmt An If Statement

input = expr*
expr =
    identifier : ident
  .operators postfix
    [ '(' ((expr ',')* expr)? ')' ] : call

# Operators can match complex patterns.
# This grammar matches the arguments of a
# procedure call as a postfix operator.
`,"input":
`f()
g(f(x), y)
h(x)(y)(z)
`},
"stmt":{"grammar":
`# PREV < #call Function Calls
# NEXT > #if If as an Operator

# The next few grammars will explore
# different ways to write an "if" statement.

input = stmt*
stmt =
    [ 'if' expr 'then' stmt*
     ('elseif' expr 'then' stmt*)*
     ('else' stmt*)? 'end' ] : if-else
    expr : expr
expr = identifier

# This approach uses an explicit 'end'
# keyword to delimit the end of the
# statement.  You can also use braces if
# you want.
`,"input":
`if x then
  do-a
elseif y then
  do-b
else
  if z then do-c end
  do-d
end
`},
"if":{"grammar":
`# PREV < #stmt An If Statement
# NEXT > #ternary Ternary Operators

# Another way to define "if" is as a prefix
# operator.

input = expr*
expr =
    identifier : ident
  .operators prefix
    [ 'if' expr 'then' ] : if
  .operators infix right
    'else' : else

# In this grammar, "a else b" is also a
# valid expression.
`,"input":
`if x then a
if y then b else c
p else q
`},
"ternary":{"grammar":
`# PREV < #if If as an Operator
# NEXT > #end End of Tour

# You can also write "if" as a ternary
# operator, either using the classic C
# ternary syntax or 'if'/'then'/'else'
# keywords.

input = expr*
expr =
    identifier : ident
  .operators prefix
    [ 'if' expr 'then' expr 'else' ] : if
  .operators infix right
    [ '?' expr ':' ] : ternary

# With this approach, you always need to
# write an else clause.
`,"input":
`x ? y : z
if x then y else z
`},
"end":{"grammar":
`# PREV < #ternary Ternary Operators

# You've reached the end of the tour!
# Check out <https://github.com/ianh/owl>
# for more information. Or go back to the
# #start.

# Here's Owl's grammar parsing itself:

grammar = (rule | comment-token)*
rule = identifier '=' body
body = expr | (expr ':' identifier)+ operators*
operators = '.operators' fixity operator+
fixity =
  'postfix' : postfix-op
  'prefix' : prefix-op
  'infix' assoc : infix-op
assoc =
  'flat' : flat-op
  'left' : left-op
  'right' : right-op
  'nonassoc' : nonassoc-op
operator = expr ':' identifier
expr =
  identifier ('\\\\' ':' identifier@exception)*
    ('@' identifier@rename)? : ident
  string : literal
  [ '(' expr ')' ] : parens
  [ '[' string@begin-token expr\\:choice?
    string@end-token ']' ] : bracketed
 .operators postfix
  '*' : zero-or-more
  '+' : one-or-more
  '?' : optional
 .operators infix flat
  '' : concatenation
 .operators infix flat
  '|' : choice
comment-token = 'line-comment-token' string
line-comment-token '#'
`,"input":
`grammar = (rule | comment-token)*
rule = identifier '=' body
body = expr | (expr ':' identifier)+ operators*
operators = '.operators' fixity operator+
fixity =
  'postfix' : postfix-op
  'prefix' : prefix-op
  'infix' assoc : infix-op
assoc =
  'flat' : flat-op
  'left' : left-op
  'right' : right-op
  'nonassoc' : nonassoc-op
operator = expr ':' identifier
expr =
  identifier ('\\\\' ':' identifier@exception)* ('@' identifier@rename)? : ident
  string : literal
  [ '(' expr ')' ] : parens
  [ '[' string@begin-token expr\\:choice? string@end-token ']' ] : bracketed
 .operators postfix
  '*' : zero-or-more
  '+' : one-or-more
  '?' : optional
 .operators infix flat
  '' : concatenation
 .operators infix flat
  '|' : choice
comment-token = 'line-comment-token' string
line-comment-token '#'
`},
"table-of-contents":{"grammar":
`
this = 'Table of Contents'

# #start Start Page
# #tour A Grammar for Arrays
# #nested Arrays of Arrays
# #ambiguity Detecting Ambiguity
# #named Named Choices
# #expr Operators & Expressions
# #call Function Calls
# #stmt An If Statement
# #if If as an Operator
# #ternary Ternary Operators
# #end End of Tour
`,"input":"Table of Contents"}};
