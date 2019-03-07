tour = {
"start":{"grammar":
`# Welcome to Owl's interpreter mode!

# Here's an example grammar (feel free to
# modify or delete it):

face = eyes nose mouth
eyes =
  ':' : normal
  ';' : winking
nose =
  '' : none
  '-' : straight
  '^' : pointy
  '*' : clown
mouth =
  ')' : smile
  '(' : frown
  'D' : open-smile

# A copy of the Owl tool runs in your
# browser each time you modify this grammar
# or the input below.  Its output appears in
# the box on the right.

# You can also take a tour here: #tour
`,"input": 
`:^)
`},
"tour":{"grammar":
`# PREV < #start Start Page
# NEXT > #nested Arrays of Arrays

# Welcome to the tour of Owl!

# In this tour, we'll go through a bunch
# of small examples which show off various
# features of Owl.

# Here's a grammar for arrays:

input = array*
array = '[' (identifier ',')* identifier ']'

# To match repetitions, this grammar uses *,
# which matches its operand any number of
# times -- like a regular expression.  Owl
# grammars also support ?, +, and |.
`,"input":
`[red, orange, yellow, green, blue, purple]
[up, down, left, right]
[good, evil]
[x, y, z]
`},
"nested":{"grammar":
`# PREV < #tour A Grammar for Arrays
# NEXT > #ambiguity Detecting Ambiguity

# This array contains other arrays:

input = array*
array = [ '[' ((array ',')* array)? ']' ]

# Note the use of guard brackets [ ... ]
# which allow the rule to refer to itself.
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

# If a single input can produce two different
# parse trees, Owl will report it as an
# ambiguity.

a = three+ | five+
three = ('.'|'o') ('.'|'o') ('.'|'o')
five = 'o' '.' '.' '.' '.'

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
# NEXT > #complex Complex Operators

# Owl's ".operators" keyword can be used to
# match mathematical expressions.

input = expr\\:negate*
expr =
    number : num
    identifier : var
    [ '(' expr ')' ] : parens
  .operators prefix
    '-' : negate
  .operators infix left
    '^' : pow
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
x + v*t + 1/2*a*t^2
`},
"complex":{"grammar":
`# PREV < #expr Operators & Expressions
# NEXT > #stmt An If Statement

# Operators can match complex patterns as
# well as individual tokens.

input = expr*
expr =
    identifier : ident
  .operators postfix
    [ '(' ((expr ',')* expr)? ')' ] : call
  .operators infix right
    [ '?' expr ':' ] : ternary

# This grammar matches the arguments of a
# procedure call as a postfix operator.  A
# C-like ternary operator is also provided
# as an infix operator.
`,"input":
`fire_the_missiles()
add(exp(x), y)
light_intensity_at(x)(y)(z)
out_of_space ? allocate() : p
`},
"stmt":{"grammar":
`# PREV < #complex Complex Operators
# NEXT > #json A Grammar for JSON

# Here's an "if" statement which supports
# optional "else if" and "else" clauses:

input = stmt*
stmt =
    'if' expr [ '{' stmt* '}' ]
     ('else' 'if' expr [ '{' stmt* '}' ])*
     ('else' [ '{' stmt* '}' ])? : if-else
    expr '.' : expr
expr = identifier+
`,"input":
`if input is ready {
  dequeue the input.
  process the input.
} else if we reached the end of the input {
  validate work.
  if work is complete and valid {
    finish successfully.
  } else {
    return error.
  }
} else {
  wait for more input.
}
`},
"json":{"grammar":
`# PREV < #stmt An If Statement
# NEXT > #end End of Tour

# A grammar for JSON.

input = value*
value =
 [ '{' (string ':' value
  (',' string ':' value)*)? '}' ] : object
 [ '[' (value (',' value)*)? ']' ] : array
 string : string
 number : pos-number
 '-' number : neg-number
 'true' : true
 'false' : false
 'null' : null

# (It's actually not quite JSON, since
# Owl doesn't handle string escapes in the
# same way).

`,"input":
`{
  "name": "Jane",
  "posts":[{
    "title": "The uses of sidewalks",
    "visible": true,
    "text": "..."
  }]
}
`},
"end":{"grammar":
`# PREV < #json A Grammar for JSON

# You've reached the end of the tour!
# Check out <https://github.com/ianh/owl>
# for more information. Or go back to the
# #start.

# Here's Owl's own grammar parsing itself:

grammar = (rule | comment-token | custom-token | whitespace)*
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
  [ '{' (repetition | expr (',' repetition)?)? '}' ] : repetition
 .operators infix flat
  '' : concatenation
 .operators infix flat
  '|' : choice
repetition =
  integer@begin : exact
  integer@begin '+' : at-least
  integer@begin ',' integer@end : range
comment-token = '.line-comment-token' string | comment-token-v1
comment-token-v1 = 'line-comment-token' string
custom-token = '.token' identifier string*
whitespace = '.whitespace' string*

.line-comment-token '#'`,"input":
`grammar = (rule | comment-token | custom-token | whitespace)*
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
  [ '{' (repetition | expr (',' repetition)?)? '}' ] : repetition
 .operators infix flat
  '' : concatenation
 .operators infix flat
  '|' : choice
repetition =
  integer@begin : exact
  integer@begin '+' : at-least
  integer@begin ',' integer@end : range
comment-token = '.line-comment-token' string | comment-token-v1
comment-token-v1 = 'line-comment-token' string
custom-token = '.token' identifier string*
whitespace = '.whitespace' string*

.line-comment-token '#'
`},
"arith":{"grammar":
`input = expr*
expr =
    number : num
    [ '(' expr ')' ] : parens
  .operators prefix
    '-' : negate
  .operators infix left
    '+' : plus
    '-' : minus
`,"input":""},
"example":{"grammar":
`program = stmt*
stmt =
   'print' expr : print
   identifier '=' expr : assign
expr =
   [ '(' expr ')' ] : parens
   identifier : variable
   number : literal
 .operators prefix
   '-' : negative
 .operators infix left
   '*' : times
   '/' : divided-by
 .operators infix left
   '+' : plus
   '-' : minus
`,"input":
`x = 7
print 1 + 2 * x
`}};
