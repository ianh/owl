#using bluebird.v1
command =
    identifier '=' expression : assign
    expression : evaluate
expression =
    identifier : variable
    number : number
    [ '(' expression ')' ] : parens
  .operators prefix
    '-' : negate
  .operators infix left
    '*' : multiply
    '/' : divide
  .operators infix left
    '+' : add
    '-' : subtract
