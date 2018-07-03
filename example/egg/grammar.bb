#using bluebird.v1

program = stmt-list
stmt-list = stmt*
stmt =
    [ 'function' identifier parameter-list stmt-list 'end' ] : function
    identifier '=' expr : assignment
    'local' identifier '=' expr : variable
    expr\:negate\:table\:parens@table '.' identifier '=' expr : field-assignment
    expr\:negate\:table\:parens@table [ '[' expr@lookup ']' ] '=' expr : lookup-assignment
    'return' expr : return
    [ 'if' expr 'then' stmt-list
      ('elseif' expr 'then' stmt-list)*
      ('else' stmt-list@else-stmts)? 'end' ] : if-then
    [ 'while' expr 'do' stmt-list 'end' ] : while
    [ 'for' (identifier@key '=')? identifier@value 'in' expr 'do' stmt-list 'end' ] : for
    'break' : break
    'continue' : continue
    expr\:negate\:table\:parens : expr
table-entry = (identifier '=')? expr
expr =
    string : string
    number : number
    'true' : true
    'false' : false
    identifier : variable
    [ '(' expr ')' ] : parens
    [ '[' (table-entry (',' table-entry)*)? ']' ] : table
  .operators postfix
    [ '(' (expr (',' expr)*)? ')' ] : call
    [ '[' expr ']' ] : lookup
    '.' identifier : field
  .operators prefix
    '-' : negate
  .operators infix left
    '*' : times
    '/' : divided-by
    '%' : modulus
  .operators infix left
    '+' : plus
    '-' : minus
  .operators infix left
    '&&' : and
  .operators infix left
    '||' : or
  .operators infix left
    '==' : equal-to
    '!=' : not-equal-to
    '<' : less-than
    '>' : greater-than
    '<=' : less-than-or-equal-to
    '>=' : greater-than-or-equal-to
parameter-list =
    [ '(' (identifier (',' identifier)*)? ')' ]

line-comment-token '#'
