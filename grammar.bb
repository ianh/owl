# This is the grammar for bluebird itself.
# Compile with `bluebird -c grammar.bb -o 1-parse.h`.

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
  identifier ('\\' ':' identifier@exception)* ('@' identifier@rename)? : ident
  string : literal
  [ '(' expr ')' ] : parens
  [ '[' string@begin-token expr? string@end-token ']' ] : bracketed
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
