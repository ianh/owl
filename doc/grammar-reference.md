# grammar reference

A bluebird grammar is a sequence of rules: `rule-name = ...`.

## patterns

The simplest kind of rule has the form `rule-name = pattern`.

A *pattern* matches sequences of tokens. It's made up of *atomic patterns*, which match individual tokens:

| atomic pattern | what it matches |
| --- | --- |
| `'keyword'` | A keyword matches a literal sequence of characters; this keyword would match the text `keyword`.  The keyword string can contain any character. |
| `'a;ksdf;aklsdjf'` | This keyword would match the text `a;ksdf;aklsdjf`. |
| `string` | A string begins with a `'` or `"`, then continues to the next matching `'` or `"`, skipping over characters escaped with `\`.  For example, `"\n"` produces the same string as `"n"`. |
| `number` | A number begins with a digit (0-9) or a decimal point (`.`) followed by a digit.  Once the beginning of the number is matched, the rest of the number is read using `strtod()`.  Note that `number` doesn't match a leading `-` to avoid ambiguity with the `-` operator: you should add negation at a higher level in your grammar. |
| `identifier` | An identifier begins with a letter or an underscore `_`, then continues with letters, underscores, and digits (0-9).  Dashes are also allowed as long as you don't have a `-` keyword anywhere in your grammar. |

The input text is decomposed into tokens based on all the atomic patterns that appear in the grammar.  At each point, the longest token which matches the input text is chosen.  In case of ties between keywords and other tokens, the keyword is chosen.

The space, tab, carriage return, and newline characters are considered *whitespace*.  Whitespace can be used to separate tokens but is otherwise ignored.

The `#` character begins a comment, which causes the rest of its line to be interpreted as whitespace.

As a special case, the empty keyword `''` matches an empty sequence of tokens:

| atomic pattern | what it matches |
| --- | --- |
| `''` | *this cell intentionally left blank* |

Patterns can be combined in the following ways to match longer sequences:

| combined pattern | what it matches | notes |
| --- | --- | --- |
| `(a)` | The same thing as `a`. | `(a)` is the same as `a` |
| `a b` | `a` followed by `b`. | `a ''` is the same as `a` |
| `a | b` | Either `a` or `b`. | `a | a` is the same as `a` |
| `a?` | Either `a` or nothing. | `a?` is the same as `a | ''` |
| `a*` | Any number of `a` (including no `a`). | `(a* b*)*` is the same as `(a | b)*` |
| `a+` | One or more `a`. | `a+` is the same as `a a*` |

Concatenation `a b` binds more tightly than choice `|`, so `a b | c` is the same as `(a b) | c`.

Guard brackets enclose a pattern in begin and end keywords:

| combined pattern | what it matches |
| --- | --- |
| `[ '(' a ')' ]` | The begin keyword `(`, followed by `a`, followed by the end keyword `)`. |

The keywords that begin and end a guard bracket are called the *begin keyword* and the *end keyword*.  A keyword used in a normal pattern can't be used as a begin or end keyword.  A begin keyword also can't be used as an end keyword (or vice versa).

## named choices

A rule with named choices has the form

```
rule-name =
    a : choice-1
    b : choice-2
    ...
```

This rule matches the same token sequences as

```
rule-name = a | b | ...
```

but keeps track of which choice was picked in the resulting parse tree.

## operators

A rule with operators is a rule with named choices followed by at least one *operator group*.


```
rule-name =
    a : choice-1
    b : choice-2
    ...
  .operators prefix
    c : op-1-1
    d : op-1-2
    ...
  .operators infix left
    e : op-2-1
    ...
```

An operator group begins with the `.operators` keyword, followed by the operator group's *fixity* (and *associativity*, for infix operators).  Groups are listed in decreasing order of precedence—groups closer to the top bind more tightly.

Each operator in the group has the form `pattern : op-name`.

An operator group matches token sequences based on its fixity.  If `next` represents the next group up in the list, and `op` represents the operators in the group, each group matches the sequences:

| operator group | what each operator matches |
| --- | --- |
| `.operators prefix` | `op* next` |
| `.operators postfix` | `next op*` |
| `.operators infix left` | `next (op next)*` |
| `.operators infix right` | `next (op next)*` |
| `.operators infix flat` | `next (op next)*` |
| `.operators infix nonassoc` | `next op next` |

Note that the `infix left`, `infix right`, and `infix flat` groups match the same token sequences, but the resulting parse trees will be different.

*`infix left` tree:*

```
       rule           
       /  \           
    rule  right       
    /  \             
 left  right         
```

*`infix right` tree:*

```
    rule         
    /  \         
 left  rule      
       /  \
    left  right
```

*`infix flat` tree:*

```
          rule
        /   |   \
 operand operand operand
```


## referring to rules

Every rule name is also a pattern.

| pattern | what it matches |
| --- | --- |
| `a` | The rule `a`. |
| `a@rename` | The rule `a`, but renamed to `rename` in the resulting parse tree. |
| `a\:b` | The rule `a`, but excluding the choice or operator `b`. |
| `a\:b\:c@d` | The rule `a`, excluding the choices (or operators) `b` and `c`, renamed to `d` in the resulting parse tree. |

Any identifier can be a rule name, including `identifier`, `string`, and `number`.  If you name a rule after one of these tokens, its name will refer to the rule instead of the token.

Except within guard brackets `[ ]`, a rule can only refer to rules after it in the grammar.  The first rule in the grammar is matched against the input text directly; other rules are only useful as patterns.

## comments

A *line comment token* will cause the rest of the line on which it appears to be treated like whitespace.  You can specify line comment tokens using `line-comment-token`.

```
line-comment-token '//'
```

## grammar.bb

For a precise description of the grammar, here's the file bluebird uses to generate its own parser:

```
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
```

