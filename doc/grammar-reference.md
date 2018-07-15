# grammar reference

An Owl grammar is made up of rules of the form `rule-name = ...`.  The first rule (or *root rule*) is matched against the input—the other rules can be used in patterns (described below) within the root rule or other rules.

## patterns

A *pattern* matches sequences of tokens.

The simplest kind of rule is a single pattern: `rule-name = pattern`.

Each pattern is made up of *atomic patterns*, which match individual tokens:

| atomic pattern | what it matches |
| --- | --- |
| `'keyword'` | A keyword is a literal sequence of characters; this keyword would match the token `keyword`.  The keyword string can contain any character. |
| `'a;ksdf;aklsdjf'` | This keyword would match the token `a;ksdf;aklsdjf`. |
| `string` | A string begins with a `'` or `"`, then continues to the next matching `'` or `"`, skipping over characters escaped with `\`.  For example, `"\n"` produces the same string as `"n"`. |
| `number` | A number begins with a digit (0-9) or a decimal point (`.`) followed by a digit.  Once the beginning of the number is matched, the rest of the number is read using `strtod()`.  Note that `number` doesn't match a leading `-` to avoid ambiguity with the `-` operator: you should add negation at a higher level in your grammar. |
| `identifier` | An identifier begins with a letter or an underscore `_`, then continues with letters, underscores, and digits (0-9).  Dashes are also allowed as long as you don't have a `-` keyword anywhere in your grammar. |

Owl decomposes the input text into tokens based on all the atomic patterns that appear in the grammar.  At each point, the longest token which matches the input text is chosen.  In case of ties between keywords and other tokens, the keyword is chosen.

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

This restriction limits Owl to parsing *visibly push-down* languages, which it can more easily check for ambiguity.

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

but keeps track of which choice was picked (and records the choice in the resulting parse tree).

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

An operator group begins with the `.operators` keyword and the operator group's *fixity* (and *associativity*, for infix operators).  Groups are listed in decreasing order of precedence—groups closer to the top bind more tightly.

Each operator in the group has the form `pattern : op-name`.

An operator matches token sequences based on the fixity of its group.  In the following table, `next` represents the next group up in the list, and `op` represents the operators in the group.

| operator group | what it matches |
| --- | --- |
| `.operators prefix` | `op* next` |
| `.operators postfix` | `next op*` |
| `.operators infix left` | `next (op next)*` |
| `.operators infix right` | `next (op next)*` |
| `.operators infix flat` | `next (op next)*` |
| `.operators infix nonassoc` | `next op next` |

Note that the `infix left`, `infix right`, and `infix flat` groups match the same token sequences.  The resulting parse trees will be different, however:

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

You can name a rule using any identifier, including `identifier`, `string`, and `number`.  If you name a rule after one of these tokens, its name will refer to the rule instead of the token.

Except within guard brackets `[ ]`, a rule can only refer to rules after it in the grammar.  The first rule in the grammar is matched against the input text directly; other rules are only useful as patterns.

## comments

A *line comment token* will cause the rest of the line on which it appears to be treated like whitespace.  You can specify line comment tokens using `line-comment-token`.

```
line-comment-token '//'
```

## versioning

The Owl grammar format may change in the future.  To allow future versions to interpret older grammars as they were originally written, Owl matches its current version against a version specified at the beginning of your grammar file:

```
#using owl.v1
```

If the version is older, Owl may attempt to interpret the grammar in the same way that older version did.  If the version is newer, Owl will exit with an error.  If no version is specified, Owl will assume you're OK with the current version.

To see the current version string for your installation, run `owl --version`:

```
$ owl --version
owl.v1
```

## grammar.owl

Here's the file Owl uses to generate its own parser—you can read it both as an example of a grammar and as the definition of the grammar syntax itself.

```
#using owl.v1

# This is the grammar for Owl itself.
# Compile with `owl -c grammar.owl -o 1-parse.h`.

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
  [ '[' string@begin-token expr\:choice? string@end-token ']' ] : bracketed
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
