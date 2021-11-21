# grammar reference

An Owl grammar is made up of rules of the form `rule-name = ...`.  The first rule (or *root rule*) is matched against the input—the other rules can be used in patterns (described below) within the root rule or other rules.

## patterns

A *pattern* matches sequences of tokens.

The simplest kind of rule is a single pattern: `rule-name = pattern`.

Each pattern is made up of *atomic patterns*, which match individual tokens:

| atomic pattern | what it matches |
| --- | --- |
| `'keyword'` | A keyword is a literal sequence of characters; this keyword would match the token `keyword`.  The keyword string can contain any character. |
| `'a;ksdf\'aklsdjf'` | This keyword would match the token `a;ksdf'aklsdjf`. |
| `string` | A string begins with a `'` or `"`, then continues to the next matching `'` or `"`.  Any character can be escaped with `\` so that it doesn't end the string.  For example, `"\""` is the string for `"`.  Escaping the characters `b`, `f`, `n`, `r`, or `t` will produce the backspace, form-feed, newline, carriage return, and tab characters, respectively. |
| `number` | A number begins with a digit (0-9) or a decimal point (`.`) followed by a digit.  Once the beginning of the number is matched, the rest of the number is read using `strtod()`.  Note that `number` doesn't match a leading `-` to avoid ambiguity with the `-` operator: you should add negation at a higher level in your grammar. |
| `integer` | An integer either consists entirely of digits (0-9) or begins with `0x` or `0X` and consists entirely of hexadecimal digits (A-Za-z0-9).  Integers must be less than 18446744073709551616.  If both `integer` and `number` appear in the same grammar, `integer` matches are preferred. |
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
| `a \| b` | Either `a` or `b`. | `a \| a` is the same as `a` |
| `a?` | Either `a` or nothing. | `a?` is the same as `a \| ''` |
| `a*` | Any number of `a` (including no `a`). | `(a* b*)*` is the same as `(a \| b)*` |
| `a+` | One or more `a`. | `a+` is the same as `a a*` |

Concatenation `a b` binds more tightly than choice `|`, so `a b | c` is the same as `(a b) | c`.

Curly brackets match explicitly-numbered repetition:

| combined pattern | what it matches | notes |
| --- | --- | --- |
| `a{3}` | Exactly three `a` in a row. | `a{1}` is the same as `a` |
| `a{3,5}` | Between three and five `a`. | `a{0,1}` is the same as `a?` |
| `a{3+}` | Three or more `a` in a row. | `a{1+}` is the same as `a+` |
| `a{}` | Any number of `a`. | `a{}` is the same as `a{0+}` and `a*` |
| `a{b}` | A list of `a` separated by `b`. | `a{b}` is the same as `(a (b a)*)?` |
| `a{b, 1+}` | A list of one or more `a` separated by `b`. | `a{b, 1+}` is the same as `a (b a)*` |

The delimiter syntax `a{b}` is useful for lists: `expr{','}` represents a list of `expr` matches separated by commas.  Optional trailing delimiters must be written out by hand: e.g., `(expr{',', 1+} ','?)?`.

Guard brackets enclose a pattern in begin and end keywords:

| combined pattern | what it matches |
| --- | --- |
| `[ '(' a ')' ]` | The begin keyword `(`, followed by `a`, followed by the end keyword `)`. |

The keywords that begin and end a guard bracket are called the *begin keyword* and the *end keyword*.  A keyword used in a normal pattern can't be used as a begin or end keyword.  A begin keyword also can't be used as an end keyword (or vice versa).  This restriction limits Owl to parsing visibly push-down languages.

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

You can name a rule using any identifier, including `identifier`, `integer`, `string`, and `number`.  If you name a rule after one of these tokens, its name will refer to the rule instead of the token.

Except within guard brackets `[ ]`, a rule can only refer to rules after it in the grammar.  The first rule in the grammar is matched against the input text directly; other rules are only useful as patterns.

## comments

A *line comment token* will cause the rest of the line on which it appears to be treated like whitespace.  You can specify line comment tokens using `.line-comment-token`.

```
.line-comment-token '//'
```

## configuring whitespace

Use `.whitespace` to specify strings that should be treated as whitespace.  The longest matching whitespace string is removed from the input before any other tokens are recognized.  Using `.whitespace` will override the default whitespace characters (space, tab, newline, and carriage return).

For example,


```
.whitespace ' ' '\t'
```

specifies spaces and tabs, preventing newlines or carriage returns from being considered whitespace.

Since they're no longer considered whitespace, newlines can now be used as a normal keyword:

```
lines = (line? '\n')*
line = ...
```

An empty list will disable built-in whitespace recognition altogether:

```
.whitespace
```

Note: no whitespace means no way to reliably separate tokens from one another.  This means ambiguity errors may not be presented correctly.

## <a id="user-defined-tokens">user-defined tokens</a>

To define your own type of token, use `.token`:

```
.token hex-color
```

Like rule names, the names of these *user-defined tokens* can also be used as patterns.

```
color = hex-color | ...
```

### exemplar strings

User-defined tokens are matched by [code you write](generated-parser.md#user-defined-tokens).  Owl's interpreter mode doesn't have access to this code, so it can't match user-defined tokens precisely.

Any strings following the token name become *exemplar strings*:

```
.token hex-color '#fff' '#DEFACE'
input = hex-color*
```

which stand in for the user-defined token in interpreter mode.

```shell
$ owl hex-color.owl
#fff #fff #DEFACE
^D
. #fff #fff #DEFACE
  input------------
```

If you don't provide any exemplar strings, the token name itself becomes the exemplar:

```shell
$ owl -g ".token hex-color  input = hex-color*"
hex-color
^D
. hex-color
  input----
```

## versioning

The Owl grammar format may change in the future.  To allow future versions to interpret older grammars as they were originally written, Owl matches its current version against a version specified at the beginning of your grammar file:

```
#using owl.v?
```

If the version is older, Owl may attempt to interpret the grammar in the same way that older version did.  If the version is newer, Owl will exit with an error.  If no version is specified, Owl will assume you're OK with the current version.

To see the current version string for your installation, run `owl --version`:

```
$ owl --version
owl.v?
```

## grammar.owl

Here's the file Owl uses to generate its own parser—you can read it both as an example of a grammar and as the definition of the grammar syntax itself.

```
#using owl.v4

# This is the grammar for Owl itself.
# Compile with `owl -c grammar.owl -o src/1-parse.h`.

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
  identifier ('\\' ':' identifier@exception)* ('@' identifier@rename)? : ident
  string : literal
  [ '(' expr ')' ] : parens
  [ '[' string@begin-token expr\:choice? string@end-token ']' ] : bracketed
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
```
