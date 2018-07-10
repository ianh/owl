# using the generated parser

When you run `owl` with the `-c` option, it outputs a C header file representing a generated parser.  This document describes the functions and data types provided by this header.

## integrating the parser

The header file has two parts (in [single-file library](https://github.com/nothings/single_file_libs) style): a header-like part and an implementation-like part.  By default, including the header only includes the header-like part.  To include the implementation as well, define `OWL_PARSER_IMPLEMENTATION` before using `#include`:

```
// Include parser implementation.
#define OWL_PARSER_IMPLEMENTATION
#include "parser.h"
```

The implementation should be included by a single `.c` file somewhere in your project.

### step-by-step

Here are some steps you can follow to create a new program that uses a generated parser:

1. Write a grammar for your language in `your-grammar.owl` (you can test it out before you compile it by running `owl your-grammar.owl`).
2. Run `owl -c your-grammar.owl -o parser.h` to generate the `parser.h` header file.
3. Create a file called `main.c` that looks like:

   ```
   #define OWL_PARSER_IMPLEMENTATION
   #include "parser.h"
   
   int main()
   {
       struct owl_tree *tree;
       tree = owl_tree_create_from_file(stdin);
       owl_tree_print(tree);
       owl_tree_destroy(tree);
       return 0;
   }
   ```
   
4. Build and run `main.c` using your favorite build system (or by running `cc -o test main.c && ./test`).
5. Type some input matching your grammar into standard input to test that things are working.  You should see a parse tree matching your input.  Make sure to mark the end of the input using Control+D (or whatever the key shortcut for "end of input" is in your terminal).

## creating a tree

Owl uses the `struct owl_tree` type to represent a parse tree.  There are two ways to create a tree:

### from a string

```
struct owl_tree *tree = owl_tree_create_from_string(string);
```

The tree returned by `owl_create_tree_from_string` may reference pieces of its string argument.  It's important to keep the string around until you're done with the tree.

### from a file

```
struct owl_tree *tree = owl_tree_create_from_file(file);
```

Owl will copy the contents of the file into an internal buffer, so feel free to close the file after calling this function.

### reporting errors

There are a few kinds of errors that can happen while creating a tree:

| error type | what it means | error range |
| --- | --- | --- |
| `ERROR_INVALID_FILE` | The argument to `owl_tree_create_from_file` was NULL, or there was an error while reading it. | None. |
| `ERROR_INVALID_TOKEN` | Part of the text didn't match any valid token. | A range that begins with the first unrecognized character. |
| `ERROR_UNEXPECTED_TOKEN` | The parser encountered an out-of-place token that didn't fit the grammar. | The range of the unexpected token. |
| `ERROR_MORE_INPUT_NEEDED` | The input is valid so far, but incomplete; more tokens are necessary to complete it. | A range positioned at the end of the input. |

If one of these errors happens, the `owl_create_tree_from_...` functions return an *error tree*.  Calling any function other than `owl_tree_destroy` on an error tree will print the error and exit.

To handle the error yourself, you can use `owl_tree_get_error`.

```
struct source_range range;
switch (owl_tree_get_error(tree, &range)) {
case ERROR_NONE:
    // No error; continue with the rest of the program.
    break;
case ERROR_INVALID_FILE:
    fprintf(stderr, "error: invalid file\n");
    exit(1);
default:
    fprintf(stderr, "error: at range %zu %zu\n", range.start, range.end);
    exit(1);
}
```

### cleaning up

When you're done with a tree, use `owl_tree_destroy(tree)` to reclaim its memory.

## inside the tree

Each time a rule matches part of the input, Owl records details of the match in a hierarchical structure—that's the parse tree.  Let's see what this tree looks like for an example grammar that matches lists:

```
list = item (',' item)*
item =
  'nil' : nothing
  number : numeric
  [ '[' list ']' ] : list
```

If the rule

```
list = item (',' item)*
```

matches some part of the input, Owl will record

* the range of text that `list` matches, and
* match details for each `item` in the list.

This recorded data is represented by a `parsed_list` struct:

```
struct parsed_list {
    struct source_range range;
    struct owl_ref item;
};
```

Note that `item` is a `struct owl_ref`, not a `struct parsed_item`.  To save memory and improve locality, Owl stores parse tree data in a compressed format.  A `struct owl_ref` is like a pointer into this compressed data.  You can unpack a ref into a `struct parsed_item` using the function `parsed_item_get`, which returns the unpacked data.

```
struct parsed_item first_item = parsed_item_get(list.item);
```

There's a function like this for every rule:

| rule name | match data type | unpacking function |
| --- | --- | --- |
| `list` | `struct parsed_list` | `parsed_list_get` |
| `item` | `struct parsed_item` | `parsed_item_get` |

### named options

If a rule has named options, the chosen option is indicated by the `type` field in the match struct.  For example, our `item` rule

```
item =
  'nil' : nothing
  number : numeric
  [ '[' list ']' ] : list
```

has three named options.  Its struct has a `type` field to indicate which option was chosen:

```
struct parsed_item {
    struct source_range range;
    enum parsed_type type;
    struct owl_ref number;
    struct owl_ref list;
};
```

and a `parsed_type` enum is generated with three values.

```
enum parsed_type {
    PARSED_LIST = 1,
    PARSED_NOTHING,
    PARSED_NUMERIC,
};
```

All option names go into the same `parsed_type` enum to avoid unneccessary prefixing.

### renames

Occasionally, a rule will have multiple references to the same rule that need to be distinguished from one another:

```
set-index = expr '[' expr ']' '=' expr
```

The `@` operator in the grammar can be used to rename these references.

```
set-index = expr@array '[' expr@index ']' '=' expr@value
```

The new names appear in the fields of the parse tree struct:

```
struct parsed_set_index {
    struct source_range range;
    struct owl_ref array;
    struct owl_ref index;
    struct owl_ref value;
};
```

### getting the root match

The first rule (or *root rule*) in the grammar matches the entire input.  This *root match* is our starting point in the parse tree.  If `list` is the first rule in the grammar, Owl will generate an `owl_tree_get_parsed_list` function which returns the root match:

```
struct parsed_list list = owl_tree_get_parsed_list(tree);
```

### iterating

You can iterate over the list's items using `owl_next`:

```
while (!list.item.empty) {
    struct parsed_item item = parsed_item_get(list.item);
    // ...do something with item...
    list.item = owl_next(list.item);
}
```

The compressed parse tree data is immutable—reassigning `list.item` just reassigns the local copy.  You can also use a `for` loop if you don't want to reassign anything:

```
for (struct owl_ref r = list.item; !r.empty; r = owl_next(r)) {
    struct parsed_item item = parsed_item_get(r);
    // ...do something with item...
}
```

Each `owl_ref` is valid for as long as the tree is—you can store them and reuse them as much as you want.

### token data

The `identifier`, `number`, and `string` tokens record data about their match like rules do.  This data can also be unpacked using `parsed_identifier_get`, `parsed_number_get`, or `parsed_string_get`.

```
struct parsed_identifier {
    struct source_range range;
    const char *identifier;
    size_t length;
};

struct parsed_number {
    struct source_range range;
    double number;
};

struct parsed_string {
    struct source_range range;
    const char *string;
    size_t length;
    bool has_escapes;
};

struct parsed_identifier parsed_identifier_get(struct owl_ref);
struct parsed_number parsed_number_get(struct owl_ref);
struct parsed_string parsed_string_get(struct owl_ref);
```

If `has_escapes` is true, the string data is owned by the `owl_tree`—otherwise, it's a direct reference to the parsed text.

## function index

`ROOT` is the root rule name.  `RULE` ranges over all rules.

| name | arguments | return value |
| --- | --- | --- |
| `owl_next` | An `owl_ref`. | The next ref matching the corresponding field in the rule, or an empty ref. |
| `owl_refs_equal` | Two `owl_ref` values. | `true` if the refs refer to the same match; `false` otherwise. |
| `owl_tree_create_from_file` | A `FILE *` to read from.  The file is read into an intermediate string and may be closed immediately. | A new tree. |
| `owl_tree_create_from_string` | A null-terminated string to parse.  You retain ownership and must keep the string around until the tree is destroyed. | A new tree. |
| `owl_tree_destroy` | An `owl_tree *` to destroy, freeing its resources back to the system.  May be `NULL`. | None. |
| `owl_tree_get_error` | An `owl_tree *` and an `error_range` out-parameter.  The error range may be `NULL`. | An error which interrupted parsing, or `ERROR_NONE` if there was no error. |
| `owl_tree_get_parsed_ROOT` | An `owl_tree *`. | A `parsed_ROOT` struct corresponding to the root match. |
| `owl_tree_print` | An `owl_tree *` to print to stdout (typically for debugging purposes).  Must not be `NULL`. | None. |
| `owl_tree_root_ref` | An `owl_tree *`. | The ref corresponding to the root match. |
| `parsed_identifier_get` | An `owl_ref` corresponding to an identifier match. | A `parsed_identifier` struct corresponding to the identifier match. |
| `parsed_number_get` | An `owl_ref` corresponding to a number match. | A `parsed_number` struct corresponding to the number match. |
| `parsed_string_get` | An `owl_ref` corresponding to a string match. | A `parsed_string` struct corresponding to the identifier match. |
| `parsed_RULE_get` | An `owl_ref` corresponding to a match for `RULE`. | A `parsed_RULE` struct corresponding to the ref's match. |
