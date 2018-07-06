# using the generated parser

When you run bluebird with the `-c` option, it outputs a C header file representing a generated parser.  This document describes the functions and data types provided by this header.

## getting something working

Here are some steps you can follow to create a new program that uses a generated parser:

1. Write a grammar for your language in `your-grammar.bb` (you can test it out before you compile it by running `bluebird your-grammar.bb`).
2. Run `bluebird -c your-grammar.bb -o parser.h` to generate the `parser.h` header file.
3. Create a file called `main.c` that looks like:

   ```
   #define BLUEBIRD_PARSER_IMPLEMENTATION
   #include "parser.h"
   
   int main()
   {
       struct bluebird_tree *tree;
       tree = bluebird_tree_create_from_file(stdin);
       bluebird_tree_print(tree);
       bluebird_tree_destroy(tree);
       return 0;
   }
   ```
   
4. Build and run `main.c` using your favorite build system (or by running `cc -o test main.c && ./test`).
5. Type some input matching your grammar into standard input to test that things are working.  You should see a parse tree matching your input.  Make sure to mark the end of the input using Control+D (or whatever the key shortcut for "end of input" is in your terminal).

Now that we've got something working, let's take a closer look at what's happening in this program.

## creating a tree

Bluebird uses the `struct bluebird_tree` type to represent a parse tree.  There are two ways to create a tree:

### from a string

```
struct bluebird_tree *tree = bluebird_tree_create_from_string(string);
```

The tree returned by `bluebird_create_tree_from_string` may reference pieces of its string argument.  It's important to keep the string around until you're done with the tree.

### from a file

```
struct bluebird_tree *tree = bluebird_tree_create_from_file(file);
```

Bluebird will copy the contents of the file into an internal buffer, so feel free to close the file after calling this function.

### reporting errors

There are a few kinds of errors that can happen while creating a tree:

| error type | what it means | error range |
| --- | --- | --- |
| `ERROR_INVALID_FILE` | The argument to `bluebird_tree_create_from_file` was NULL, or there was an error while reading it. | None. |
| `ERROR_INVALID_TOKEN` | Part of the text didn't match any valid token. | A range that begins with the first unrecognized character. |
| `ERROR_UNEXPECTED_TOKEN` | The parser encountered an out-of-place token that didn't fit the grammar. | The range of the unexpected token. |
| `ERROR_MORE_INPUT_NEEDED` | The input is valid so far, but incomplete; more tokens are necessary to complete it. | A range positioned at the end of the input. |

If one of these errors happens, the `bluebird_create_tree_from_...` functions return an *error tree*.  Calling any function other than `bluebird_tree_destroy` on an error tree will print the error and exit.

To handle the error yourself, you can use `bluebird_tree_get_error`.

```
struct source_range range;
switch (bluebird_tree_get_error(tree, &range)) {
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

## inside the tree

Each time a rule matches part of the input, bluebird records details of the match in a hierarchical structure—that's the parse tree.  Let's take a look at an example grammar that matches lists:

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

matches some part of the input, bluebird will record

* the range of text that `list` matches, and
* match details for each `item` in the list.

This recorded data is represented by a `parsed_list` struct:

```
struct parsed_list {
    struct source_range range;
    struct bluebird_ref item;
};
```

Note that `item` is a `struct bluebird_ref`, not a `struct parsed_item`.  To save memory and improve locality, bluebird stores parse tree data in a compressed format.  A `struct bluebird_ref` is like a pointer into this compressed data.  You can unpack a ref into a `struct parsed_item` using the function `parsed_item_get`, which returns the unpacked data.

```
struct parsed_item first_item = parsed_item_get(list.item);
```

There's a function like this for every rule:

| rule name | match data type | unpacking function |
| --- | --- | --- |
| `list` | `struct parsed_list` | `parsed_list_get` |
| `item` | `struct parsed_item` | `parsed_item_get` |

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
    struct bluebird_ref number;
    struct bluebird_ref list;
};
```

and a `parsed_type` enum is generated with three values:

```
enum parsed_type {
    PARSED_LIST = 1,
    PARSED_NOTHING,
    PARSED_NUMERIC,
};
```

All option names go into the same `parsed_type` enum to avoid unneccessary prefixing.

## moving around the tree

The first rule (or *root rule*) in the grammar matches the entire input.  This *root match* is our starting point in the parse tree.  If `list` is the first rule in the grammar, bluebird will generate a `bluebird_tree_get_parsed_list` function which returns the root match:

```
struct parsed_list list = bluebird_tree_get_parsed_list(tree);
```

You can iterate over the list's items using `bluebird_next`:

```
while (!list.item.empty) {
    struct parsed_item item = parsed_item_get(list.item);
    // ...do something with item...
    list.item = bluebird_next(list.item);
}
```

The compressed parse tree data is immutable—reassigning `list.item` just reassigns the local copy.  You can also use a `for` loop if you don't want to reassign anything:

```
for (struct bluebird_ref r = list.item; !r.empty; r = bluebird_next(r)) {
    struct parsed_item item = parsed_item_get(r);
    // ...do something with item...
}
```

Each `bluebird_ref` is valid for as long as the tree is—you can store them and reuse them as much as you want.

## token data

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

struct parsed_identifier parsed_identifier_get(struct bluebird_ref);
struct parsed_number parsed_number_get(struct bluebird_ref);
struct parsed_string parsed_string_get(struct bluebird_ref);
```

If `has_escapes` is true, the string data is owned by the `bluebird_tree`—otherwise, it's a direct reference to the parsed text.

## cleaning up

When you're done with a tree, use `bluebird_tree_destroy(tree)` to reclaim its memory.
