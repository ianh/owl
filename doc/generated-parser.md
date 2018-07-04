# using the generated parser

When you run bluebird with the `-c` option, it outputs a C header file representing a generated parser.  Here's how to use this generated parser.

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
5. Type some input matching your grammar into standard input to test that things are working.  You should see a parse tree matching your input.

Once you've got this working, it should be easier to make progress toward a more complete program.

## creating a tree

Bluebird uses `struct bluebird_tree` to represent a parse tree.  There are two ways to create a tree:

### from a string

```
struct bluebird_tree *tree = bluebird_tree_create_from_string(string);
```

The tree returned by `bluebird_create_tree_from_string` may reference pieces of its string argument, so you'd better keep the string around until you're done with the tree.

### from a file

```
struct bluebird_tree *tree = bluebird_tree_create_from_file(file);
```

Bluebird will copy the contents of the file into an internal buffer, so feel free to close the file after calling this function.

### reporting errors

There are four kinds of errors that can happen while creating a tree:

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
    fprintf(stderr, "error: invalid file '%s'\n", filename);
    exit(1);
default:
    fprintf(stderr, "error: at range %zu %zu\n", range.start, range.end);
    exit(1);
}
```

### cleaning up

When you're done with a tree, use `bluebird_tree_destroy(tree)` to reclaim its memory.

## climbing the tree

The `struct bluebird_node` type represents a location in the parse tree.

The entry point to the tree is called the *root node*—it corresponds to the first rule in the grammar (the *root rule*).  This node is the "trunk" of the parse tree; get a hold of it with `bluebird_tree_root_node`:

```
struct bluebird_node root = bluebird_tree_root_node(tree);
```



### more about nodes


(error_range)