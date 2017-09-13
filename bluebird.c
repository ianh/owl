#include <stdio.h>
#include <string.h>

#include "1-parse.h"

int main()
{
    const char *string =
    "a = testing  b = one | two `one` two | (three | four)* `another` [ left [ expr ] right ] `third`"
    " op = ident `ident` infix left $ '*' `mult` '/' `div` infix left $ '+' `add` '-' `subtract`";
    struct bluebird_tree *tree = bluebird_tree_create_from_string(string,
     strlen(string));
    print_grammar(tree, bluebird_tree_root(tree), 0);
    bluebird_tree_destroy(tree);
    return 0;
}
