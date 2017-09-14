#include <stdio.h>
#include <string.h>

#include "1-parse.h"
#include "2-build.h"

int main()
{
    const char *string = "a = x `x` y `y` infix left $ '+' `plus`";
    struct bluebird_tree *tree = bluebird_tree_create_from_string(string,
     strlen(string));
    print_grammar(tree, bluebird_tree_root(tree), 0);

    struct grammar grammar = {0};
    build(&grammar, tree);

    for (uint32_t i = 0; i < grammar.number_of_rules; ++i) {
        printf("%u / %u (%u):\n", grammar.rules[i].name, grammar.rules[i].choice_name, grammar.rules[i].type);
        automaton_print(grammar.rules[i].automaton);
    }

    bluebird_tree_destroy(tree);
    return 0;
}
