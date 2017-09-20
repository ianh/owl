#include <stdio.h>
#include <string.h>

#include "1-parse.h"
#include "2-build.h"
#include "3-combine.h"
#include "4-determinize.h"

int main()
{
    const char *string =
    "a = ''";
    //"grammar = rule*   rule = identifier '=' body   body = expr | (expr ':' identifier)+ operators*   expr =     identifier ('@' identifier@rename)? `identifier`     single-quoted-string `literal`     [ '(' [ expr ] ')' ] `parens`     [ '[' [ identifier@left expr identifier@right ] ']' ] `guarded`    postfix $     '*' `zero-or-more`     '+' `one-or-more`     '?' `optional`    infix flat $     '' `concatenation`    infix flat $     '|' `choice`   operators = '.operators' fixity operator+   operator = expr ':' identifier   fixity =    'postfix' `postfix`    'prefix' `prefix`    'infix' assoc `infix`   assoc =    'flat' `flat`    'left' `left`    'right' `right`    'nonassoc' `nonassoc`";
    //"a = [s[a]e] | [s[b]e] b = [s[a]e] | [s[b]e]";
    //"a = b b b = c c c = d d d = e e e = f f f = g g g = h h h = i i i = j j j = k k k = l l l = m m m = n n n = o o o = p p p = q q q = r r r = s s s = t t t = u u u = v v v = w w w = x x x = y y y = z";
    struct bluebird_tree *tree = bluebird_tree_create_from_string(string,
     strlen(string));
    print_grammar(tree, bluebird_tree_root(tree), 0);

    struct grammar grammar = {0};
    build(&grammar, tree);

    for (uint32_t i = 0; i < grammar.number_of_rules; ++i) {
        printf("%x - %u / %u (%u):\n", grammar.rules[i].identifier, grammar.rules[i].name, grammar.rules[i].choice_name, grammar.rules[i].type);
        automaton_print(grammar.rules[i].automaton);
    }
    printf("---\n");

    struct combined_grammar combined = {0};
    combine(&combined, &grammar);
    automaton_print(&combined.automaton);
    automaton_print(&combined.bracket_automaton);

    struct bracket_transitions bracket_transitions = {0};
    determinize_bracket_transitions(&bracket_transitions, &combined);
    printf("---\n");

    struct deterministic_grammar deterministic = {0};
    determinize(&combined, &deterministic, &bracket_transitions);
    automaton_print(&deterministic.automaton);
    automaton_print(&deterministic.bracket_automaton);

    bluebird_tree_destroy(tree);
    return 0;
}
