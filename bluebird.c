#include <stdio.h>
#include <string.h>

#include "1-parse.h"
#include "2-build.h"
#include "3-combine.h"
#include "4-determinize.h"
#include "6b-interpret.h"

// TODO:
// - tokenization
// - interpreter
// - operator precedence
// - code generation
// - self hosting
// - interoperability
// - ambiguity checking
// - json grammar output
// - fancy interpreter output
// - json interpreter output
// - clean up memory leaks
// - perf optimization

int main(int argc, char *argv[])
{
    const char *string =
    //"a = b@x b = 'foo'";
    //"a = [ '(' [ a ] ')' ] | 'x'";

    "expr = [ '(' [ expr ] ')' ] `parens`"
     "identifier `ident`"
     "infix flat $ '*' `times` '/' `divided-by`"
     "infix flat $ '+' `plus` '-' `minus`";

    //"grammar = rule*   rule = identifier '=' body   body = expr | (expr ':' identifier)+ operators*   operators = '.operators' fixity operator+   operator = expr ':' identifier   fixity =    'postfix' `postfix`    'prefix' `prefix`    'infix' assoc `infix`   assoc =    'flat' `flat`    'left' `left`    'right' `right`    'nonassoc' `nonassoc`    expr =     identifier ('@' identifier@rename)? `identifier`     single-quoted-string `literal`     [ '(' [ expr ] ')' ] `parens`     [ '[' [ identifier@left expr? identifier@right ] ']' ] `guarded`    postfix $     '*' `zero-or-more`     '+' `one-or-more`     '?' `optional`    infix flat $     '' `concatenation`    infix flat $     '|' `choice`";
    //"a = [s[a]e] | [s[b]e] b = [s[a]e] | [s[b]e]";
    //"a = b b b = c c c = d d d = e e e = f f f = g g g = h h h = i i i = j j j = k k k = l l l = m m m = n n n = o o o = p p p = q q q = r r r = s s s = t t t = u u u = v";// v v = w w w = x x x = y y y = z";
    struct bluebird_tree *tree = bluebird_tree_create_from_string(string,
     strlen(string));
    print_grammar(tree, bluebird_tree_root(tree), 0);

    struct grammar grammar = {0};
    build(&grammar, tree);

    for (uint32_t i = 0; i < grammar.number_of_rules; ++i) {
        printf("%x - %u / %u (%u):\n", grammar.rules[i].identifier, grammar.rules[i].name, grammar.rules[i].choice_name, grammar.rules[i].type);
        automaton_print(grammar.rules[i].automaton);
    }
    for (uint32_t i = 0; i < grammar.number_of_tokens; ++i) {
        printf("keyword: %x - '%.*s'\n", grammar.tokens[i].symbol, grammar.tokens[i].length, grammar.tokens[i].string);
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

    const char *text_to_parse = "a + b + c * (e + f + g) + h * i + a0 + a1 + a2";
    //const char *text_to_parse = "a + (b - c) * d / e + f + g * h";
    //const char *text_to_parse = "(((x)))";
    //const char *text_to_parse = "a = [ x (a@b | a1 a2 a3) y ] | (c | b)* : eee  .operators infix left  p : p  .operators prefix pre : pre";
    //const char *text_to_parse = "q + (x + y) + z + ((d + ((w))) + r) + k";
    //const char *text_to_parse = "a = (b)";
    interpret(&grammar, &combined, &bracket_transitions, &deterministic, tree, text_to_parse);

    /*
    const char *tok;
#define EVALUATE_MACROS_AND_STRINGIFY(...) #__VA_ARGS__
#define TOKENIZE_BODY(...) tok = EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__);
#define READ_KEYWORD_TOKEN(...) 0
#include "tokenize.h"
    printf("%s\n", tok);
*/

    bluebird_tree_destroy(tree);
    return 0;
}
