#include "1-parse.h"
#include "2-build.h"
#include "4-determinize.h"
#include "5-check-for-ambiguity.h"
#include "6a-generate.h"
#include "6b-interpret.h"
#include "fancy-tree-output.h"
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#define DEBUG_OUTPUT 0

// TODO:
// - ambiguity checking
// - update generated code to include offset information
// - error messages that include line/column/visuals
// - don't output unreachable nfa states
//  - this probably means filtering out intermediate states from the epsilon
//    closure
// - string escape sequences
// - clean up memory leaks
// - get rid of RULE_LOOKUP?
// - check if all the input is properly validated
// - properly lay out / comment source files
// - perf optimization?
// - testing!
//  - add code to generate random words in the grammar together with the
//    expected parse tree and verify that they match when parsed
//  - test long tokens (over 256 bytes)
//  - test whitespace at beginning/end of string

// ASIDE:
// - write a blog post or something about how the old parser worked?

static FILE *output_file = 0;
static long terminal_columns();
static int terminal_colors();
static char *read_string(FILE *file);
static void write_to_output(const char *string, size_t len);

int main(int argc, char *argv[])
{
    // Parse arguments.
    bool needs_help = false;
    char *grammar_string = 0;
    const char *input_filename = 0;
    const char *output_filename = 0;
    bool compile = false;
    bool color_output = false;
    enum {
        NO_PARAMETER,
        INPUT_FILE_PARAMETER,
        OUTPUT_FILE_PARAMETER,
        GRAMMAR_TEXT_PARAMETER,
    } parameter_state = NO_PARAMETER;
    for (int i = 1; i < argc; ++i) {
        const char *short_name = "";
        const char *long_name = "";
        if (argv[i][0] == '-') {
            if (argv[i][1] == '-')
                long_name = argv[i] + 2;
            else
                short_name = argv[i] + 1;
        }
        switch (parameter_state) {
        case NO_PARAMETER:
            if (!strcmp(short_name, "h") || !strcmp(long_name, "help"))
                needs_help = true;
            else if (!strcmp(short_name, "i") || !strcmp(long_name, "input")) {
                if (input_filename) {
                    fprintf(stderr, "error: multiple input filenames.\n");
                    return 1;
                }
                parameter_state = INPUT_FILE_PARAMETER;
            } else if (!strcmp(short_name, "o") ||
             !strcmp(long_name, "output")) {
                if (output_filename) {
                    fprintf(stderr, "error: multiple output filenames.\n");
                    return 1;
                }
                parameter_state = OUTPUT_FILE_PARAMETER;
            } else if (!strcmp(short_name, "g") ||
             !strcmp(long_name, "grammar")) {
                if (grammar_string) {
                    fprintf(stderr, "error: bluebird only supports one grammar "
                     "at a time.\n");
                    return 1;
                }
                parameter_state = GRAMMAR_TEXT_PARAMETER;
            } else if (!strcmp(short_name, "c") ||
             !strcmp(long_name, "compile"))
                compile = true;
            else if (!strcmp(short_name, "C") || !strcmp(long_name, "color"))
                color_output = true;
            else if (long_name[0] || short_name[0]) {
                fprintf(stderr, "error: unknown option: %s%s\n",
                 long_name[0] ? "--" : "-",
                 long_name[0] ? long_name : short_name);
                needs_help = true;
            } else if (!grammar_string) {
                FILE *grammar_file = fopen(argv[i], "r");
                if (!grammar_file) {
                    fprintf(stderr, "error: couldn't open file:\n");
                    fprintf(stderr, " %s\n", argv[i]);
                    return 1;
                }
                grammar_string = read_string(grammar_file);
                fclose(grammar_file);
            } else {
                fprintf(stderr, "error: bluebird only supports one grammar at "
                 "a time.\n");
                return 1;
            }
            break;
        case INPUT_FILE_PARAMETER:
            if (short_name[0] || long_name[0]) {
                fprintf(stderr, "error: missing input filename.\n");
                needs_help = true;
                break;
            }
            input_filename = argv[i];
            parameter_state = NO_PARAMETER;
            break;
        case OUTPUT_FILE_PARAMETER:
            if (short_name[0] || long_name[0]) {
                fprintf(stderr, "error: missing output filename.\n");
                needs_help = true;
                break;
            }
            output_filename = argv[i];
            parameter_state = NO_PARAMETER;
            break;
        case GRAMMAR_TEXT_PARAMETER: {
            if (short_name[0] || long_name[0]) {
                fprintf(stderr, "error: missing grammar text.\n");
                needs_help = true;
                break;
            }
            size_t len = strlen(argv[i]);
            if (len + 1 < len)
                abort();
            grammar_string = malloc(len + 1);
            memcpy(grammar_string, argv[i], len + 1);
            parameter_state = NO_PARAMETER;
            break;
        }
        }
        if (needs_help)
            break;
    }
    if (!needs_help && !grammar_string) {
        fprintf(stderr, "error: missing grammar.\n");
        needs_help = true;
    }
    if (needs_help) {
        fprintf(stderr, "usage: bluebird [options] grammar.bb\n");
        fprintf(stderr, " -i file     --input file       read from file instead of standard input\n");
        fprintf(stderr, " -o file     --output file      write to file instead of standard output\n");
        fprintf(stderr, " -c          --compile          output a C header file instead of parsing input\n");
        fprintf(stderr, " -g grammar  --grammar grammar  specify the grammar text on the command line\n");
        fprintf(stderr, " -C          --color            force 256-color parse tree output\n");
        fprintf(stderr, " -h          --help             output this help text\n");
        return 1;
    }

    struct terminal_info terminal_info = { .columns = 80 };
    if (output_filename) {
        output_file = fopen(output_filename, "w");
        if (!output_file) {
            fprintf(stderr, "error: couldn't open file:\n");
            fprintf(stderr, " %s\n", output_filename);
            return 1;
        }
    } else {
        output_file = stdout;
        terminal_info.columns = terminal_columns();
        if (terminal_info.columns <= 0)
            terminal_info.columns = 80;
        if (!color_output) {
            int colors = terminal_colors();
            if (colors >= 256)
                color_output = true;
            else if (colors >= 8) {
                terminal_info.reset = "\033[0m";
                terminal_info.line_indicator = "\033[90m";
                terminal_info.row_colors = (const char *[]){
                    "\033[31m",
                    "\033[32m",
                    "\033[33m",
                    "\033[34m",
                    "\033[35m",
                    "\033[36m",
                };
                terminal_info.number_of_row_colors = 6;
            }
        }
    }
    if (color_output) {
        // Enable 256-color output if the terminal supports it, or if it's been
        // specified as a parameter.
        terminal_info.reset = "\033[0m";
        terminal_info.line_indicator = "\033[90m";
        terminal_info.row_colors = (const char *[]){
            "\033[38;5;168m",
            "\033[38;5;113m",
            "\033[38;5;68m",
            "\033[38;5;214m",
            "\033[38;5;97m",
        };
        terminal_info.number_of_row_colors = 5;
    }

    // This is the part where things actually happen.
    struct bluebird_tree *tree;
    tree = bluebird_tree_create_from_string(grammar_string);
#if DEBUG_OUTPUT
    bluebird_tree_print(tree);
#endif

    struct grammar grammar = {0};
    build(&grammar, tree);

    struct combined_grammar combined = {0};
    combine(&combined, &grammar);

    automaton_print(&combined.automaton);
    automaton_print(&combined.bracket_automaton);
    for (uint32_t i = 0; i < combined.number_of_tokens; ++i) {
        printf("token %x: %.*s\n", i, (int)combined.tokens[i].length,
               combined.tokens[i].string);
    }

    struct ambiguity ambiguity = {0};
    check_for_ambiguity(&combined, &ambiguity);
    if (ambiguity.has_ambiguity) {
        struct interpreter interpreter = {
            .grammar = &grammar,
            .combined = &combined,
            .terminal_info = terminal_info,
        };
        output_ambiguity(&interpreter, &ambiguity, stderr);
        return 2;
    }

    struct bracket_transitions bracket_transitions = {0};
    determinize_bracket_transitions(&bracket_transitions, &combined);

    struct deterministic_grammar deterministic = {0};
    determinize(&combined, &deterministic, &bracket_transitions);

    if (compile) {
        struct generator generator = {
            .output = write_to_output,
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
            .transitions = &bracket_transitions,
        };
        generate(&generator);
    } else {
        FILE *input_file = stdin;
        if (input_filename) {
            input_file = fopen(input_filename, "r");
            if (!input_file) {
                fprintf(stderr, "error: couldn't open file:\n");
                fprintf(stderr, " %s\n", input_filename);
                return 1;
            }
        }
        char *input_string = read_string(input_file);
        fclose(input_file);
        struct interpreter interpreter = {
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
            .transitions = &bracket_transitions,
            .terminal_info = terminal_info,
        };
        interpret(&interpreter, input_string, output_file);
        free(input_string);
    }

    fclose(output_file);
    bluebird_tree_destroy(tree);
    free(grammar_string);
    return 0;
}

static long terminal_columns()
{
#ifdef TIOCGWINSZ
    struct winsize winsize = {0};
    if (!ioctl(STDOUT_FILENO, TIOCGWINSZ, (char *)&winsize))
        return (long)winsize.ws_col;
#endif
    char *env = getenv("COLUMNS");
    if (!env || *env == '\0')
        return -1;
    char *endptr = env;
    long columns = strtol(env, &endptr, 10);
    if (*endptr != '\0')
        return -1;
    return columns;
}

static int terminal_colors()
{
    if (!isatty(STDOUT_FILENO))
        return 1;
    // Try some different names that "ncurses" goes by on various platforms.
    // Each name is annotated with the platform it was added to support.
    char *libs[] = {
        // Ubuntu 18.04 amd64
        "libncurses.so.5",

        // Arch Linux 2018.05.01 amd64
        "libncursesw.so",

        // macOS 10.12.6
        "libncurses.dylib",

        // These are here just in case...
        "libncursesw.so.6",
        "libncurses.so",
        "libncurses.so.6",
    };
    for (int i = 0; i < sizeof(libs) / sizeof(libs[0]); ++i) {
        void *handle = dlopen(libs[i], RTLD_LAZY | RTLD_LOCAL);
        if (!handle)
            continue;
        int (*setupterm)(char *, int, int *) = dlsym(handle, "setupterm");
        int (*tigetnum)(char *) = dlsym(handle, "tigetnum");
        int value = -1;
        int err;
        if (setupterm && tigetnum && !setupterm(0, STDOUT_FILENO, &err))
            value = tigetnum("colors");
        dlclose(handle);
        if (value > 0)
            return value;
    }
    return 1;
}

static char *read_string(FILE *file)
{
    char *string = 0;
    uint32_t len = 0;
    uint32_t offset = 0;
    while (true) {
        string = grow_array(string, &len, len + 256);
        uint32_t n = (uint32_t)fread(string + offset, 1, len - offset, file);
        if (n < len - offset) {
            string[offset + n] = '\0';
            return string;
        }
        offset += n;
    }
}

static void write_to_output(const char *string, size_t len)
{
    fwrite(string, len, 1, output_file);
}

#if 0
{
    const char *string =
    //"a = b* [ '(' b* ')' ] | b [ '(' b | c ')' ]  b = 'x'  c = 'x'";
    //"a = [ '(' b ')' ] | [ '(' c ')' ]  b = 'x'  c = 'x'*";
    //"a = b | c  b = [ '(' 'x' ')' ]  c = [ '(' 'x'* ')' ]";
    //"a = [ '(' b* ')' ] | [ '(' c? ')' ]  b = 'x'?  c = 'y'?";
    //"a = 'a' b b 'c'  b = 'x'+";
    //"a = b | 'x'  b = [ '(' b ')' ]";
    //"a = b? b?  b = [ '(' 'x'* ')' ]?";
    //"a = b? b?  b = 'x'";
    //"a = x-spot y-spot z-spot  x-spot = 'x'*  y-spot = 'y'*  z-spot = 'z'*";
    //"a = [ '(' b ')' ] | 'x' b = [ '(' a ')' ] | 'x'";
    //"a = [ '(' a ')' ]";
    //"a = ('x' 'x')* : even  'x' ('x' 'x')* : odd";
    //"a = 'x'*";
    //"a = (x y)*  x = 'x'+  y = 'y'+";
    //"a = [ 'x' a 'x' ] | 'x'";
    //"a = '' x*  x = 'x'";
    //"a = b*  b = 'x' : one  'xx' : two";
    //"a = 'a' 'b' 'c'";
    //"a = b  b = 'x'";
    //"a = b b  b = 'x'?";
    //"a = identifier | number";
    //"a = b@x [ '(' [ a@x ] ')' ]  b = a";
    //"a = identifier ('+' identifier)*";
    //"a = identifier `ident`  infix flat $ '+' `plus`";
    //"a = b*  b = identifier";
    //"a = b@x b = 'foo'";
    /*
    "expr = [ '(' expr ')' ] : parens "
     "identifier : ident "
     "number : literal "
     ".operators infix left "
     "'*' : times "
     "'/' : divided-by "
     ".operators infix left "
     "'+' : plus "
     "'-' : minus ";
    // */
    //"expr = identifier `ident`  postfix $ '*' `zero-or-more`  infix flat $ '|' `choice`  infix flat $ test `test`  test = [ '(' [ expr@nested ] ')' ]";
    //"expr = identifier `ident`  number `number`  infix flat $ '+' `plus`";
    //"grammar = rule*   rule = identifier '=' body   body = expr | (expr ':' identifier)+ operators*   operators = '.operators' fixity operator+   operator = expr ':' identifier   fixity =    'postfix' `postfix`    'prefix' `prefix`    'infix' assoc `infix`   assoc =    'flat' `flat`    'left' `left`    'right' `right`    'nonassoc' `nonassoc`    expr =     identifier ('@' identifier@rename)? `ident`     string `literal`     [ '(' [ expr ] ')' ] `parens`     [ '[' [ identifier@left expr? identifier@right ] ']' ] `bracketed`    postfix $     '*' `zero-or-more`     '+' `one-or-more`     '?' `optional`    infix flat $     '' `concatenation`    infix flat $     '|' `choice`";
    //"grammar = rule*   rule = identifier '=' body   body = expr | (expr ':' identifier)+ operators*   operators = '.operators' fixity operator+   operator = expr ':' identifier   fixity =    'postfix' : postfix    'prefix' : prefix    'infix' assoc : infix   assoc =    'flat' : flat    'left' : left    'right' : right    'nonassoc' : nonassoc    expr =     identifier ('@' identifier@rename)? : ident     string : literal     [ '(' expr ')' ] : parens     [ '[' string@left expr? string@right ']' ] : bracketed   .operators postfix     '*' : zero-or-more     '+' : one-or-more     '?' : optional    .operators infix flat     '' : concatenation    .operators infix flat     '|' : choice";
//*
    "grammar = rule* "
    "rule = identifier '=' body "
    "body = expr | (expr ':' identifier)+ operators* "
    "operators = '.operators' fixity operator+ "
    "operator = expr ':' identifier "
    "fixity =    'postfix' : postfix-op    'prefix' : prefix-op    'infix' assoc : infix-op "
    "assoc =    'flat' : flat-op    'left' : left-op    'right' : right-op    'nonassoc' : nonassoc-op "
    "expr = "
    "identifier ('@' identifier@rename)? : ident "
    "string : literal "
    "[ '(' expr ')' ] : parens "
    "[ '[' string@begin-token expr? string@end-token ']' ] : bracketed "
    ".operators postfix "
    "'*' : zero-or-more "
    "'+' : one-or-more "
    "'?' : optional "
    ".operators infix flat "
    "'' : concatenation "
    ".operators infix flat "
    "'|' : choice ";
 //*/
    //"a = [ '(' a ')' ] | [ '(' b ')' ] b = [ '(' a ')' ] | [ '(' b ')' ]";
    //"a = [s[a]e] | [s[b]e] b = [s[a]e] | [s[b]e]";
    //"a = b b b = c c c = d d d = e e e = f f f = g g g = h h h = i i i = j j j = k k k = l l l = m m m = n n n = o o o = p p p = q q q = r r r = s s s = t t t = u u u = 'v'";// v v = w w w = x x x = y y y = z";
    struct bluebird_tree *tree = bluebird_tree_create_from_string(string);
#if DEBUG_OUTPUT
    bluebird_tree_print(tree);
#endif

    struct grammar grammar = {0};
    build(&grammar, tree);

#if DEBUG_OUTPUT
    for (uint32_t i = 0; i < grammar.number_of_rules; ++i) {
        if (grammar.rules[i].is_token) {
            printf("%u: token '%.*s'\n", i, (int)grammar.rules[i].name_length,
             grammar.rules[i].name);
            continue;
        }
        printf("%u: rule '%.*s'\n", i, (int)grammar.rules[i].name_length,
         grammar.rules[i].name);
        if (grammar.rules[i].number_of_choices == 0)
            automaton_print(&grammar.rules[i].automaton);
        else {
            for (uint32_t j = 0; j < grammar.rules[i].number_of_choices; ++j) {
                printf("choice %u: '%.*s'\n", j,
                 (int)grammar.rules[i].choices[j].name_length,
                 grammar.rules[i].choices[j].name);
                automaton_print(&grammar.rules[i].choices[j].automaton);
            }
            for (uint32_t j = 0; j < grammar.rules[i].number_of_operators; ++j){
                printf("operator %u (prec %d): '%.*s'\n", j,
                 grammar.rules[i].operators[j].precedence,
                 (int)grammar.rules[i].operators[j].name_length,
                 grammar.rules[i].operators[j].name);
                automaton_print(&grammar.rules[i].operators[j].automaton);
            }
        }
        if (grammar.rules[i].number_of_keyword_tokens) {
            printf("keywords:\n");
            for (uint32_t j = 0; j < grammar.rules[i].number_of_keyword_tokens; ++j) {
                printf("\t%.*s (type %d): %x\n",
                 (int)grammar.rules[i].keyword_tokens[j].length,
                 grammar.rules[i].keyword_tokens[j].string,
                 grammar.rules[i].keyword_tokens[j].type,
                 grammar.rules[i].keyword_tokens[j].symbol);
            }
        }
        if (grammar.rules[i].number_of_slots) {
            printf("slots:\n");
            for (uint32_t j = 0; j < grammar.rules[i].number_of_slots; ++j) {
                printf("\t%.*s: %x -> %u\n",
                 (int)grammar.rules[i].slots[j].name_length,
                 grammar.rules[i].slots[j].name,
                 grammar.rules[i].slots[j].symbol,
                 grammar.rules[i].slots[j].rule_index);
            }
        }
        if (grammar.rules[i].number_of_brackets) {
            printf("brackets:\n");
            for (uint32_t j = 0; j < grammar.rules[i].number_of_brackets; ++j) {
                printf("%x -> [ %x %x ]\n", grammar.rules[i].brackets[j].symbol,
                 grammar.rules[i].brackets[j].start_symbol,
                 grammar.rules[i].brackets[j].end_symbol);
                automaton_print(&grammar.rules[i].brackets[j].automaton);
            }
        }
    }
    printf("---\n");
#endif
    struct combined_grammar combined = {0};
    combine(&combined, &grammar);
#if DEBUG_OUTPUT
    automaton_print(&combined.automaton);
    automaton_print(&combined.bracket_automaton);
    for (uint32_t i = 0; i < combined.number_of_tokens; ++i) {
        printf("token %x: %.*s\n", i, (int)combined.tokens[i].length,
         combined.tokens[i].string);
    }
#endif

#if DEBUG_OUTPUT
    printf("------\n");
#endif
    struct bracket_transitions bracket_transitions = {0};
    determinize_bracket_transitions(&bracket_transitions, &combined);
#if DEBUG_OUTPUT
    printf("------\n");
#endif

    struct deterministic_grammar deterministic = {0};
    determinize(&combined, &deterministic, &bracket_transitions);
#if DEBUG_OUTPUT
    automaton_print(&deterministic.automaton);
    automaton_print(&deterministic.bracket_automaton);
#endif

    printf("- ambiguity start -\n");
//    check_for_ambiguity(&combined, &bracket_transitions);
    printf("- ambiguity end -\n");

    //const char *text_to_parse = "xx x x x x x";
    //const char *text_to_parse = "x y x y y y x x x y x y";
    //const char *text_to_parse = "a x x x c";
    //const char *text_to_parse = "a * 2 + (b / c) + c * 3";
    //const char *text_to_parse = "a | b | c | d | e";
    //const char *text_to_parse = "a + b + c + d + 3";
    //const char *text_to_parse = "a + b + c * (e + f + g) + h * 7 + a0 + a1 + a2";
    //const char *text_to_parse = "a + (b - c) * d / e + 3 * f + g * h";
    //const char *text_to_parse = "(((x)))";
    //const char *text_to_parse = "x x x x x x x";
    //const char *text_to_parse = "test = a | b*  a = identifier  b = number";
    const char *text_to_parse = "test = [ 'x' (a@b | a1 'a2' a3) 'y' ] | (c | b)* : eee  .operators infix left  p : p  .operators prefix pre : pre";
    //const char *text_to_parse = "q + (x + y) + z + ((d + ((w))) + r) + k";
    //const char *text_to_parse = "a = (b)";
    /*
    const char *text_to_parse =
     "expr = [ '(' expr ')' ] : parens "
     "identifier : ident "
     "number : literal "
     ".operators infix left "
     "'*' : times "
     "'/' : divided-by "
     ".operators infix left "
     "'+' : plus "
     "'-' : minus ";
    //*/

#if DEBUG_OUTPUT
    interpret(&grammar, &combined, &bracket_transitions, &deterministic, text_to_parse);
#endif

#if !DEBUG_OUTPUT
    struct generator generator = {
        .output = output_stdout,
        .grammar = &grammar,
        .combined = &combined,
        .deterministic = &deterministic,
        .transitions = &bracket_transitions,
    };
    generate(&generator);
#endif

#if 0
#define TOK(st, txt, clr) {.start = st, .end = st + 1, .text = txt, .length = strlen(txt), .color = clr}
#define TOK_NEWLINE(st, txt, clr) {.start = st, .end = st + 1, .text = txt, .length = strlen(txt), .color = clr, .starts_with_newline = true}
    struct document document = {
        .number_of_rows = 5,
        .rows = (struct row []) {
            {
                .number_of_labels = 26,
                .labels = (struct label[]) {
                    TOK(1, "for", 4),
                    TOK(3, " ", 4),
                    TOK(5, "i", 4),
                    TOK(7, " ", 4),
                    TOK(9, "in", 4),
                    TOK(11, " ", 4),
                    TOK(15, "range", 2),
                    TOK(18, "(", 3),
                    TOK(21, "0", 2),
                    TOK(24, ",", 3),
                    TOK(26, " ", 3),
                    TOK(29, "10", 2),
                    TOK(32, ")", 3),
                    TOK(35, " ", 4),
                    TOK(37, "do", 4),
                    TOK_NEWLINE(39, "    ", 4),
                    TOK(42, "x", 3),
                    TOK(44, " ", 3),
                    TOK(46, "=", 3),
                    TOK(48, " ", 3),
                    TOK(52, "x", 1),
                    TOK(55, " ", 2),
                    TOK(57, "+", 2),
                    TOK(59, " ", 2),
                    TOK(62, "1", 1),
                    TOK_NEWLINE(67, "end", 4),
                }
            },
            {
                .number_of_labels = 2,
                .labels = (struct label[]) {
                    {.start = 51, .end = 54, .text = "expr:ident", .length = 10},
                    {.start = 61, .end = 64, .text = "expr:literal", .length = 12},
                }
            },
            {
                .number_of_labels = 4,
                .labels = (struct label[]) {
                    {.start = 14, .end = 17, .text = "expr:ident", .length = 10},
                    {.start = 20, .end = 23, .text = "expr:literal", .length = 12},
                    {.start = 28, .end = 31, .text = "expr:literal", .length = 12},
                    {.start = 50, .end = 65, .text = "expr:plus", .length = 9},
                }
            },
            {
                .number_of_labels = 2,
                .labels = (struct label[]) {
                    {.start = 13, .end = 33, .text = "expr:function-call", .length = 18},
                    {.start = 41, .end = 66, .text = "stmt:assignment", .length = 15},
                }
            },
            {
                .number_of_labels = 1,
                .labels = (struct label[]) {
                    {.start = 0, .end = 69, .text = "stmt:for-loop", .length = 13},
                }
            },
        },
        .number_of_columns = 80,
    };
    if (getenv("TERM")) {
        document.reset_color_code = "\033[0m";
        document.line_indicator_color_code = "\033[90m";
        document.color_codes = (const char *[]){
            "\033[38;5;168m",
            "\033[38;5;113m",
            "\033[38;5;68m",
            "\033[38;5;214m",
            "\033[38;5;97m",
        };
        document.number_of_color_codes = 5;
    }
    output_document(&document);
#endif

    /*
    const char *tok;
#define EVALUATE_MACROS_AND_STRINGIFY(...) #__VA_ARGS__
#define TOKENIZE_BODY(...) tok = EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__);
#define READ_KEYWORD_TOKEN(...) 0
#include "x-tokenize.h"
    printf("%s\n", tok);
*/

    bluebird_tree_destroy(tree);
    return 0;
}
#endif
