#include "1-parse.h"
#include "2-build.h"
#include "4-check-for-ambiguity.h"
#include "5-determinize.h"
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
// - error messages that include line/column/visuals
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

#if 0
    automaton_print(&combined.automaton);
    automaton_print(&combined.bracket_automaton);
    for (uint32_t i = 0; i < combined.number_of_tokens; ++i) {
        printf("token %x: %.*s\n", i, (int)combined.tokens[i].length,
               combined.tokens[i].string);
    }
#endif

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
