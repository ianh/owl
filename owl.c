#include "1-parse.h"
#include "2-build.h"
#include "4-check-for-ambiguity.h"
#include "5-determinize.h"
#include "6a-generate.h"
#include "6b-interpret.h"
#include "alloc.h"
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#define DEBUG_OUTPUT 0

// TODO:
// - error messages that include line/column/visuals
// - clean up memory leaks
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
static struct terminal_info get_terminal_info(int fileno, bool force_color);
static long terminal_columns(int fileno);
static int terminal_colors(int fileno);
static FILE *fopen_or_error(const char *filename, const char *mode);
static char *read_string(FILE *file);
static void write_to_output(const char *string, size_t len);

static char *error_in_string;

static const char *version_string = "owl.v1";

int main(int argc, char *argv[])
{
    // Parse arguments.
    bool needs_help = false;
    const char *input_filename = 0;
    const char *output_filename = 0;
    char *grammar_string = 0;
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
            if (!strcmp(short_name, "V") || !strcmp(long_name, "version")) {
                fprintf(stderr, "%s\n", version_string);
                return 0;
            } else if (!strcmp(short_name, "i") || !strcmp(long_name, "input")) {
                if (input_filename)
                    exit_with_errorf("multiple input filenames");
                parameter_state = INPUT_FILE_PARAMETER;
            } else if (!strcmp(short_name, "o") ||
             !strcmp(long_name, "output")) {
                if (output_filename)
                    exit_with_errorf("multiple output filenames");
                parameter_state = OUTPUT_FILE_PARAMETER;
            } else if (!strcmp(short_name, "g") ||
             !strcmp(long_name, "grammar")) {
                if (grammar_string)
                    exit_with_errorf("owl only supports one grammar at a time");
                parameter_state = GRAMMAR_TEXT_PARAMETER;
            } else if (!strcmp(short_name, "c") ||
             !strcmp(long_name, "compile"))
                compile = true;
            else if (!strcmp(short_name, "C") || !strcmp(long_name, "color"))
                color_output = true;
            else if (long_name[0] || short_name[0]) {
                errorf("unknown option: %s%s", long_name[0] ? "--" : "-",
                 long_name[0] ? long_name : short_name);
                print_error();
                needs_help = true;
            } else if (!grammar_string) {
                FILE *grammar_file = fopen_or_error(argv[i], "r");
                grammar_string = read_string(grammar_file);
                fclose(grammar_file);
            } else
                exit_with_errorf("owl only supports one grammar at a time");
            break;
        case INPUT_FILE_PARAMETER:
            if (short_name[0] || long_name[0]) {
                errorf("missing input filename");
                print_error();
                needs_help = true;
                break;
            }
            input_filename = argv[i];
            parameter_state = NO_PARAMETER;
            break;
        case OUTPUT_FILE_PARAMETER:
            if (short_name[0] || long_name[0]) {
                errorf("missing output filename");
                print_error();
                needs_help = true;
                break;
            }
            output_filename = argv[i];
            parameter_state = NO_PARAMETER;
            break;
        case GRAMMAR_TEXT_PARAMETER: {
            if (short_name[0] || long_name[0]) {
                errorf("missing grammar text");
                print_error();
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
        errorf("missing grammar");
        print_error();
        needs_help = true;
    }
    if (needs_help) {
        fprintf(stderr, "usage: owl [options] grammar.owl\n");
        fprintf(stderr, " -i file     --input file       read from file instead of standard input\n");
        fprintf(stderr, " -o file     --output file      write to file instead of standard output\n");
        fprintf(stderr, " -c          --compile          output a C header file instead of parsing input\n");
        fprintf(stderr, " -g grammar  --grammar grammar  specify the grammar text on the command line\n");
        fprintf(stderr, " -C          --color            force 256-color parse tree output\n");
        fprintf(stderr, " -V          --version          print version info and exit\n");
        fprintf(stderr, " -h          --help             output this help text\n");
        return 1;
    }
    error_in_string = grammar_string;

    // Check to see if the version is compatible.
    size_t using_length = strlen("#using ");
    size_t version_length = strlen(version_string);
    if (strncmp(grammar_string, "#using ", using_length)) {
        if (compile) {
            errorf("compiling a grammar without a version string");
            error.level = WARNING;
            print_error();
            error = (struct error){0};

            fprintf(stderr, "\n  Owl's grammar format may change between "
             "versions.  Add the string\n\n");
            fprintf(stderr, "  #using owl.v1\n\n");
            fprintf(stderr, "  to the top of your grammar file to lock in "
             "this version.\n\n");
        }
    } else if (strncmp(grammar_string + using_length, version_string,
     version_length) || !(grammar_string[using_length + version_length] == '\r'
     || grammar_string[using_length + version_length] == '\n')) {
        size_t i = using_length;
        error.ranges[0].start = i;
        while (grammar_string[i] && grammar_string[i] != '\r' &&
         grammar_string[i] != '\n')
            i++;
        error.ranges[0].end = i;
        exit_with_errorf("incompatible version");
    }

    int output_fileno = -1;
    if (output_filename)
        output_file = fopen_or_error(output_filename, "w");
    else {
        output_file = stdout;
        output_fileno = STDOUT_FILENO;
    }

    // This is the part where things actually happen.
    struct owl_tree *tree;
    tree = owl_tree_create_from_string(grammar_string);
    switch (owl_tree_get_error(tree, &error.ranges[0])) {
    case ERROR_INVALID_TOKEN:
        exit_with_errorf("'%.*s' isn't a valid operator",
         (int)(error.ranges[0].end - error.ranges[0].start),
         grammar_string + error.ranges[0].start);
    case ERROR_UNEXPECTED_TOKEN:
        exit_with_errorf("unexpected token '%.*s' while parsing grammar",
         (int)(error.ranges[0].end - error.ranges[0].start),
         grammar_string + error.ranges[0].start);
    case ERROR_MORE_INPUT_NEEDED:
        exit_with_errorf("expected more text at the end of the grammar");
    default:
        break;
    }
#if DEBUG_OUTPUT
    owl_tree_print(tree);
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
            .terminal_info = get_terminal_info(STDERR_FILENO, color_output),
        };
        output_ambiguity(&interpreter, &ambiguity, stderr);
        return 3;
    }

    struct deterministic_grammar deterministic = {0};
    determinize(&combined, &deterministic);

    if (compile) {
        struct generator generator = {
            .output = write_to_output,
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
        };
        generate(&generator);
    } else {
        FILE *input_file = stdin;
        if (input_filename)
            input_file = fopen_or_error(input_filename, "r");
        char *input_string = read_string(input_file);
        fclose(input_file);
        struct interpreter interpreter = {
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
            .terminal_info = get_terminal_info(output_fileno,
             color_output),
        };
        error_in_string = input_string;
        interpret(&interpreter, input_string, output_file);
        free(input_string);
    }

    fclose(output_file);
    deterministic_grammar_destroy(&deterministic);
    combined_grammar_destroy(&combined);
    grammar_destroy(&grammar);
    owl_tree_destroy(tree);
    free(grammar_string);
    return 0;
}

static int compare_source_ranges(const void *aa, const void *bb);
struct error error = {0};

void print_error(void)
{
    // TODO: Line breaking, if there's time.
    int colors = terminal_colors(STDERR_FILENO);
    if (error.level == WARNING) {
        if (colors >= 256)
            fputs("\033[38;5;97m", stderr);
        else if (colors >= 8)
            fputs("\033[5;31m", stderr);
        fputs("warning:", stderr);
    } else {
        if (colors >= 256)
            fputs("\033[38;5;168m", stderr);
        else if (colors >= 8)
            fputs("\033[1;31m", stderr);
        fputs("error:", stderr);
    }
    if (colors >= 8)
        fputs("\033[0m", stderr);
    fprintf(stderr, " %s\n", error.text);
    qsort(error.ranges, MAX_ERROR_RANGES, sizeof(struct source_range),
     compare_source_ranges);
    if (error.ranges[0].end == 0)
        return;
    fputs("\n", stderr);
    size_t line_start = 0;
    bool line_marked = false;
    bool last_line_marked = false;
    int range = 0;
    for (size_t i = 0; range < MAX_ERROR_RANGES; ++i) {
        if (error.ranges[range].end == 0)
            break;
        if (error_in_string[i] == '\0' || error_in_string[i] == '\n') {
            if (line_marked) {
                fputs("  ", stderr);
                fwrite(error_in_string + line_start, 1, i - line_start, stderr);
                fputs("\n  ", stderr);
                int max_range = range;
                if (colors >= 256)
                    fputs("\033[38;5;113m", stderr);
                else if (colors >= 8)
                    fputs("\033[32m", stderr);
                for (size_t j = line_start; j < i; ++j) {
                    bool marked = false;
                    for (int k = range; k < MAX_ERROR_RANGES; ++k) {
                        if (error.ranges[k].end == 0)
                            break;
                        if (j >= error.ranges[k].end)
                            continue;
                        if (j < error.ranges[k].start)
                            continue;
                        marked = true;
                        if (k > max_range)
                            max_range = k;
                    }
                    if (marked)
                        fputs("~", stderr);
                    else
                        fputs(" ", stderr);
                }
                if (colors >= 8)
                    fputs("\033[0m", stderr);
                fputs("\n", stderr);
                range = max_range + 1;
                line_marked = false;
                last_line_marked = true;
            } else if (last_line_marked) {
                if (colors >= 8)
                    fputs("\033[90m  ...\033[0m\n", stderr);
                else
                    fputs("  ...\n", stderr);
                last_line_marked = false;
            }
            if (error_in_string[i] == '\0')
                break;
            line_start = i + 1;
        }
        if (i >= error.ranges[range].start)
            line_marked = true;
    }
    fputs("\n", stderr);
}

static int compare_source_ranges(const void *aa, const void *bb)
{
    const struct source_range *a = aa;
    const struct source_range *b = bb;
    if (a->end != 0 && b->end == 0)
        return -1;
    if (a->end == 0 && b->end != 0)
        return 1;
    if (a->start < b->start)
        return -1;
    if (a->start > b->start)
        return 1;
    return 0;
}

static const char *colors_8[] = {
    "\033[31m",
    "\033[32m",
    "\033[33m",
    "\033[34m",
    "\033[35m",
    "\033[36m",
};

static const char *colors_256[] = {
    "\033[38;5;168m",
    "\033[38;5;113m",
    "\033[38;5;68m",
    "\033[38;5;214m",
    "\033[38;5;97m",
};

static struct terminal_info get_terminal_info(int fileno, bool force_color)
{
    struct terminal_info info = {0};
    info.columns = terminal_columns(fileno);
    if (info.columns <= 0)
        info.columns = 80;
    int colors = terminal_colors(fileno);
    if (colors >= 256 || force_color) {
        info.reset = "\033[0m";
        info.line_indicator = "\033[90m";
        info.row_colors = colors_256;
        info.number_of_row_colors = sizeof(colors_256) / sizeof(colors_256[0]);
    } else if (colors >= 8) {
        info.reset = "\033[0m";
        info.line_indicator = "\033[90m";
        info.row_colors = colors_8;
        info.number_of_row_colors = sizeof(colors_8) / sizeof(colors_8[0]);
    }
    return info;
}

static long terminal_columns(int fileno)
{
    if (fileno < 0 || !isatty(fileno))
        return -1;
#ifdef TIOCGWINSZ
    struct winsize winsize = {0};
    if (!ioctl(fileno, TIOCGWINSZ, (char *)&winsize))
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

static int terminal_colors(int fileno)
{
    if (fileno < 0 || !isatty(fileno))
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
        int (*setupterm)(char *, int, int *) =
         (int (*)(char *, int, int *))dlsym(handle, "setupterm");
        int (*tigetnum)(char *) = (int (*)(char *))dlsym(handle, "tigetnum");
        int value = -1;
        int err;
        if (setupterm && tigetnum && !setupterm(0, fileno, &err))
            value = tigetnum("colors");
        dlclose(handle);
        if (value > 0)
            return value;
    }
    return 1;
}

static FILE *fopen_or_error(const char *filename, const char *mode)
{
    FILE *file = fopen(filename, mode);
    if (file)
        return file;
    errorf("couldn't open file:");
    print_error();
    fprintf(stderr, "\n  %s\n\n", filename);
    exit(-1);
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
