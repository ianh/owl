#include "1-parse.h"
#include "2-build.h"
#include "4-check-for-ambiguity.h"
#include "5-determinize.h"
#include "6a-generate.h"
#include "6b-interpret.h"
#include "alloc.h"
#include "terminal.h"
#include "test.h"
#include <stdio.h>
#include <string.h>

static FILE *output_file = 0;
static struct terminal_info get_terminal_info(int fileno);
static FILE *fopen_or_error(const char *filename, const char *mode);
static char *read_string(FILE *file);
static void write_to_output(const char *string, size_t len);

static const char *version_string = "owl.v4";
static const char *compatible_versions[] = {
    "owl.v1",
    "owl.v2",
    "owl.v3",
    "owl.v4",
};

int main(int argc, char *argv[])
{
    // This useless-looking call to memset is important for the Try Owl web
    // tool, which re-runs main repeatedly without clearing static variables.
    memset(&error, 0, sizeof(error));

    // Parse arguments.
    bool needs_help = false;
    char *input_filename = 0;
    char *output_filename = 0;
    char *grammar_string = 0;
    // May be different if the grammar is in "test format".
    char *grammar_string_to_free = 0;
    char *prefix_string = 0;
    char *input_string = 0;
    bool compile = false;
    bool test_format = false;
    enum {
        NO_PARAMETER,
        INPUT_FILE_PARAMETER,
        OUTPUT_FILE_PARAMETER,
        GRAMMAR_TEXT_PARAMETER,
        PREFIX_PARAMETER,
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
            else if (!strcmp(short_name, "V") || !strcmp(long_name, "version")) {
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
            } else if (!strcmp(short_name, "p") ||
             !strcmp(long_name, "prefix")) {
                if (prefix_string)
                    exit_with_errorf("multiple prefixes specified on the command line");
                parameter_state = PREFIX_PARAMETER;
            } else if (!strcmp(short_name, "T") ||
             !strcmp(long_name, "test-format"))
                test_format = true;
            else if (!strcmp(short_name, "c") ||
             !strcmp(long_name, "compile"))
                compile = true;
            else if (!strcmp(short_name, "C") || !strcmp(long_name, "color"))
                force_terminal_colors = true;
            else if (long_name[0] || short_name[0]) {
                errorf("unknown option: %s%s", long_name[0] ? "--" : "-",
                 long_name[0] ? long_name : short_name);
                print_error();
                needs_help = true;
            } else if (!grammar_string) {
                FILE *grammar_file = fopen_or_error(argv[i], "r");
                grammar_string = read_string(grammar_file);
                grammar_string_to_free = grammar_string;
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
        case PREFIX_PARAMETER:
            if (short_name[0] || long_name[0]) {
                errorf("missing prefix");
                print_error();
                needs_help = true;
                break;
            }
            prefix_string = argv[i];
            parameter_state = NO_PARAMETER;
            break;
        }
        }
        if (needs_help)
            break;
    }
    switch (parameter_state) {
    case INPUT_FILE_PARAMETER:
        errorf("missing input filename");
        print_error();
        needs_help = true;
        break;
    case OUTPUT_FILE_PARAMETER:
        errorf("missing output filename");
        print_error();
        needs_help = true;
        break;
    case GRAMMAR_TEXT_PARAMETER:
        errorf("missing grammar text");
        print_error();
        needs_help = true;
        break;
    case PREFIX_PARAMETER:
        errorf("missing prefix");
        print_error();
        needs_help = true;
        break;
    case NO_PARAMETER:
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
        fprintf(stderr, " -p prefix   --prefix prefix    output prefix_ instead of owl_ and parsed_\n");
        fprintf(stderr, " -T          --test-format      use test format with combined input and grammar\n");
        fprintf(stderr, " -C          --color            force 256-color parse tree output\n");
        fprintf(stderr, " -V          --version          print version info and exit\n");
        fprintf(stderr, " -h          --help             output this help text\n");
        return 1;
    }
    if (test_format) {
        size_t i = 0;
        int dash_count = 0;
        for (; grammar_string[i]; ++i) {
            if (grammar_string[i] == '-')
                dash_count++;
            else
                dash_count = 0;
            if (dash_count == 3)
                break;
        }
        if (grammar_string[i] == '\0') {
            exit_with_errorf("a file in test format requires the string '---' "
             "to separate input from grammars");
        }
        input_string = malloc(i - 1);
        strncpy(input_string, grammar_string, i - 2);
        input_string[i - 2] = '\0';
        grammar_string += i + 1;
    }

    error_in_string = grammar_string;

    // Check to see if the version is compatible.
    size_t using_length = strlen("#using ");
    bool compatible_version = false;
    struct grammar_version version = {
        .string = version_string,
    };
    if (strncmp(grammar_string, "#using ", using_length)) {
        compatible_version = true;
        if (compile && !test_format) {
            errorf("compiling a grammar without a version string");
            error.level = WARNING;
            print_error();
            error = (struct error){0};

            fprintf(stderr, "\n  Owl's grammar format may change between "
             "versions.  Add the string\n\n");
            fprintf(stderr, "  #using %s\n\n", version_string);
            fprintf(stderr, "  to the top of your grammar file to lock in "
             "this version.\n\n");
        }
    } else for (size_t i = 0; i < sizeof(compatible_versions) /
     sizeof(compatible_versions[0]); ++i) {
        size_t version_length = strlen(compatible_versions[i]);
        size_t j = using_length;
        size_t end = j + version_length;
        if (strncmp(grammar_string + j, compatible_versions[i], version_length)
         || !(grammar_string[end] == '\r' || grammar_string[end] == '\n'))
            continue;
        version.string = compatible_versions[i];
        version.range.start = j;
        version.range.end = end;
        compatible_version = true;
    }
    if (!compatible_version) {
        size_t i = using_length;
        error.ranges[0].start = i;
        while (grammar_string[i] != '\0' && grammar_string[i] != '\r' &&
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
    case ERROR_INVALID_TOKEN: {
        char *s = grammar_string + error.ranges[0].start;
        if (s[0] == '-' && s[1] == '-' && s[2] == '-') {
            error.ranges[0].end = error.ranges[0].start + 3;
            exit_with_errorf("to interpret a grammar in test format, use -T");
        }
        exit_with_errorf("'%.*s' isn't a valid token",
         (int)(error.ranges[0].end - error.ranges[0].start),
         grammar_string + error.ranges[0].start);
    }
    case ERROR_UNEXPECTED_TOKEN:
        exit_with_errorf("unexpected token '%.*s' while parsing grammar",
         (int)(error.ranges[0].end - error.ranges[0].start),
         grammar_string + error.ranges[0].start);
    case ERROR_MORE_INPUT_NEEDED:
        exit_with_errorf("expected more text at the end of the grammar");
    default:
        break;
    }

    struct grammar grammar = {0};
    build(&grammar, tree, version);

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
            .terminal_info = get_terminal_info(STDERR_FILENO),
            .version = version,
        };
        output_ambiguity(&interpreter, &ambiguity, stderr);
        return 3;
    }

    struct deterministic_grammar deterministic = {0};
    determinize(&combined, &deterministic);

    if (compile) {
#ifndef NOT_UNIX
        struct test_compilation test;
        // If -T is specified, test the code generator.
        if (test_format) {
            if (!output_filename) {
                exit_with_errorf("-T -c must include -o -- an executable will "
                 "be written to the output path");
            }
            test.executable_filename = output_filename;
            output_filename = 0;
            fclose(output_file);
            begin_test_compilation(&test);
            output_file = test.file;
        }
#endif
        struct generator generator = {
            .output = write_to_output,
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
            .version = version,
            .prefix = prefix_string,
        };
        generate(&generator);
#ifndef NOT_UNIX
        if (test_format) {
            finish_test_compilation(&test, input_string);
            output_file = 0;
        }
#endif
    } else {
        if (!input_string) {
            FILE *input_file = stdin;
            if (input_filename)
                input_file = fopen_or_error(input_filename, "r");
            input_string = read_string(input_file);
            if (input_filename)
                fclose(input_file);
        }
        struct interpreter interpreter = {
            .grammar = &grammar,
            .combined = &combined,
            .deterministic = &deterministic,
            .terminal_info = get_terminal_info(output_fileno),
            .version = version,
        };
        error_in_string = input_string;
        interpret(&interpreter, input_string, output_file);
    }

    if (output_filename)
        fclose(output_file);
    deterministic_grammar_destroy(&deterministic);
    combined_grammar_destroy(&combined);
    grammar_destroy(&grammar);
    owl_tree_destroy(tree);
    free(input_string);
    free(grammar_string_to_free);
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

static struct terminal_info get_terminal_info(int fileno)
{
    struct terminal_info info = {0};
    info.columns = terminal_columns(fileno);
    if (info.columns <= 0)
        info.columns = 80;
    int colors = terminal_colors(fileno);
    if (colors >= 256) {
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
        string = grow_array(string, &len, (size_t)len + 256);
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
    if (len > 0 && fwrite(string, len, 1, output_file) != 1) {
        fputs("critical error: write to output file failed\n", stderr);
        exit(-1);
    }
}
