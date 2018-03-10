#include "6a-generate.h"

#include "grow-array.h"
#include <stdio.h>
#include <string.h>

enum substitution_transform {
    NO_TRANSFORM,
    LOWERCASE_WITH_UNDERSCORES,
    UPPERCASE_WITH_UNDERSCORES,
};

struct substitution {
    // Zero-terminated.
    const char *variable;

    bool is_number;
    uint32_t number;

    const char *value;
    size_t value_length;

    enum substitution_transform transform;
};

struct generator_internal {
    struct substitution *substitutions;
    uint32_t substitutions_allocated_bytes;
    uint32_t number_of_substitutions;

    char *formatted_string;
    uint32_t formatted_string_allocated_bytes;
};

static struct substitution *find_substitution(struct generator *gen,
 const char *variable)
{
    uint32_t n = gen->internal->number_of_substitutions;
    for (uint32_t i = 0; i < n; ++i) {
        struct substitution *s = &gen->internal->substitutions[i];
        for (size_t j = 0; ; ++j) {
            if (s->variable[j] == 0)
                return s;
            if (s->variable[j] != variable[j])
                break;
        }
    }
    return 0;
}

static void apply_transform(char *string, size_t length,
 enum substitution_transform transform)
{
    if (transform == NO_TRANSFORM)
        return;
    for (size_t i = 0; i < length; ++i) {
        if (transform == UPPERCASE_WITH_UNDERSCORES && string[i] >= 'a'
         && string[i] <= 'z')
            string[i] = string[i] - 'a' + 'A';
        if (string[i] == '-')
            string[i] = '_';
    }
}

static void output_string_length(struct generator *gen, const char *string,
 size_t len)
{
    if (len == 0)
        return;
    size_t next_output_index = 0;
    char *format = gen->internal->formatted_string;
    uint32_t format_bytes = gen->internal->formatted_string_allocated_bytes;
    for (size_t i = 0; i < len - 1; ++i) {
        if (string[i] == '%' && string[i + 1] == '%') {
            struct substitution *sub = find_substitution(gen, string + i + 2);
            if (!sub)
                abort();
            gen->output(gen, string + next_output_index, i - next_output_index);
            if (sub->is_number) {
                int len = snprintf(0, 0, "%uU", sub->number);
                format = grow_array(format, &format_bytes, (uint32_t)len + 1);
                snprintf(format, len + 1, "%uU", sub->number);
                gen->output(gen, format, len);
            } else {
                if (sub->value_length > UINT32_MAX)
                    abort();
                format = grow_array(format, &format_bytes,
                 (uint32_t)sub->value_length);
                memcpy(format, sub->value, sub->value_length);
                apply_transform(format, sub->value_length, sub->transform);
                gen->output(gen, format, sub->value_length);
            }
            next_output_index = i + 2 + strlen(sub->variable);
        }
    }
    gen->internal->formatted_string = format;
    gen->internal->formatted_string_allocated_bytes = format_bytes;
    if (next_output_index < len)
        gen->output(gen, string + next_output_index, len - next_output_index);
}

static void output_string(struct generator *gen, const char *string)
{
    output_string_length(gen, string, strlen(string));
}

static void output_line(struct generator *gen, const char *string)
{
    output_string(gen, string);
    output_string(gen, "\n");
}

static void output_formatted_source(struct generator *gen, const char *string);

static uint32_t create_substitution(struct generator *gen, const char *variable)
{
    uint32_t index = 0;
    for (; index < gen->internal->number_of_substitutions; ++index) {
        if (strcmp(variable, gen->internal->substitutions[index].variable) == 0)
            break;
    }
    if (index >= gen->internal->number_of_substitutions) {
        gen->internal->number_of_substitutions = index + 1;
        gen->internal->substitutions = grow_array(gen->internal->substitutions,
         &gen->internal->substitutions_allocated_bytes,
         gen->internal->number_of_substitutions * sizeof(struct substitution));
        gen->internal->substitutions[index].variable = variable;
    }
    return index;
}

static void set_substitution(struct generator *gen, const char *variable,
 const char *value, size_t value_length, enum substitution_transform transform)
{
    uint32_t index = create_substitution(gen, variable);
    gen->internal->substitutions[index].is_number = false;
    gen->internal->substitutions[index].value = value;
    gen->internal->substitutions[index].value_length = value_length;
    gen->internal->substitutions[index].transform = transform;
}

static void set_number_substitution(struct generator *gen,
 const char *variable, uint32_t value)
{
    uint32_t index = create_substitution(gen, variable);
    gen->internal->substitutions[index].is_number = true;
    gen->internal->substitutions[index].number = value;
}

static void set_literal_substitution(struct generator *gen,
 const char *variable, const char *value)
{
    set_substitution(gen, variable, value, strlen(value), NO_TRANSFORM);
}

static bool rule_is_named(struct rule *rule, const char *name)
{
    return rule->name_length == strlen(name) &&
     !memcmp(name, rule->name, rule->name_length);
}

static bool token_is(struct token *token, const char *name)
{
    return token->length == strlen(name) &&
     !memcmp(name, token->string, token->length);
}

enum automaton_type { NORMAL_AUTOMATON, BRACKET_AUTOMATON };
static void generate_automaton(struct generator *gen, struct automaton *a,
 uint32_t offset, enum automaton_type type);

static bool should_escape(char c);

void generate(struct generator *gen)
{
    struct generator_internal internal = {0};
    gen->internal = &internal;

    set_substitution(gen, "root-rule",
     gen->grammar->rules[gen->grammar->root_rule].name,
     gen->grammar->rules[gen->grammar->root_rule].name_length,
     LOWERCASE_WITH_UNDERSCORES);

    output_line(gen, "// -----------------------------------------------------------------------------");
    output_line(gen, "// This file was generated by the bluebird parsing tool.");
    output_line(gen, "// Make sure to #define BLUEBIRD_PARSER_IMPLEMENTATION somewhere so the parser");
    output_line(gen, "// is compiled properly.  Just two lines are enough -- a typical parser.c might");
    output_line(gen, "// look like:");
    output_line(gen, "//");
    output_line(gen, "//   #define BLUEBIRD_PARSER_IMPLEMENTATION");
    output_line(gen, "//   #include \"bluebird-parser.h\"");
    output_line(gen, "");
    output_line(gen, "#ifndef _BLUEBIRD_PARSER_H_");
    output_line(gen, "#define _BLUEBIRD_PARSER_H_");
    output_line(gen, "");
    output_line(gen, "#include \"stdbool.h\"");
    output_line(gen, "#include \"stddef.h\"");
    output_line(gen, "#include \"stdint.h\"");
    output_line(gen, "");
    output_line(gen, "// A parsed_id represents an element in the parse tree.  Use the");
    output_line(gen, "// parsed_..._get() function corresponding to the element type to unpack the");
    output_line(gen, "// element into its appropriate type of parsed_... element struct.");
    output_line(gen, "typedef size_t parsed_id;");
    output_line(gen, "");
    output_line(gen, "// The bluebird_tree struct represents an entire parse tree.  Use the");
    output_line(gen, "// bluebird_tree_create_...() functions to create a tree, then call");
    output_line(gen, "// bluebird_tree_root() to get the root bluebird_id.");
    output_line(gen, "struct bluebird_tree;");
    output_line(gen, "");
    output_line(gen, "// Creates a bluebird_tree from a string.  Remember to call");
    output_line(gen, "// bluebird_tree_destroy() when you're done with it.");
    output_line(gen, "struct bluebird_tree *bluebird_tree_create_from_string(const char *string);");
    output_line(gen, "");
    output_line(gen, "// Destroys a bluebird_tree, freeing its resources back to the system.");
    output_line(gen, "void bluebird_tree_destroy(struct bluebird_tree *);");
    output_line(gen, "");
    output_line(gen, "// Returns the root parsed_id.");
    output_line(gen, "parsed_id bluebird_tree_root_id(struct bluebird_tree *tree);");
    output_line(gen, "");
    output_line(gen, "// As a shortcut, returns the parsed_%%root-rule struct corresponding to the root parsed_id.");
    output_line(gen, "struct parsed_%%root-rule bluebird_tree_get_parsed_%%root-rule(struct bluebird_tree *tree);");

    uint32_t n = gen->grammar->number_of_rules;
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = &gen->grammar->rules[i];
        set_substitution(gen, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(gen, "");
        if (rule->number_of_choices > 0) {
            output_line(gen, "enum parsed_%%rule_type {");
            for (uint32_t j = 0; j < rule->number_of_choices; ++j) {
                set_substitution(gen, "choice-name", rule->choices[j].name,
                 rule->choices[j].name_length, UPPERCASE_WITH_UNDERSCORES);
                output_line(gen, "    PARSED_%%choice-name,");
            }
            output_line(gen, "};");
        }
        output_line(gen, "struct parsed_%%rule {");
        output_line(gen, "    struct bluebird_tree *_tree;");
        output_line(gen, "    parsed_id _next;");
        output_line(gen, "    bool empty;");
        if (rule->number_of_choices > 0)
            output_line(gen, "    enum parsed_%%rule_type type;");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            struct slot slot = rule->slots[j];
            set_substitution(gen, "referenced-slot", slot.name,
             slot.name_length, LOWERCASE_WITH_UNDERSCORES);
            output_line(gen, "    parsed_id %%referenced-slot;");
        }
        if (rule->is_token) {
            if (rule_is_named(rule, "identifier")) {
                output_line(gen, "    const char *identifier;");
                output_line(gen, "    size_t length;");
            } else if (rule_is_named(rule, "number"))
                output_line(gen, "    double number;");
            else if (rule_is_named(rule, "string")) {
                output_line(gen, "    const char *string;");
                output_line(gen, "    size_t length;");
            }
        }
        output_line(gen, "};");
    }
    output_line(gen, "");
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = &gen->grammar->rules[i];
        set_substitution(gen, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(gen, "struct parsed_%%rule parsed_%%rule_get(struct bluebird_tree *, parsed_id);");
    }
    output_line(gen, "");
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = &gen->grammar->rules[i];
        set_substitution(gen, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(gen, "static inline struct parsed_%%rule parsed_%%rule_next(struct parsed_%%rule parsed)");
        output_line(gen, "{");
        output_line(gen, "    return parsed_%%rule_get(parsed._tree, parsed._next);");
        output_line(gen, "}");
    }
    output_line(gen, "");
    output_line(gen, "#endif");

    output_line(gen, "");
    output_line(gen, "#ifdef BLUEBIRD_PARSER_IMPLEMENTATION");
    output_line(gen, "// Code implementing the parser.  This might get a bit messy!");
    output_line(gen, "#include <stdio.h>");
    output_line(gen, "#include <stdlib.h>");
    output_line(gen, "#include <string.h>");
    output_line(gen, "");
    output_line(gen, "struct bluebird_tree {");
    output_line(gen, "    const char *string;");
    output_line(gen, "};");

    set_literal_substitution(gen, "token-type", "uint32_t");
    set_literal_substitution(gen, "state-type", "uint32_t");

    set_number_substitution(gen, "identifier-token", 0xffffffff);
    set_number_substitution(gen, "number-token", 0xffffffff);
    set_number_substitution(gen, "string-token", 0xffffffff);
    set_number_substitution(gen, "bracket-transition-token", 0xffffffff);
    for (uint32_t i = gen->combined->number_of_keyword_tokens;
     i < gen->combined->number_of_tokens; ++i) {
        if (token_is(&gen->combined->tokens[i], "identifier"))
            set_number_substitution(gen, "identifier-token", i);
        else if (token_is(&gen->combined->tokens[i], "number"))
            set_number_substitution(gen, "number-token", i);
        else if (token_is(&gen->combined->tokens[i], "string"))
            set_number_substitution(gen, "string-token", i);
    }
    const char *tokenizer_source;
#define TOKEN_T %%token-type
#define STATE_T %%state-type
#define READ_KEYWORD_TOKEN read_keyword_token
#define WRITE_NUMBER_TOKEN(...)
#define WRITE_IDENTIFIER_TOKEN(...)
#define WRITE_STRING_TOKEN(...)
#define IDENTIFIER_TOKEN %%identifier-token
#define NUMBER_TOKEN %%number-token
#define STRING_TOKEN %%string-token
#define BRACKET_TRANSITION_TOKEN %%bracket-transition-token

#define EVALUATE_MACROS_AND_STRINGIFY(...) #__VA_ARGS__
#define TOKENIZE_BODY(...) tokenizer_source = EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__);
#include "x-tokenize.h"
    output_line(gen, "static size_t read_keyword_token(%%token-type *token, bool *end_token, const char *text, void *info);");
    output_formatted_source(gen, tokenizer_source);
    set_number_substitution(gen, "start-state",
     gen->deterministic->automaton.start_state);
    output_line(gen, "");
    output_line(gen, "struct fill_run_continuation {");
    output_line(gen, "    %%state-type state;");
    output_line(gen, "    %%state-type *state_stack;");
    output_line(gen, "    size_t state_stack_capacity;");
    output_line(gen, "    size_t state_stack_depth;");
    output_line(gen, "};");
    output_line(gen, "static void fill_run_states(struct bluebird_token_run *, struct fill_run_continuation *);");
    output_line(gen, "");
    output_line(gen, "struct bluebird_tree *bluebird_tree_create_from_string(const char *string) {");
    output_line(gen, "    struct bluebird_tree *tree = calloc(1, sizeof(struct bluebird_tree));");
    output_line(gen, "    tree->string = string;");
    output_line(gen, "    struct bluebird_default_tokenizer tokenizer = {");
    output_line(gen, "        .text = string,");
    output_line(gen, "        .info = tree,");
    output_line(gen, "    };");
    output_line(gen, "    struct bluebird_token_run *token_run = 0;");
    output_line(gen, "    struct fill_run_continuation c = {");
    output_line(gen, "        .state = %%start-state,");
    output_line(gen, "    };");
    output_line(gen, "    while (bluebird_default_tokenizer_advance(&tokenizer, &token_run))");
    output_line(gen, "        fill_run_states(token_run, &c);");
    output_line(gen, "    if (string[tokenizer.offset] != '\\0') {");
    output_line(gen, "        // TODO: Return error instead of printing it");
    output_line(gen, "        fprintf(stderr, \"error: tokenizing failed. next char was %u\\n\", string[tokenizer.offset]);");
    output_line(gen, "        abort();");
    output_line(gen, "    }");
    output_line(gen, "    if (c.state_stack_depth > 0) {");
    output_line(gen, "        // TODO: Return error instead of printing it");
    output_line(gen, "        fprintf(stderr, \"error: parsing failed because the stack was still full\\n\");");
    output_line(gen, "    }");
    output_line(gen, "    free(c.state_stack);");
    output_line(gen, "    c.state_stack = 0;");
    output_line(gen, "    c.state_stack_capacity = 0;");
    output_line(gen, "    while (token_run) {");
    output_line(gen, "        for (uint32_t i = 0; i < token_run->number_of_tokens; ++i) {");
    output_line(gen, "            printf(\"%u -> %u\\n\", token_run->tokens[i], token_run->states[i]);");
    output_line(gen, "        }");
    output_line(gen, "        printf(\"--\\n\");");
    output_line(gen, "        token_run = token_run->prev;");
    output_line(gen, "    }");
    output_line(gen, "    return tree;");
    output_line(gen, "}");
    output_line(gen, "void bluebird_tree_destroy(struct bluebird_tree *tree) {");
    output_line(gen, "    free(tree);");
    output_line(gen, "}");
    output_line(gen, "static bool grow_state_stack(struct fill_run_continuation *cont) {");
    output_line(gen, "    size_t new_capacity = (cont->state_stack_capacity + 2) * 3 / 2;");
    output_line(gen, "    if (new_capacity <= cont->state_stack_capacity)");
    output_line(gen, "        return false;");
    output_line(gen, "    %%state-type *new_stack = realloc(cont->state_stack, new_capacity * sizeof(%%state-type));");
    output_line(gen, "    if (!new_stack)");
    output_line(gen, "        return false;");
    output_line(gen, "    cont->state_stack = new_stack;");
    output_line(gen, "    cont->state_stack_capacity = new_capacity;");
    output_line(gen, "    return true;");
    output_line(gen, "}");
    output_line(gen, "static void fill_run_states(struct bluebird_token_run *run, struct fill_run_continuation *cont) {");
    output_line(gen, "    uint16_t token_index = 0;");
    output_line(gen, "    uint16_t number_of_tokens = run->number_of_tokens;");
    output_line(gen, "    uint16_t start_state = cont->state;");
    output_line(gen, "start:");
    output_line(gen, "    switch (start_state) {");
    struct automaton *a = &gen->deterministic->automaton;
    struct automaton *b = &gen->deterministic->bracket_automaton;
    set_number_substitution(gen, "first-bracket-state-id", a->number_of_states);
    generate_automaton(gen, a, 0, NORMAL_AUTOMATON);
    generate_automaton(gen, b, a->number_of_states, BRACKET_AUTOMATON);
    output_line(gen, "    }");
    output_line(gen, "}");
    output_line(gen, "static size_t read_keyword_token(%%token-type *token, bool *end_token, const char *text, void *info) {");
    for (uint32_t i = 0; i < gen->combined->number_of_keyword_tokens; ++i) {
        struct token keyword = gen->combined->tokens[i];
        set_number_substitution(gen, "token-index", keyword.symbol);
        if (keyword.length > UINT32_MAX) {
            // Why even store the length as a size_t if we're just gonna do
            // this?
            abort();
        }
        set_number_substitution(gen, "token-length", (uint32_t)keyword.length);
        if (keyword.type == TOKEN_END)
            set_literal_substitution(gen, "is-end-token", "true");
        else
            set_literal_substitution(gen, "is-end-token", "false");
        // TODO: Use a trie or something slightly less N^2.
        // TODO: This is incorrect; it doesn't handle tokens that are prefixes
        // of each other if they happen to appear in the wrong order.
        output_string(gen, "    if (strncmp(text, \"");
        for (size_t j = 0; j < keyword.length; ++j) {
            if (!should_escape(keyword.string[j])) {
                gen->output(gen, &keyword.string[j], 1);
                continue;
            }
            char string[8];
            snprintf(string, sizeof(string), "\\x%02x", keyword.string[j]);
            gen->output(gen, string, strlen(string));
        }
        output_line(gen, "\", %%token-length) == 0) {");
        output_line(gen, "        *token = %%token-index;");
        output_line(gen, "        *end_token = %%is-end-token;");
        output_line(gen, "        return %%token-length;");
        output_line(gen, "    }");
    }
    output_line(gen, "    return 0;");
    output_line(gen, "}");
    output_line(gen, "#endif");
    output_line(gen, "");

    free(internal.substitutions);
    gen->internal = 0;
}

static void generate_automaton(struct generator *gen, struct automaton *a,
 uint32_t offset, enum automaton_type type)
{
    for (uint32_t i = 0; i < a->number_of_states; ++i) {
        struct state s = a->states[i];
        set_number_substitution(gen, "state-id", i + offset);
        output_line(gen, "    case %%state-id:");
        output_line(gen, "state_%%state-id: {");
        if (s.accepting && type == BRACKET_AUTOMATON) {
            set_number_substitution(gen, "state-transition-symbol",
             s.transition_symbol);
            output_line(gen, "        start_state = cont->state_stack[--cont->state_stack_depth];");
            output_line(gen, "        run->tokens[token_index] = %%state-transition-symbol;");
            output_line(gen, "        goto start;");
            output_line(gen, "    }");
            continue;
        }
        output_line(gen, "        if (token_index >= number_of_tokens) {");
        output_line(gen, "            cont->state = %%state-id;");
        output_line(gen, "            return;");
        output_line(gen, "        }");
        output_line(gen, "        %%token-type token = run->tokens[token_index];");
        output_line(gen, "        run->states[token_index] = %%state-id;");
        output_line(gen, "        token_index++;");
        output_line(gen, "        switch (token) {");
        bool has_bracket_symbols = false;
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            if (t.symbol >= gen->combined->number_of_tokens) {
                // Symbols are either tokens or bracket symbols, so this must
                // be a bracket symbol.
                has_bracket_symbols = true;
            }
            set_number_substitution(gen, "token-symbol", t.symbol);
            set_number_substitution(gen, "token-target", t.target + offset);
            output_line(gen, "        case %%token-symbol: goto state_%%token-target;");
        }
        output_string(gen, "        default:");
        if (has_bracket_symbols) {
            output_line(gen, "");
            output_line(gen, "            if (cont->state_stack_depth >= cont->state_stack_capacity) {");
            output_line(gen, "                if (!grow_state_stack(cont))");
            output_line(gen, "                    break;"); // TODO: Error handling.
            output_line(gen, "            }");
            output_line(gen, "            cont->state_stack[cont->state_stack_depth++] = %%state-id;");
            output_line(gen, "            token_index--;");
            output_line(gen, "            goto state_%%first-bracket-state-id;");
        } else
            output_line(gen, " break;"); // TODO: Error handling.
        output_line(gen, "        }");
        output_line(gen, "        break;");
        output_line(gen, "    }");
    }
}

static void output_formatted_source(struct generator *gen, const char *string)
{
    // This implements a very simple C source formatter.
    size_t next_output_index = 0;
    char quoted_string_char = 0;
    int indent = 0;
    size_t i = 0;
    for (; string[i]; ++i) {
        bool line_break_after = false;
        if (quoted_string_char) {
            if (string[i] == '\\') {
                i++;
                if (string[i] == '\0')
                    break;
            } else if (string[i] == quoted_string_char)
                quoted_string_char = 0;
            continue;
        }
        if (string[i] == ';' || string[i] == '{')
            line_break_after = true;
        else if (string[i] == '"' || string[i] == '\'')
            quoted_string_char = string[i];
        else if (string[i] == '}') {
            indent--;
            if (string[i + 1] != ';')
                line_break_after = true;
        } else if (string[i] == ':') {
            line_break_after = true;
            indent--;
        }
        if (line_break_after) {
            for (int j = 0; j < indent; ++j)
                output_string(gen, "    ");
            output_string_length(gen, string + next_output_index,
             i + 1 - next_output_index);
            output_string(gen, "\n");
            if (string[i + 1] == ' ')
                next_output_index = i + 2;
            else
                next_output_index = i + 1;
        }
        if (string[i] == '{' || string[i] == ':')
            indent++;
    }
    output_string_length(gen, string + next_output_index,
     i - next_output_index);
}

static bool should_escape(char c)
{
    if (c >= 'a' && c <= 'z')
        return false;
    if (c >= 'A' && c <= 'Z')
        return false;
    if (c >= '0' && c <= '9')
        return false;
    const char *symbols = "!#%&'()*+,-./:;<=>?[]^_{|}~";
    for (size_t i = 0; symbols[i]; ++i) {
        if (c == symbols[i])
            return false;
    }
    return true;
}
