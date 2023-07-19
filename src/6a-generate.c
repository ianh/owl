// This stuff has to appear before any other #includes to avoid unwanted macro
// expansion from standard headers (e.g., memset -> __builtin___memset_chk).
#define EVALUATE_MACROS_AND_STRINGIFY(...) #__VA_ARGS__
#define STRINGIFY(...) EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__)
#define TOKEN_T %%token-type
#define STATE_T %%state-type
#define READ_WHITESPACE read_whitespace
#define READ_KEYWORD_TOKEN read_keyword_token
#define READ_CUSTOM_TOKEN %%read-custom-token
#define NUMBER_TOKEN_DATA NUMBER_TOKEN_DATA
#define CUSTOM_TOKEN_DATA CUSTOM_TOKEN_DATA
#define STRING_TOKEN_HAS_ESCAPES STRING_TOKEN_HAS_ESCAPES
#define WRITE_NUMBER_TOKEN %%write-number-token
#define WRITE_IDENTIFIER_TOKEN %%write-identifier-token
#define WRITE_INTEGER_TOKEN %%write-integer-token
#define WRITE_STRING_TOKEN %%write-string-token
#define WRITE_CUSTOM_TOKEN %%write-custom-token
#define ALLOCATE_STRING allocate_string_contents
#define ALLOW_DASHES_IN_IDENTIFIERS(...) %%allow-dashes-in-identifiers
#define IF_NUMBER_TOKEN IF_NUMBER_TOKEN
#define IF_STRING_TOKEN IF_STRING_TOKEN
#define IF_IDENTIFIER_TOKEN IF_IDENTIFIER_TOKEN
#define IF_INTEGER_TOKEN IF_INTEGER_TOKEN
#define ESCAPE_CHAR ESCAPE_CHAR
#define IDENTIFIER_TOKEN %%identifier-token
#define INTEGER_TOKEN %%integer-token
#define NUMBER_TOKEN %%number-token
#define STRING_TOKEN %%string-token
#define BRACKET_SYMBOL_TOKEN %%bracket-symbol-token
#define COMMENT_TOKEN %%comment-token
#define OWL_DONT_INLINE OWL_DONT_INLINE
#define TOKENIZE_BODY(...) static const char *tokenizer_source = EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__);
#include "x-tokenize.h"
#define FINISHED_NODE_T size_t
#define FINISH_NODE finish_node
#define FINISH_TOKEN finish_token
#define RULE_T uint32_t
#define RULE_LOOKUP rule_lookup
#define ROOT_RULE(...) %%root-rule-index
#define FIXITY_ASSOCIATIVITY_PRECEDENCE_LOOKUP(fixity_associativity, precedence, rule, choice, context) \
 do { \
     int local = 0; \
     fixity_associativity_precedence_lookup(&local, &precedence, rule, choice, context); \
     fixity_associativity = local; \
 } while (0)
#define NUMBER_OF_SLOTS_LOOKUP number_of_slots_lookup
#define LEFT_RIGHT_OPERAND_SLOTS_LOOKUP(rule, left, right, operand, info) \
 (left_right_operand_slots_lookup(rule, &(left), &(right), &(operand), info))
#define CONSTRUCT_BODY(...) static const char *construct_source = EVALUATE_MACROS_AND_STRINGIFY(__VA_ARGS__);
#include "x-construct-parse-tree.h"

// Now we can start the #includes as usual.
#include "6a-generate.h"

#include "6a-generate-output.h"
#include "construct-actions.h"
#include "grow-array.h"
#include <stdio.h>
#include <string.h>

struct generated_token {
    struct token token;
    struct generated_token *prefix;
};

static void generate_keyword_reader(struct generated_token *tokens,
 uint32_t number_of_tokens, struct generator_output *out);

static void generate_action_table(struct generator *gen,
 struct generator_output *out);

static void output_indentation(struct generator_output *out,
 size_t indentation);

static int compare_tokens(const void *a, const void *b);

static int compare_choice_names(const void *aa, const void *bb)
{
    struct choice *a = *(struct choice * const*)aa;
    struct choice *b = *(struct choice * const*)bb;
    if (a->name_length < b->name_length)
        return -1;
    if (a->name_length > b->name_length)
        return 1;
    return memcmp(a->name, b->name, a->name_length);
}

static uint32_t reachability_mask_width(struct generator *gen)
{
    return (gen->deterministic->transitions.number_of_transitions + 31) / 32;
}

struct state_in_automaton {
    struct bitset *reachability_mask;
    struct automaton *automaton;
    state_id state;
    state_id state_offset;
    bool bracket_accepting;
};
static int compare_state_transitions(const void *aa, const void *bb) {
    const struct state_in_automaton *a = (const struct state_in_automaton *)aa;
    const struct state_in_automaton *b = (const struct state_in_automaton *)bb;
    struct state sa = a->automaton->states[a->state];
    struct state sb = b->automaton->states[b->state];
    if (!a->bracket_accepting && b->bracket_accepting)
        return -1;
    if (a->bracket_accepting && !b->bracket_accepting)
        return -1;
    if (a->bracket_accepting && b->bracket_accepting) {
        if (sa.transition_symbol < sb.transition_symbol)
            return -1;
        if (sa.transition_symbol > sb.transition_symbol)
            return 1;
    }
    if (sa.number_of_transitions < sb.number_of_transitions)
        return -1;
    if (sa.number_of_transitions > sb.number_of_transitions)
        return 1;
    if (!a->reachability_mask && b->reachability_mask)
        return -1;
    if (a->reachability_mask && !b->reachability_mask)
        return 1;
    if (a->reachability_mask && b->reachability_mask) {
        int c = bitset_compare(a->reachability_mask, b->reachability_mask);
        if (c != 0)
            return c;
    }
    for (uint32_t i = 0; i < sa.number_of_transitions; ++i) {
        if (sa.transitions[i].symbol < sb.transitions[i].symbol)
            return -1;
        if (sa.transitions[i].symbol > sb.transitions[i].symbol)
            return 1;
        if (sa.transitions[i].target + a->state_offset <
         sb.transitions[i].target + b->state_offset)
            return -1;
        if (sa.transitions[i].target + a->state_offset >
         sb.transitions[i].target + b->state_offset)
            return 1;
    }
    return 0;
}
static void generate_reachability_mask_check(struct generator *gen,
 struct generator_output *out, struct bitset *r);

void generate(struct generator *gen)
{
    struct generator_output *out = output_create(gen->output);

    set_substitution(out, "root-rule",
     gen->grammar->rules[gen->grammar->root_rule]->name,
     gen->grammar->rules[gen->grammar->root_rule]->name_length,
     LOWERCASE_WITH_UNDERSCORES);
    set_unsigned_number_substitution(out, "root-rule-index",
     gen->grammar->root_rule);

    uint32_t n = gen->grammar->number_of_rules;
    bool has_custom_tokens = false;
    for (uint32_t i = 0; i < n; ++i) {
        if (gen->grammar->rules[i]->is_token &&
         gen->grammar->rules[i]->token_type == RULE_TOKEN_CUSTOM) {
            has_custom_tokens = true;
            break;
        }
    }
    if (gen->prefix) {
        set_substitution(out, "PREFIX", gen->prefix, strlen(gen->prefix),
         UPPERCASE_WITH_UNDERSCORES);
        set_substitution(out, "prefix", gen->prefix, strlen(gen->prefix),
         LOWERCASE_WITH_UNDERSCORES);
        set_substitution(out, "PARSED", gen->prefix, strlen(gen->prefix),
         UPPERCASE_WITH_UNDERSCORES);
        set_substitution(out, "parsed", gen->prefix, strlen(gen->prefix),
         LOWERCASE_WITH_UNDERSCORES);
    } else {
        set_substitution(out, "PREFIX", "owl", strlen("owl"),
         UPPERCASE_WITH_UNDERSCORES);
        set_substitution(out, "prefix", "owl", strlen("owl"),
         LOWERCASE_WITH_UNDERSCORES);
        set_substitution(out, "PARSED", "parsed", strlen("parsed"),
         UPPERCASE_WITH_UNDERSCORES);
        set_substitution(out, "parsed", "parsed", strlen("parsed"),
         LOWERCASE_WITH_UNDERSCORES);
    }

    output_line(out, "// This file was generated by the Owl parsing tool.");
    output_line(out, "// Make sure to #define %%PREFIX_PARSER_IMPLEMENTATION somewhere so the parser");
    output_line(out, "// is compiled properly.  Just two lines are enough -- a typical parser.c might");
    output_line(out, "// look like:");
    output_line(out, "//");
    output_line(out, "//   #define %%PREFIX_PARSER_IMPLEMENTATION");
    output_line(out, "//   #include \"parser.h\"");
    output_line(out, "");
    output_line(out, "#ifndef %%PREFIX_PARSER_H");
    output_line(out, "#define %%PREFIX_PARSER_H");
    output_line(out, "");
    output_line(out, "#include <stdbool.h>");
    output_line(out, "#include <stddef.h>");
    output_line(out, "#include <stdint.h>");
    output_line(out, "#include <stdio.h>");
    output_line(out, "");
    output_line(out, "// The %%prefix_tree struct represents an entire parse tree.  Use the");
    output_line(out, "// %%prefix_tree_create_...() functions to create a tree, then call %%prefix_tree_root()");
    output_line(out, "// to get the root %%prefix_ref.");
    output_line(out, "struct %%prefix_tree;");
    output_line(out, "");
    output_line(out, "// Creates an %%prefix_tree from a string.  The tree may directly reference pieces of");
    output_line(out, "// the string -- you're responsible for keeping it around until");
    output_line(out, "// %%prefix_tree_destroy() is called.");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_from_string(const char *string);");
    output_line(out, "");
    output_line(out, "// Creates an %%prefix_tree by reading from a file.");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_from_file(FILE *file);");
    output_line(out, "");
    output_line(out, "// Explicitly create a tree with particular options.");
    if (has_custom_tokens)
        output_line(out, "typedef struct %%prefix_token (*%%prefix_token_func_t)(const char *string, void *info);");
    output_line(out, "struct %%prefix_tree_options {");
    output_line(out, "    // Exactly one of string or file should be set.");
    output_line(out, "    const char *string;");
    output_line(out, "    FILE *file;");
    if (has_custom_tokens) {
        output_line(out, "    // A custom tokenizer function.");
        output_line(out, "    %%prefix_token_func_t tokenize;");
        output_line(out, "    // This pointer will be passed into the tokenize function.");
        output_line(out, "    void *tokenize_info;");
    }
    output_line(out, "};");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_with_options(struct %%prefix_tree_options options);");
    output_line(out, "");
    output_line(out, "// Destroys an %%prefix_tree, freeing its resources back to the system.");
    output_line(out, "void %%prefix_tree_destroy(struct %%prefix_tree *);");
    output_line(out, "");
    output_line(out, "// Prints a representation of the tree to standard output.");
    output_line(out, "void %%prefix_tree_print(struct %%prefix_tree *);");
    output_line(out, "");
    output_line(out, "// An %%prefix_ref references a list of children in the parse tree.  Use the");
    output_line(out, "// %%parsed_..._get() function corresponding to the element type to unpack the");
    output_line(out, "// child into its appropriate %%parsed_... struct.");
    output_line(out, "struct %%prefix_ref {");
    output_line(out, "    struct %%prefix_tree *_tree;");
    output_line(out, "    size_t _offset;");
    output_line(out, "    uint32_t _type;");
    output_line(out, "    bool empty;");
    output_line(out, "};");
    output_line(out, "");
    output_line(out, "// The %%prefix_next function advances a ref to the next sibling element.");
    output_line(out, "struct %%prefix_ref %%prefix_next(struct %%prefix_ref);");
    output_line(out, "");
    output_line(out, "// Tests two refs for equality.");
    output_line(out, "bool %%prefix_refs_equal(struct %%prefix_ref a, struct %%prefix_ref b);");
    output_line(out, "");
    output_line(out, "// Returns the root %%prefix_ref.");
    output_line(out, "struct %%prefix_ref %%prefix_tree_root_ref(struct %%prefix_tree *tree);");
    output_line(out, "");
    output_line(out, "// As a shortcut, returns the %%parsed_%%root-rule struct corresponding to the root ref.");
    output_line(out, "struct %%parsed_%%root-rule %%prefix_tree_get_%%parsed_%%root-rule(struct %%prefix_tree *tree);");
    output_line(out, "");
    output_line(out, "// The range of text corresponding to a tree element.");
    output_line(out, "struct source_range {");
    output_line(out, "    size_t start;");
    output_line(out, "    size_t end;");
    output_line(out, "};");
    output_line(out, "");
    output_line(out, "enum %%prefix_error {");
    output_line(out, "    // No error -- everything's fine!");
    output_line(out, "    ERROR_NONE,");
    output_line(out, "");
    output_line(out, "    // The file passed to %%prefix_tree_create_from_file was NULL.");
    output_line(out, "    ERROR_INVALID_FILE,");
    output_line(out, "");
    output_line(out, "    // The options passed to %%prefix_tree_create_with_options weren't valid --");
    output_line(out, "    // both file and string were specified at the same time or neither were.");
    output_line(out, "    ERROR_INVALID_OPTIONS,");
    output_line(out, "");
    output_line(out, "    // A piece of text couldn't be matched as a token.");
    output_line(out, "    ERROR_INVALID_TOKEN,");
    output_line(out, "");
    output_line(out, "    // The parser encountered an out-of-place token that doesn't fit the grammar.");
    output_line(out, "    ERROR_UNEXPECTED_TOKEN,");
    output_line(out, "");
    output_line(out, "    // The input is valid so far, but incomplete; more tokens could be added to");
    output_line(out, "    // complete it.");
    output_line(out, "    ERROR_MORE_INPUT_NEEDED,");
    output_line(out, "};");
    output_line(out, "// Returns an error code, or ERROR_NONE if there wasn't an error.");
    output_line(out, "// The error_range parameter can be null.");
    output_line(out, "enum %%prefix_error %%prefix_tree_get_error(struct %%prefix_tree *tree, struct source_range *error_range);");
    struct choice **choices = 0;
    uint32_t choices_allocated_bytes = 0;
    uint32_t choice_index = 0;
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        for (uint32_t j = 0; j < rule->number_of_choices; ++j) {
            uint32_t k = choice_index++;
            if (k == UINT32_MAX)
                abort();
            choices = grow_array(choices, &choices_allocated_bytes,
             choice_index * sizeof(struct choice *));
            choices[k] = &rule->choices[j];
        }
    }
    if (choice_index > 0) {
        output_line(out, "");
        qsort(choices, choice_index, sizeof(struct choice *),
         compare_choice_names);
        output_line(out, "enum %%parsed_type {");
        for (uint32_t i = 0; i < choice_index; ++i) {
            size_t len = choices[i]->name_length;
            if (i > 0 && choices[i - 1]->name_length == len &&
             !memcmp(choices[i - 1]->name, choices[i]->name, len))
                continue;
            set_substitution(out, "choice-name", choices[i]->name, len,
             UPPERCASE_WITH_UNDERSCORES);
            if (i == 0)
                output_line(out, "    %%PARSED_%%choice-name = 1,");
            else
                output_line(out, "    %%PARSED_%%choice-name,");
        }
        output_line(out, "};");
    }
    free(choices);
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "");
        output_line(out, "struct %%parsed_%%rule {");
        output_line(out, "    struct source_range range;");
        if (rule->number_of_choices > 0)
            output_line(out, "    enum %%parsed_type type;");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            struct slot slot = rule->slots[j];
            set_substitution(out, "referenced-slot", slot.name,
             slot.name_length, LOWERCASE_WITH_UNDERSCORES);
            output_line(out, "    struct %%prefix_ref %%referenced-slot;");
        }
        if (rule->is_token) {
            switch (rule->token_type) {
            case RULE_TOKEN_IDENTIFIER:
                output_line(out, "    const char *identifier;");
                output_line(out, "    size_t length;");
                break;
            case RULE_TOKEN_INTEGER:
                output_line(out, "    uint64_t integer;");
                break;
            case RULE_TOKEN_NUMBER:
                output_line(out, "    double number;");
                break;
            case RULE_TOKEN_STRING:
                output_line(out, "    const char *string;");
                output_line(out, "    size_t length;");
                break;
            case RULE_TOKEN_CUSTOM:
                output_line(out, "    union {");
                output_line(out, "        uint64_t integer;");
                output_line(out, "        double real;");
                output_line(out, "        void *pointer;");
                output_line(out, "    } data;");
                break;
            }
        }
        output_line(out, "};");
    }
    output_line(out, "");
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "struct %%parsed_%%rule %%parsed_%%rule_get(struct %%prefix_ref);");
    }
    if (has_custom_tokens) {
        output_line(out, "");
        output_line(out, "enum %%prefix_token_type {");
        for (uint32_t i = gen->combined->number_of_keyword_tokens;
         i < gen->combined->number_of_tokens; ++i) {
            struct token *token = &gen->combined->tokens[i];
            struct rule *rule = gen->grammar->rules[token->rule_index];
            if (rule->token_type != RULE_TOKEN_CUSTOM)
                continue;
            set_substitution(out, "name", token->string, token->length,
             UPPERCASE_WITH_UNDERSCORES);
            set_unsigned_number_substitution(out, "index", i);
            output_line(out, "    %%PREFIX_TOKEN_%%name = %%index,");
        }
        output_line(out, "    %%PREFIX_WHITESPACE = -1,");
        output_line(out, "};");
        output_line(out, "struct %%prefix_token {");
        output_line(out, "    enum %%prefix_token_type type;");
        output_line(out, "    size_t length;");
        output_line(out, "    union {");
        output_line(out, "        uint64_t integer;");
        output_line(out, "        double real;");
        output_line(out, "        void *pointer;");
        output_line(out, "    } data;");
        output_line(out, "};");
        output_line(out, "static struct %%prefix_token %%prefix_token_no_match;");
    }
    output_line(out, "");
    output_line(out, "#endif");
    output_line(out, "");
    output_line(out, "#ifdef %%PREFIX_PARSER_IMPLEMENTATION");
    output_line(out, "// Code implementing the parser.  This might get a bit messy!");
    output_line(out, "#include <assert.h>");
    output_line(out, "#include <inttypes.h>");
    output_line(out, "#include <stdio.h>");
    output_line(out, "#include <stdlib.h>");
    output_line(out, "#include <string.h>");

    output_line(out, "#if defined(__clang__) || defined(__GNUC__)");
    output_line(out, "#define OWL_DONT_INLINE __attribute__((noinline))");
    output_line(out, "#else");
    output_line(out, "#define OWL_DONT_INLINE");
    output_line(out, "#endif");

    output_line(out, "");
    output_line(out, "struct %%prefix_tree {");
    output_line(out, "    const char *string;");
    output_line(out, "    bool owns_string;");
    if (has_custom_tokens) {
        output_line(out, "    %%prefix_token_func_t custom_tokenize;");
        output_line(out, "    void *custom_tokenize_info;");
    }
    output_line(out, "    uint8_t *parse_tree;");
    output_line(out, "    size_t parse_tree_size;");
    output_line(out, "    size_t next_offset;");
    output_line(out, "    enum %%prefix_error error;");
    output_line(out, "    struct source_range error_range;");
    output_line(out, "    size_t root_offset;");
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (!rule->is_token || rule->token_type == RULE_TOKEN_CUSTOM)
            continue;
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "    size_t next_%%rule_token_offset;");
    }
    if (has_custom_tokens)
        output_line(out, "    size_t next_custom_token_offset;");
    output_line(out, "};");

    set_literal_substitution(out, "token-type", "uint32_t");
    set_literal_substitution(out, "state-type", "uint32_t");

    // Code for reading and writing packed parse trees.
    output_line(out, "// Reserve 10 bytes for each entry (the maximum encoded size of a 64-bit value).");
    output_line(out, "#define RESERVATION_AMOUNT 10");
    output_line(out, "static inline uint64_t read_tree(size_t *offset, struct %%prefix_tree *tree) {");
    output_line(out, "    uint8_t *parse_tree = tree->parse_tree;");
    output_line(out, "    size_t parse_tree_size = tree->parse_tree_size;");
    output_line(out, "    size_t i = *offset;");
    output_line(out, "    if (i + RESERVATION_AMOUNT >= parse_tree_size)");
    output_line(out, "        return 0;");
    output_line(out, "    uint64_t result = 0;");
    output_line(out, "    int shift_amount = 0;");
    output_line(out, "    while ((parse_tree[i] & 0x80) != 0 && shift_amount < 64) {");
    output_line(out, "        result |= ((uint64_t)parse_tree[i] & 0x7f) << shift_amount;");
    output_line(out, "        shift_amount += 7;");
    output_line(out, "        i++;");
    output_line(out, "    }");
    output_line(out, "    result |= ((uint64_t)parse_tree[i] & 0x7f) << shift_amount;");
    output_line(out, "    i++;");
    output_line(out, "    *offset = i;");
    output_line(out, "    return result;");
    output_line(out, "}");
    output_line(out, "static bool grow_tree(struct %%prefix_tree *tree, size_t size)");
    output_line(out, "{");
    output_line(out, "    size_t n = tree->parse_tree_size;");
    output_line(out, "    while (n < size || n < 4096)");
    output_line(out, "        n = (n + 1) * 3 / 2;");
    output_line(out, "    uint8_t *parse_tree = realloc(tree->parse_tree, n);");
    output_line(out, "    if (!parse_tree)");
    output_line(out, "        return false;");
    output_line(out, "    tree->parse_tree_size = n;");
    output_line(out, "    tree->parse_tree = parse_tree;");
    output_line(out, "    return true;");
    output_line(out, "}");
    output_line(out, "static void write_tree(struct %%prefix_tree *tree, uint64_t value)");
    output_line(out, "{");
    output_line(out, "    size_t reserved_size = tree->next_offset + RESERVATION_AMOUNT;");
    output_line(out, "    if (tree->parse_tree_size <= reserved_size && !grow_tree(tree, reserved_size))");
    output_line(out, "        abort();");
    output_line(out, "    while (value >> 7 != 0) {");
    output_line(out, "        tree->parse_tree[tree->next_offset++] = 0x80 | (value & 0x7f);");
    output_line(out, "        value >>= 7;");
    output_line(out, "    }");
    output_line(out, "    tree->parse_tree[tree->next_offset++] = value & 0x7f;");
    output_line(out, "}");
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        set_unsigned_number_substitution(out, "rule-index", i);
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "struct %%parsed_%%rule %%parsed_%%rule_get(struct %%prefix_ref ref) {");
        output_line(out, "    if (ref.empty || ref._type != %%rule-index) {");
        output_line(out, "        return (struct %%parsed_%%rule){");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            set_substitution(out, "referenced-slot", rule->slots[j].name,
             rule->slots[j].name_length, LOWERCASE_WITH_UNDERSCORES);
            output_line(out, "            .%%referenced-slot.empty = true,");
        }
        if (rule->number_of_slots == 0)
            output_line(out, "            {0}");
        output_line(out, "        };");
        output_line(out, "    }");
        output_line(out, "    size_t offset = ref._offset;");
        output_line(out, "    read_tree(&offset, ref._tree); // Read and ignore the 'next offset' field.");
        if (rule->is_token) {
            output_line(out, "    size_t token_offset = read_tree(&offset, ref._tree);");
            output_line(out, "    read_tree(&token_offset, ref._tree);");
            output_line(out, "    size_t start_location = read_tree(&token_offset, ref._tree);");
            output_line(out, "    size_t end_location = start_location + read_tree(&token_offset, ref._tree);");
            if (rule->token_type == RULE_TOKEN_STRING) {
                output_line(out, "    size_t string_offset = read_tree(&token_offset, ref._tree);");
                output_line(out, "    const char *string = string_offset ?");
                output_line(out, "     (const char *)ref._tree->parse_tree + string_offset : ref._tree->string + start_location + 1;");
                output_line(out, "    size_t string_length = string_offset ?");
                output_line(out, "     read_tree(&token_offset, ref._tree) : end_location - start_location - 2;");
            }
        } else {
            output_line(out, "    size_t start_location = read_tree(&offset, ref._tree);");
            output_line(out, "    size_t end_location = start_location + read_tree(&offset, ref._tree);");
        }
        output_line(out, "    struct %%parsed_%%rule result = {");
        output_line(out, "        .range.start = start_location,");
        output_line(out, "        .range.end = end_location,");
        if (rule->is_token) {
            switch (rule->token_type) {
            case RULE_TOKEN_IDENTIFIER:
                output_line(out, "        .identifier = ref._tree->string + start_location,");
                output_line(out, "        .length = end_location - start_location,");
                break;
            case RULE_TOKEN_INTEGER:
                output_line(out, "        .integer = read_tree(&token_offset, ref._tree),");
                break;
            case RULE_TOKEN_NUMBER:
                output_line(out, "        .number = (union { double n; uint64_t v; }){ .v = read_tree(&token_offset, ref._tree) }.n,");
                break;
            case RULE_TOKEN_STRING:
                output_line(out, "        .string = string,");
                output_line(out, "        .length = string_length,");
                break;
            case RULE_TOKEN_CUSTOM:
                output_line(out, "        .data.integer = read_tree(&token_offset, ref._tree),");
                break;
            }
        }
        if (rule->number_of_choices > 0)
            output_line(out, "        .type = (enum %%parsed_type)read_tree(&offset, ref._tree),");
        output_line(out, "    };");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            struct slot slot = rule->slots[j];
            set_substitution(out, "referenced-slot", slot.name,
             slot.name_length, LOWERCASE_WITH_UNDERSCORES);
            set_unsigned_number_substitution(out, "referenced-slot-type",
             slot.rule_index);
            output_line(out, "    result.%%referenced-slot._tree = ref._tree;");
            output_line(out, "    result.%%referenced-slot._offset = read_tree(&offset, ref._tree);");
            output_line(out, "    result.%%referenced-slot._type = %%referenced-slot-type;");
            output_line(out, "    result.%%referenced-slot.empty = result.%%referenced-slot._offset == 0;");
        }
        output_line(out, "    return result;");
        output_line(out, "}");
    }
    output_line(out, "static size_t finish_node(uint32_t rule, uint32_t choice, "
     "size_t next_sibling, size_t *slots, size_t start_location, size_t end_location, void *info) {");
    output_line(out, "    struct %%prefix_tree *tree = info;");
    output_line(out, "    size_t offset = tree->next_offset;");
    output_line(out, "    write_tree(tree, next_sibling ? offset - next_sibling : 0);");
    output_line(out, "    write_tree(tree, start_location);");
    output_line(out, "    write_tree(tree, end_location - start_location);");
    output_line(out, "    switch (rule) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (rule->is_token)
            continue;
        set_unsigned_number_substitution(out, "rule-index", i);
        output_line(out, "    case %%rule-index: {");
        if (rule->number_of_choices > 0) {
            output_line(out, "        switch (choice) {");
            for (uint32_t i = 0; i < rule->number_of_choices; ++i) {
                set_unsigned_number_substitution(out, "choice-index", i);
                set_substitution(out, "choice-name", rule->choices[i].name,
                 rule->choices[i].name_length, UPPERCASE_WITH_UNDERSCORES);
                output_line(out, "        case %%choice-index:");
                output_line(out, "            write_tree(tree, %%PARSED_%%choice-name);");
                output_line(out, "            break;");
            }
            output_line(out, "        }");
        }
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            set_unsigned_number_substitution(out, "slot-index", j);
            output_line(out, "        write_tree(tree, slots[%%slot-index]);");
        }
        output_line(out, "        break;");
        output_line(out, "    }");
    }
    output_line(out, "    default:");
    output_line(out, "        break;");
    output_line(out, "    }");
    output_line(out, "    return offset;");
    output_line(out, "}");
    output_line(out, "static size_t finish_token(uint32_t rule, size_t next_sibling, void *info) {");
    output_line(out, "    struct %%prefix_tree *tree = info;");
    output_line(out, "    size_t offset = tree->next_offset;");
    output_line(out, "    write_tree(tree, next_sibling ? offset - next_sibling : 0);");
    output_line(out, "    switch (rule) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (!rule->is_token)
            continue;
        set_unsigned_number_substitution(out, "rule-index", i);
        if (rule->token_type == RULE_TOKEN_CUSTOM)
            set_literal_substitution(out, "rule", "custom");
        else {
            set_substitution(out, "rule", rule->name, rule->name_length,
             LOWERCASE_WITH_UNDERSCORES);
        }
        output_line(out, "    case %%rule-index: {");
        output_line(out, "        size_t offset%%rule-index = tree->next_%%rule_token_offset;");
        output_line(out, "        if (offset%%rule-index == 0)");
        output_line(out, "            abort();");
        output_line(out, "        write_tree(tree, offset%%rule-index);");
        output_line(out, "        tree->next_%%rule_token_offset = offset%%rule-index;");
        output_line(out, "        tree->next_%%rule_token_offset -= read_tree(&offset%%rule-index, tree);");
        output_line(out, "        break;");
        output_line(out, "    }");
    }
    output_line(out, "    default:");
    output_line(out, "        break;");
    output_line(out, "    }");
    output_line(out, "    return offset;");
    output_line(out, "}");
    output_line(out, "static void check_for_error(struct %%prefix_tree *tree) {");
    output_line(out, "    if (tree->error == ERROR_NONE)");
    output_line(out, "        return;");
    output_line(out, "    fprintf(stderr, \"parse error: \");");
    output_line(out, "    switch (tree->error) {");
    output_line(out, "    case ERROR_INVALID_FILE:");
    output_line(out, "        fprintf(stderr, \"invalid file\\n\");");
    output_line(out, "        break;");
    output_line(out, "    case ERROR_INVALID_OPTIONS:");
    output_line(out, "        fprintf(stderr, \"invalid options\\n\");");
    output_line(out, "        break;");
    output_line(out, "    case ERROR_INVALID_TOKEN:");
    output_line(out, "        fprintf(stderr, \"invalid token '%.*s'\\n\", (int)(tree->error_range.end - tree->error_range.start), tree->string + tree->error_range.start);");
    output_line(out, "        break;");
    output_line(out, "    case ERROR_UNEXPECTED_TOKEN:");
    output_line(out, "        fprintf(stderr, \"unexpected token '%.*s'\\n\", (int)(tree->error_range.end - tree->error_range.start), tree->string + tree->error_range.start);");
    output_line(out, "        break;");
    output_line(out, "    case ERROR_MORE_INPUT_NEEDED:");
    output_line(out, "        fprintf(stderr, \"more input needed\\n\");");
    output_line(out, "        break;");
    output_line(out, "    default:");
    output_line(out, "        break;");
    output_line(out, "    }");
    output_line(out, "    exit(-1);");
    output_line(out, "}");
    // Only generate print functions that will actually be called (mostly to
    // avoid compiler warnings).
    struct bitset printable_rules = bitset_create_empty(n);
    bitset_add(&printable_rules, gen->grammar->root_rule);
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        for (uint32_t j = 0; j < rule->number_of_slots; ++j)
            bitset_add(&printable_rules, rule->slots[j].rule_index);
    }
    for (uint32_t i = 0; i < n; ++i) {
        if (!bitset_contains(&printable_rules, i))
            continue;
        struct rule *rule = gen->grammar->rules[i];
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "static void %%parsed_%%rule_print(struct %%prefix_tree *tree, struct %%prefix_ref ref, const char *slot_name, int indent);");
    }
    for (uint32_t i = 0; i < n; ++i) {
        if (!bitset_contains(&printable_rules, i))
            continue;
        struct rule *rule = gen->grammar->rules[i];
        set_substitution(out, "rule", rule->name, rule->name_length,
         LOWERCASE_WITH_UNDERSCORES);
        output_line(out, "static void %%parsed_%%rule_print(struct %%prefix_tree *tree, struct %%prefix_ref ref, const char *slot_name, int indent) {");
        output_line(out, "    int i;");
        output_line(out, "    while (!ref.empty) {");
        output_line(out, "        struct %%parsed_%%rule it = %%parsed_%%rule_get(ref);");
        output_line(out, "        for (i = 0; i < indent; ++i) printf(\"  \");");
        output_line(out, "        printf(\"%%rule\");");
        output_line(out, "        if (strcmp(\"%%rule\", slot_name))");
        output_line(out, "            printf(\"@%s\", slot_name);");
        if (rule->number_of_choices > 0) {
            output_line(out, "        switch (it.type) {");
            for (uint32_t j = 0; j < rule->number_of_choices; ++j) {
                set_substitution(out, "choice-name", rule->choices[j].name,
                 rule->choices[j].name_length, UPPERCASE_WITH_UNDERSCORES);
                output_line(out, "        case %%PARSED_%%choice-name:");
                output_line(out, "            printf(\" : %%choice-name\");");
                output_line(out, "            break;");
            }
            output_line(out, "        default:");
            output_line(out, "            break;");
            output_line(out, "        }");
        }
        if (rule->is_token) {
            switch (rule->token_type) {
            case RULE_TOKEN_IDENTIFIER:
                output_line(out, "        printf(\" - %.*s\", (int)it.length, it.identifier);");
                break;
            case RULE_TOKEN_INTEGER:
                output_line(out, "        printf(\" - %\" PRIu64, it.integer);");
                break;
            case RULE_TOKEN_NUMBER:
                output_line(out, "        printf(\" - %f\", it.number);");
                break;
            case RULE_TOKEN_STRING:
                output_line(out, "        printf(\" - %.*s\", (int)it.length, it.string);");
                break;
            case RULE_TOKEN_CUSTOM:
                output_line(out, "        printf(\" - %.*s\", (int)(it.range.end - it.range.start), tree->string + it.range.start);");
                break;
            }
        }
        output_line(out, "        printf(\" (%zu - %zu)\\n\", it.range.start, it.range.end);");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            struct slot slot = rule->slots[j];
            set_substitution(out, "slot-name", slot.name, slot.name_length,
             LOWERCASE_WITH_UNDERSCORES);
            struct rule *slot_rule = gen->grammar->rules[slot.rule_index];
            set_substitution(out, "slot-rule", slot_rule->name,
             slot_rule->name_length, LOWERCASE_WITH_UNDERSCORES);
            output_line(out, "        %%parsed_%%slot-rule_print(tree, it.%%slot-name, \"%%slot-name\", indent + 1);");
        }
        output_line(out, "        ref = %%prefix_next(ref);");
        output_line(out, "    }");
        output_line(out, "}");
    }
    bitset_destroy(&printable_rules);
    output_line(out, "void %%prefix_tree_print(struct %%prefix_tree *tree) {");
    output_line(out, "    check_for_error(tree);");
    output_line(out, "    %%parsed_%%root-rule_print(tree, %%prefix_tree_root_ref(tree), \"%%root-rule\", 0);");
    output_line(out, "}");

    output_line(out, "struct %%prefix_ref %%prefix_next(struct %%prefix_ref ref) {");
    output_line(out, "    if (ref.empty) return ref;");
    output_line(out, "    size_t offset = ref._offset;");
    output_line(out, "    size_t delta = read_tree(&ref._offset, ref._tree);");
    output_line(out, "    return (struct %%prefix_ref){");
    output_line(out, "        ._tree = ref._tree,");
    output_line(out, "        ._offset = offset - delta,");
    output_line(out, "        ._type = ref._type,");
    output_line(out, "        .empty = delta == 0,");
    output_line(out, "    };");
    output_line(out, "}");

    output_line(out, "bool %%prefix_refs_equal(struct %%prefix_ref a, struct %%prefix_ref b) {");
    output_line(out, "    return a._tree == b._tree && a._offset == b._offset;");
    output_line(out, "}");

    output_line(out, "struct %%prefix_ref %%prefix_tree_root_ref(struct %%prefix_tree *tree) {");
    output_line(out, "    check_for_error(tree);");
    output_line(out, "    return (struct %%prefix_ref){");
    output_line(out, "        ._tree = tree,");
    output_line(out, "        ._offset = tree->root_offset,");
    output_line(out, "        ._type = %%root-rule-index,");
    output_line(out, "        .empty = tree->root_offset == 0,");
    output_line(out, "    };");
    output_line(out, "}");

    output_line(out, "struct %%parsed_%%root-rule %%prefix_tree_get_%%parsed_%%root-rule(struct %%prefix_tree *tree) {");
    output_line(out, "    check_for_error(tree);");
    output_line(out, "    return %%parsed_%%root-rule_get(%%prefix_tree_root_ref(tree));");
    output_line(out, "}");

    if (version_capable(gen->version, SINGLE_CHAR_ESCAPES)) {
        set_literal_substitution(out, "escape-char-single",
         STRINGIFY(ESCAPE_CHAR_SINGLE(c, info)));
        output_line(out, "#define ESCAPE_CHAR(c, info) %%escape-char-single");
    } else
        output_line(out, "#define ESCAPE_CHAR(c, info) (c)");
    set_unsigned_number_substitution(out, "identifier-token", 0xffffffff);
    set_unsigned_number_substitution(out, "integer-token", 0xffffffff);
    set_unsigned_number_substitution(out, "number-token", 0xffffffff);
    set_unsigned_number_substitution(out, "string-token", 0xffffffff);
    set_unsigned_number_substitution(out, "bracket-symbol-token", 0xffffffff);
    set_unsigned_number_substitution(out, "comment-token", 0xffffffff);
    output_line(out, "#define IGNORE_TOKEN_WRITE(...)");
    set_literal_substitution(out, "write-identifier-token", "IGNORE_TOKEN_WRITE");
    set_literal_substitution(out, "write-integer-token", "IGNORE_TOKEN_WRITE");
    set_literal_substitution(out, "write-number-token", "IGNORE_TOKEN_WRITE");
    set_literal_substitution(out, "write-string-token", "IGNORE_TOKEN_WRITE");
    set_literal_substitution(out, "write-custom-token", "IGNORE_TOKEN_WRITE");
    output_line(out, "#define IGNORE_TOKEN_READ(...) (0)");
    if (has_custom_tokens) {
        output_line(out, "#define CUSTOM_TOKEN_DATA(identifier) uint64_t identifier = 0");
        set_literal_substitution(out, "read-custom-token", "read_custom_token");
        output_line(out, "static bool read_custom_token(uint32_t *token, size_t *token_length, const char *text, bool *whitespace, uint64_t *data, void *info) {");
        output_line(out, "    struct %%prefix_tree *tree = info;");
        output_line(out, "    if (!tree->custom_tokenize)");
        output_line(out, "        return false;");
        output_line(out, "    struct %%prefix_token t = tree->custom_tokenize(text, tree->custom_tokenize_info);");
        output_line(out, "    if (t.length <= *token_length)");
        output_line(out, "        return false;");
        output_line(out, "    *token = t.type;");
        output_line(out, "    *token_length = t.length;");
        output_line(out, "    *whitespace = (t.type == %%PREFIX_WHITESPACE);");
        output_line(out, "    *data = t.data.integer;");
        output_line(out, "    return true;");
        output_line(out, "}");
    } else {
        set_literal_substitution(out, "read-custom-token", "IGNORE_TOKEN_READ");
        output_line(out, "#define CUSTOM_TOKEN_DATA(...)");
    }
    bool has_identifier_token = false;
    bool has_integer_token = false;
    bool has_number_token = false;
    bool has_string_token = false;
    for (uint32_t i = gen->combined->number_of_keyword_tokens;
     i < gen->combined->number_of_tokens; ++i) {
        uint32_t rule_index = gen->combined->tokens[i].rule_index;
        struct rule *rule = gen->grammar->rules[rule_index];
        switch (rule->token_type) {
        case RULE_TOKEN_IDENTIFIER:
            set_unsigned_number_substitution(out, "identifier-token", i);
            has_identifier_token = true;
            break;
        case RULE_TOKEN_INTEGER:
            set_unsigned_number_substitution(out, "integer-token", i);
            has_integer_token = true;
            break;
        case RULE_TOKEN_NUMBER:
            set_unsigned_number_substitution(out, "number-token", i);
            has_number_token = true;
            break;
        case RULE_TOKEN_STRING:
            set_unsigned_number_substitution(out, "string-token", i);
            has_string_token = true;
            break;
        case RULE_TOKEN_CUSTOM:
            break;
        }
    }
    if (!has_identifier_token)
        output_line(out, "#define IF_IDENTIFIER_TOKEN(...) if (0) { /* no identifier tokens */  }");
    else
        output_line(out, "#define IF_IDENTIFIER_TOKEN(cond, ...) if (cond) __VA_ARGS__");
    if (!has_integer_token) {
        output_line(out, "#define INTEGER_TOKEN_DATA(...)");
        output_line(out, "#define IF_INTEGER_TOKEN(...) if (0) { /* no integer tokens */  }");
    } else {
        output_line(out, "#define INTEGER_TOKEN_DATA(name) uint64_t name");
        output_line(out, "#define IF_INTEGER_TOKEN(cond, ...) if (cond) __VA_ARGS__");
    }
    if (!has_number_token) {
        output_line(out, "#define NUMBER_TOKEN_DATA(...)");
        output_line(out, "#define IF_NUMBER_TOKEN(...) if (0) { /* no number tokens */  }");
    } else {
        output_line(out, "#define NUMBER_TOKEN_DATA(name) double name = 0");
        output_line(out, "#define IF_NUMBER_TOKEN(cond, ...) if (cond) __VA_ARGS__");
    }
    if (!has_string_token) {
        output_line(out, "#define STRING_TOKEN_HAS_ESCAPES(...)");
        output_line(out, "#define IF_STRING_TOKEN(...) if (0) { /* no string tokens */  }");
    } else {
        output_line(out, "#define STRING_TOKEN_HAS_ESCAPES(name) bool name = false");
        output_line(out, "#define IF_STRING_TOKEN(cond, ...) if (cond) __VA_ARGS__");
    }
    output_line(out, "static size_t read_whitespace(const char *text, void *info);");
    output_line(out, "static size_t read_keyword_token(%%token-type *token, bool *end_token, const char *text, void *info);");
    bool has_write_custom_token = false;
    for (uint32_t i = 0; i < n; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (!rule->is_token)
            continue;
        if (rule->token_type == RULE_TOKEN_CUSTOM) {
            if (has_write_custom_token)
                continue;
            has_write_custom_token = true;
            set_literal_substitution(out, "rule", "custom");
        } else {
            set_substitution(out, "rule", rule->name, rule->name_length,
             LOWERCASE_WITH_UNDERSCORES);
        }
        switch (rule->token_type) {
        case RULE_TOKEN_IDENTIFIER:
            set_literal_substitution(out, "write-identifier-token", "write_identifier_token");
            output_line(out, "static void write_identifier_token(size_t offset, size_t length, void *info) {");
            output_line(out, "    struct %%prefix_tree *tree = info;");
            break;
        case RULE_TOKEN_INTEGER:
            set_literal_substitution(out, "write-integer-token", "write_integer_token");
            output_line(out, "static void write_integer_token(size_t offset, size_t length, uint64_t integer, void *info) {");
            output_line(out, "    struct %%prefix_tree *tree = info;");
            break;
        case RULE_TOKEN_NUMBER:
            set_literal_substitution(out, "write-number-token", "write_number_token");
            output_line(out, "static void write_number_token(size_t offset, size_t length, double number, void *info) {");
            output_line(out, "    struct %%prefix_tree *tree = info;");
            break;
        case RULE_TOKEN_STRING:
            set_literal_substitution(out, "write-string-token", "write_string_token");
            output_line(out, "static void write_string_token(size_t offset, size_t length, const char *string, size_t string_length, bool has_escapes, void *info) {");
            output_line(out, "    struct %%prefix_tree *tree = info;");
            output_line(out, "    size_t string_offset = has_escapes ? (uint8_t *)string - tree->parse_tree : 0;");
            break;
        case RULE_TOKEN_CUSTOM:
            set_literal_substitution(out, "write-custom-token", "write_custom_token");
            output_line(out, "static void write_custom_token(size_t offset, size_t length, uint32_t token, uint64_t data, void *info) {");
            output_line(out, "    struct %%prefix_tree *tree = info;");
            break;
        }
        output_line(out, "    size_t token_offset = tree->next_offset;");
        output_line(out, "    write_tree(tree, token_offset - tree->next_%%rule_token_offset);");
        output_line(out, "    write_tree(tree, offset);");
        output_line(out, "    write_tree(tree, length);");
        switch (rule->token_type) {
        case RULE_TOKEN_IDENTIFIER:
            // We don't need to do anything else.
            break;
        case RULE_TOKEN_INTEGER:
            output_line(out, "    write_tree(tree, integer);");
            break;
        case RULE_TOKEN_NUMBER:
            output_line(out, "    union { double n; uint64_t v; } u = { .n = number };");
            output_line(out, "    write_tree(tree, u.v);");
            break;
        case RULE_TOKEN_STRING:
            output_line(out, "    if (string_offset) {");
            output_line(out, "        write_tree(tree, string_offset);");
            output_line(out, "        write_tree(tree, string_length);");
            output_line(out, "    } else");
            output_line(out, "        write_tree(tree, 0);");
            break;
        case RULE_TOKEN_CUSTOM:
            output_line(out, "    write_tree(tree, data);");
            break;
        }
        output_line(out, "    tree->next_%%rule_token_offset = token_offset;");
        output_line(out, "}");
    }
    if (has_string_token) {
        output_line(out, "static void *allocate_string_contents(size_t size, void *info) {");
        output_line(out, "    struct %%prefix_tree *tree = info;");
        output_line(out, "    if (tree->next_offset + size > tree->parse_tree_size)");
        output_line(out, "        grow_tree(tree, tree->next_offset + size);");
        output_line(out, "    void *p = tree->parse_tree + tree->next_offset;");
        output_line(out, "    tree->next_offset += size;");
        output_line(out, "    return p;");
        output_line(out, "}");
    }
    if (SHOULD_ALLOW_DASHES_IN_IDENTIFIERS(gen->combined))
        set_literal_substitution(out, "allow-dashes-in-identifiers", "true");
    else
        set_literal_substitution(out, "allow-dashes-in-identifiers", "false");
    output_formatted_source(out, tokenizer_source);
    output_line(out, "static uint32_t rule_lookup(uint32_t parent, uint32_t slot, void *context);");
    output_line(out, "static void fixity_associativity_precedence_lookup(int *fixity_associativity, int *precedence, uint32_t rule, uint32_t choice, void *context);");
    output_line(out, "static size_t number_of_slots_lookup(uint32_t rule, void *context);");
    output_line(out, "static void left_right_operand_slots_lookup(uint32_t rule, uint32_t *left, uint32_t *right, uint32_t *operand, void *context);");

    output_formatted_source(out, construct_source);

    set_unsigned_number_substitution(out, "start-state",
     gen->deterministic->automaton.start_state);
    output_line(out, "");
    uint32_t mask_width = reachability_mask_width(gen);
    if (mask_width == 0)
        mask_width = 1;
    set_unsigned_number_substitution(out, "reachability-mask-width", mask_width);
    output_line(out, "struct fill_run_continuation;");
    output_line(out, "struct fill_run_state {");
    output_line(out, "    %%state-type state;");
    output_line(out, "    uint32_t reachability_mask[%%reachability-mask-width];");
    output_line(out, "    struct fill_run_continuation *cont;");
    output_line(out, "};");
    output_line(out, "struct fill_run_continuation {");
    output_line(out, "    struct fill_run_state *stack;");
    output_line(out, "    size_t top_index;");
    output_line(out, "    size_t capacity;");
    output_line(out, "    int error;");
    output_line(out, "};");
    struct automaton *a = &gen->deterministic->automaton;
    struct automaton *b = &gen->deterministic->bracket_automaton;
    set_unsigned_number_substitution(out, "first-bracket-state-id",
     a->number_of_states);
    output_string(out, "static void bracket_entry_state(struct owl_token_run *run, struct fill_run_state *top, uint16_t token_index");
    for (uint32_t i = 0; i < mask_width; ++i) {
        set_unsigned_number_substitution(out, "mask-index", i);
        output_string(out, ", uint32_t mask%%mask-index");
    }
    output_line(out, ");");
    uint32_t total_states = a->number_of_states + b->number_of_states;
    struct state_in_automaton *sorted_states =
     malloc(sizeof(struct state_in_automaton) * total_states);
    for (state_id i = 0; i < a->number_of_states; ++i) {
        sorted_states[i] = (struct state_in_automaton){
            .reachability_mask = 0,
            .automaton = a,
            .state = i,
            .state_offset = 0,
        };
    }
    for (state_id i = 0; i < b->number_of_states; ++i) {
        sorted_states[i + a->number_of_states] = (struct state_in_automaton){
            .reachability_mask = &gen->deterministic->bracket_reachability[i],
            .automaton = b,
            .state = i,
            .state_offset = a->number_of_states,
            .bracket_accepting = b->states[i].accepting,
        };
    }
    qsort(sorted_states, total_states, sizeof(struct state_in_automaton),
     compare_state_transitions);
    set_unsigned_number_substitution(out, "total-number-of-states", total_states);
    output_line(out, "static void (*state_funcs[%%total-number-of-states])(struct owl_token_run *, struct fill_run_state *, uint16_t);");
    state_id *func_id_for_state = calloc(total_states, sizeof(state_id));
    state_id func_id = 0;
    for (uint32_t i = 0; i < total_states; ++i) {
        if (i > 0 && compare_state_transitions(sorted_states + i,
         sorted_states + i - 1) == 0) {
            func_id_for_state[sorted_states[i].state +
             sorted_states[i].state_offset] = func_id;
            continue;
        }
        func_id = sorted_states[i].state + sorted_states[i].state_offset;
        func_id_for_state[sorted_states[i].state +
         sorted_states[i].state_offset] = func_id;
        struct state s = sorted_states[i].automaton->states[sorted_states[i].state];
        set_unsigned_number_substitution(out, "func-id", func_id);
        output_line(out, "static void state_func_%%func-id(struct owl_token_run *run, struct fill_run_state *top, uint16_t token_index) {");
        uint32_t mask_width = reachability_mask_width(gen);
        if (sorted_states[i].reachability_mask) {
            generate_reachability_mask_check(gen, out,
             sorted_states[i].reachability_mask);
        }
        if (sorted_states[i].bracket_accepting) {
            set_unsigned_number_substitution(out, "state-transition-symbol",
             s.transition_symbol);
            output_line(out, "    if (top->cont->top_index == 0) {");
            output_line(out, "        top->cont->error = 1;");
            output_line(out, "        return;");
            output_line(out, "    }");
            output_line(out, "    top->cont->top_index--;");
            output_line(out, "    top--;");
            output_line(out, "    run->tokens[token_index] = %%state-transition-symbol;");
            output_line(out, "    run->states[token_index] = top->state;");
            output_line(out, "    state_funcs[top->state](run, top, token_index);");
            output_line(out, "    return;");
            output_line(out, "}");
            continue;
        }
        output_line(out, "    %%token-type token = run->tokens[token_index];");
        output_line(out, "    switch (token) {");
        struct bitset reachability_mask = bitset_create_empty(gen->deterministic->transitions.number_of_transitions);
        for (uint32_t j = 0; j < s.number_of_transitions; ++j) {
            struct transition t = s.transitions[j];
            if (t.symbol >= gen->combined->number_of_tokens) {
                // Symbols are either tokens or bracket symbols, so this must
                // be a bracket symbol.
                struct bracket_transitions ts = gen->deterministic->transitions;
                for (uint32_t k = 0; k < ts.number_of_transitions; ++k) {
                    if (ts.transitions[k].deterministic_transition_symbol == t.symbol)
                        bitset_add(&reachability_mask, k);
                }
            }
            set_unsigned_number_substitution(out, "token-symbol", t.symbol);
            set_unsigned_number_substitution(out, "token-target", t.target + sorted_states[i].state_offset);
            output_line(out, "    case %%token-symbol: top->state = %%token-target; return;");
        }
        output_string(out, "    default:");
        if (!bitset_is_empty(&reachability_mask)) {
            output_line(out, "");
            output_string(out, "        bracket_entry_state(run, top, token_index");
            for (uint32_t i = 0; i < mask_width; ++i) {
                set_unsigned_number_substitution(out, "mask-index", i);
                uint64_t bits = reachability_mask.bit_groups[i / 2];
                if (i % 2)
                    bits >>= 32;
                else
                    bits &= UINT32_MAX;
                set_unsigned_number_substitution(out, "mask-bits", (uint32_t)bits);
                output_string(out, ", %%mask-bits");
            }
            output_line(out, ");");
            output_line(out, "        return;");
        } else
            output_line(out, " top->cont->error = 1; return;");
        bitset_destroy(&reachability_mask);
        output_line(out, "    }");
        output_line(out, "}");
    }
    output_string(out, "static void (*state_funcs[%%total-number-of-states])(struct owl_token_run *, struct fill_run_state *, uint16_t) = {");
    const int funcs_per_line = 4;
    for (state_id i = 0; i < total_states; ++i) {
        set_unsigned_number_substitution(out, "func-id", func_id_for_state[i]);
        if (i % funcs_per_line == 0) {
            output_line(out, "");
            output_string(out, "   ");
        }
        output_string(out, " state_func_%%func-id,");
    }
    output_line(out, "};");
    free(sorted_states);
    free(func_id_for_state);
    output_string(out, "static void bracket_entry_state(struct owl_token_run *run, struct fill_run_state *top, uint16_t token_index");
    for (uint32_t i = 0; i < mask_width; ++i) {
        set_unsigned_number_substitution(out, "mask-index", i);
        output_string(out, ", uint32_t mask%%mask-index");
    }
    output_line(out, ") {");
    output_line(out, "    struct fill_run_continuation *cont = top->cont;");
    output_line(out, "    cont->top_index++;");
    output_line(out, "    if (cont->top_index >= cont->capacity) {");
    output_line(out, "        size_t new_capacity = (cont->capacity + 2) * 3 / 2;");
    output_line(out, "        if (new_capacity <= cont->capacity)");
    output_line(out, "            abort();");
    output_line(out, "        struct fill_run_state *new_states = realloc(cont->stack, new_capacity * sizeof(struct fill_run_state));");
    output_line(out, "        if (!new_states)");
    output_line(out, "            abort();");
    output_line(out, "        cont->stack = new_states;");
    output_line(out, "        cont->capacity = new_capacity;");
    output_line(out, "        top = &cont->stack[cont->top_index];");
    output_line(out, "    } else");
    output_line(out, "        top++;");
    output_line(out, "    top->cont = cont;");
    for (uint32_t i = 0; i < mask_width; ++i) {
        set_unsigned_number_substitution(out, "mask-index", i);
        output_line(out, "    top->reachability_mask[%%mask-index] = mask%%mask-index;");
    }
    output_line(out, "    run->states[token_index] = %%first-bracket-state-id;");
    output_line(out, "    state_func_%%first-bracket-state-id(run, top, token_index);");
    output_line(out, "    if (top->cont->error == -1)");
    output_line(out, "        top->cont->error = 1;");
    output_line(out, "}");
    output_line(out, "static bool fill_run_states(struct owl_token_run *run, struct fill_run_continuation *cont, uint16_t *failing_index);");
    output_line(out, "static size_t build_parse_tree(struct owl_default_tokenizer *, struct owl_token_run *, struct %%prefix_tree *);");
    output_line(out, "");
    output_line(out, "static struct %%prefix_tree *%%prefix_tree_create_empty(void) {");
    output_line(out, "    return calloc(1, sizeof(struct %%prefix_tree));");
    output_line(out, "}");
    output_line(out, "");
    output_line(out, "static void free_token_runs(struct owl_token_run **run) {");
    output_line(out, "    while (*run) {");
    output_line(out, "        struct owl_token_run *prev = (*run)->prev;");
    output_line(out, "        free(*run);");
    output_line(out, "        *run = prev;");
    output_line(out, "    }");
    output_line(out, "}");
    output_line(out, "static void parse_string(struct %%prefix_tree *tree, const char *string) {");
    output_line(out, "    tree->string = string;");
    output_line(out, "    tree->next_offset = 1;");
    output_line(out, "    struct owl_default_tokenizer tokenizer = {");
    output_line(out, "        .text = string,");
    output_line(out, "        .info = tree,");
    output_line(out, "    };");
    output_line(out, "    struct owl_token_run *token_run = 0;");
    output_line(out, "    struct fill_run_continuation c = {");
    output_line(out, "        .capacity = 8,");
    output_line(out, "        .top_index = 0,");
    output_line(out, "    };");
    output_line(out, "    c.stack = calloc(c.capacity, sizeof(struct fill_run_state));");
    output_line(out, "    c.stack[0].state = %%start-state;");
    output_line(out, "    c.stack[0].cont = &c;");
    output_line(out, "    uint16_t failing_index = 0;");
    output_line(out, "    while (owl_default_tokenizer_advance(&tokenizer, &token_run)) {");
    output_line(out, "        if (!fill_run_states(token_run, &c, &failing_index)) {");
    output_line(out, "            free(c.stack);");
    output_line(out, "            tree->error = ERROR_UNEXPECTED_TOKEN;");
    output_line(out, "            find_token_range(&tokenizer, token_run, failing_index, &tree->error_range.start, &tree->error_range.end);");
    output_line(out, "            free_token_runs(&token_run);");
    output_line(out, "            return;");
    output_line(out, "        }");
    output_line(out, "    }");
    output_line(out, "    struct fill_run_state top = c.stack[c.top_index];");
    output_line(out, "    free(c.stack);");
    output_line(out, "    if (string[tokenizer.offset] != '\\0') {");
    output_line(out, "        tree->error = ERROR_INVALID_TOKEN;");
    output_line(out, "        estimate_next_token_range(&tokenizer, &tree->error_range.start, &tree->error_range.end);");
    output_line(out, "        free_token_runs(&token_run);");
    output_line(out, "        return;");
    output_line(out, "    }");
    output_line(out, "    switch (top.state) {");
    for (state_id i = 0; i < gen->deterministic->automaton.number_of_states; ++i) {
        if (!gen->deterministic->automaton.states[i].accepting)
            continue;
        set_unsigned_number_substitution(out, "state-id", i);
        output_line(out, "    case %%state-id:");
    }
    output_line(out, "        break;");
    output_line(out, "    default:");
    output_line(out, "        tree->error = ERROR_MORE_INPUT_NEEDED;");
    output_line(out, "        find_end_range(&tokenizer, &tree->error_range.start, &tree->error_range.end);");
    output_line(out, "        free_token_runs(&token_run);");
    output_line(out, "        return;");
    output_line(out, "    }");
    /*
    output_line(out, "    struct owl_token_run *run_to_print = token_run;");
    output_line(out, "    while (run_to_print) {");
    output_line(out, "        for (uint32_t i = 0; i < run_to_print->number_of_tokens; ++i) {");
    output_line(out, "            printf(\"%u -> %u\\n\", run_to_print->tokens[i], run_to_print->states[i]);");
    output_line(out, "        }");
    output_line(out, "        printf(\"--\\n\");");
    output_line(out, "        run_to_print = run_to_print->prev;");
    output_line(out, "    }");
     */
    output_line(out, "    tree->root_offset = build_parse_tree(&tokenizer, token_run, tree);");
    output_line(out, "}");
    output_line(out, "static struct %%prefix_tree *%%prefix_tree_create_with_error(enum %%prefix_error e) {");
    output_line(out, "    struct %%prefix_tree *tree = %%prefix_tree_create_empty();");
    output_line(out, "    tree->error = e;");
    output_line(out, "    return tree;");
    output_line(out, "}");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_from_string(const char *string) {");
    output_line(out, "    return %%prefix_tree_create_with_options((struct %%prefix_tree_options){ .string = string });");
    output_line(out, "}");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_from_file(FILE *file) {");
    output_line(out, "    if (!file)");
    output_line(out, "        return %%prefix_tree_create_with_error(ERROR_INVALID_FILE);");
    output_line(out, "    return %%prefix_tree_create_with_options((struct %%prefix_tree_options){ .file = file });");
    output_line(out, "}");
    output_line(out, "struct %%prefix_tree *%%prefix_tree_create_with_options(struct %%prefix_tree_options options) {");
    output_line(out, "    if (!options.file == !options.string)");
    output_line(out, "        return %%prefix_tree_create_with_error(ERROR_INVALID_OPTIONS);");
    output_line(out, "    if (options.file) {");
    output_line(out, "        char *str = 0;");
    output_line(out, "        size_t len = 32;");
    output_line(out, "        size_t off = 0;");
    output_line(out, "        while (true) {");
    output_line(out, "            len = len * 3 / 2;");
    output_line(out, "            char *s = realloc(str, len);");
    output_line(out, "            if (!s) {");
    output_line(out, "                free(str);");
    output_line(out, "                return 0;");
    output_line(out, "            }");
    output_line(out, "            str = s;");
    output_line(out, "            off += fread(str + off, 1, len - off, options.file);");
    output_line(out, "            if (off < len) {");
    output_line(out, "                str[off] = '\\0';");
    output_line(out, "                break;");
    output_line(out, "            }");
    output_line(out, "        }");
    output_line(out, "        options.string = str;");
    output_line(out, "    }");
    output_line(out, "    struct %%prefix_tree *tree = %%prefix_tree_create_empty();");
    output_line(out, "    if (options.file)");
    output_line(out, "        tree->owns_string = true;");
    if (has_custom_tokens) {
        output_line(out, "    tree->custom_tokenize = options.tokenize;");
        output_line(out, "    tree->custom_tokenize_info = options.tokenize_info;");
    }
    output_line(out, "    parse_string(tree, options.string);");
    output_line(out, "    return tree;");
    output_line(out, "}");
    output_line(out, "enum %%prefix_error %%prefix_tree_get_error(struct %%prefix_tree *tree, struct source_range *error_range) {");
    output_line(out, "    if (error_range)");
    output_line(out, "        *error_range = tree->error_range;");
    output_line(out, "    return tree->error;");
    output_line(out, "}");
    output_line(out, "void %%prefix_tree_destroy(struct %%prefix_tree *tree) {");
    output_line(out, "    if (!tree)");
    output_line(out, "        return;");
    output_line(out, "    if (tree->owns_string)");
    output_line(out, "        free((void *)tree->string);");
    output_line(out, "    free(tree->parse_tree);");
    output_line(out, "    free(tree);");
    output_line(out, "}");
    output_line(out, "static bool fill_run_states(struct owl_token_run *run, struct fill_run_continuation *cont, uint16_t *failing_index) {");
    output_line(out, "    uint16_t token_index = 0;");
    output_line(out, "    uint16_t number_of_tokens = run->number_of_tokens;");
    output_line(out, "    while (token_index < number_of_tokens) {");
    output_line(out, "        struct fill_run_state *top = &cont->stack[cont->top_index];");
    output_line(out, "        run->states[token_index] = top->state;");
    output_line(out, "        state_funcs[top->state](run, top, token_index);");
    output_line(out, "        if (cont->error) {");
    output_line(out, "            *failing_index = token_index - (cont->error > 0 ? 0 : 1);");
    output_line(out, "            return false;");
    output_line(out, "        }");
    output_line(out, "        token_index++;");
    output_line(out, "    }");
    output_line(out, "    return true;");
    output_line(out, "}");
    generate_action_table(gen, out);
    output_line(out, "static size_t read_whitespace(const char *text, void *info) {");
    struct generated_token *tokens = malloc(sizeof(struct generated_token) *
     gen->grammar->number_of_whitespace_tokens);
    if (!tokens) {
        fputs("critical error: out of memory\n", stderr);
        exit(-1);
    }
    for (uint32_t i = 0; i < gen->grammar->number_of_whitespace_tokens; ++i)
        tokens[i].token = gen->grammar->whitespace_tokens[i];
    generate_keyword_reader(tokens, gen->grammar->number_of_whitespace_tokens,
     out);
    free(tokens);
    output_line(out, "}");
    output_line(out, "static size_t read_keyword_token(%%token-type *token, bool *end_token, const char *text, void *info) {");
    uint32_t number_of_tokens = gen->combined->number_of_keyword_tokens +
     gen->grammar->number_of_comment_tokens;
    tokens = malloc(sizeof(struct generated_token) * (size_t)number_of_tokens);
    if (!tokens) {
        fputs("critical error: out of memory\n", stderr);
        exit(-1);
    }
    uint32_t i = 0;
    for (; i < gen->combined->number_of_keyword_tokens; ++i)
        tokens[i].token = gen->combined->tokens[i];
    for (uint32_t j = 0; j < gen->grammar->number_of_comment_tokens; ++j)
        tokens[i + j].token = gen->grammar->comment_tokens[j];
    qsort(tokens, number_of_tokens, sizeof(struct generated_token),
     compare_tokens);
    generate_keyword_reader(tokens, number_of_tokens, out);
    free(tokens);
    output_line(out, "}");
    output_line(out, "static uint32_t rule_lookup(uint32_t parent, uint32_t slot, void *context) {");
    output_line(out, "    switch (parent) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (rule->number_of_slots == 0)
            continue;
        set_unsigned_number_substitution(out, "rule-index", i);
        output_line(out, "    case %%rule-index:");
        output_line(out, "        switch (slot) {");
        for (uint32_t j = 0; j < rule->number_of_slots; ++j) {
            set_unsigned_number_substitution(out, "slot-index", j);
            set_unsigned_number_substitution(out, "slot-rule-index",
             rule->slots[j].rule_index);
            output_line(out, "        case %%slot-index: return %%slot-rule-index;");
        }
        output_line(out, "        default: break;");
        output_line(out, "        }");
        output_line(out, "        break;");
    }
    output_line(out, "    default: break;");
    output_line(out, "    }");
    output_line(out, "    return UINT32_MAX;");
    output_line(out, "}");
    output_line(out, "static void fixity_associativity_precedence_lookup(int *fixity_associativity, int *precedence, uint32_t rule, uint32_t choice, void *context) {");
    output_line(out, "    switch (rule) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        if (rule->first_operator_choice == rule->number_of_choices)
            continue;
        set_unsigned_number_substitution(out, "rule-index", i);
        output_line(out, "    case %%rule-index:");
        output_line(out, "        switch (choice) {");
        for (uint32_t j = rule->first_operator_choice;
         j < rule->number_of_choices; ++j) {
            struct choice op = rule->choices[j];
            set_unsigned_number_substitution(out, "choice-index", j);
            set_signed_number_substitution(out, "operator-precedence",
             op.precedence);
            output_line(out, "        case %%choice-index:");
            output_line(out, "            *precedence = %%operator-precedence;");
            if (op.fixity == PREFIX)
                output_line(out, "            *fixity_associativity = CONSTRUCT_PREFIX;");
            else if (op.fixity == POSTFIX)
                output_line(out, "            *fixity_associativity = CONSTRUCT_POSTFIX;");
            else if (op.associativity == RIGHT)
                output_line(out, "            *fixity_associativity = CONSTRUCT_INFIX_RIGHT;");
            else if (op.associativity == FLAT)
                output_line(out, "            *fixity_associativity = CONSTRUCT_INFIX_FLAT;");
            else
                output_line(out, "            *fixity_associativity = CONSTRUCT_INFIX_LEFT;");
            output_line(out, "            return;");
        }
        output_line(out, "        default: return;");
        output_line(out, "        }");
    }
    output_line(out, "    default: return;");
    output_line(out, "    }");
    output_line(out, "}");
    output_line(out, "static size_t number_of_slots_lookup(uint32_t rule, void *context) {");
    output_line(out, "    switch (rule) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        set_unsigned_number_substitution(out, "rule-index", i);
        set_unsigned_number_substitution(out, "number-of-slots",
         gen->grammar->rules[i]->number_of_slots);
        output_line(out, "    case %%rule-index: return %%number-of-slots;");
    }
    output_line(out, "    default: return 0;");
    output_line(out, "    }");
    output_line(out, "}");
    output_line(out, "static void left_right_operand_slots_lookup(uint32_t rule, uint32_t *left, uint32_t *right, uint32_t *operand, void *context) {");
    output_line(out, "    switch (rule) {");
    for (uint32_t i = 0; i < gen->grammar->number_of_rules; ++i) {
        struct rule *rule = gen->grammar->rules[i];
        set_unsigned_number_substitution(out, "rule-index", i);
        set_unsigned_number_substitution(out, "left-slot",
         rule->left_slot_index);
        set_unsigned_number_substitution(out, "right-slot",
         rule->right_slot_index);
        set_unsigned_number_substitution(out, "operand-slot",
         rule->operand_slot_index);
        output_line(out, "    case %%rule-index:");
        output_line(out, "        *left = %%left-slot;");
        output_line(out, "        *right = %%right-slot;");
        output_line(out, "        *operand = %%operand-slot;");
        output_line(out, "        break;");
    }
    output_line(out, "    }");
    output_line(out, "}");
    output_line(out, "#endif");
    output_line(out, "");
    output_destroy(out);
}

static void generate_keyword(struct generator_output *out, struct token keyword,
 size_t indentation)
{
    if (keyword.length > UINT32_MAX)
        abort();
    set_unsigned_number_substitution(out, "token-length",
     (uint32_t)keyword.length);
    if (keyword.type != TOKEN_WHITESPACE) {
        output_indentation(out, indentation);
        if (keyword.type == TOKEN_END)
            output_line(out, "*end_token = true;");
        else
            output_line(out, "*end_token = false;");
        output_indentation(out, indentation);
        if (keyword.type == TOKEN_START_LINE_COMMENT)
            output_line(out, "*token = %%comment-token;");
        else {
            set_unsigned_number_substitution(out, "token-index",
             keyword.symbol);
            output_line(out, "*token = %%token-index;");
        }
    }
    output_indentation(out, indentation);
    output_line(out, "return %%token-length;");
}

static void generate_keyword_reader(struct generated_token *tokens,
 uint32_t number_of_tokens, struct generator_output *out) {
    output_line(out, "    switch (text[0]) {");
    uint32_t n = number_of_tokens;
    size_t shared_length = 0;
    struct generated_token *prefix = 0;
    for (uint32_t i = 0; i < n; ++i) {
        struct generated_token *token = &tokens[i];
        struct token keyword = token->token;
        struct generated_token *next = 0;
        size_t next_length = 0;
        if (i + 1 < n) {
            next = &tokens[i + 1];
            next_length = next->token.length;
        }
        size_t shared = 0;
        for (; shared < keyword.length && shared < next_length; shared++) {
            if (keyword.string[shared] != next->token.string[shared])
                break;
        }
        while (shared > shared_length) {
            set_unsigned_number_substitution(out, "character",
             keyword.string[shared_length]);
            output_indentation(out, shared_length + 1);
            output_line(out, "case %%character:");
            shared_length++;
            if (shared_length > UINT32_MAX)
                abort();
            set_unsigned_number_substitution(out, "index",
             (uint32_t)shared_length);
            output_indentation(out, shared_length + 1);
            output_line(out, "switch (text[%%index]) {");
        }
        token->prefix = prefix;
        if (shared == keyword.length) {
            prefix = token;
            continue;
        }
        set_unsigned_number_substitution(out, "character",
         keyword.string[shared_length]);
        output_indentation(out, shared_length + 1);
        output_line(out, "case %%character:");
        if (keyword.length <= shared_length + 1)
            generate_keyword(out, keyword, shared_length + 2);
        else {
            if (shared_length + 1 > UINT32_MAX)
                abort();
            set_unsigned_number_substitution(out, "offset",
             (uint32_t)(shared_length + 1));
            output_indentation(out, shared_length + 2);
            output_string(out, "if (");
            for (size_t j = shared_length + 1; j < keyword.length; ++j) {
                set_unsigned_number_substitution(out, "character",
                 keyword.string[j]);
                set_unsigned_number_substitution(out, "index", (uint32_t)j);
                output_string(out, "text[%%index] == %%character");
                if (j + 1 < keyword.length)
                    output_string(out, " && ");
            }
            if (keyword.length - shared_length - 1 > UINT32_MAX)
                abort();
            output_line(out, ") {");
            generate_keyword(out, keyword, shared_length + 3);
            output_indentation(out, shared_length + 2);
            output_line(out, "} else {");
            if (prefix)
                generate_keyword(out, prefix->token, shared_length + 3);
            else {
                output_indentation(out, shared_length + 3);
                output_line(out, "return 0;");
            }
            output_indentation(out, shared_length + 2);
            output_line(out, "}");
        }
        while (shared_length > shared) {
            output_indentation(out, shared_length + 1);
            output_line(out, "default:");
            if (prefix) {
                generate_keyword(out, prefix->token, shared_length + 2);
                if (prefix->token.length >= shared_length)
                    prefix = prefix->prefix;
            } else {
                output_indentation(out, shared_length + 2);
                output_line(out, "return 0;");
            }
            output_indentation(out, shared_length + 1);
            output_line(out, "}");
            shared_length--;
        }
    }
    output_line(out, "    default:");
    output_line(out, "        return 0;");
    output_line(out, "    }");
}

static void generate_reachability_mask_check(struct generator *gen,
 struct generator_output *out, struct bitset *r)
{
    uint32_t mask_width = reachability_mask_width(gen);
    if (mask_width == 0)
        return;
    // Check if any of the end states we're expecting are still
    // reachable.
    output_string(out, "    if (");
    for (uint32_t i = 0; i < mask_width; ++i) {
        if (i > 0)
            output_string(out, " && ");
        set_unsigned_number_substitution(out, "mask-index", i);
        uint64_t bits = r->bit_groups[i / 2];
        if (i % 2)
            bits >>= 32;
        else
            bits &= UINT32_MAX;
        set_unsigned_number_substitution(out, "mask-bits", (uint32_t)bits);
        output_string(out, "!(%%mask-bits & top->reachability_mask[%%mask-index])");
    }
    output_line(out, ") {");
    output_line(out, "        top->cont->error = -1;");
    output_line(out, "        return;");
    output_line(out, "    }");
}

struct action_table_bucket_group {
    uint32_t index;
    uint32_t length;
};

struct action_table_bucket {
    struct action_table_bucket *next;
    // These include offsets for bracket states.
    state_id target_nfa_state;
    state_id dfa_state;
    state_id nfa_state;
    symbol_id dfa_symbol;
    // For bracket transitions; this is the state to push on the stack.
    state_id push_nfa_state;
    uint32_t action_index;
    uint32_t table_index;
};

static int compare_action_table_bucket_groups(const void *aa, const void *bb)
{
    const struct action_table_bucket_group *a = aa;
    const struct action_table_bucket_group *b = bb;
    // Reverse-sort by length.
    if (a->length > b->length)
        return -1;
    if (a->length < b->length)
        return 1;
    return 0;
}

// 0xe5aa55e5 is just an arbitrary 32-bit prime number.
#define ACTION_TABLE_ENTRY_HASH(target_nfa_state, dfa_state, dfa_symbol) \
 ((((((0xe5aa55e5 ^ (target_nfa_state)) * 0xe5aa55e5) ^ (dfa_state)) * \
 0xe5aa55e5) ^ (dfa_symbol)) * 0xe5aa55e5)

static uint32_t log2u(uint32_t n)
{
    uint32_t b = 0;
    for (; n > 0; n >>= 1, b++);
    return b;
}

struct bit_range {
    uint8_t start_byte;
    uint8_t start_bit;
    uint8_t end_byte;
    uint8_t end_bit;
};

// sB=0 sb=2
// eB=2 eb=1
// 00111111 1

static struct bit_range next_bit_range(struct bit_range r, uint8_t bits)
{
    struct bit_range s;
    s.start_byte = r.end_byte;
    s.start_bit = r.end_bit;
    if (r.end_bit == 8) {
        s.start_byte++;
        s.start_bit = 0;
    }
    uint8_t end_bit = 8 * s.start_byte + s.start_bit + bits;
    if (end_bit == 0)
        abort();
    s.end_byte = (end_bit - 1) / 8;
    s.end_bit = (end_bit + 7) % 8 + 1;
    return s;
}

static void set_bit_range(uint8_t *bytes, struct bit_range r, uint32_t val)
{
    for (uint8_t i = r.start_byte; i <= r.end_byte; ++i) {
        int32_t offset = 8 * (int32_t)(i - r.start_byte) - r.start_bit;
        int32_t end = 8 * (int32_t)(i - r.end_byte + 1) - r.end_bit;
        uint32_t v = offset < 0 ? val << -offset : val >> offset;
        if (end > 0)
            v &= 0xff >> end;
        bytes[i] |= v;
    }
}

static void encode_bit_range(struct generator_output *out, struct bit_range r,
 const char *variable)
{
    set_literal_substitution(out, "bits-variable", variable);
    for (uint8_t i = r.start_byte; i <= r.end_byte; ++i) {
        int32_t offset = 8 * (int32_t)(i - r.start_byte) - r.start_bit;
        int32_t end = 8 * (int32_t)(i - r.end_byte + 1) - r.end_bit;
        set_unsigned_number_substitution(out, "byte-index", i);
        output_string(out, "    key.bytes[%%byte-index] |= ");
        if (offset < 0) {
            set_unsigned_number_substitution(out, "shift-amount", -offset);
            output_string(out, "(%%bits-variable << %%shift-amount)");
        } else {
            set_unsigned_number_substitution(out, "shift-amount", offset);
            output_string(out, "(%%bits-variable >> %%shift-amount)");
        }
        uint32_t mask = 0xff;
        if (end > 0)
            mask >>= end;
        set_unsigned_number_substitution(out, "mask", mask);
        output_line(out, " & %%mask;");
    }
}

static void decode_bit_range(struct generator_output *out, struct bit_range r,
 const char *variable)
{
    set_literal_substitution(out, "bits-variable", variable);
    for (uint8_t i = r.start_byte; i <= r.end_byte; ++i) {
        int32_t offset = 8 * (int32_t)(i - r.start_byte) - r.start_bit;
        int32_t end = 8 * (int32_t)(i - r.end_byte + 1) - r.end_bit;
        set_unsigned_number_substitution(out, "byte-index", i);
        uint8_t mask = 0xff;
        if (end > 0)
            mask >>= end;
        set_unsigned_number_substitution(out, "mask", mask);
        output_string(out, "    %%bits-variable |= ((uint32_t)bytes[%%byte-index] & %%mask)");
        if (offset < 0) {
            set_unsigned_number_substitution(out, "shift-amount", -offset);
            output_line(out, " >> %%shift-amount;");
        } else {
            set_unsigned_number_substitution(out, "shift-amount", offset);
            output_line(out, " << %%shift-amount;");
        }
    }
}

static void generate_action_table(struct generator *gen,
 struct generator_output *out)
{
    struct deterministic_grammar *d = gen->deterministic;

    // Collect all the "groups" of action map entries with the same
    // target_nfa_state.  Sort the groups by size, longest to shortest.
    struct action_table_bucket *buckets =
     calloc(d->action_map.number_of_entries +
     d->bracket_action_map.number_of_entries,
     sizeof(struct action_table_bucket));
    struct action_table_bucket_group *groups = 0;
    state_id max_nfa_state = 0;
    uint32_t groups_allocated_bytes = 0;
    uint32_t number_of_groups = 0;
    for (int i = 0; i < 2; ++i) {
        struct action_map *map = i == 0 ? &d->action_map :
         &d->bracket_action_map;
        uint32_t offset = i == 0 ? 0 : d->action_map.number_of_entries;
        uint32_t nfa_state_offset = i == 0 ? 0 :
         gen->combined->automaton.number_of_states;
        for (uint32_t j = 0; j < map->number_of_entries;) {
            uint32_t length = 0;
            for (; j + length < map->number_of_entries; ++length) {
                struct action_table_bucket *b = &buckets[offset + j + length];
                struct action_map_entry e = map->entries[j + length];
                *b = (struct action_table_bucket){
                    .target_nfa_state = e.target_nfa_state + nfa_state_offset,
                    .dfa_state = e.dfa_state + (i==0 ? 0 :
                     d->automaton.number_of_states),
                    .nfa_state = e.nfa_state + nfa_state_offset,
                    .dfa_symbol = e.dfa_symbol,
                    .action_index = (uint32_t)(e.actions - d->actions),
                };
                if (e.dfa_symbol >= gen->combined->number_of_tokens) {
                    // This is a bracket transition.  Find the corresponding
                    // accepting state and store it in the table.
                    struct automaton bracket = gen->combined->bracket_automaton;
                    for (state_id i = 0; i < bracket.number_of_states; ++i) {
                        struct state s = bracket.states[i];
                        if (!s.accepting || s.transition_symbol != e.nfa_symbol)
                            continue;
                        b->push_nfa_state = b->nfa_state;
                        b->nfa_state = i +
                         gen->combined->automaton.number_of_states;
                        break;
                    }
                }
                if (b->target_nfa_state > max_nfa_state)
                    max_nfa_state = b->target_nfa_state;
                if (b->nfa_state > max_nfa_state)
                    max_nfa_state = b->nfa_state;
                if (map->entries[j].target_nfa_state !=
                 map->entries[j + length].target_nfa_state)
                    break;
            }
            if (number_of_groups == UINT32_MAX)
                abort();
            groups = grow_array(groups, &groups_allocated_bytes,
             (number_of_groups + 1) * sizeof(struct action_table_bucket_group));
            groups[number_of_groups++] = (struct action_table_bucket_group){
                .index = offset + j,
                .length = length,
            };
            j += length;
        }
    }
    qsort(groups, number_of_groups, sizeof(struct action_table_bucket_group),
     compare_action_table_bucket_groups);

    // Size the table to a power of two.
    uint32_t table_size_bits = 7;
    uint32_t table_size = 1 << table_size_bits;
    uint32_t total_entries = d->action_map.number_of_entries +
     d->bracket_action_map.number_of_entries;
    uint32_t divisor = 1;
    // FIXME: Figure out the actual math for the size of this table.
    while (table_size <= total_entries / divisor) {
        table_size *= 2;
        table_size_bits += 1;
        divisor += 1;
    }
    uint32_t table_mask = table_size - 1;
    // Use the top bits of the hash for a secondary hash value.
    uint32_t hash2_shift = 32 - table_size_bits;

    // This array maps old NFA states to new NFA states.
    state_id *nfa_states = malloc((max_nfa_state + 1) * sizeof(state_id));
    for (state_id i = 0; i < max_nfa_state + 1; ++i)
        nfa_states[i] = i;

    struct action_table_bucket **table_buckets = calloc(table_size,
     sizeof(struct action_table_bucket *));
    uint32_t *bucket_sizes = calloc(table_size, sizeof(uint32_t));
    uint32_t nfa_state = max_nfa_state + 1;
    uint32_t saved_nfa_state = 0;
    // Start at the minimum possible bucket limit.
    uint32_t bucket_limit = (total_entries + table_size - 1) / table_size;
    // How many times should we try to randomize indexes?
#define MAX_TRIES 1000
    uint32_t tries_left = MAX_TRIES;
    for (uint32_t i = 0; i < number_of_groups; ++i) {
        struct action_table_bucket_group group = groups[i];
        while (true) {
            if (nfa_state == UINT32_MAX)
                abort();
            uint32_t j = 0;
            for (; j < group.length; ++j) {
                struct action_table_bucket *bucket = &buckets[group.index + j];
                uint32_t h = ACTION_TABLE_ENTRY_HASH(nfa_state,
                 bucket->dfa_state, bucket->dfa_symbol);
                uint32_t k1 = h & table_mask;
                uint32_t k2 = h >> hash2_shift;
                uint32_t k = bucket_sizes[k1] <= bucket_sizes[k2] ? k1 : k2;
                bucket->table_index = k;
                bucket->next = table_buckets[k];
                table_buckets[k] = bucket;
                if (bucket_sizes[k]++ >= bucket_limit)
                    goto retry;
            }
            nfa_states[buckets[group.index].target_nfa_state] = nfa_state;
            nfa_state++;
            saved_nfa_state = nfa_state;
            break;
retry:
            // Roll back changes and try a new nfa_state.
            for (; j < group.length; --j) {
                struct action_table_bucket *bucket = &buckets[group.index + j];
                uint32_t k = bucket->table_index;
                bucket_sizes[k]--;
                table_buckets[k] = table_buckets[k]->next;
            }
            nfa_state++;
            if (tries_left-- == 0) {
                // If we ran out of tries, give ourselves some more room and
                // keep going.
                bucket_limit += 1;
                tries_left = MAX_TRIES;
                nfa_state = saved_nfa_state;
            }
        }
    }
    free(groups);
    const int actions_per_line = 30;
    output_line(out, "static const uint16_t actions[] = {");
    for (uint32_t i = 0; i < d->number_of_actions; ++i) {
        set_unsigned_number_substitution(out, "action", d->actions[i]);
        output_string(out, "%%action,");
        if ((i + 1) % actions_per_line == 0)
            output_line(out, "");
    }
    output_line(out, "};");
    uint32_t nfa_state_bits = log2u(nfa_state);
    uint32_t dfa_state_bits = log2u(d->automaton.number_of_states +
     d->bracket_automaton.number_of_states + 1);
    uint32_t dfa_symbol_bits;
    if (d->automaton.number_of_symbols > d->bracket_automaton.number_of_symbols)
        dfa_symbol_bits = log2u(d->automaton.number_of_symbols + 1);
    else
        dfa_symbol_bits = log2u(d->bracket_automaton.number_of_symbols + 1);
    uint32_t action_bits = log2u(d->number_of_actions);
    uint32_t key_bits = nfa_state_bits + dfa_state_bits + dfa_symbol_bits;
    uint32_t key_bytes = (key_bits + 7) / 8;
    uint32_t value_bits = nfa_state_bits * 2 + action_bits;
    uint32_t value_bytes = (value_bits + 7) / 8;
    struct bit_range target_nfa_state_range = next_bit_range((struct bit_range){0}, nfa_state_bits);
    struct bit_range dfa_state_range = next_bit_range(target_nfa_state_range, dfa_state_bits);
    struct bit_range dfa_symbol_range = next_bit_range(dfa_state_range, dfa_symbol_bits);
    struct bit_range nfa_state_range = next_bit_range((struct bit_range){.end_byte=key_bytes}, nfa_state_bits);
    struct bit_range action_range = next_bit_range(nfa_state_range, action_bits);
    struct bit_range push_nfa_state_range = next_bit_range(action_range, nfa_state_bits);
    set_unsigned_number_substitution(out, "entry-bytes", key_bytes + value_bytes);
    set_unsigned_number_substitution(out, "table-size", table_size);
    set_unsigned_number_substitution(out, "bucket-limit", bucket_limit);
    output_line(out, "static const uint8_t action_table[%%table-size][%%bucket-limit][%%entry-bytes] = {");
    const int entries_per_line = 6;
    int next_newline = entries_per_line;
    uint8_t *bytes = malloc(key_bytes + value_bytes);
    for (uint32_t i = 0; i < table_size; ++i) {
        output_string(out, "{");
        struct action_table_bucket *bucket = table_buckets[i];
        if (!bucket)
            output_string(out, "{0}");
        for (; bucket; bucket = bucket->next) {
            output_string(out, "{");
            memset(bytes, 0, key_bytes + value_bytes);
            set_bit_range(bytes, target_nfa_state_range, nfa_states[bucket->target_nfa_state]);
            set_bit_range(bytes, dfa_state_range, bucket->dfa_state);
            set_bit_range(bytes, dfa_symbol_range, bucket->dfa_symbol);
            set_bit_range(bytes, nfa_state_range, nfa_states[bucket->nfa_state]);
            set_bit_range(bytes, action_range, bucket->action_index);
            if (bucket->dfa_symbol >= gen->combined->number_of_tokens)
                set_bit_range(bytes, push_nfa_state_range, nfa_states[bucket->push_nfa_state]);
            for (uint32_t i = 0; i < key_bytes + value_bytes; ++i) {
                set_unsigned_number_substitution(out, "byte", bytes[i]);
                output_string(out, "%%byte,");
            }
            output_string(out, "},");
            if (--next_newline == 0 && bucket->next) {
                output_line(out, "");
                next_newline = entries_per_line;
            }
        }
        output_string(out, "},");
        if (next_newline == 0) {
            output_line(out, "");
            next_newline = entries_per_line;
        }
    }
    free(bytes);
    output_line(out, "};");
    output_line(out, "");
    set_unsigned_number_substitution(out, "key-bytes", key_bytes);
    output_line(out, "struct action_table_key {");
    output_line(out, "    uint8_t bytes[%%key-bytes];");
    output_line(out, "};");
    output_line(out, "static inline struct action_table_key encode_key(%%state-type target_nfa_state, %%state-type dfa_state, %%token-type dfa_symbol) {");
    output_line(out, "    struct action_table_key key = {{0}};");
    encode_bit_range(out, target_nfa_state_range, "target_nfa_state");
    encode_bit_range(out, dfa_state_range, "dfa_state");
    encode_bit_range(out, dfa_symbol_range, "dfa_symbol");
    output_line(out, "    return key;");
    output_line(out, "}");
    output_line(out, "struct action_table_entry {");
    output_line(out, "    %%state-type nfa_state;");
    output_line(out, "    uint32_t actions;");
    output_line(out, "    %%state-type push_nfa_state;");
    output_line(out, "};");
    output_line(out, "static struct action_table_entry decode_entry(const uint8_t *bytes) {");
    output_line(out, "    struct action_table_entry entry = {0};");
    decode_bit_range(out, nfa_state_range, "entry.nfa_state");
    decode_bit_range(out, action_range, "entry.actions");
    decode_bit_range(out, push_nfa_state_range, "entry.push_nfa_state");
    output_line(out, "    return entry;");
    output_line(out, "}");
    output_line(out, "static struct action_table_entry action_table_lookup(%%state-type nfa_state, %%state-type dfa_state, %%token-type token) {");
    set_literal_substitution(out, "action-table-entry-hash", STRINGIFY(ACTION_TABLE_ENTRY_HASH(nfa_state, dfa_state, token)));
    set_unsigned_number_substitution(out, "action-table-mask", table_mask);
    set_unsigned_number_substitution(out, "action-table-hash2-shift", hash2_shift);
    output_line(out, "    uint32_t hash = %%action-table-entry-hash;");
    output_line(out, "    uint32_t index1 = hash & %%action-table-mask;");
    output_line(out, "    uint32_t index2 = hash >> %%action-table-hash2-shift;");
    output_line(out, "    struct action_table_key key = encode_key(nfa_state, dfa_state, token);");
    output_line(out, "    uint32_t j = 0;");
    output_line(out, "    const uint8_t *entry = 0;");
    output_line(out, "    for (; j < %%bucket-limit; ++j) {");
    output_line(out, "        entry = action_table[index1][j];");
    output_line(out, "        if (!memcmp(key.bytes, entry, sizeof(key.bytes)))");
    output_line(out, "            break;");
    output_line(out, "        entry = action_table[index2][j];");
    output_line(out, "        if (!memcmp(key.bytes, entry, sizeof(key.bytes)))");
    output_line(out, "            break;");
    output_line(out, "    }");
    output_line(out, "    if (j >= %%bucket-limit)");
    output_line(out, "        abort();");
    output_line(out, "    return decode_entry(entry);");
    output_line(out, "}");
    output_line(out, "static void apply_actions(struct construct_state *state, uint32_t index, size_t start, size_t end) {");
    output_line(out, "    size_t offset = end;");
    output_line(out, "    uint32_t i;");
    output_line(out, "    for (i = index; actions[i]; ++i) {");
    set_literal_substitution(out, "is-end-action", STRINGIFY(CONSTRUCT_IS_END_ACTION(actions[i])));
    output_line(out, "        if (%%is-end-action)");
    output_line(out, "            offset = start;");
    output_line(out, "        construct_action_apply(state, actions[i], offset);");
    output_line(out, "    }");
    output_line(out, "}");
    output_line(out, "static size_t build_parse_tree(struct owl_default_tokenizer *tokenizer, struct owl_token_run *run, struct %%prefix_tree *tree) {");
    output_line(out, "    struct construct_state construct_state = { .info = tree };");
    output_line(out, "    %%state-type *state_stack = 0;");
    output_line(out, "    uint32_t stack_depth = 0;");
    output_line(out, "    size_t stack_capacity = 0;");
    output_line(out, "    size_t whitespace = tokenizer->whitespace;");
    output_line(out, "    size_t offset = tokenizer->offset - whitespace;");
    if (gen->combined->root_rule_is_expression)
        output_line(out, "    construct_begin(&construct_state, offset, CONSTRUCT_EXPRESSION_ROOT);");
    else
        output_line(out, "    construct_begin(&construct_state, offset, CONSTRUCT_NORMAL_ROOT);");
    set_unsigned_number_substitution(out, "final-nfa-state",
     nfa_states[gen->combined->final_nfa_state]);
    output_line(out, "    %%state-type nfa_state = %%final-nfa-state;");
    output_line(out, "    while (run) {");
    output_line(out, "        uint16_t length_offset = run->lengths_size - 1;");
    output_line(out, "        uint16_t n = run->number_of_tokens;");
    output_line(out, "        uint16_t i;");
    output_line(out, "        for (i = n - 1; i < n; i--) {");
    output_line(out, "            size_t end = offset;");
    output_line(out, "            size_t len = 0;");
    output_line(out, "            struct action_table_entry entry = action_table_lookup(nfa_state, run->states[i], run->tokens[i]);");
    if (gen->combined->number_of_tokens > 0) {
        // avoid "warning: comparison of unsigned expression < 0 is always false"
        set_unsigned_number_substitution(out, "number-of-tokens",
         gen->combined->number_of_tokens);
        output_line(out, "            if (run->tokens[i] < %%number-of-tokens)");
    } else
        output_line(out, "            if (true)");
    output_line(out, "                len = decode_token_length(run, &length_offset, &offset);");
    output_line(out, "            else {");
    output_line(out, "                if (stack_depth >= stack_capacity) {");
    output_line(out, "                    size_t new_capacity = (stack_capacity + 2) * 3 / 2;");
    output_line(out, "                    if (new_capacity <= stack_capacity)");
    output_line(out, "                        abort();");
    output_line(out, "                    %%state-type *new_stack = realloc(state_stack, new_capacity * sizeof(%%state-type));");
    output_line(out, "                    if (!new_stack)");
    output_line(out, "                        abort();");
    output_line(out, "                    state_stack = new_stack;");
    output_line(out, "                    stack_capacity = new_capacity;");
    output_line(out, "                }");
    output_line(out, "                state_stack[stack_depth++] = entry.push_nfa_state;");
    output_line(out, "            }");
    output_line(out, "            apply_actions(&construct_state, entry.actions, end, end + whitespace);");
    set_unsigned_number_substitution(out, "bracket-start-state",
     gen->deterministic->bracket_automaton.start_state +
     gen->deterministic->automaton.number_of_states);
    output_line(out, "            if (run->states[i] == %%bracket-start-state) {");
    output_line(out, "                if (stack_depth == 0)");
    output_line(out, "                    abort();");
    output_line(out, "                nfa_state = state_stack[--stack_depth];");
    output_line(out, "            } else");
    output_line(out, "                nfa_state = entry.nfa_state;");
    output_line(out, "            whitespace = end - offset - len;");
    output_line(out, "        }");
    output_line(out, "        struct owl_token_run *old = run;");
    output_line(out, "        run = run->prev;");
    output_line(out, "        free(old);");
    output_line(out, "    }");
    output_line(out, "    struct action_table_entry entry = action_table_lookup(nfa_state, UINT32_MAX, UINT32_MAX);");
    output_line(out, "    apply_actions(&construct_state, entry.actions, offset, offset + whitespace);");
    output_line(out, "    free(state_stack);");
    output_line(out, "    free_token_runs(&run);");
    output_line(out, "    return construct_finish(&construct_state, offset);");
    output_line(out, "}");
    free(bucket_sizes);
    free(buckets);
    free(table_buckets);
    free(nfa_states);
}

static void output_indentation(struct generator_output *out, size_t indentation)
{
    for (int i = 0; i < indentation; ++i)
        output_string(out, "    ");
}

static int compare_tokens(const void *aa, const void *bb)
{
    const struct token *a = &((struct generated_token *)aa)->token;
    const struct token *b = &((struct generated_token *)bb)->token;
    size_t minlen = a->length < b->length ? a->length : b->length;
    int cmp = memcmp(a->string, b->string, minlen);
    if (cmp != 0)
        return cmp;
    if (a->length < b->length)
        return -1;
    if (a->length > b->length)
        return 1;
    return 0;
}
