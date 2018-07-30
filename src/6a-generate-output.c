#include "6a-generate-output.h"

#include "alloc.h"
#include "grow-array.h"
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

enum substitution_type {
    STRING,
    UNSIGNED_NUMBER,
    SIGNED_NUMBER,
};

struct substitution {
    // Zero-terminated.
    const char *variable;

    enum substitution_type type;
    uint32_t unsigned_number;
    int32_t signed_number;

    const char *value;
    size_t value_length;

    enum substitution_transform transform;
};

struct generator_output {
    void (*output)(const char *, size_t);

    struct substitution *substitutions;
    uint32_t substitutions_allocated_bytes;
    uint32_t number_of_substitutions;

    char *formatted_string;
    uint32_t formatted_string_allocated_bytes;
};

static void apply_transform(char *string, size_t length,
 enum substitution_transform transform);
static struct substitution *find_substitution(struct generator_output *output,
 const char *variable);

struct generator_output *output_create(void (*function)(const char *, size_t))
{
    struct generator_output *output = calloc(1, sizeof(*output));
    output->output = function;
    return output;
}

void output_destroy(struct generator_output *output)
{
    free(output->substitutions);
    free(output->formatted_string);
    free(output);
}

void output_string_length(struct generator_output *output, const char *string,
 size_t len)
{
    if (len == 0)
        return;
    size_t next_output_index = 0;
    char *format = output->formatted_string;
    uint32_t format_bytes = output->formatted_string_allocated_bytes;
    for (size_t i = 0; i < len - 1; ++i) {
        if (string[i] == '%' && string[i + 1] == '%') {
            struct substitution *s = find_substitution(output, string + i + 2);
            if (!s)
                abort();
            output->output(string + next_output_index, i - next_output_index);
            switch (s->type) {
            case UNSIGNED_NUMBER: {
                const char *f = "%u";
                if (s->unsigned_number > INT_MAX)
                    f = "%uU";
                int len = snprintf(0, 0, f, s->unsigned_number);
                format = grow_array(format, &format_bytes, (uint32_t)len + 1);
                snprintf(format, len + 1, f, s->unsigned_number);
                output->output(format, len);
                break;
            }
            case SIGNED_NUMBER: {
                int len = snprintf(0, 0, "%d", s->signed_number);
                format = grow_array(format, &format_bytes, (uint32_t)len + 1);
                snprintf(format, len + 1, "%d", s->signed_number);
                output->output(format, len);
                break;
            }
            case STRING:
                format = grow_array(format, &format_bytes, s->value_length);
                memcpy(format, s->value, s->value_length);
                apply_transform(format, s->value_length, s->transform);
                output->output(format, s->value_length);
                break;
            }
            next_output_index = i + 2 + strlen(s->variable);
        }
    }
    output->formatted_string = format;
    output->formatted_string_allocated_bytes = format_bytes;
    if (next_output_index < len)
        output->output(string + next_output_index, len - next_output_index);
}

void output_string(struct generator_output *output, const char *string)
{
    output_string_length(output, string, strlen(string));
}

void output_line(struct generator_output *output, const char *string)
{
    output_string(output, string);
    output_string(output, "\n");
}

void output_formatted_source(struct generator_output *output,
 const char *string)
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
                output_string(output, "    ");
            output_string_length(output, string + next_output_index,
             i + 1 - next_output_index);
            output_string(output, "\n");
            if (string[i + 1] == ' ')
                next_output_index = i + 2;
            else
                next_output_index = i + 1;
        }
        if (string[i] == '{' || string[i] == ':')
            indent++;
    }
    output_string_length(output, string + next_output_index,
     i - next_output_index);
}

static uint32_t create_substitution(struct generator_output *output,
 const char *variable)
{
    uint32_t index = 0;
    for (; index < output->number_of_substitutions; ++index) {
        if (strcmp(variable, output->substitutions[index].variable) == 0)
            break;
    }
    if (index >= output->number_of_substitutions) {
        output->number_of_substitutions = index + 1;
        if (index == UINT32_MAX)
            abort();
        output->substitutions = grow_array(output->substitutions,
         &output->substitutions_allocated_bytes,
         output->number_of_substitutions * sizeof(struct substitution));
        output->substitutions[index].variable = variable;
    }
    return index;
}

void set_substitution(struct generator_output *output, const char *variable,
 const char *value, size_t value_length, enum substitution_transform transform)
{
    uint32_t index = create_substitution(output, variable);
    output->substitutions[index].type = STRING;
    output->substitutions[index].value = value;
    output->substitutions[index].value_length = value_length;
    output->substitutions[index].transform = transform;
}

void set_unsigned_number_substitution(struct generator_output *output,
 const char *variable, uint32_t value)
{
    uint32_t index = create_substitution(output, variable);
    output->substitutions[index].type = UNSIGNED_NUMBER;
    output->substitutions[index].unsigned_number = value;
}

void set_signed_number_substitution(struct generator_output *output,
 const char *variable, int32_t value)
{
    uint32_t index = create_substitution(output, variable);
    output->substitutions[index].type = SIGNED_NUMBER;
    output->substitutions[index].signed_number = value;
}

void set_literal_substitution(struct generator_output *output,
 const char *variable, const char *value)
{
    set_substitution(output, variable, value, strlen(value), NO_TRANSFORM);
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

static struct substitution *find_substitution(struct generator_output *output,
 const char *variable)
{
    uint32_t n = output->number_of_substitutions;
    size_t longest_match = 0;
    struct substitution *longest_match_substitution = 0;
    for (uint32_t i = 0; i < n; ++i) {
        struct substitution *s = &output->substitutions[i];
        for (size_t j = 0; ; ++j) {
            if (s->variable[j] == 0 && j > longest_match) {
                longest_match = j;
                longest_match_substitution = s;
            }
            if (!s->variable[j] || s->variable[j] != variable[j])
                break;
        }
    }
    return longest_match_substitution;
}
