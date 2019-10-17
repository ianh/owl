#ifndef GENERATE_OUTPUT_H
#define GENERATE_OUTPUT_H

#include <stddef.h>
#include <stdint.h>

struct generator_output;

struct generator_output *output_create(void (*output)(const char *, size_t));
void output_destroy(struct generator_output *output);

void output_string_length(struct generator_output *output, const char *string,
 size_t len);
void output_string(struct generator_output *output, const char *string);
void output_line(struct generator_output *output, const char *string);

void output_formatted_source(struct generator_output *output,
 const char *string);

enum substitution_transform {
    NO_TRANSFORM,
    LOWERCASE_WITH_UNDERSCORES,
    UPPERCASE_WITH_UNDERSCORES,
};

void set_substitution(struct generator_output *output, const char *variable,
 const char *value, size_t value_length, enum substitution_transform transform);
void set_unsigned_number_substitution(struct generator_output *output,
 const char *variable, uint32_t value);
void set_signed_number_substitution(struct generator_output *output,
 const char *variable, int32_t value);
void set_literal_substitution(struct generator_output *output,
 const char *variable, const char *value);

#endif
