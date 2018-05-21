#ifndef _FANCY_TREE_OUTPUT_H_
#define _FANCY_TREE_OUTPUT_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct document;
struct label;
struct row;

struct document {
    // The first row contains the "tokens" of the document.  The following rows
    // represent nodes in the parse tree.
    struct row *rows;
    uint32_t number_of_rows;

    // The number of columns in the terminal.  Lines longer than this will be
    // wrapped.
    long number_of_columns;

    // Used to reinforce the connection between tokens and their containing
    // labels.  If there are more rows than color codes, we reuse the colors by
    // wrapping around.
    const char **color_codes;
    int number_of_color_codes;
    // Used at the beginning of lines and for overflow indicators.
    const char *line_indicator_color_code;
    // Used to reset the color back to the default.
    const char *reset_color_code;
};

struct row {
    struct label *labels;
    uint32_t number_of_labels;
};

struct label {
    const char *text;
    size_t length;
    uint32_t start;
    uint32_t end;
    // A color of zero means the color is inherited from the row.  Otherwise,
    // this label will be the color corresponding to the row index in `color`.
    uint32_t color;
    bool starts_with_newline;
};

void output_document(struct document *document);

#endif
