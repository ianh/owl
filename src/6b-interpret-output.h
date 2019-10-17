#ifndef INTERPRET_OUTPUT_H
#define INTERPRET_OUTPUT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

// Set this as a label color for it to appear as the default terminal color
// (usually white or black).
#define DEFAULT_COLOR UINT32_MAX

struct document;
struct label;
struct row;
struct terminal_info;

struct document {
    // The first row contains the "tokens" of the document.  The following rows
    // represent nodes in the parse tree.
    struct row *rows;
    uint32_t number_of_rows;

    // If we want two documents with different numbers of rows to have the same
    // colors, we need to apply a color offset.
    int32_t color_offset;
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

struct terminal_info {
    // Used to reinforce the connection between tokens and their containing
    // labels.  If there are more rows than color codes, we reuse the colors by
    // wrapping around.
    const char **row_colors;
    int number_of_row_colors;
    // Used at the beginning of lines and for overflow indicators.
    const char *line_indicator;
    // Used to reset the color back to the default.
    const char *reset;

    // The number of columns in the terminal.  Lines longer than this will be
    // wrapped.
    long columns;
};

void output_document(FILE *file, struct document *document,
 struct terminal_info terminal_info);

#endif
