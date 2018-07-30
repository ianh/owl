#include "6b-interpret-output.h"

#include "alloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct range {
    uint32_t start;
    uint32_t end;
};

struct line {
    struct range *row_ranges;
    struct range offset_range;
    size_t end_offset;
    size_t end_offset_buffer;

    // This array must always be non-empty.
    size_t *offsets;
    uint32_t *rows_of_offsets;
    uint32_t offsets_capacity;

    bool has_overflow;
    bool is_overflow;

    struct terminal_info terminal_info;
    int32_t color_offset;
};

// This is how many dots we leave at the end of wrapped lines.
#define ELLIPSIS_COUNT 2
// This is how much space we leave for the dots at the end of wrapped lines.
#define ELLIPSIS_LENGTH (ELLIPSIS_COUNT + 1)
// The minimum allowed spacing between adjacent labels.
#define LABEL_PADDING 1
// This is how much space we leave for the "." at the beginning of each line.
#define LINE_MARKER_LENGTH 2
// The amount of space to add before a wrapped line.
#define LINE_WRAP_MARGIN 2
// If a label overflows to the next line, leave a minimum amount of space so the
// "-" is guaranteed to appear.
#define OVERFLOW_INDICATOR_MARGIN 1

static void output_line(FILE *file, struct line *line,
 struct document *document);
static void output_spacing(FILE *file, struct line *line, uint32_t row,
 size_t offset, uint32_t start_index, uint32_t end_index);
static bool advance_line(struct line *line, struct document *document);
static bool advance_label(struct line *line, struct document *document,
 uint32_t row, uint32_t label_index);

static void init_line(struct line *line, struct document *document);
static void destroy_line(struct line *line);
static void break_line(struct line *line, struct document *document);
static void copy_line(struct line *from, struct line *to,
 struct document *document);

static void push_offsets(struct line *line, uint32_t index, size_t target);

#define BEFORE_START (SIZE_MAX - 2)
#define PAST_END (SIZE_MAX - 1)
static size_t offset_at(struct line *line, uint32_t index);
static void expand_offset_range(struct line *line, uint32_t index);
static void ensure_offsets_capacity(struct line *line, uint32_t capacity);

static void set_color(FILE *file, struct line *line, uint32_t color);

void output_document(FILE *file, struct document *document,
 struct terminal_info terminal_info)
{
    struct line line = {0};
    struct line next_line = {0};
    struct line last_fit_line = {0};
    init_line(&line, document);
    init_line(&next_line, document);
    init_line(&last_fit_line, document);
    line.terminal_info = terminal_info;
    next_line.terminal_info = terminal_info;
    line.color_offset = document->color_offset;
    next_line.color_offset = document->color_offset;
    bool can_break_line = false;
    long columns = terminal_info.columns;
    while (true) {
        copy_line(&next_line, &line, document);
        if (!advance_line(&next_line, document)) {
            output_line(file, &line, document);
            break;
        }
        if (!can_break_line) {
            copy_line(&next_line, &last_fit_line, document);
            can_break_line = true;
            continue;
        }
        struct label *last_token = 0;
        struct range token_range = next_line.row_ranges[0];
        if (token_range.end > token_range.start)
            last_token = &document->rows[0].labels[token_range.end - 1];
        if (last_token && last_token->starts_with_newline) {
            // Break a line at a newline.
            output_line(file, &line, document);
            fputs("\n", file);
            copy_line(&line, &next_line, document);
            break_line(&next_line, document);
            can_break_line = false;
            continue;
        }

        size_t end = next_line.end_offset;
        if (last_token) {
            size_t offset = offset_at(&next_line, last_token->end);
            if (offset < BEFORE_START && offset + ELLIPSIS_LENGTH > end)
                end = offset + ELLIPSIS_LENGTH;
        }
        if (end + LINE_MARKER_LENGTH <= columns) {
            copy_line(&next_line, &last_fit_line, document);
            continue;
        }
        if (next_line.end_offset + LINE_MARKER_LENGTH <= columns)
            continue;
        // Break a line in the middle.
        copy_line(&last_fit_line, &line, document);
        size_t offset = offset_at(&line, line.offset_range.end - 1);
        if (offset < BEFORE_START && offset + ELLIPSIS_LENGTH > line.end_offset)
            line.end_offset = offset + ELLIPSIS_LENGTH;
        if (line.end_offset + LINE_MARKER_LENGTH > columns)
            line.end_offset = columns - LINE_MARKER_LENGTH;
        line.has_overflow = true;
        output_line(file, &line, document);
        copy_line(&line, &next_line, document);
        break_line(&next_line, document);
        next_line.offsets[0] = LINE_WRAP_MARGIN;
        next_line.end_offset = LINE_WRAP_MARGIN;
        next_line.is_overflow = true;
        can_break_line = false;
    }
    destroy_line(&line);
    destroy_line(&next_line);
    destroy_line(&last_fit_line);
}

static void output_line(FILE *file, struct line *line,
 struct document *document)
{
    for (uint32_t i = 0; i < document->number_of_rows; ++i) {
        struct range rr = line->row_ranges[i];
        if (rr.start == rr.end)
            continue;
        if (i == 0) {
            if (line->terminal_info.line_indicator)
                fputs(line->terminal_info.line_indicator, file);
            if (line->is_overflow) {
                // The second dot is drawn by output_spacing.
                fputs(" .", file);
            } else
                fputs(". ", file);
        } else
            fputs("  ", file);
        struct range r = line->offset_range;
        uint32_t index = r.start;
        size_t offset = 0;
        for (uint32_t j = rr.start; j < rr.end; ++j) {
            struct label l = document->rows[i].labels[j];
            if (l.start >= r.end)
                continue;
            if (l.start >= r.start) {
                // Output the actual text for this label.
                output_spacing(file, line, i, offset, index, l.start);
                index = l.start;
                offset = offset_at(line, index);
                set_color(file, line, l.color ? l.color : i);
                fwrite(l.text, 1, l.length, file);
                offset += l.length;
            } else
                set_color(file, line, i);
            size_t end_offset = offset_at(line, l.end);
            if (end_offset == BEFORE_START)
                continue;
            if (end_offset == PAST_END)
                end_offset = line->end_offset;
            while (offset < end_offset) {
                fputs("-", file);
                offset++;
            }
            index = l.end;
        }
        output_spacing(file, line, i, offset, index, r.end);
        if (line->terminal_info.reset)
            fputs(line->terminal_info.reset, file);
        fputs("\n", file);
    }
}

static void output_spacing(FILE *file, struct line *line, uint32_t row,
 size_t offset, uint32_t start_index, uint32_t end_index)
{
    uint32_t offset_row = 0;
    struct range r = line->offset_range;
    for (uint32_t i = start_index; i <= end_index; ++i) {
        if (i < r.start)
            continue;
        size_t end_offset = line->end_offset;
        if (i < r.end)
            end_offset = line->offsets[i - r.start];
        for (size_t j = offset; j < end_offset; ++j) {
            if (row != 0 && offset_row != 0) {
                set_color(file, line, offset_row);
                fputs("|", file);
                offset_row = 0;
            } else if (row == 0 &&
             ((line->has_overflow && j + ELLIPSIS_COUNT >= line->end_offset) ||
             (line->is_overflow && j == 0))) {
                if (line->terminal_info.line_indicator)
                    fputs(line->terminal_info.line_indicator, file);
                fputs(".", file);
            } else
                fputs(" ", file);
        }
        offset = end_offset;
        if (i < r.end && line->rows_of_offsets[i - r.start] > row)
            offset_row = line->rows_of_offsets[i - r.start];
    }
}

static bool advance_line(struct line *line, struct document *document)
{
    bool advanced = false;
    for (uint32_t i = 0; i < document->number_of_rows; ++i) {
        struct range rr = line->row_ranges[i];
        if (rr.start != rr.end) {
            struct label l = document->rows[i].labels[rr.end - 1];
            // When a label extends past the end of offset_range, its layout is
            // incomplete -- the end offset won't appear in the offsets array.
            // Check to see if we've reached the end of the label, and if we
            // have, we complete the layout here.
            if (l.end <= line->offset_range.end)
                advance_label(line, document, i, rr.end - 1);
        }
        if (rr.end == document->rows[i].number_of_labels)
            continue;
        if (advance_label(line, document, i, rr.end)) {
            line->row_ranges[i].end++;
            advanced = true;
        }
    }
    return advanced;
}

static bool advance_label(struct line *line, struct document *document,
 uint32_t row, uint32_t label_index)
{
    struct label l = document->rows[row].labels[label_index];
    if (l.end == line->offset_range.end || row == 0)
        expand_offset_range(line, l.end);
    struct range r = line->offset_range;
    if (l.start >= r.end)
        return false;
    size_t end_offset = 0;
    if (l.start >= r.start) {
        if (row > 0 && label_index > line->row_ranges[row].start) {
            // Apply minimum inter-label spacing.
            struct label prev = document->rows[row].labels[label_index - 1];
            if (prev.end >= r.start && prev.end < r.end) {
                push_offsets(line, l.start, offset_at(line, prev.end) +
                 + LABEL_PADDING);
            }
        }
        end_offset = offset_at(line, l.start) + l.length;
        if (line->rows_of_offsets[l.start - r.start] == 0 || (row > 0 &&
         row < line->rows_of_offsets[l.start - r.start]))
            line->rows_of_offsets[l.start - r.start] = row;
    } else
        end_offset = line->offsets[0] + 1;
    push_offsets(line, l.end, end_offset);
    return true;
}

static void init_line(struct line *line, struct document *document)
{
    memset(line, 0, sizeof(*line));
    line->row_ranges = calloc(1, sizeof(struct range) *
     document->number_of_rows);
    line->offset_range.end = 1;
    line->offsets_capacity = 1;
    line->offsets = calloc(1, sizeof(size_t));
    line->rows_of_offsets = calloc(1, sizeof(uint32_t));
}

static void destroy_line(struct line *line)
{
    free(line->row_ranges);
    free(line->offsets);
    free(line->rows_of_offsets);
    memset(line, 0, sizeof(*line));
}

static void break_line(struct line *line, struct document *document)
{
    for (uint32_t i = 0; i < document->number_of_rows; ++i) {
        struct row *row = &document->rows[i];
        while (line->row_ranges[i].start < row->number_of_labels &&
         row->labels[line->row_ranges[i].start].end < line->offset_range.end)
            line->row_ranges[i].start++;
    }
    line->rows_of_offsets[0] = line->rows_of_offsets[line->offset_range.end -
     line->offset_range.start - 1];
    line->offset_range.start = line->offset_range.end - 1;
    line->offset_range.end = line->offset_range.end;
    line->offsets[0] = 0;
    line->end_offset = 0;
    line->end_offset_buffer = 0;
    line->has_overflow = false;
    line->is_overflow = false;
}

static void copy_line(struct line *from, struct line *to,
 struct document *document)
{
    memcpy(to->row_ranges, from->row_ranges,
     sizeof(struct range) * document->number_of_rows);
    to->end_offset = from->end_offset;
    to->end_offset_buffer = from->end_offset_buffer;

    struct range r = from->offset_range;
    to->offset_range = r;
    ensure_offsets_capacity(to, r.end - r.start);
    memcpy(to->offsets, from->offsets, sizeof(size_t) * (r.end - r.start));
    memcpy(to->rows_of_offsets, from->rows_of_offsets,
     sizeof(uint32_t) * (r.end - r.start));

    to->is_overflow = from->is_overflow;
    to->has_overflow = from->has_overflow;
}

static void push_offsets(struct line *line, uint32_t index, size_t target)
{
    struct range r = line->offset_range;
    if (index < r.start)
        return;
    if (index >= r.end) {
        target += OVERFLOW_INDICATOR_MARGIN;
        if (target > line->end_offset) {
            line->end_offset_buffer += target - line->end_offset;
            line->end_offset = target;
        }
        return;
    }
    size_t offset = offset_at(line, index);
    if (target <= offset)
        return;
    for (uint32_t i = index; i < r.end; ++i)
        line->offsets[i - r.start] += target - offset;
    if (target - offset > line->end_offset_buffer) {
        line->end_offset += target - offset - line->end_offset_buffer;
        line->end_offset_buffer = 0;
    } else
        line->end_offset_buffer -= target - offset;
}

static size_t offset_at(struct line *line, uint32_t index)
{
    if (index < line->offset_range.start)
        return BEFORE_START;
    else if (index >= line->offset_range.end)
        return PAST_END;
    else
        return line->offsets[index - line->offset_range.start];
}

static void expand_offset_range(struct line *line, uint32_t index)
{
    struct range r = line->offset_range;
    if (index < r.end)
        return;
    ensure_offsets_capacity(line, index - r.start + 1);
    for (uint32_t i = line->offset_range.end; i < index + 1; ++i) {
        line->offsets[i - r.start] = line->offsets[i - r.start - 1];
        line->rows_of_offsets[i - r.start] = 0;
    }
    line->offset_range.end = index + 1;
}

void ensure_offsets_capacity(struct line *line, uint32_t capacity)
{
    if (capacity <= line->offsets_capacity)
        return;
    void *offsets = realloc(line->offsets,
     (size_t)capacity * sizeof(size_t) * 2);
    void *offset_rows = realloc(line->rows_of_offsets,
     (size_t)capacity * sizeof(uint32_t) * 2);
    line->offsets = offsets;
    line->rows_of_offsets = offset_rows;
    line->offsets_capacity = capacity * 2;
}

static void set_color(FILE *file, struct line *line, uint32_t color)
{
    const char **colors = line->terminal_info.row_colors;
    if (color == 0 || !colors)
        return;
    if (color == DEFAULT_COLOR) {
        if (line->terminal_info.reset) {
            fputs(line->terminal_info.reset, file);
            return;
        } else
            color = 1;
    }
    int32_t offset = line->color_offset;
    while (offset < 0)
        offset += line->terminal_info.number_of_row_colors;
    color += offset;
    fputs(colors[(color - 1) % line->terminal_info.number_of_row_colors], file);
}
