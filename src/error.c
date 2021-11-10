#include "error.h"

#ifndef NOT_UNIX
#define _XOPEN_SOURCE 700
#include <unistd.h>
#endif

#include "terminal.h"
#include <string.h>

static int compare_source_ranges(const void *aa, const void *bb);
struct error error = {0};
char *error_in_string = 0;

void print_error(void)
{
    int colors = terminal_colors(STDERR_FILENO);
    long columns = terminal_columns(STDERR_FILENO);
    if (columns <= 0)
        columns = 80;
    const char *level;
    if (error.level == WARNING) {
        if (colors >= 256)
            fputs("\033[38;5;97m", stderr);
        else if (colors >= 8)
            fputs("\033[5;31m", stderr);
        level = "warning: ";
    } else {
        if (colors >= 256)
            fputs("\033[38;5;168m", stderr);
        else if (colors >= 8)
            fputs("\033[1;31m", stderr);
        level = "error: ";
    }
    fputs(level, stderr);
    if (colors >= 8)
        fputs("\033[0m", stderr);
    size_t offset = strlen(level);
    size_t line_offset = 0;
    size_t wrap_offset = 0;
    size_t i = 0;
    for (i = 0; error.text[i]; ++i) {
        if (i + offset - line_offset >= columns) {
            if (error.text[i] == ' ' || 3*(i - wrap_offset) >= columns)
                wrap_offset = i;
            fwrite(error.text + line_offset, 1, wrap_offset - line_offset,
             stderr);
            fputs("\n", stderr);
            line_offset = wrap_offset;
            i = wrap_offset;
            offset = 0;
        }
        if (error.text[i] == ' ')
            wrap_offset = i + 1;
    }
    fwrite(error.text + line_offset, 1, i - line_offset, stderr);
    fputs("\n", stderr);
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
        bool eol = error_in_string[i] == '\0' || error_in_string[i] == '\n';
        if (eol || (line_marked && i - line_start + 2 >= columns)) {
            if (line_marked) {
                fputs("  ", stderr);
                bool ellipsis = false;
                if (i - line_start + 2 > columns) {
                    line_start = i + 5 - columns;
                    fputs("...", stderr);
                    ellipsis = true;
                }
                fwrite(error_in_string + line_start, 1, i - line_start, stderr);
                fputs("\n  ", stderr);
                if (ellipsis)
                    fputs("   ", stderr);
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
                    }
                    if (marked)
                        fputs("~", stderr);
                    else
                        fputs(" ", stderr);
                }
                if (colors >= 8)
                    fputs("\033[0m", stderr);
                fputs("\n", stderr);
                while (i >= error.ranges[range].end && range < MAX_ERROR_RANGES)
                    range++;
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
            if (eol)
                line_start = i + 1;
            else
                line_start = i;
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
