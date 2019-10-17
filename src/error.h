#ifndef ERROR_H
#define ERROR_H

#include "1-parse.h"
#include <stdlib.h>

enum error_level {
    ERROR,
    WARNING,
};

#define MAX_ERROR_RANGES 16
struct error {
    struct source_range ranges[MAX_ERROR_RANGES];
    enum error_level level;
    char text[1024];
};

extern char *error_in_string;
extern struct error error;
void print_error(void);
#define exit_with_error() do { print_error(); exit(-1); } while (0)
#define errorf(...) do { snprintf(error.text, sizeof(error.text), \
 __VA_ARGS__); } while (0)
#define exit_with_errorf(...) do { errorf(__VA_ARGS__); print_error(); \
 exit(-1); } while (0)

#endif
