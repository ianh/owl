#ifndef _ERROR_H_
#define _ERROR_H_

#include <stdlib.h>

struct source_range {
    size_t start_location;
    size_t end_location;
};

#define MAX_ERROR_RANGES 16
struct error {
    char text[1024];
    struct source_range ranges[MAX_ERROR_RANGES];
};

extern struct error error;
void print_error();
#define exit_with_error() do { print_error(); exit(-1); } while (0)

#endif
