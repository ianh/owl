#ifndef NOT_UNIX
#ifndef TEST_H
#define TEST_H

#include <stdio.h>
#include <unistd.h>

#define MAX_ARGS 16
struct test_compilation {
    char *executable_filename;
    FILE *file;
    pid_t child;
    char *args[MAX_ARGS];
    char *program;
};

void begin_test_compilation(struct test_compilation *t);
void finish_test_compilation(struct test_compilation *t, char *input_string);

#endif
#endif
