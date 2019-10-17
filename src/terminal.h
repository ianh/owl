#ifndef TERMINAL_H
#define TERMINAL_H

#include <stdbool.h>

long terminal_columns(int fileno);
int terminal_colors(int fileno);

extern bool force_terminal_colors;

// These are needed for MSYS2 (msys2-x86_64-20180531) on Windows.
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#endif
