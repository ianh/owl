#ifndef _TERMINAL_H_
#define _TERMINAL_H_

#include <stdbool.h>

long terminal_columns(int fileno);
int terminal_colors(int fileno);

extern bool force_terminal_colors;

#endif
