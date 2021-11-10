#include "terminal.h"

#ifndef NOT_UNIX
#define _XOPEN_SOURCE 700
#include <dlfcn.h>
#include <sys/ioctl.h>
#include <unistd.h>
#endif

#include <stdlib.h>

bool force_terminal_colors = false;

long terminal_columns(int fileno)
{
#ifdef TIOCGWINSZ
    if (fileno >= 0 && isatty(fileno)) {
        struct winsize winsize = {0};
        if (!ioctl(fileno, TIOCGWINSZ, (char *)&winsize))
            return (long)winsize.ws_col;
    }
#endif
    char *env = getenv("COLUMNS");
    if (!env || *env == '\0')
        return -1;
    char *endptr = env;
    long columns = strtol(env, &endptr, 10);
    if (*endptr != '\0')
        return -1;
    return columns;
}

int terminal_colors(int fileno)
{
    if (force_terminal_colors)
        return 256;
#ifndef NOT_UNIX
    if (fileno < 0 || !isatty(fileno))
        return 1;
    // Try some different names that "ncurses" goes by on various platforms.
    // Each name is annotated with the platform it was added to support.
    char *libs[] = {
        // Ubuntu 18.04 amd64
        "libncurses.so.5",

        // Arch Linux 2018.05.01 amd64
        "libncursesw.so",

        // macOS 10.12.6
        "libncurses.dylib",

        // These are here just in case...
        "libncursesw.so.6",
        "libncurses.so",
        "libncurses.so.6",
    };
    for (int i = 0; i < sizeof(libs) / sizeof(libs[0]); ++i) {
        void *handle = dlopen(libs[i], RTLD_LAZY | RTLD_LOCAL);
        if (!handle)
            continue;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
        int (*setupterm)(char *, int, int *) =
         (int (*)(char *, int, int *))dlsym(handle, "setupterm");
        int (*tigetnum)(char *) = (int (*)(char *))dlsym(handle, "tigetnum");
#pragma GCC diagnostic pop
        int value = -1;
        int err;
        if (setupterm && tigetnum && !setupterm(0, fileno, &err))
            value = tigetnum("colors");
        dlclose(handle);
        if (value > 0)
            return value;
    }
#endif
    return 1;
}
