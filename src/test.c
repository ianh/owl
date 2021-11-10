#ifndef NOT_UNIX
#include "test.h"

#include "error.h"
#include <errno.h>
#include <string.h>
#include <sys/wait.h>

static void spawn_child(struct test_compilation *t);
static void wait_for_child(struct test_compilation *t);

void begin_test_compilation(struct test_compilation *t)
{
    t->args[0] = getenv("CC");
    if (!t->args[0])
        t->args[0] = "cc";
    int i = 1;
    t->args[i++] = "-pedantic";
    t->args[i++] = "-Wall";
    t->args[i++] = "-Wno-unknown-warning-option"; // clang needs this to avoid giving a warning about -Wno-unused-but-set-variable
    t->args[i++] = "-Wno-unused-function"; // Modern compilers all understand DCE and functions/variables
    t->args[i++] = "-Wno-unused-variable"; // that go unused for some grammars may be necessary for others
    t->args[i++] = "-Wno-unused-but-set-variable";
    t->args[i++] = "-x";
    t->args[i++] = "c";
    t->args[i++] = "-";
    t->args[i++] = "-o";
    t->args[i++] = t->executable_filename;
    t->args[i++] = 0;
    t->program = t->args[0];
    spawn_child(t);
    fprintf(t->file, "#define OWL_PARSER_IMPLEMENTATION\n");
}

void finish_test_compilation(struct test_compilation *t, char *input_string)
{
    fprintf(t->file, "int main() {\n");
    fprintf(t->file, "    struct owl_tree *tree;\n");
    fprintf(t->file, "    tree = owl_tree_create_from_file(stdin);\n");
    fprintf(t->file, "    owl_tree_print(tree);\n");
    fprintf(t->file, "    owl_tree_destroy(tree);\n");
    fprintf(t->file, "    return 0;\n");
    fprintf(t->file, "}\n");
    fclose(t->file);
    wait_for_child(t);

    t->args[0] = t->executable_filename;
    t->args[1] = 0;
    t->program = "test executable";
    spawn_child(t);
    fprintf(t->file, "%s", input_string);
    fclose(t->file);
    wait_for_child(t);
}

static void spawn_child(struct test_compilation *t)
{
    int fd[2];
    if (pipe(fd) == -1)
        exit_with_errorf("pipe() failed - %s", strerror(errno));
    t->child = fork();
    if (t->child == -1)
        exit_with_errorf("fork() failed - %s", strerror(errno));
    else if (t->child == 0) {
        close(fd[1]);
        dup2(fd[0], STDIN_FILENO);
        close(fd[0]);
        execvp(t->args[0], t->args);
        exit_with_errorf("couldn't invoke '%s' - %s", t->program, strerror(errno));
        exit(1);
    } else {
        t->file = fdopen(fd[1], "w");
        close(fd[0]);
    }
}

static void wait_for_child(struct test_compilation *t)
{
    int status;
    pid_t pid;
    while ((pid = wait(&status)) != t->child) {
        if (pid == -1)
            exit_with_errorf("wait() failed - %s", strerror(errno));
    }
    if (!WIFEXITED(status)) {
        exit_with_errorf("'%s' didn't exit normally", t->program);
        exit(1);
    } else if (WEXITSTATUS(status) != 0) {
        exit_with_errorf("'%s' exited with status %d", t->program,
         WEXITSTATUS(status));
        exit(WEXITSTATUS(status));
    }
}

#endif
