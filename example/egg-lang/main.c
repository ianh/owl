#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum type {
    TYPE_FALSE,
    TYPE_TRUE,
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_TABLE,
    TYPE_CLOSURE,
    TYPE_BUILTIN_FUNCTION,
};

typedef uint32_t table_id;

struct closure {
    uint32_t function_index;
    table_id environment_table;
};

struct val {
    union {
        double number;
        table_id table;
        struct closure closure;
        struct val (*builtin)(struct owl_ref args);
    };
    enum type type;
};

enum mark {
    MARK_FREE,
    MARK_BLACK,
    MARK_WHITE,
};

struct table {
    struct val *keys;
    struct val *values;
    uint32_t length;
    uint32_t capacity;

    // For linking environments together.
    uint32_t next;

    // For the garbage collector.
    enum mark mark;
};

enum control_flow {
    NORMAL,
    RETURN,
    BREAK,
    CONTINUE,
};

static struct table *tables;
static uint32_t number_of_tables = 1;

static size_t bytes_in_use;
static size_t byte_sweep_limit = 4096;

static table_id free_list;
static enum mark current_mark = MARK_BLACK;
static table_id stack_root;

static struct owl_ref *functions;
static uint32_t number_of_functions;

static table_id environment;

static struct val builtin_print(struct owl_ref arg);
static struct val builtin_println(struct owl_ref arg);
static struct val builtin_isspace(struct owl_ref arg);
static struct val builtin_isdigit(struct owl_ref arg);
static struct val builtin_todigit(struct owl_ref arg);
static struct val builtin_read_input_length(struct owl_ref arg);

static enum control_flow eval_stmt_list(struct owl_ref stmt_list_ref,
 struct val *return_val);
static enum control_flow eval_stmt(struct owl_ref stmt_ref,
 struct val *return_val);
static struct val eval_expr(struct owl_ref expr_ref);

static int val_snprint(char *s, int len, struct val value);
static bool val_equal(struct val a, struct val b);
static void val_print(FILE *file, struct val value);

static struct val table_lookup(table_id table, struct val key);
static uint32_t table_lookup_index(table_id table, struct val key);
static void table_set(table_id table, struct val key, struct val value);
static void table_set_deep(table_id table, struct val key, struct val value);
static void table_push(table_id *table);
static void table_pop(table_id *table);

static table_id alloc_table(void);
static table_id alloc_env_table(void);
static table_id alloc_string(size_t length);
static char *get_string(table_id index);

static const struct val true_val = { .type = TYPE_TRUE };
static const struct val false_val = { .type = TYPE_FALSE };
static struct val number_val(double number);
static struct val string_val(const char *string, size_t len);

static struct val string_for_identifier(struct owl_ref ident_ref);
static double eval_number(struct owl_ref number_expr);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        fprintf(stderr, "needs input file\n");
        return -1;
    }
    FILE *file;
    if (argv[1][0] == '-' && argv[1][1] == '\0')
        file = stdin;
    else
        file = fopen(argv[1], "r");
    struct owl_tree *tree = owl_tree_create_from_file(file);
    struct parsed_program program = owl_tree_get_parsed_program(tree);
    tables = calloc(1, sizeof(struct table));
    tables[0].mark = MARK_FREE;
    environment = alloc_env_table();
    table_set(environment, string_val("print", strlen("print")),
     (struct val){ .type = TYPE_BUILTIN_FUNCTION, .builtin = builtin_print });
    table_set(environment, string_val("println", strlen("println")),
     (struct val){ .type = TYPE_BUILTIN_FUNCTION, .builtin = builtin_println });
    table_set(environment, string_val("isspace", strlen("isspace")),
     (struct val){ .type = TYPE_BUILTIN_FUNCTION, .builtin = builtin_isspace });
    table_set(environment, string_val("isdigit", strlen("isdigit")),
     (struct val){ .type = TYPE_BUILTIN_FUNCTION, .builtin = builtin_isdigit });
    table_set(environment, string_val("todigit", strlen("todigit")),
     (struct val){ .type = TYPE_BUILTIN_FUNCTION, .builtin = builtin_todigit });
    table_set(environment, string_val("read_input_length",
     strlen("read_input_length")), (struct val){ .type = TYPE_BUILTIN_FUNCTION,
     .builtin = builtin_read_input_length });
    enum control_flow flow = eval_stmt_list(program.stmt_list, 0);
    if (flow == BREAK)
        fprintf(stderr, "error: 'break' used outside loop\n");
    else if (flow == CONTINUE)
        fprintf(stderr, "error: 'continue' used outside loop\n");
    else if (flow == RETURN)
        fprintf(stderr, "error: 'return' used outside function\n");
    else
        return 0;
    exit(-1);
}

static enum control_flow eval_stmt_list(struct owl_ref stmt_list,
 struct val *return_val)
{
    struct parsed_stmt_list stmts = parsed_stmt_list_get(stmt_list);
    for (; !stmts.stmt.empty; stmts.stmt = owl_next(stmts.stmt)) {
        table_id saved_root = stack_root;
        enum control_flow flow = eval_stmt(stmts.stmt, return_val);
        stack_root = saved_root;
        if (flow != NORMAL)
            return flow;
    }
    return NORMAL;
}

static enum control_flow eval_stmt(struct owl_ref stmt_ref,
 struct val *return_val)
{
    struct parsed_stmt stmt = parsed_stmt_get(stmt_ref);
    switch (stmt.type) {
    case PARSED_FUNCTION: {
        struct val f = { .type = TYPE_CLOSURE };
        f.closure.environment_table = environment;
        uint32_t i = 0;
        for (; i < number_of_functions; ++i) {
            if (owl_refs_equal(stmt_ref, functions[i]))
                break;
        }
        if (i >= number_of_functions) {
            functions = realloc(functions, (i+1)*sizeof(struct owl_ref));
            number_of_functions = i + 1;
            functions[i] = stmt_ref;
        }
        f.closure.function_index = i;
        table_set(environment, string_for_identifier(stmt.identifier), f);
        break;
    }
    case PARSED_ASSIGNMENT:
        table_set_deep(environment, string_for_identifier(stmt.identifier),
         eval_expr(stmt.expr));
        break;
    case PARSED_VARIABLE:
        table_set(environment, string_for_identifier(stmt.identifier),
         eval_expr(stmt.expr));
        break;
    case PARSED_FIELD_ASSIGNMENT: {
        struct val val = eval_expr(stmt.expr);
        struct val table = eval_expr(stmt.table);
        if (table.type != TYPE_TABLE) {
            fprintf(stderr, "error: expected table\n");
            exit(-1);
        }
        table_set(table.table, string_for_identifier(stmt.identifier), val);
        break;
    }
    case PARSED_LOOKUP_ASSIGNMENT: {
        struct val val = eval_expr(stmt.expr);
        struct val table = eval_expr(stmt.table);
        if (table.type != TYPE_TABLE) {
            fprintf(stderr, "error: expected table\n");
            exit(-1);
        }
        table_set(table.table, eval_expr(stmt.lookup), val);
        break;
    }
    case PARSED_RETURN:
        if (return_val)
            *return_val = eval_expr(stmt.expr);
        return RETURN;
    case PARSED_IF_THEN: {
        enum control_flow flow = NORMAL;
        while (!stmt.expr.empty) {
            if (eval_expr(stmt.expr).type != TYPE_FALSE) {
                table_push(&environment);
                flow = eval_stmt_list(stmt.stmt_list, return_val);
                table_pop(&environment);
                break;
            }
            stmt.expr = owl_next(stmt.expr);
            stmt.stmt_list = owl_next(stmt.stmt_list);
        }
        if (!stmt.expr.empty) {
            // We exited the loop early, so don't look for an 'else' clause.
            return flow;
        }
        if (!stmt.else_stmts.empty) {
            table_push(&environment);
            flow = eval_stmt_list(stmt.else_stmts, return_val);
            table_pop(&environment);
        }
        return flow;
    }
    case PARSED_WHILE: {
        enum control_flow flow = NORMAL;
        table_push(&environment);
        while (eval_expr(stmt.expr).type != TYPE_FALSE) {
            flow = eval_stmt_list(stmt.stmt_list, return_val);
            if (flow == BREAK || flow == RETURN)
                break;
        }
        table_pop(&environment);
        if (flow == BREAK || flow == CONTINUE)
            flow = NORMAL;
        return flow;
    }
    case PARSED_FOR: {
        struct val table = eval_expr(stmt.expr);
        if (table.type != TYPE_TABLE) {
            fprintf(stderr, "error: expected table\n");
            exit(-1);
        }
        table_push(&environment);
        enum control_flow flow = NORMAL;
        for (uint32_t i = 0; i < tables[table.table].length; ++i) {
            if (!stmt.key.empty) {
                table_set(environment, string_for_identifier(stmt.key),
                 tables[table.table].keys[i]);
            }
            table_set(environment, string_for_identifier(stmt.value),
             tables[table.table].values[i]);
            flow = eval_stmt_list(stmt.stmt_list, return_val);
            if (flow == BREAK || flow == RETURN)
                break;
        }
        if (flow == BREAK || flow == CONTINUE)
            flow = NORMAL;
        table_pop(&environment);
        return flow;
    }
    case PARSED_BREAK:
        return BREAK;
    case PARSED_CONTINUE:
        return CONTINUE;
    case PARSED_EXPR:
        eval_expr(stmt.expr);
        break;
    default:
        break;
    }
    return NORMAL;
}

static struct val eval_expr(struct owl_ref expr_ref)
{
    struct parsed_expr expr = parsed_expr_get(expr_ref);
    switch (expr.type) {
    case PARSED_STRING: {
        struct parsed_string s = parsed_string_get(expr.string);
        return string_val(s.string, s.length);
    }
    case PARSED_NUMBER:
        return number_val(parsed_number_get(expr.number).number);
    case PARSED_TRUE:
        return true_val;
    case PARSED_FALSE:
        return false_val;
    case PARSED_VARIABLE: {
        struct val key = string_for_identifier(expr.identifier);
        for (table_id e = environment; e; e = tables[e].next) {
            uint32_t i = table_lookup_index(e, key);
            if (i < tables[e].length)
                return tables[e].values[i];
        }
        fprintf(stderr, "error: unknown variable '");
        val_print(stderr, key);
        fprintf(stderr, "'\n");
        exit(-1);
    }
    case PARSED_PARENS:
        return eval_expr(expr.operand);
    case PARSED_TABLE: {
        double index_key = 0;
        table_id table = alloc_table();
        while (!expr.table_entry.empty) {
            struct parsed_table_entry entry =
             parsed_table_entry_get(expr.table_entry);
            struct val key;
            if (!entry.identifier.empty)
                key = string_for_identifier(entry.identifier);
            else if (!entry.key.empty)
                key = eval_expr(entry.key);
            else {
                key = (struct val){ .type = TYPE_NUMBER, .number = index_key };
                index_key++;
            }
            table_set(table, key, eval_expr(entry.expr));
            expr.table_entry = owl_next(expr.table_entry);
        }
        return (struct val){ .type = TYPE_TABLE, .table = table };
    }
    case PARSED_CALL: {
        struct val f = eval_expr(expr.operand);
        if (f.type == TYPE_BUILTIN_FUNCTION)
            return f.builtin(expr.expr);
        else if (f.type != TYPE_CLOSURE) {
            fprintf(stderr, "error: called something that isn't a function\n");
            exit(-1);
        }
        struct val env_key = string_val("__env", strlen("__env"));
        table_id saved_root = stack_root;
        table_id local = alloc_table();
        table_set(local, env_key,
         (struct val){ .type = TYPE_TABLE, .table = environment });
        struct parsed_stmt function =
         parsed_stmt_get(functions[f.closure.function_index]);
        struct owl_ref param =
         parsed_parameter_list_get(function.parameter_list).identifier;
        struct owl_ref arg = expr.expr;
        for (; !param.empty; param = owl_next(param)) {
            if (arg.empty) {
                fprintf(stderr, "error: not enough arguments\n");
                exit(-1);
            }
            table_set(local, string_for_identifier(param), eval_expr(arg));
            arg = owl_next(arg);
        }
        if (!arg.empty) {
            fprintf(stderr, "error: too many arguments\n");
            exit(-1);
        }
        stack_root = saved_root;
        tables[local].next = f.closure.environment_table;
        environment = local;
        struct val retval = {0};
        enum control_flow flow = eval_stmt_list(function.stmt_list, &retval);
        if (flow == BREAK) {
            fprintf(stderr, "error: 'break' used outside loop\n");
            exit(-1);
        } else if (flow == CONTINUE) {
            fprintf(stderr, "error: 'continue' used outside loop\n");
            exit(-1);
        }
        struct val env = table_lookup(environment, env_key);
        assert(env.type == TYPE_TABLE);
        environment = env.table;
        return retval;
    }
    case PARSED_LOOKUP: {
        struct val table = eval_expr(expr.operand);
        if (table.type != TYPE_TABLE) {
            fprintf(stderr, "error: value is not a table\n");
            exit(-1);
        }
        return table_lookup(table.table, eval_expr(expr.expr));
    }
    case PARSED_FIELD: {
        struct val table = eval_expr(expr.operand);
        if (table.type != TYPE_TABLE) {
            fprintf(stderr, "error: value is not a table\n");
            exit(-1);
        }
        return table_lookup(table.table,
         string_for_identifier(expr.identifier));
    }
    case PARSED_NEGATE:
        return number_val(-eval_number(expr.operand));
    case PARSED_NOT:
        return eval_expr(expr.operand).type == TYPE_FALSE ?
         true_val : false_val;
    case PARSED_TIMES:
        return number_val(eval_number(expr.left) * eval_number(expr.right));
    case PARSED_DIVIDED_BY:
        return number_val(eval_number(expr.left) / eval_number(expr.right));
    case PARSED_MODULUS:
        return number_val((uint64_t)eval_number(expr.left) %
         (uint64_t)eval_number(expr.right));
    case PARSED_PLUS: {
        struct val left = eval_expr(expr.left);
        struct val right = eval_expr(expr.right);
        if (left.type == TYPE_STRING || right.type == TYPE_STRING) {
            int left_size = val_snprint(NULL, 0, left);
            int right_size = val_snprint(NULL, 0, right);
            table_id string = alloc_string(left_size + right_size + 1);
            val_snprint(get_string(string), left_size + 1, left);
            val_snprint(get_string(string) + left_size, right_size + 1, right);
            get_string(string)[left_size + right_size] = '\0';
            return (struct val){ .type = TYPE_STRING, .table = string };
        } else if (left.type == TYPE_NUMBER && right.type == TYPE_NUMBER)
            return number_val(left.number + right.number);
        fprintf(stderr, "error: '+' only applies to numbers or strings\n");
        exit(-1);
    }
    case PARSED_MINUS:
        return number_val(eval_number(expr.left) - eval_number(expr.right));
    case PARSED_AND: {
        struct val left = eval_expr(expr.left);
        if (left.type == TYPE_FALSE)
            return left;
        else
            return eval_expr(expr.right);
    }
    case PARSED_OR: {
        struct val left = eval_expr(expr.left);
        if (left.type == TYPE_FALSE)
            return eval_expr(expr.right);
        else
            return left;
    }
    case PARSED_EQUAL_TO:
        return val_equal(eval_expr(expr.left), eval_expr(expr.right)) ?
         true_val : false_val;
    case PARSED_NOT_EQUAL_TO:
        return val_equal(eval_expr(expr.left), eval_expr(expr.right)) ?
         false_val : true_val;
    case PARSED_LESS_THAN:
        return eval_number(expr.left) < eval_number(expr.right) ?
         true_val : false_val;
    case PARSED_GREATER_THAN:
        return eval_number(expr.left) > eval_number(expr.right) ?
         true_val : false_val;
    case PARSED_LESS_THAN_OR_EQUAL_TO:
        return eval_number(expr.left) <= eval_number(expr.right) ?
         true_val : false_val;
    case PARSED_GREATER_THAN_OR_EQUAL_TO:
        return eval_number(expr.left) >= eval_number(expr.right) ?
         true_val : false_val;
    default:
        return false_val;
    }
}

static int val_snprint(char *s, int len, struct val value)
{
    switch (value.type) {
    case TYPE_FALSE:
        return snprintf(s, len, "false");
    case TYPE_TRUE:
        return snprintf(s, len, "true");
    case TYPE_NUMBER:
        return snprintf(s, len, "%g", value.number);
    case TYPE_STRING:
        return snprintf(s, len, "%s", get_string(value.table));
    case TYPE_TABLE:
        return snprintf(s, len, "[table #%u]", value.table);
    case TYPE_CLOSURE:
        return snprintf(s, len, "[closure]");
    case TYPE_BUILTIN_FUNCTION:
        return snprintf(s, len, "[built-in function]");
    default:
        return snprintf(s, len, "[unknown]");
    }
}

static bool val_equal(struct val a, struct val b)
{
    if (a.type != b.type)
        return false;
    switch (a.type) {
    case TYPE_FALSE:
    case TYPE_TRUE:
        return true;
    case TYPE_NUMBER:
        return a.number == b.number;
    case TYPE_STRING:
        return !strcmp(get_string(a.table), get_string(b.table));
    case TYPE_TABLE:
        return a.table == b.table;
    case TYPE_CLOSURE:
        return false;
    case TYPE_BUILTIN_FUNCTION:
        return a.builtin == b.builtin;
    default:
        return false;
    }
}

static void val_print(FILE *file, struct val value)
{
    int size = val_snprint(NULL, 0, value);
    char *string = malloc(size + 1);
    val_snprint(string, size + 1, value);
    string[size] = '\0';
    fprintf(file, "%s", string);
    free(string);
}

static struct val table_lookup(table_id table, struct val key)
{
    uint32_t i = table_lookup_index(table, key);
    if (i < tables[table].length)
        return tables[table].values[i];
    fprintf(stderr, "error: key '");
    val_print(stderr, key);
    fprintf(stderr, "' not found in table\n");
    exit(-1);
}

static uint32_t table_lookup_index(table_id table, struct val key)
{
    if (tables[table].mark == MARK_FREE)
        abort();
    uint32_t i = 0;
    for (; i < tables[table].length; ++i) {
        if (val_equal(tables[table].keys[i], key))
            break;
    }
    return i;
}

static void table_set(table_id table, struct val key, struct val value)
{
    uint32_t i = table_lookup_index(table, key);
    if (i >= tables[table].length) {
        if (i >= tables[table].capacity) {
            uint32_t capacity = tables[table].capacity;
            capacity = 8 + capacity * 2;
            tables[table].keys = realloc(tables[table].keys,
             capacity * sizeof(struct val));
            tables[table].values = realloc(tables[table].values,
             capacity * sizeof(struct val));
            bytes_in_use += (capacity - tables[table].capacity) * 2 *
             sizeof(struct val);
            tables[table].capacity = capacity;
        }
        tables[table].length = i + 1;
        tables[table].keys[i] = key;
    }
    tables[table].values[i] = value;
}

static void table_set_deep(table_id table, struct val key, struct val value)
{
    for (table_id e = table; e; e = tables[e].next) {
        uint32_t i = table_lookup_index(e, key);
        if (i < tables[e].length) {
            tables[e].values[i] = value;
            return;
        }
    }
    table_set(table, key, value);
}

static void table_push(table_id *table)
{
    table_id pushed = alloc_table();
    tables[pushed].next = *table;
    *table = pushed;
}

static void table_pop(table_id *table)
{
    *table = tables[*table].next;
}

static void mark_val(struct val val, enum mark next_mark);
static void mark(table_id index, enum mark next_mark)
{
    while (index && tables[index].mark != next_mark) {
        struct table *table = &tables[index];
        table->mark = next_mark;
        for (uint32_t i = 0; i < table->length; ++i) {
            mark_val(table->keys[i], next_mark);
            mark_val(table->values[i], next_mark);
        }
        index = table->next;
    }
}
static void mark_val(struct val val, enum mark next_mark)
{
    if (val.type == TYPE_TABLE || val.type == TYPE_STRING)
        mark(val.table, next_mark);
    else if (val.type == TYPE_CLOSURE)
        mark(val.closure.environment_table, next_mark);
}

static table_id alloc_table(void)
{
    table_id table = alloc_env_table();
    tables[table].next = stack_root;
    stack_root = table;
    return table;
}

static table_id alloc_env_table(void)
{
    if (bytes_in_use >= byte_sweep_limit) {
        enum mark next = current_mark == MARK_BLACK ? MARK_WHITE : MARK_BLACK;
        mark(environment, next);
        mark(stack_root, next);
        // Add unreachable tables to the free list.
        for (table_id i = 0; i < number_of_tables; ++i) {
            if (tables[i].mark != current_mark)
                continue;
            free(tables[i].keys);
            free(tables[i].values);
            bytes_in_use -= tables[i].capacity * 2 * sizeof(struct val);
            bytes_in_use -= sizeof(struct table);
            tables[i].mark = MARK_FREE;
            tables[i].next = free_list;
            free_list = i;
        }
        byte_sweep_limit = bytes_in_use * 3 / 2 + 128;
        current_mark = next;
    }
    if (!free_list) {
        uint32_t n = number_of_tables * 3 / 2 + 32;
        tables = realloc(tables, sizeof(struct table) * n);
        for (table_id i = number_of_tables; i < n; ++i) {
            tables[i].mark = MARK_FREE;
            tables[i].next = free_list;
            free_list = i;
        }
        number_of_tables = n;
    }
    table_id i = free_list;
    free_list = tables[i].next;
    memset(&tables[i], 0, sizeof(struct table));
    tables[i].mark = current_mark;
    bytes_in_use += sizeof(struct table);
    return i;
}

static table_id alloc_string(size_t length)
{
    table_id i = alloc_table();
    tables[i].keys = calloc(length, 1);
    // We pretend like the string is a list of keys for memory accounting.
    tables[i].capacity =
     (uint32_t)(length + 2 * sizeof(struct val) - 1) / (2 * sizeof(struct val));
    bytes_in_use += tables[i].capacity * 2 * sizeof(struct val);
    return i;
}

static char *get_string(table_id index)
{
    return (char *)tables[index].keys;
}

static struct val number_val(double number)
{
    return (struct val){ .type = TYPE_NUMBER, .number = number };
}

static struct val string_val(const char *string, size_t len)
{
    table_id i = alloc_string(len + 1);
    memcpy(get_string(i), string, len);
    get_string(i)[len] = '\0';
    return (struct val){ .type = TYPE_STRING, .table = i };
}

static struct val string_for_identifier(struct owl_ref ident_ref)
{
    struct parsed_identifier ident = parsed_identifier_get(ident_ref);
    return string_val(ident.identifier, ident.length);
}

static double eval_number(struct owl_ref number_expr)
{
    struct val a = eval_expr(number_expr);
    if (a.type != TYPE_NUMBER) {
        fprintf(stderr, "error: arithmetic can only be done on numbers\n");
        exit(-1);
    }
    return a.number;
}

static struct val builtin_print(struct owl_ref arg)
{
    for (; !arg.empty; arg = owl_next(arg))
        val_print(stdout, eval_expr(arg));
    return false_val;
}

static struct val builtin_println(struct owl_ref arg)
{
    for (; !arg.empty; arg = owl_next(arg))
        val_print(stdout, eval_expr(arg));
    printf("\n");
    return false_val;
}

static struct val builtin_isspace(struct owl_ref arg)
{
    if (arg.empty)
        return false_val;
    struct val v = eval_expr(arg);
    if (v.type != TYPE_STRING)
        return false_val;
    char c = get_string(v.table)[0];
    return (c == ' ' || c == '\t' || c == '\r' || c == '\n') ?
     true_val : false_val;
}

static struct val builtin_isdigit(struct owl_ref arg)
{
    if (arg.empty)
        return false_val;
    struct val v = eval_expr(arg);
    if (v.type != TYPE_STRING)
        return false_val;
    char c = get_string(v.table)[0];
    return (c >= '0' && c <= '9') ? true_val : false_val;
}

static struct val builtin_todigit(struct owl_ref arg)
{
    if (arg.empty)
        return number_val(0);
    struct val v = eval_expr(arg);
    if (v.type != TYPE_STRING)
        return number_val(0);
    char c = get_string(v.table)[0];
    if (c >= '0' && c <= '9')
        return number_val(c - '0');
    else
        return number_val(0);
}

static struct val builtin_read_input_length(struct owl_ref arg)
{
    if (arg.empty)
        return false_val;
    struct val v = eval_expr(arg);
    if (v.type != TYPE_NUMBER)
        return false_val;
    size_t len = v.number;
    table_id string = alloc_string(len + 1);
    size_t n = fread(get_string(string), len, 1, stdin);
    get_string(string)[n] = '\0';
    return (struct val){ .type = TYPE_STRING, .table = string };
}
