// These "x-files" are written in a somewhat unusual way.  When interpreting a
// grammar, we use the code in this file directly.  When compiling a grammar, we
// turn this code into a string to include in the generated file.  To avoid
// involving any external build tools, we enclose the source code in a macro
// invocation.  The interpreter includes the source directly, while the compiler
// redefines the macro to return the source code as a string.

// FIXME: We should probably support Unicode.

#ifndef TOKEN_T
#define TOKEN_T uint32_t
#endif
#ifndef STATE_T
#define STATE_T uint32_t
#endif

#ifndef TOKENIZE_BODY
#define TOKENIZE_BODY(...) __VA_ARGS__
#endif

#ifndef READ_WHITESPACE
#define READ_WHITESPACE(...) (0)
#endif

#ifndef READ_KEYWORD_TOKEN
#define READ_KEYWORD_TOKEN(...) (0)
#endif

#ifndef READ_CUSTOM_TOKEN
#define READ_CUSTOM_TOKEN(...) (0)
#endif

#ifndef NUMBER_TOKEN_DATA
#define NUMBER_TOKEN_DATA(name) double name = 0
#endif

#ifndef INTEGER_TOKEN_DATA
#define INTEGER_TOKEN_DATA(name) uint64_t name = 0
#endif

#ifndef CUSTOM_TOKEN_DATA
#define CUSTOM_TOKEN_DATA(...) do { } while (0)
#endif

#ifndef STRING_TOKEN_HAS_ESCAPES
#define STRING_TOKEN_HAS_ESCAPES(name) bool name = false
#endif

#ifndef IF_NUMBER_TOKEN
#define IF_NUMBER_TOKEN(cond, ...) if (cond) __VA_ARGS__
#endif
#ifndef IF_STRING_TOKEN
#define IF_STRING_TOKEN(cond, ...) if (cond) __VA_ARGS__
#endif
#ifndef IF_IDENTIFIER_TOKEN
#define IF_IDENTIFIER_TOKEN(cond, ...) if (cond) __VA_ARGS__
#endif
#ifndef IF_INTEGER_TOKEN
#define IF_INTEGER_TOKEN(cond, ...) if (cond) __VA_ARGS__
#endif

#ifndef WRITE_NUMBER_TOKEN
#define WRITE_NUMBER_TOKEN(...)
#endif
#ifndef WRITE_IDENTIFIER_TOKEN
#define WRITE_IDENTIFIER_TOKEN(...)
#endif
#ifndef WRITE_INTEGER_TOKEN
#define WRITE_INTEGER_TOKEN(...)
#endif
#ifndef WRITE_STRING_TOKEN
#define WRITE_STRING_TOKEN(...)
#endif
#ifndef WRITE_CUSTOM_TOKEN
#define WRITE_CUSTOM_TOKEN(...)
#endif

#ifndef ALLOCATE_STRING
#define ALLOCATE_STRING(n, info) malloc(n)
#endif

#ifndef ESCAPE_CHAR
#define ESCAPE_CHAR(c, info) (c)
#endif

#ifndef ALLOW_DASHES_IN_IDENTIFIERS
#define ALLOW_DASHES_IN_IDENTIFIERS(...) false
#endif

// Single-character string escapes.
#define ESCAPE_CHAR_SINGLE(c, info) \
 ((c) == 'b' ? '\b' : \
  (c) == 'f' ? '\f' : \
  (c) == 'n' ? '\n' : \
  (c) == 'r' ? '\r' : \
  (c) == 't' ? '\t' : (c))

#if !defined(IDENTIFIER_TOKEN) || !defined(INTEGER_TOKEN) || \
 !defined(NUMBER_TOKEN) || !defined(STRING_TOKEN) || \
 !defined(BRACKET_SYMBOL_TOKEN) || !defined(COMMENT_TOKEN)
#error The built-in tokenizer needs definitions of basic tokens to work.
#endif

#ifndef OWL_DONT_INLINE
#if defined(__clang__) || defined(__GNUC__)
#define OWL_DONT_INLINE __attribute__((noinline))
#else
#define OWL_DONT_INLINE
#endif
#endif

#define TOKEN_RUN_LENGTH 4096

TOKENIZE_BODY
(

struct owl_token_run {
    struct owl_token_run *prev;
    uint16_t number_of_tokens;
    uint16_t lengths_size;
    uint8_t lengths[TOKEN_RUN_LENGTH * 2];
    TOKEN_T tokens[TOKEN_RUN_LENGTH];
    STATE_T states[TOKEN_RUN_LENGTH];
};

struct owl_default_tokenizer {
    const char *text;
    size_t offset;

    size_t whitespace;

    TOKEN_T identifier_token;
    TOKEN_T number_token;
    TOKEN_T string_token;

    // The `info` pointer is passed to READ_KEYWORD_TOKEN.
    void *info;
};

static bool char_is_whitespace(char c)
{
    switch (c) {
    case ' ':
    case '\t':
    case '\r':
    case '\n':
        return true;
    default:
        return false;
    }
}

static bool char_is_numeric(char c)
{
    return c >= '0' && c <= '9';
}

static bool char_is_alphabetic(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool char_is_hexadecimal_alpha(char c)
{
    return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static bool char_starts_identifier(char c)
{
    return char_is_alphabetic(c) || c == '_';
}

static bool char_continues_identifier(char c, void *info)
{
    if (ALLOW_DASHES_IN_IDENTIFIERS(info) && c == '-')
        return true;
    return char_is_numeric(c) || char_starts_identifier(c);
}

static bool char_ends_identifier(char c)
{
    return c != '-';
}

static bool encode_length(struct owl_token_run *run, uint16_t *lengths_size,
 size_t length)
{
    uint8_t mark = 0;
    while (*lengths_size < sizeof(run->lengths)) {
        run->lengths[*lengths_size] = mark | (length & 0x7f);
        mark = 0x80;
        length >>= 7;
        (*lengths_size)++;
        if (length == 0)
            return true;
    }
    return false;
}

static bool encode_token_length(struct owl_token_run *run,
 uint16_t *lengths_size, size_t length, size_t whitespace)
{
    uint16_t size = *lengths_size;
    if (encode_length(run, lengths_size, length) &&
     encode_length(run, lengths_size, whitespace))
        return true;
    *lengths_size = size;
    return false;
}

static size_t decode_length(struct owl_token_run *run, uint16_t *length_offset)
{
    size_t length = 0;
    while (*length_offset < sizeof(run->lengths)) {
        size_t l = run->lengths[(*length_offset)--];
        length <<= 7;
        length += l & 0x7f;
        if (!(l & 0x80))
            return length;
    }
    // A length was improperly encoded.
    abort();
}

static size_t decode_token_length(struct owl_token_run *run,
 uint16_t *length_offset, size_t *string_offset)
{
    size_t whitespace = decode_length(run, length_offset);
    size_t length = decode_length(run, length_offset);
    *string_offset -= whitespace + length;
    return length;
}

// Not inlining this function improves performance (clang-1000.11.45.5 at -O3).
static bool OWL_DONT_INLINE
owl_default_tokenizer_advance(struct owl_default_tokenizer *tokenizer,
 struct owl_token_run **previous_run)
{
    struct owl_token_run *run = malloc(sizeof(struct owl_token_run));
    if (!run)
        return false;
    uint16_t number_of_tokens = 0;
    uint16_t lengths_size = 0;
    const char *text = tokenizer->text;
    size_t whitespace = tokenizer->whitespace;
    size_t offset = tokenizer->offset;
    while (number_of_tokens < TOKEN_RUN_LENGTH) {
        char c = text[offset];
        if (c == '\0')
            break;
        size_t whitespace_length = READ_WHITESPACE(text + offset,
         tokenizer->info);
        if (whitespace_length > 0) {
            whitespace += whitespace_length;
            offset += whitespace_length;
            continue;
        }
        TOKEN_T token = -1;
        CUSTOM_TOKEN_DATA(custom_data);
        INTEGER_TOKEN_DATA(integer);
        NUMBER_TOKEN_DATA(number);
        STRING_TOKEN_HAS_ESCAPES(has_escapes);
        bool is_token = false;
        bool end_token = false;
        bool custom_token = false;
        bool comment = false;
        bool custom_whitespace = false;
        size_t token_length = READ_KEYWORD_TOKEN(&token, &end_token,
         text + offset, tokenizer->info);
        if (token_length > 0) {
            is_token = true;
            if (token == COMMENT_TOKEN)
                comment = true;
        }
        if (READ_CUSTOM_TOKEN(&token, &token_length, text + offset,
         &custom_whitespace, &custom_data, tokenizer->info)) {
            is_token = true;
            custom_token = true;
            end_token = false;
            comment = false;
        }
        IF_INTEGER_TOKEN(char_is_numeric(c), {
            // Integer.
            size_t integer_offset = offset;
            uint64_t base = 10;
            if (c == '0' && (text[offset + 1] == 'x' || text[offset + 1] == 'X') && (char_is_numeric(text[offset + 2]) || char_is_hexadecimal_alpha(text[integer_offset + 2]))) {
                integer_offset = offset + 2;
                base = 16;
            }
            integer = 0;
            bool overflow = false;
            while (char_is_numeric(text[integer_offset]) || (base == 16 && char_is_hexadecimal_alpha(text[integer_offset]))) {
                uint64_t last = integer;
                integer *= base;
                uint64_t ch = text[integer_offset];
                if (ch >= '0' && ch <= '9')
                    integer += ch - '0';
                else if (ch >= 'a' && ch <= 'f')
                    integer += ch - 'a' + 0xa;
                else if (ch >= 'A' && ch <= 'F')
                    integer += ch - 'A' + 0xA;
                if (integer < last) {
                    overflow = true;
                    break;
                }
                integer_offset++;
            }
            if (!overflow && integer_offset - offset > token_length) {
                token_length = integer_offset - offset;
                is_token = true;
                end_token = false;
                comment = false;
                custom_whitespace = false;
                token = INTEGER_TOKEN;
            }
        })
        IF_NUMBER_TOKEN(char_is_numeric(c) ||
         (c == '.' && char_is_numeric(text[offset + 1])), {
            // Number.
            const char *start = (const char *)text + offset;
            char *rest = 0;
            number = strtod(start, &rest);
            if (rest > start && rest - start > token_length) {
                token_length = rest - start;
                is_token = true;
                end_token = false;
                comment = false;
                custom_whitespace = false;
                token = NUMBER_TOKEN;
            }
        }) else IF_STRING_TOKEN(c == '\'' || c == '"', {
            // String.
            size_t string_offset = offset + 1;
            while (text[string_offset] != '\0') {
                if (text[string_offset] == c) {
                    token_length = string_offset + 1 - offset;
                    is_token = true;
                    end_token = false;
                    comment = false;
                    custom_whitespace = false;
                    token = STRING_TOKEN;
                    break;
                }
                if (text[string_offset] == '\\') {
                    has_escapes = true;
                    string_offset++;
                    if (text[string_offset] == '\0')
                        break;
                }
                string_offset++;
            }
        }) else IF_IDENTIFIER_TOKEN(char_starts_identifier(c), {
            // Identifier.
            size_t identifier_offset = offset + 1;
            while (char_continues_identifier(text[identifier_offset],
             tokenizer->info))
                identifier_offset++;
            while (!char_ends_identifier(text[identifier_offset - 1]))
                identifier_offset--;
            if (identifier_offset - offset > token_length) {
                token_length = identifier_offset - offset;
                is_token = true;
                end_token = false;
                comment = false;
                custom_whitespace = false;
                token = IDENTIFIER_TOKEN;
            }
        })
        if (custom_whitespace) {
            whitespace += token_length;
            offset += token_length;
            continue;
        } else if (comment) {
            while (text[offset] != '\0' && text[offset] != '\n') {
                whitespace++;
                offset++;
            }
            continue;
        } else if (!is_token || token == COMMENT_TOKEN) {
            tokenizer->offset = offset;
            tokenizer->whitespace = whitespace;
            free(run);
            return false;
        }
        if (end_token && number_of_tokens + 1 >= TOKEN_RUN_LENGTH)
            break;
        if (!encode_token_length(run, &lengths_size, token_length, whitespace))
            break;
        if (token == IDENTIFIER_TOKEN) {
            WRITE_IDENTIFIER_TOKEN(offset, token_length, tokenizer->info);
        } else if (token == INTEGER_TOKEN) {
            WRITE_INTEGER_TOKEN(offset, token_length, integer, tokenizer->info);
        } else if (token == NUMBER_TOKEN) {
            WRITE_NUMBER_TOKEN(offset, token_length, number, tokenizer->info);
        } else IF_STRING_TOKEN(token == STRING_TOKEN, {
            size_t content_offset = offset + 1;
            size_t content_length = token_length - 2;
            const char *string = text + content_offset;
            size_t string_length = content_length;
            if (has_escapes) {
                // Apply escape sequences.
                size_t i;
                for (i = 0; i < content_length; ++i) {
                    if (text[content_offset + i] == '\\') {
                        string_length--;
                        i++;
                    }
                }
                char *unescaped = ALLOCATE_STRING(string_length,
                 tokenizer->info);
                size_t j = 0;
                for (i = 0; i < content_length; ++i) {
                    if (text[content_offset + i] == '\\')
                        i++;
                    unescaped[j++] = ESCAPE_CHAR(text[content_offset + i],
                     tokenizer->info);
                }
                string = unescaped;
            }
            WRITE_STRING_TOKEN(offset, token_length, string, string_length,
             has_escapes, tokenizer->info);
        }) else if (custom_token) {
            WRITE_CUSTOM_TOKEN(offset, token_length, token, custom_data,
             tokenizer->info);
        }
        run->tokens[number_of_tokens] = token;
        whitespace = 0;
        number_of_tokens++;
        offset += token_length;
        if (end_token) {
            assert(number_of_tokens < TOKEN_RUN_LENGTH);
            run->tokens[number_of_tokens] = BRACKET_SYMBOL_TOKEN;
            number_of_tokens++;
        }
    }
    if (number_of_tokens == 0) {
        tokenizer->offset = offset;
        tokenizer->whitespace = whitespace;
        free(run);
        return false;
    }
    tokenizer->offset = offset;
    tokenizer->whitespace = whitespace;
    run->prev = *previous_run;
    run->number_of_tokens = number_of_tokens;
    run->lengths_size = lengths_size;
    *previous_run = run;
    return true;
}

// Here, 'run' must be the most recent run produced by the tokenizer.
static void find_token_range(struct owl_default_tokenizer *tokenizer,
 struct owl_token_run *run, uint16_t index, size_t *start, size_t *end)
{
    size_t offset = tokenizer->offset - tokenizer->whitespace;
    size_t last_offset = offset;
    size_t len = 0;
    uint16_t length_offset = run->lengths_size - 1;
    uint16_t j;
    for (j = index; j < run->number_of_tokens; ++j) {
        if (run->tokens[j] == BRACKET_SYMBOL_TOKEN)
            continue;
        last_offset = offset;
        len = decode_token_length(run, &length_offset, &offset);
    }
    *start = last_offset - len;
    *end = last_offset;
}

static void estimate_next_token_range(struct owl_default_tokenizer
 *tokenizer, size_t *start, size_t *end)
{
    *start = tokenizer->offset;
    size_t i = tokenizer->offset + 1;
    while (tokenizer->text[i] != '\0' && !char_is_whitespace(tokenizer->text[i])
     && !char_continues_identifier(tokenizer->text[i], tokenizer->info))
        i++;
    *end = i;
}

static void find_end_range(struct owl_default_tokenizer *tokenizer,
 size_t *start, size_t *end)
{
    *start = tokenizer->offset - tokenizer->whitespace - 1;
    *end = tokenizer->offset - tokenizer->whitespace;
    if (*start > *end) {
        *start = *end;
        *end += 1;
    }
}

)
