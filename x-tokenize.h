// These "x-files" are written in a somewhat unusual way.  When interpreting a
// grammar, we use the code in this file directly.  When compiling a grammar, we
// turn this code into a string to include in the generated file.  To avoid
// involving any external build tools, we enclose the entire file in a macro
// invocation.  The interpreter includes the source directly, while the compiler
// redefines the macro to return the source code as a string.

#ifndef TOKEN_T
#define TOKEN_T uint32_t
#endif
#ifndef STATE_T
#define STATE_T uint32_t
#endif

#ifndef TOKENIZE_BODY
#define TOKENIZE_BODY(...) __VA_ARGS__
#endif

#ifndef READ_KEYWORD_TOKEN
#define READ_KEYWORD_TOKEN(...) (0)
#endif

#ifndef WRITE_NUMBER_TOKEN
#define WRITE_NUMBER_TOKEN(...)
#endif
#ifndef WRITE_IDENTIFIER_TOKEN
#define WRITE_IDENTIFIER_TOKEN(...)
#endif
#ifndef WRITE_STRING_TOKEN
#define WRITE_STRING_TOKEN(...)
#endif

#ifndef ALLOW_DASHES_IN_IDENTIFIERS
#define ALLOW_DASHES_IN_IDENTIFIERS(...) false
#endif

// FIXME: I'm not sure where to put this "function".
#define SHOULD_ALLOW_DASHES_IN_IDENTIFIERS(combined) \
 (find_token((combined)->tokens, (combined)->number_of_keyword_tokens, "-", 1, \
  TOKEN_DONT_CARE) >= (combined)->number_of_keyword_tokens)

#if !defined(IDENTIFIER_TOKEN) || !defined(NUMBER_TOKEN) || \
 !defined(STRING_TOKEN) || !defined(BRACKET_TRANSITION_TOKEN)
#error The built-in tokenizer needs definitions of basic tokens to work.
#endif

// TODO: 4096 seems to be a decent value for this.  Do some more testing later.
#define TOKEN_RUN_LENGTH 4096

TOKENIZE_BODY
(

struct bluebird_token_run {
    struct bluebird_token_run *prev;
    uint16_t number_of_tokens;
    uint16_t lengths_size;
    uint8_t lengths[TOKEN_RUN_LENGTH * 2];
    TOKEN_T tokens[TOKEN_RUN_LENGTH];
    STATE_T states[TOKEN_RUN_LENGTH];
};

struct bluebird_default_tokenizer {
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

// TODO: Unicode.
static bool char_is_alphabetic(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
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

static bool encode_length(struct bluebird_token_run *run,
 uint16_t *lengths_size, size_t length)
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

static bool encode_token_length(struct bluebird_token_run *run,
 uint16_t *lengths_size, size_t length, size_t whitespace)
{
    uint16_t size = *lengths_size;
    if (encode_length(run, lengths_size, length) &&
     encode_length(run, lengths_size, whitespace))
        return true;
    *lengths_size = size;
    return false;
}

static size_t decode_length(struct bluebird_token_run *run,
 uint16_t *length_offset)
{
    size_t length = 0;
    size_t shift_amount = 0;
    while (*length_offset < sizeof(run->lengths) &&
     shift_amount < sizeof(size_t) * 8) {
        size_t l = run->lengths[(*length_offset)--];
        length += (l & 0x7f) << shift_amount;
        shift_amount += 7;
        if (!(l & 0x80))
            return length;
    }
    // A length was improperly encoded.
    abort();
}

static size_t decode_token_length(struct bluebird_token_run *run,
 uint16_t *length_offset, size_t *string_offset)
{
    size_t whitespace = decode_length(run, length_offset);
    size_t length = decode_length(run, length_offset);
    *string_offset -= whitespace + length;
    return length;
}

static bool bluebird_default_tokenizer_advance(struct bluebird_default_tokenizer
 *tokenizer, struct bluebird_token_run **previous_run)
{
    struct bluebird_token_run *run = malloc(sizeof(struct bluebird_token_run));
    uint16_t number_of_tokens = 0;
    uint16_t lengths_size = 0;
    const char *text = tokenizer->text;
    size_t whitespace = tokenizer->whitespace;
    size_t offset = tokenizer->offset;
    while (number_of_tokens < TOKEN_RUN_LENGTH) {
        char c = text[offset];
        if (c == '\0')
            break;
        if (char_is_whitespace(c)) {
            whitespace++;
            offset++;
            continue;
        }
        TOKEN_T token;
        bool is_token = false;
        bool end_token = false;
        size_t token_length = READ_KEYWORD_TOKEN(&token, &end_token,
         text + offset, tokenizer->info);
        if (token_length > 0)
            is_token = true;
        double number = 0;
        if (char_is_numeric(c) ||
         (c == '.' && char_is_numeric(text[offset + 1]))) {
            // Number.
            const char *start = (const char *)text + offset;
            char *rest = 0;
            number = strtod(start, &rest);
            if (rest > start) {
                token_length = rest - start;
                is_token = true;
                end_token = false;
                token = NUMBER_TOKEN;
            }
        } else if (c == '\'' || c == '"') {
            // String.
            size_t string_offset = offset + 1;
            while (text[string_offset] != '\0') {
                if (text[string_offset] == c) {
                    token_length = string_offset + 1 - offset;
                    is_token = true;
                    end_token = false;
                    token = STRING_TOKEN;
                    break;
                }
                if (text[string_offset] == '\\') {
                    // TODO: Escapes.
                    string_offset++;
                    if (text[string_offset] == '\0')
                        break;
                }
                string_offset++;
            }
        } else if (char_starts_identifier(c)) {
            // Identifier.
            size_t identifier_offset = offset + 1;
            while (char_continues_identifier(text[identifier_offset],
             tokenizer->info))
                identifier_offset++;
            if (identifier_offset - offset > token_length) {
                token_length = identifier_offset - offset;
                is_token = true;
                end_token = false;
                token = IDENTIFIER_TOKEN;
            }
        }
        if (!is_token || token == SYMBOL_EPSILON) {
            // Error.
            // TODO: Report the error in a better way?
            tokenizer->offset = offset;
            tokenizer->whitespace = whitespace;
            free(run);
            return false;
        }
        if (!encode_token_length(run, &lengths_size, token_length, whitespace))
            break;
        if (token == IDENTIFIER_TOKEN) {
            WRITE_IDENTIFIER_TOKEN(offset, token_length, tokenizer->info);
        } else if (token == NUMBER_TOKEN) {
            WRITE_NUMBER_TOKEN(offset, token_length, number, tokenizer->info);
        } else if (token == STRING_TOKEN) {
            WRITE_STRING_TOKEN(offset, token_length, offset + 1,
             token_length - 2, tokenizer->info);
        }
        run->tokens[number_of_tokens] = token;
        whitespace = 0;
        number_of_tokens++;
        offset += token_length;
        if (end_token) {
            if (number_of_tokens >= TOKEN_RUN_LENGTH)
                break;
            run->tokens[number_of_tokens] = BRACKET_TRANSITION_TOKEN;
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

)
