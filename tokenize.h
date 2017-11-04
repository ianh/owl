// This file is written in a somewhat unusual way.  When interpreting a grammar,
// we use this tokenizer code directly.  When compiling a grammar, we generate a
// tokenizer based on this code.  To avoid involving any external build tools,
// we enclose the entire file in a macro invocation -- the interpreter includes
// the source directly, while the compiler defines TOKENIZE_BODY(...) to return
// the source as a string.

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
static size_t read_keyword_token(TOKEN_T *token, bool *end_token,
 const unsigned char *text, size_t length, void *info)
{
    return 0;
}
#define READ_KEYWORD_TOKEN read_keyword_token
#endif

#if !defined(IDENTIFIER_TOKEN) || !defined(NUMBER_TOKEN) || \
 !defined(STRING_TOKEN) || !defined(BRACKET_TRANSITION_TOKEN)
#error The built-in tokenizer needs definitions of basic tokens to work.
#endif

#define TOKEN_RUN_LENGTH 8

TOKENIZE_BODY
(

struct bluebird_token_run {
    struct bluebird_token_run *prev;
    struct bluebird_token_run **next;
    uint16_t number_of_tokens;
    uint16_t number_of_lengths;
    uint8_t lengths[TOKEN_RUN_LENGTH * 2];
    TOKEN_T tokens[TOKEN_RUN_LENGTH];
    STATE_T states[TOKEN_RUN_LENGTH];
};

struct bluebird_default_tokenizer {
    const unsigned char *text;
    size_t length;
    size_t offset;

    size_t whitespace;

    TOKEN_T identifier_token;
    TOKEN_T number_token;
    TOKEN_T string_token;

    // The `info` pointer is passed to READ_KEYWORD_TOKEN.
    void *info;
};

static bool char_is_whitespace(unsigned char c)
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

static bool char_is_numeric(unsigned char c)
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

static bool char_continues_identifier(char c)
{
    return char_is_numeric(c) || char_starts_identifier(c);
}

static bool bluebird_default_tokenizer_advance(struct bluebird_default_tokenizer
 *tokenizer, struct bluebird_token_run **previous_run)
{
    if (tokenizer->offset >= tokenizer->length)
        return false;
    struct bluebird_token_run *run = malloc(sizeof(struct bluebird_token_run));
    uint16_t number_of_tokens = 0;
    uint16_t number_of_lengths = 0;
    const unsigned char *text = tokenizer->text;
    size_t whitespace = tokenizer->whitespace;
    size_t offset = tokenizer->offset;
    size_t length = tokenizer->length;
    while (offset < length && number_of_tokens < TOKEN_RUN_LENGTH) {
        unsigned char c = text[offset];
        if (char_is_whitespace(c)) {
            whitespace++;
            offset++;
            continue;
        }
        TOKEN_T token;
        bool is_token = false;
        bool end_token = false;
        size_t token_length = READ_KEYWORD_TOKEN(&token, &end_token,
         text + offset, length - offset, tokenizer->info);
        if (token_length > 0)
            is_token = true;
        if (char_is_numeric(c) || c == '.') {
            // Number.
            // TODO: Exponents, maybe actually parse the number?
            bool decimal = false;
            bool is_number = false;
            size_t number_offset = offset;
            while (number_offset < length) {
                unsigned char c = text[number_offset];
                if (!decimal && c == '.') {
                    decimal = true;
                    offset++;
                    continue;
                }
                if (!char_is_numeric(c))
                    break;
                number_offset++;
                is_number = true;
            }
            if (is_number && number_offset - offset > token_length) {
                token_length = number_offset - offset;
                is_token = true;
                end_token = false;
                token = NUMBER_TOKEN;
            }
        } else if (c == '\'' || c == '"') {
            // String.
            token = STRING_TOKEN;
            // ...
        } else if (char_starts_identifier(c)) {
            size_t identifier_offset = offset + 1;
            while (identifier_offset < length &&
             char_continues_identifier(text[identifier_offset])) {
                identifier_offset++;
            }
            if (identifier_offset - offset > token_length) {
                token_length = identifier_offset - offset;
                is_token = true;
                end_token = false;
                token = IDENTIFIER_TOKEN;
            }
        }
        if (!is_token) {
            // Error.
            free(run);
            return false;
        }
        run->tokens[number_of_tokens] = token;
        if (end_token) {
            if (number_of_tokens + 1 >= TOKEN_RUN_LENGTH)
                break;
            run->tokens[number_of_tokens + 1] = BRACKET_TRANSITION_TOKEN;
            number_of_tokens++;
        }
        // Encode the length here.
        whitespace = 0;
        number_of_tokens++;
        offset += token_length;
    }
    if (number_of_tokens == 0) {
        free(run);
        return false;
    }
    tokenizer->offset = offset;
    tokenizer->whitespace = whitespace;
    if (*previous_run)
        (*previous_run)->next = &run->prev;
    run->prev = *previous_run;
    run->next = previous_run;
    run->number_of_tokens = number_of_tokens;
    run->number_of_lengths = number_of_lengths;
    *previous_run = run;
    return true;
}

)
