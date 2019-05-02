#include "parser.h"

#include <stdbool.h>
#include <stdio.h>

static struct owl_token tokenize(const char *text, void *info);
static uint64_t decode_hex(const char *chs);
static bool is_hex(char c);

int main(int argc, char *argv[])
{
    if (argc < 2) {
        fprintf(stderr, "usage: hex-color [color]\n");
        return -1;
    }
    struct owl_tree_options options = {
        .string = argv[1],
        .tokenize = tokenize,
    };
    struct owl_tree *tree = owl_tree_create_with_options(options);
    struct parsed_color color = owl_tree_get_parsed_color(tree);
    int r, g, b;
    if (!color.hex_color.empty) {
        struct parsed_hex_color hex = parsed_hex_color_get(color.hex_color);
        uint64_t v = hex.data.integer;
        r = (v >> 16) & 0xff;
        g = (v >> 8) & 0xff;
        b = v & 0xff;
    } else {
        r = parsed_number_get(color.r).number;
        g = parsed_number_get(color.g).number;
        b = parsed_number_get(color.b).number;
    }
    printf("r = %d\n", r);
    printf("g = %d\n", g);
    printf("b = %d\n", b);
    return 0;
}

static struct owl_token tokenize(const char *text, void *info)
{
    if (text[0] != '#')
        return owl_token_no_match;
    if (!is_hex(text[1]) || !is_hex(text[2]) || !is_hex(text[3]))
        return owl_token_no_match;
    if (!is_hex(text[4]) || !is_hex(text[5]) || !is_hex(text[6])) {
        return (struct owl_token){
            .type = OWL_TOKEN_HEX_COLOR,
            .length = 4, // #fff
            .data.integer = decode_hex((const char[]){
                text[1],text[1],
                text[2],text[2],
                text[3],text[3],
            }),
        };
    } else {
        return (struct owl_token){
            .type = OWL_TOKEN_HEX_COLOR,
            .length = 7, // #ffffff
            .data.integer = decode_hex(text + 1),
        };
    }
}

static uint64_t decode_hex(const char *chs)
{
    uint64_t result = 0;
    for (int i = 0; i < 6; ++i) {
        char c = chs[i];
        result <<= 4;
        if ('0' <= c && c <= '9')
            result += c - '0';
        else if ('a' <= c && c <= 'f')
            result += c - 'a' + 10;
        else if ('A' <= c && c <= 'F')
            result += c - 'A' + 10;
    }
    return result;
}

static bool is_hex(char c)
{
    return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
     ('A' <= c && c <= 'F');
}
