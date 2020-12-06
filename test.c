#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

typedef enum {
    TOpenBrace,
    TClosedBrace,
    TOpenParen,
    TClosedParen,
    TOpenBracket,
    TClosedBracket,
    TPlus,
    TMinus,
    TMul,
    TDiv,
    TPlusEq,
    TMinusEq,
    TMulEq,
    TDivEq,
    TSemicolon,
    TComma,
    TEq,
    TComp,
    TAmpersand,
    TInout,
    TIf,
    TElse,
    TWhile,
    TDo,
    TLoop,
    TRet,
    TInt,
    TVoid,
    TAsm,
    TConst,
    TAlias,
    TFalse,
    TTrue,
    TLit,
    TString,
    TIdent,
    T_EOF,
    T_ERROR,
} Token;

#include "output.c"

typedef struct {
    const unsigned char *text;
    size_t length;
    size_t next_index;
} LexerState;

LexResult next(LexerState *const state) {
    LexResult result = next_token(state->text, state->next_index, state->length);
    state->next_index = result.end;
    return result;
}

// Returns -1 for failure, 0 for success
static int from_file(const char *const filename, LexerState *out) {
    char *buffer = NULL;
    long length;
    FILE *const f = fopen (filename, "rb");
    if (!f)
        return -1;

    if (fseek(f, 0, SEEK_END) == -1)
        return -1;
    if ((length = ftell(f)) == -1)
        return -1;
    if (fseek(f, 0, SEEK_SET) == -1)
        return -1;

    buffer = malloc(length + 1);
    if (!buffer)
        return -1;

    fread(buffer, 1, length, f);
    buffer[length] = '\0';
    fclose(f);

    *out = (LexerState) {
        .text = (const unsigned char *) buffer,
        .length = (size_t) length,
        .next_index = 0,
    };
    return 0;
}

int main(int argc, char **argv) {
    LexerState st;
    if (from_file(argv[1], &st) == -1) {
        printf("Could not load file %s\n", argv[1]);
        return 1;
    }

    long count = 0;
    long ident_count = 0;
    while (true) {
        LexResult r = next(&st);
        if (r.token_type == T_ERROR)
            printf("Error at %lu-%lu\n", r.start, r.end);
        else if (r.token_type == T_EOF)
            break;
        else
            count++;
        if (r.token_type == TIdent)
            ident_count++;
    }

    printf("There were %ld tokens, of which %ld were identifiers\n", count,
            ident_count);
}
