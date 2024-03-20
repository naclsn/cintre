#include "parser.h"

lex_state ls = {0};
FILE* result = NULL;
dyarr(struct seen {
    bufsl name;
}) seen = {0};

void cleanup(void) {
    ldel(&ls);
    if (stdout != result) fclose(result); // yyy
}

void emit(void* usl, declaration const* decl, bufsl* tok) {
    search_namespace(decl->name, seen) return;
    struct seen* p = dyarr_push(&seen);
    if (!p) exitf("OOM");
    p->name = decl->name;

    fprintf(result, "%.*s;\n", (unsigned)decl->name.len, decl->name.ptr);
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char const* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry-file> [-D...,-I...] -o <result>", prog);
    char const* file = (argc--, *argv++);
    result = stdout; // yyy

    while (argc) {
        char const* arg = (argc--, *argv++);
        char* val;

        if ('-' == *arg) switch (arg[1]) {
        case 'D':
            val = strchr(arg, '=');
            if (val) *(val++) = '\0';
            else val = "1";
            ldef(&ls, arg+2, val);
            break;

        case 'I':
            linc(&ls, arg+2);
            break;

        case 'o':
            result = fopen(arg[2] ? arg+2 : (argc--, *argv++), "wb");
            break;
        }
    }

    if (!result) exitf("Missing result operand or file not writable");

    lini(&ls, file);

    bufsl tok = lext(&ls);
    declaration base = {0};
    while (tok.len) if ((tok = parse_declaration(&ls, NULL, emit, tok, &base)).len) switch (*tok.ptr) {
    case '{':
        for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
            bool c = '}' == *tok.ptr;
            if (!tok.len || (!depth && c)) break;
            depth+= ('{' == *tok.ptr)-c;
        }
        tok = lext(&ls);
        base = (declaration){0}; // reset
        continue;

    case '=':
        tok = parse_expression(&ls, lext(&ls));
        if (tok.len && ';' == *tok.ptr)
    case ';':
            base = (declaration){0}; // reset
        // fall through
    case ',':
        tok = lext(&ls);
        continue;
    }

    return EXIT_SUCCESS;
} // main

