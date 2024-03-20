#include "preparer/common.h"
#include "preparer/lexer.h"
#include "preparer/parser.h"

/*
bufsl passthrough(bufsl it) {
    fprintf(stderr, "\x1b[36m%.*s \x1b[m", (unsigned)it.len, it.ptr);
    return it;
}
#define lext(_ls) passthrough(lext(_ls))
#define lextbang(_ls) (fprintf(stderr, "\x1b[31m!\x1b[m"), lext(_ls))
*/

void print_declaration(FILE ref strm, declaration cref decl);

void print_decl_type(FILE ref strm, struct decl_type cref type) {
#   define unkw(k) ((char[4]){((k)>>10&31)|96, ((k)>>5&31)|96, ((k)&31)|96})
    switch (type->kind) {
    case KIND_NOTAG:
        break;

    case KIND_STRUCT:
    case KIND_UNION:
        fprintf(strm, "\x1b[34m%s\x1b[m !!! ", unkw(type->kind));
        if ((size_t)-1 == type->info.obj.count) break;
        fprintf(strm, "{\n");
        for (struct decl_type_field* curr = type->info.obj.first; curr; curr = curr->next) {
            fprintf(strm, "\t");
            print_declaration(strm, curr->decl);
            fprintf(strm, ";\n");
        }
        fprintf(strm, "} ");
        break;

    case KIND_ENUM:
        fprintf(strm, "\x1b[33m%s\x1b[m ", unkw(type->kind));
        break;

    case KIND_PTR:
        fprintf(strm, "(");
        print_decl_type(strm, type->info.ptr);
        fprintf(strm, ")* ");
        break;

    case KIND_FUN:
        print_decl_type(strm, type->info.fun.ret);
        fprintf(strm, "!!!(");
        if (!type->info.fun.count) fprintf(strm, "\x1b[33mvoid\x1b[m");
        for (struct decl_type_param* curr = type->info.fun.first; curr; curr = curr->next) {
            print_declaration(strm, curr->decl);
            if (curr->next) fprintf(strm, ", ");
        }
        fprintf(strm, ") ");
        break;

    case KIND_ARR:
        print_decl_type(strm, type->info.arr.item);
        fprintf(strm, "!!![%zu] ", type->info.arr.count);
        break;
    }

    for (unsigned k = 0; type->quals[k]; k++) fprintf(strm, "\x1b[34m%s\x1b[m ", unkw(type->quals[k]));

    if (type->name.len) fprintf(strm, "\x1b[33m%.*s\x1b[m ", (unsigned)type->name.len, type->name.ptr);
    //else fprintf(strm, "\x1b[33mint\x1b[m ");
}

void print_declaration(FILE ref strm, declaration cref decl) {
    if (decl->spec) fprintf(strm, "\x1b[32m%s\x1b[m ", unkw(decl->spec));
    print_decl_type(strm, &decl->type);
    fprintf(strm, "\x1b[35m%.*s\x1b[m", (unsigned)decl->name.len, decl->name.ptr);
#   undef unkw
}

lex_state ls;

void cleanup(void) {
    ldel(&ls);
}

void show(void ref _, declaration cref decl, bufsl ref tok) {
    (void)_;
    print_declaration(stdout, decl);
    printf("; // %.*s\n", (unsigned)tok->len, tok->ptr);
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char const* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry-file> [-D...,-I...]", prog);
    char const* file = (argc--, *argv++);

    linc(&ls, "./");

    while (argc) {
        char const* arg = (argc--, *argv++);

        if (!memcmp("-D", arg, 2)) {
            char* val = strchr(arg, '=');
            if (val) *(val++) = '\0';
            else val = "1";
            ldef(&ls, arg+2, val);
        }

        else if (!memcmp("-I", arg, 2)) {
            linc(&ls, arg+2);
        }

        else notif("unused or unimplemented argument: %s", arg);
    } // while args

    lini(&ls, file);

    bufsl tok = lext(&ls);
    declaration base = {0};
    while (tok.len) if ((tok = parse_declaration(&ls, NULL, show, tok, &base)).len)
        switch (*tok.ptr) {
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

        default:
            exitf("other: %.*s", (unsigned)tok.len, tok.ptr);
        }

    return EXIT_SUCCESS;
} // main
