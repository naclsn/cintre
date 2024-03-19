#if 0
p=${0%/*}/../build/${0##*/}.exe
cc -ggdb $0 -o $p || exit 1
exec $p "$@"
#endif

#include "../preparer/parser.h"

// print (TODO: better, cleaner, simpler) {{{
void print_declaration(FILE ref strm, declaration cref decl);

void print_decl_type(FILE ref strm, struct decl_type cref type) {
#   define unkw(k) ((char[4]){((k)>>10&31)|96, ((k)>>5&31)|96, ((k)&31)|96})
    switch (type->kind) {
    case KIND_NOTAG:
        break;

    case KIND_STRUCT:
    case KIND_UNION:
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
// }}}

void show(void ref _, declaration cref decl, bufsl ref tok) {
    (void)_;
    print_declaration(stdout, decl);
    printf("; // %.*s\n", (unsigned)tok->len, tok->ptr);
}

int main(int argc, char** argv) {
    if (1 == argc) return puts("Usage: <prog> <filename>");

    lex_state ls = {0};
    lini(&ls, argv[1]);

    bufsl tok = lext(&ls);
    declaration base = {0};
    while (tok.len) {
        tok = parse_declaration(&ls, NULL, show, tok, &base);
        tok = lext(&ls);
    }

    ldel(&ls);
}
