#include "parser.h"

lex_state ls = {0};
FILE* result = NULL;
dyarr(struct seen {
    bufsl name;
}) seen = {0};

#define bufmt(x) (unsigned)x.len, x.ptr

void cleanup(void) {
    ldel(&ls);
    if (result && stdout != result) fclose(result); // yyy
}

void emit_decl(declaration cref decl);
void emit_type(struct decl_type cref type);

void emit_type(struct decl_type cref type) {
    switch (type->kind) {
    case KIND_NOTAG: fprintf(result, "%.*s ", bufmt(type->name)); break;

    case KIND_STRUCT:
        fprintf(result, "struct ");
        if (0)
    case KIND_UNION:
            fprintf(result, "union ");
        //if (type->name.len) {
            notif("NIY: emit struct/union extra info");
        //}
        //if (-1 == type->info.obj.count) break;
        //fprintf(result, "{");
        //for (struct decl_type_field* curr = type->info.obj.first; curr; curr = curr->next) {
        //    emit_decl(curr->decl, depth+1);
        //    printf("\n");
        //}
        //fprintf(result, "}");
        break;

    case KIND_ENUM:   fprintf(result, "enum ");   break;
    case KIND_PTR:
        emit_type(type->info.ptr);
        fprintf(result, "*");
        break;
    case KIND_FUN: break;
    case KIND_ARR: break;
    }

    for (unsigned k = 0; type->quals[k]; k++) switch (type->quals[k]) {
    case QUAL_END: break;
    case QUAL_CONST:     fprintf(result, "const ");     break;
    case QUAL_RESTRICT:  fprintf(result, "restrict ");  break;
    case QUAL_VOLATILE:  fprintf(result, "volatile ");  break;
    case QUAL_INLINE:    fprintf(result, "inline ");    break;
    case QUAL_SIGNED:    fprintf(result, "signed ");    break;
    case QUAL_UNSIGNED:  fprintf(result, "unsigned ");  break;
    case QUAL_SHORT:     fprintf(result, "short ");     break;
    case QUAL_LONG:      fprintf(result, "long ");      break;
    case QUAL_COMPLEX:   fprintf(result, "complex ");   break;
    case QUAL_IMAGINARY: fprintf(result, "imaginary "); break;
    }
}

void emit_decl(declaration cref decl) {
    switch (decl->spec) {
    case SPEC_NONE: break;
    case SPEC_TYPEDEF:  fprintf(result, "typedef ");  break;
    case SPEC_EXTERN:   fprintf(result, "extern ");   break;
    case SPEC_STATIC:   fprintf(result, "static ");   break;
    case SPEC_AUTO:     fprintf(result, "auto ");     break;
    case SPEC_REGISTER: fprintf(result, "register "); break;
    }

    switch (decl->type.kind) {
    case KIND_NOTAG: break;
    case KIND_STRUCT: fprintf(result, "struct "); break;
    case KIND_UNION:  fprintf(result, "union ");  break;
    case KIND_ENUM:   fprintf(result, "enum ");   break;
    case KIND_PTR: break;

    case KIND_FUN:
        if ((size_t)-1 == decl->type.info.fun.count)
            exitf("%s:%zu: not supported, use (void) or specify the actual arguments", ls.file, ls.line);

        emit_type(decl->type.info.fun.ret);
        fprintf(result, "%.*s(", bufmt(decl->name));
        if (!decl->type.info.fun.count) printf("void");
        for (struct decl_type_param* curr = decl->type.info.fun.first; curr; curr = curr->next) {
            emit_type(&curr->decl->type);
            fprintf(result, "%.*s", bufmt(curr->decl->name));
            if (curr->next) fprintf(result, ", ");
        }
        fprintf(result, ");\n");

        fprintf(result, "void %.*s_adapt(char* ret, char** args) {\n", bufmt(decl->name));
        fprintf(result, (4 == decl->type.info.fun.ret->name.len && !memcmp("void", decl->type.info.fun.ret->name.ptr, 4))
                ? "    (void)ret;\n    "
                : "    *ret = ");
        fprintf(result, "%.*s(", bufmt(decl->name));
        if (!decl->type.info.fun.count) printf("void");
        size_t k = 0;
        for (struct decl_type_param* curr = decl->type.info.fun.first; curr; curr = curr->next) {
            fprintf(result, "*(");
            emit_type(&curr->decl->type);
            fprintf(result, "*)args[%zu]", k++);
            if (curr->next) fprintf(result, ", ");
        }
        fprintf(result, ");\n");
        fprintf(result, "}\n\n");

        break;

    case KIND_ARR:
        break;
    }
}

void emit(void* usl, declaration cref decl, bufsl ref tok) {
    if (KIND_FUN != decl->type.kind) return; // yyy

    search_namespace(decl->name, seen) return;
    struct seen* p = dyarr_push(&seen);
    if (!p) exitf("OOM");
    p->name = decl->name;

    emit_decl(decl);
}

int main(int argc, char** argv) {
    atexit(cleanup);
    char* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf("Usage: %s <entry-file> [-D...,-I...] -o <result>", prog);
    char* file = (argc--, *argv++);
    result = stdout; // yyy

    while (argc) {
        char* arg = (argc--, *argv++);
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
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }

    if (!result) exitf("Missing result operand or file not writable");
    fprintf(result, "#include \"adapter.h\"\n\n");

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

    fprintf(result, "#include \"cintre.c\"\n");

    return EXIT_SUCCESS;
} // main

